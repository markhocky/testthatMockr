#' Create a Mock class to support unit testing.
#' 
#' The \code{Mock} object is designed to mimic another object's behaviour, and also 
#' provide functionality to record operations performed on it. The \code{Mock} can be 
#' used as a stub for an object yet to be developed, or it can be used to mimic an 
#' existing object.
#' The \code{Mock} can be provided with the name of a class to mimic via the \code{spec}
#' parameter. In this case the Mock will be assigned this class, which is particularly 
#' useful if the \code{Mock} is to be used in the slot of another S4 object.
#' The \code{spec} parameter is optional however, and if not provided the \code{Mock}
#' will simply be assigned the class "Mock".
#' 
#' @details 
#' The \code{Mock} contains two slots:
#' 
#' \code{calls} an environment used to store calls made on the \code{Mock} along 
#' with any arguments passed in as part of that call.
#' 
#' \code{methods} an environment containing the names of methods registered for the 
#' \code{Mock} along with the optional values to be returned from the call
#' 
#' These slots are not intended to be used directly, but rather are the containers used
#' by the \code{Mock}s methods, and for testing. 
#' 
#' @return A \code{Mock} object ready for use in your \code{testthat} unit tests.
#' 
#' @param spec a character string naming the class to be imitated.
#' 
#' @seealso \code{mockMethod}
#' 
#' @export 
Mock <- function(spec = NULL) {
	
	if (!missing(spec)) {
		if (is.name(substitute(spec))) {
			spec <- class(spec)[1]
		} else {
			as.character(spec)
		}
	}
	if (!any(grepl("mock_methods", search()))) {
		attach(NULL, pos = 2L, name = "mock_methods")
		# Intialise character vector for recording S3 generics which are created.
		assign("new_S3_generics", character(0), pos = "mock_methods")
	} else {
		mock.methods.pos <- grep("mock_methods", search())
		if (mock.methods.pos != 2) {
			mock.methods.env <- detach("mock_methods")
			attach(mock.methods.env, pos = 2L, name = "mock_methods")
		}
	}
	setClass("Mock",
			representation(
					methods = "environment", 
					calls = "environment"
			), contains = spec)
	mock <- new("Mock")
	mock@methods <- new.env(emptyenv())
	mock@calls <- new.env(emptyenv())
	return(mock)
}


#' Assign a method to a Mock object
#' 
#' The usefullness of \code{Mock} objects comes into full effect when they are assigned
#' a specific "mock method". The associated \code{Mock} will make note of calls made to 
#' this method along with any arguments and record these for future reporting.
#' The method is assigned to a particular \code{Mock} so calls of this method on another
#' \code{Mock} object will throw an error.
#' 
#' The same method can be applied to multiple \code{Mock} objects by passing in a list of
#' \code{Mock}s as the first parameter. This is effectively the same as calling the 
#' \code{mockMethod} repeatedly on each different \code{Mock}.
#' 
#' The optional \code{return.value} can be used to specifically control the output from 
#' the \code{Mock}, and different \code{return.value}s can be assigned to different 
#' \code{Mock} objects.
#' 
#' At this stage of development, only methods which take the \code{Mock} as their first
#' argument can be imitated.
#' 
#' The \code{mockMethod} function will attempt to create a method / function specific
#' to the mock without hurting any existing functions.
#' If there are no existing functions of name \code{method.name}, then an S4 method 
#' (with associated generic) is created.
#' If a function of that name exists, then \code{mockMethod} will turn this into generic 
#' S3 and S4 methods. The existing function will become the default, and the mock
#' function will be a method assigned for signature "Mock".
#' If the existing function is already an S3 or S4 generic, then \code{mockMethod} will 
#' create a method with signature of class "Mock".
#' 
#' For existing functions, the new generic is assigned in the same environment as the 
#' existing function (effectively overwriting it), and \code{Mock} methods that are
#' created are assigned in the \code{mockR} package environment. Note that this means
#' that there must be write permission to the environment where the existing function
#' resides. This will not always be the case, for example if trying to mock a 
#' non-exported function from a package. In these cases, a call to \link{unlockBinding} 
#' on the existing function prior to setting the mock method may work. This is 
#' potentially something to be included in future versions of \code{mockR}.
#' When an existing function is turned into a generic, the name of the method is also 
#' recorded in the \code{new_S3_generics} character vector (stored in \code{mock_methods})
#' so that it can be removed safely (i.e. on call to \code{cleanMockMethods}.
#' 
#' @examples
#' \dontrun{
#' mock <- Mock()
#' mock2 <- Mock()
#' mockMethod(mock, "TestMethod") # returns NULL
#' mockMethod(mock, "TestMethod", return.value = 1) # returns 1
#' mockMethod(list(mock, mock2), "TestMethod") # assigns "TestMethod" for mock and mock2
#' } 
#' 
#' @return Returns NULL, but called for it's side effect of setting up the mock methods.
#' 
#' @param mock The \code{Mock} object which the method is related to, or a list of 
#' \code{Mock}s.
#' @param method.name The method / function name.
#' @param return.value The (optional) value to be returned from future calls to 
#' \code{method.name}. Defaults to NULL.
#' 
#' @seealso \code{\link{Mock}} \code{cleanMockMethods}
#' 
#' @export
mockMethod <- function(mock, method.name, return.value = NULL) {
	
	if (is_list_of_mocks(mock)) {
		multiple_mock_method(mock, method.name, return.value)
	} else {
		method <- tryCatch(
				get(method.name, parent.frame()), 
				error = function(e) NULL)
		if (is.null(method)) {
			method <- make_S4_generic(method.name)
		}
		if (is_function_but_notS4(method)) {
			make_S3andS4_generics(method.name, mock, method)
			assign_S3_method(method.name, mock)
		}
		assign_S4_method(mock, method, method.name, return.value)
	}
}

is_list_of_mocks <- function(mock) {
	is.list(mock) && !isS4(mock)
}

is_function_but_notS4 <- function(method) {
	is.function(method) & !isS4(method)
}

make_S4_generic <- function(method.name) {
	generic <- paste0("function(mock, ...) standardGeneric(\"", method.name, "\")")
	generic <- parse(text = generic)
	setGeneric(method.name, eval(generic), where = "mock_methods")
	return(get(method.name))
}

make_S3andS4_generics <- function(method.name, mock, method) {
	if(no_dots_args(method)) {
		method <- add_dots_arg(method)
	}
	if (!is_S3generic(method)) {
		setAs_S3generic(method.name, method)
	}
	setGeneric(method.name, method, where = "mock_methods")
}

assign_S3_method <- function(method.name, mock) {
	assign(paste0(method.name, ".Mock"), create_mock_call(mock), 
			pos = "mock_methods")
}

is_S3generic <- function(method) {
	any(grepl("UseMethod", body(method)))
}

setAs_S3generic <- function(method.name, method) {
	generic <- method
	body(generic) <- parse(text = paste0("UseMethod(\"", method.name, "\")"))
	assign(method.name, generic, pos = environment(method))
	assign(paste0(method.name, ".default"), method, pos = environment(method))
	newS3generics <- get("new_S3_generics", pos = "mock_methods")
	assign("new_S3_generics", c(newS3generics, method.name), pos = "mock_methods")
}

no_dots_args <- function(method) {
	!"..." %in% names(formals(method))
}

add_dots_arg <- function(method) {
	formals(method) <- c(formals(method), alist("..." = ))
	return(method)
}

assign_S4_method <- function(mock, method, method.name, return.value) {
	mock.method <- create_mock_call(mock)
	formals(mock.method) <- formals(method)
	setMethod(method.name,
			signature("Mock"),
			mock.method, 
			where = "mock_methods")
	assign_method_to(mock, method.name, return.value)
}

assign_method_to <- function(mock, method.name, return.value) {
	method.name <- remove_classname(method.name, mock)
	mock@methods[[method.name]] <- return.value
}

multiple_mock_method <- function(mock.list, method.name, return.value) {
	mockMethod(mock.list[[1]], method.name, return.value)
	for (mock in mock.list[-1]) {
		assign_method_to(mock, method.name, return.value)
	}
}


#' The function for recording activity on the mock
#' 
#' This utility function creates the function which will form the basis of the \code{Mock}
#' method assigned as part of the \code{mockMethod} usage.
#' The body of this function is assigned to the method, and used to record the method and
#' arguments when called on the \code{Mock} it is assigned to.
#' When a method is called on a \code{Mock} the name of the method is first checked to 
#' see if it is registered for this particular mock object. If not, an error is raised. If
#' yes, then the arguments passed in the call are stored in the \code{Mock}s call.results
#' environment for later checking in a list named after the method.
#' Finally any return value stored with the \code{Mock} is returned.
#' 
#' @param mock the mock object to which the method is assigned
#' 
create_mock_call <- function(mock) {
	
	mock_call <- function(mock, ...) {
		call <- as.list(match.call())
		call.name <- as.character(call[[1]])
		call.args <- lapply(call[-1], eval, env = parent.frame())
		mock.idx <- which(sapply(call.args, function(arg) is(arg, "Mock")))
		mock <- call.args[[mock.idx]]
		if (!has_method(mock, call.name)) {
			stop(paste("Unexpected method call '", call.name, "'", sep = ""))
		}
		add_call_results(mock, call.name, call.args)
		return(method_value(mock, call.name))
	}	
	return(mock_call)
}

add_call_results <- function(mock, method, arguments) {
	method <- remove_classname(method, mock)
	ID <- as.character(length(ls(mock@calls)) + 1)
	mock.call <- c(list(as.name(method)), arguments)
	assign(ID, as.call(mock.call), mock@calls)
}

method_value <- function(mock, method.name) {
	method.name <- remove_classname(method.name, mock)
	return(mock@methods[[method.name]])
}

has_method <- function(mock, method.name) {
	method.name <- remove_classname(method.name, mock)
	methods <- ls(mock@methods)
	return(method.name %in% methods)
}

remove_classname <- function(method, mock) {
	class.name <- paste0(".", class(mock)[1])
	if (grepl(class.name, method)) {
		return(substr(method, 1, regexpr(class.name, method) - 1))
	} else {
		return(method)
	}
}


#' Assert that the named method was called once and only once.
#' 
#' The \code{called_once} function is designed to be called from within \code{test_that}s
#' \code{expect_that} assertion. This checks the call records of the \code{Mock} to 
#' confirm that the method was called once, and only once. If not, the function will
#' cause a test failure.
#' 
#' Note that the method being checked must have first been assigned to the \code{Mock} 
#' with a call to \code{\link{mockMethod}}.
#' 
#' @param method.name character string identifying the method of interest.
#' 
#' @seealso \code{\link{called_once_with}}, \code{\link{mockMethod}} 
#' 
#' @family assertions
#' 
#' @examples
#' \dontrun{
#' mock <- Mock()
#' mockMethod(mock, "TestMethod")
#' expect_that(mock, called_once("TestMethod"))
#' }
#' 
#' @export 
called_once <- function(method.name) {
	function(actual) {
		number.of.calls <- length(calls_to(actual, method.name))
		expectation(isTRUE(number.of.calls == 1), 
				paste("expected one call, was called", number.of.calls, "times"))
	}
}

all_calls_on <- function(mock) {
	calls <- as.list(mock@calls)
	names(calls) <- method_names(calls)
	return(calls)
}

method_names <- function(calls) {
	method.names <- sapply(calls, function(call) as.character(call[1]))
	attributes(method.names) <- NULL
	return(method.names)
}

calls_to <- function(mock, method) {
	call.list <- all_calls_on(mock)
	return(call.list[names(call.list) == method])
}


#' Assert method call and arguments called with.
#' 
#' Similar to \code{called_once} this function is called from within \code{testthat}s
#' \code{expect_that} assertion. The call records of the \code{Mock} are checked to
#' ensure that the method was only called once, and that the arguments supplied to the
#' \code{Mock} during the call match those specified.
#' 
#' Note that the method being checked must have first been assigned to the \code{Mock} 
#' with a call to \code{\link{mockMethod}}.
#' 
#' @param method.name character string identifying the method of interest.
#' @param ... an arbitrary number of arguments to be checked against the records of the 
#' call.
#' 
#' @seealso \code{\link{called_once_with}}, \code{\link{mockMethod}} 
#' 
#' @family assertions
#' 
#' @examples
#' \dontrun{
#' mock <- Mock()
#' mockMethod(mock, "TestMethod")
#' TestMethod(mock, 1, 2)
#' expect_that(mock, called_once_with("TestMethod", 1, 2))
#' }
#' 
#' @export
called_once_with <- function(method.name, ...) {
	
	function(actual) {
		expected.args <- list(...)
		call.list <- calls_to(actual, method.name)
		if (length(call.list) != 1) {
			expectation(FALSE, paste("was called ", length(call.list), "times"))
		} else {
			actual.args <- call_args(call.list)[[1]]
			expected_vs_actual(expected.args, actual.args, ignore.attributes = TRUE)
		}
	}
}

call_args <- function(calls) {
	clean_args <- function(call.args) {
		call.args <- strip_mock(call.args)
		# Note Conversion below required to change "pairlist" object to an ordinary list.
		return(as.list(call.args))
	}
	call.args <- lapply(calls, function(call) clean_args(call[-1]))
	names(call.args) <- method_names(calls)
	return(call.args)
}

strip_mock <- function(call.args) {
	if (is(call.args[[1]], "Mock")) {
		call.args[[1]] <- NULL
	}
	return(call.args)
}


#' Asserts that a series of calls made with specified arguments
#' 
#' Calls made checks that the calls specified were made on the mock object. Note that
#' the calls to check should be provided as comma separated arguments to the function.
#' The test will first check to see that the method names specified exist in the mock
#' call records, and if so will then check that the arguments for each call match those
#' provided.
#' If one of those conditions is not fullfilled, then the fail message is shown comparing
#' the expected method calls with the actual method calls.
#' Note that the mock object still needs to be included as the first argument of the 
#' method calls to be checked for. Basically just write it in as if you were making the
#' call again.
#' 
#' @param ... comma separated arguments of calls to check
#' 
#' @family assertions
#' 
#' @examples
#' \dontrun{
#' mock <- Mock()
#' mockMethod(mock, "myFun1")
#' mockMethod(mock, "myFun2")
#' arg1 <- 100
#' arg2 <- "yes"
#' myFun1(mock, 100)
#' myFun2(mock, "yes")
#' expect_that(mock, has_calls(myFun1(mock, arg1), myFun2(mock, arg2)))
#' }
#' 
has_calls <- function(...) {
	test.calls <- as.list(match.call()[-1])
	test.calls <- eval_args(test.calls, parent.frame())
	names(test.calls) <- method_names(test.calls)
	function(actual) {
		mock.calls <- all_calls_on(actual)
		comparison.result <- compare_calls(mock.calls, test.calls)
		expectation(comparison.result$outcome, comparison.result$message)
	}
}

eval_args <- function(test.calls, frame) {
	for (i in seq_along(test.calls)) {
		test.call.args <- test.calls[[i]][-1]
		test.call.args <- lapply(test.call.args, eval, frame)
		test.calls[[i]][-1] <- test.call.args
	}
	return(test.calls)
}

compare_calls <- function(actual.calls, expected.calls) {
	
	if (!all(names(expected.calls) %in% names(actual.calls))) {
		list(outcome = FALSE, 
				message = print_comparison(names(expected.calls), names(actual.calls)))
	}
	
	actual.args <- call_args(actual.calls)
	expected.args <- call_args(expected.calls)
	if (!all(expected.args %in% actual.args)) {
		list(outcome = FALSE, message = print_comparison(expected.args, actual.args))
	}
	
	else {
		list(outcome = TRUE, message = "Passed")
	}
}


#' Asserts method was not called
#' 
#' When called within \code{expect_that} this function returns a passing result when
#' the named method was not called on the mock object. The method need not have been
#' assigned to the mock before testing.
#' 
#' @param method.name character string naming the method to check
#' 
#' @seealso \code{\link{called_once}}
#' 
#' @family assertions
#' 
#' @export 
not_called <- function(method.name) {
	
	function(actual) {
		num.calls <- length(calls_to(actual, method.name))
		expectation(isTRUE(num.calls == 0), 
				paste("expected no calls, was called", num.calls, "times"))
	}
}


#' Retrieves a specified call argument from call record
#' 
#' This function is provided if a mock object call records needs to be specifically 
#' queried for testing of a value.
#' This is particularly useful if an object is expected to be changed somewhere within
#' the function calls, or if dealing with mutable values. In this way the mock can be
#' used as a collection tool. The value of the parameter caught can then be tested 
#' against expectations.
#' 
#' @param mock the mock object which is holding the call record
#' @param method the name of the method which uses the argument
#' @param arg.number the position number of where the argument is in the function call
#' @param call.number which instance of the call is desired (defaults to 1)
#'  
get_call_argument <- function(mock, method, arg.number, call.number = 1) {
	method.calls <- calls_to(mock, method)
	return(call_args(method.calls)[[call.number]][[arg.number]])
}


#' Attempt to clean up mock methods
#'
#' This cleaning function is needed when residual mock methods are causing failures, e.g.
#' their existance isn't expected. Typical symptoms of a need for a clean up are: 
#' functions not found, Mocks complaining about not having registered methods, function 
#' calls returning incorrect values.
#' When called this function will attempt to clean up any existing mock methods. It does
#' this by first replacing any new generics which were created with the original default
#' values. After the methods have been removed, the "mock_methods" environment is 
#' detached to clear any that were stored there.
#' Fingers crossed, all will be well.
#' 
cleanMockMethods <- function() {
	
	if (any(grepl("mock_methods", search()))) {
		for (method in get("new_S3_generics", pos = "mock_methods")) {
			reassign_default(method)
		}
		detach("mock_methods")
	}
}

reassign_default <- function(method) {

	default <- paste(method, "default", sep = ".")
	default.method <- try(getAnywhere(default)[1], silent = TRUE)
	if (is.function(default.method)) {
		assign(method, default.method, pos = environment(default.method))
	}
}








