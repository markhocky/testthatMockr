#' Create a Mock class to support unit testing.
#' 
#' The \code{Mock} object is designed to mimic another object's behaviour, and also 
#' provide functionality to record operations performed on it. The \code{Mock} can be 
#' used as a stub for an object yet to be developed, or it can be used to mimic an 
#' existing object.
#' 
#' The \code{Mock} can be provided with the name of a class to mimic via the \code{spec}
#' parameter. In this case the Mock will be assigned this class, which is particularly 
#' useful if the \code{Mock} is to be used in the slot of another S4 object.
#' The \code{spec} parameter is optional however, and if not provided the \code{Mock}
#' will simply be assigned the class "Mock".
#' 
#' @details 
#' The \code{Mock} contains two slots:
#' \item{call.results}{an environment used to store calls made on the \code{Mock} along 
#' with any arguments passed in as part of that call.}
#' \item{methods}{an environment containing the names of methods registered for the 
#' \code{Mock} along with the optional values to be returned from the call}
#' These slots are not intended to be used directly, but rather are the containers used
#' by the \code{Mock}s methods, and for testing. 
#' 
#' @return A \code{Mock} object ready for use in your \code{testthat} unit tests.
#' 
#' @param spec a character string naming the class to be imitated.
#' 
#' @seealso \link{mockMethod}
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
	setClass("Mock",
			representation(
					call.results = "environment", 
					methods = "environment"
			), contains = spec)
	mock <- new("Mock")
	mock@call.results <- new.env(emptyenv())
	mock@methods <- new.env(emptyenv())
	return(mock)
}


#' Assign a method to a Mock object
#' 
#' The usefullness of \code{Mock} objects comes into full effect when they are assigned
#' a specific "mock method". The associated \code{Mock} will make note of calls made to 
#' this method along with any arguments and record these for future reporting.
#' The method is assigned to a particular \code{Mock} so calls of this method on another
#' \code{Mock} object will throw and error.
#' 
#' The same method can be applied to multiple \code{Mock} objects by passing in a list of
#' \code{Mock}s as the first parameter. This is effectively the same as calling the 
#' \code{mockMethod} repeatedly on each different \code{Mock}.
#' 
#' The optional \code{return.value} can be used to specifically control the output from 
#' the \code{Mock}, and different \code{return.value}s can be assigned to different 
#' \code{Mock} objects.
#' 
#' #' At this stage of development, only methods which take the \code{Mock} as their first
#' argument can be imitated.
#' 
#' @details 
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
#' 
#' @examples
#' mockMethod(mock, "TestMethod") # returns NULL
#' mockMethod(mock, "TestMethod", return.value = 1) # returns 1
#' mockMethod(list(mock1, mock2), "TestMethod") # assigns "TestMethod" to both mock1 and 
#' mock2 
#' 
#' @return Returns NULL, but called for it's side effect of setting up the mock methods.
#' 
#' @param mock The \code{Mock} object which the method is related to, or a list of 
#' \code{Mock}s.
#' @param method.name The method / function name.
#' @param return.value The (optional) value to be returned from future calls to 
#' \code{method.name}. Defaults to NULL.
#' 
#' @seealso \code{\link{Mock}}
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
	setGeneric(method.name, eval(generic), where = "package:mockR")
	return(get(method.name))
}

make_S3andS4_generics <- function(method.name, mock, method) {
	if(no_dots_args(method)) {
		method <- add_dots_arg(method)
	}
	setAs_S3generic(method.name, method)
	setGeneric(method.name, method, where = "package:mockR")
	assign(paste0(method.name, ".Mock"), create_mock_call(mock), 
			envir = environment(method))
}

no_dots_args <- function(method) {
	!"..." %in% names(formals(method))
}

add_dots_arg <- function(method) {
	formals(method) <- c(formals(method), alist("..." = ))
	return(method)
}

setAs_S3generic <- function(method.name, method) {
	generic <- method
	body(generic) <- parse(text = paste0("UseMethod(\"", method.name, "\")"))
	assign(method.name, generic, pos = environment(method))
	assign(paste0(method.name, ".default"), method, pos = environment(method))
}

assign_S4_method <- function(mock, method, method.name, return.value) {
	mock.method <- create_mock_call(mock)
	formals(mock.method) <- formals(method)
	setMethod(method.name,
			signature("Mock"),
			mock.method)
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
#' @usage expect_that(mock, called_once("TestMethod"))
#' 
#' @param method.name character string identifying the method of interest.
#' 
#' @seealso \code{\link{called_once_with}}, \code{\link{mockMethod}} 
#' 
#' @export 
called_once <- function(method.name) {
	function(actual) {
		number.of.calls <- length(mock_calls(actual, method.name))
		expectation(number.of.calls == 1, 
				paste("Expected one call, was called", number.of.calls, "times"))
	}
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
#' @examples
#' expect_that(mock, called_once_with("TestMethod", arg1, arg2))
#' 
#' @export
called_once_with <- function(method.name, ...) {
	
	function(actual) {
		expected.args <- list(...)
		actual.args <- mock_calls(actual, method.name)
		if (length(actual.args) != 1) {
			expectation(FALSE, paste("Was called ", length(actual.args), "times"))
		} else {
			actual.args <- mock_arguments(actual, method.name, 1)
			expected_vs_actual(expected.args, actual.args, ignore.attributes = TRUE)
		}
	}
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

mock_calls <- function(mock, method) {
	return(mock@call.results[[method]])
}

mock_arguments <- function(mock, method, instance) {
	calls <- mock_calls(mock, method)
	called.arguments <- calls[[instance]][-1]
	attributes(called.arguments) <- NULL
	return(called.arguments)
}


#' The function for recording activity on the mock
#' 
#' This utility function creates the function which will form the basis of the \code{Mock}
#' method assigned as part of the \code{mockMethod} usage.
#' The body of this function is assigned to the method, and used to record the method and
#' arguments when called on the \code{Mock} it is assigned to.
#' When a method is called on a \code{Mock} the name of the method is first checked to 
#' see if it is registered for this particular mock object. if not an error is raised. If
#' yes, then the arguments passed in the call are stored in the \code{Mock}s call.results
#' environment for later checking in a list named after the method.
#' Finally any return value stored with the \code{Mock} is returned.
create_mock_call <- function(mock) {
	
	mock_call <- function(mock, ...) {
		call <- as.list(match.call())
		call.name <- as.character(call[[1]])
		call.args <- lapply(call, eval, env = parent.frame())
		mock.idx <- which(sapply(call.args, function(arg) is(arg, "Mock")))
		mock <- call.args[[mock.idx]]
		if (!has_method(mock, call.name)) {
			stop(paste("Unexpected method call '", call.name, "'", sep = ""))
		}
		add_call_results(mock, call.name, call.args[-mock.idx])
		return(method_value(mock, call.name))
	}	
	return(mock_call)
}

add_call_results <- function(mock, method, arguments) {
	method <- remove_classname(method, mock)
	existing.calls <- mock_calls(mock, method)
	if (length(existing.calls)) {
		mock@call.results[[method]] <- c(list(existing.calls), list(arguments))
	} else {
		mock@call.results[[method]] <- list(arguments)
	}
}

method_value <- function(mock, method.name) {
	method.name <- remove_classname(method.name, mock)
	return(mock@methods[[method.name]])
}













