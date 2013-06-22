#' Create a Mock class to support unit testing.
#' 
#' The \code{Mock} object is designed to mimic another object's behaviour, and also 
#' provide functionality to record operations performed on it. The \code{Mock} can be 
#' used to stub out an object yet to be developed, or it can be used to mimic an 
#' existing object.
#' 
#' The \code{Mock} can be provided with the name of a class to mimic via the \code{spec}
#' parameter. In this case the Mock will be assigned this class, which is particularly 
#' useful if the \code{Mock} is to be used in the slot of another S4 object.
#' The \code{spec} parameter is optional however, and if not provided the \code{Mock}
#' will simply be assigned the class "mock". 
#' 
#' @return A \code{Mock} object ready for use in your \code{testthat} unit tests.
#' 
#' @param spec a character string naming the class to be imitated.
#' 
#' @export 
Mock <- function(spec) {
	
	mock <- list()
	mock$call.results <- new.env()
	mock$method.returns <- new.env()
	if (missing(spec)) {
		class(mock) <- "mock"
	} else {
		if (is.name(substitute(spec))) {
			spec <- class(spec)
		} else {
			spec <- as.character(spec)
		}
		class(mock) <- c(spec, "mock")
	}
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
#' \code{mockMethod} repeatedly on each \code{Mock}.
#' 
#' The optional \code{return.value} can be used to specifically control the output from 
#' the \code{Mock}, and different \code{return.value}s can be assigned to different 
#' \code{Mock} objects.
#' 
#' @details 
#' The \code{mockMethod} function will attempt to create a method / function specific
#' to the mock without hurting any existing functions.
#' If there are no existing functions of name \code{method.name}, then a function is 
#' assigned with that name.
#' If a function of that name exists, then \code{mockMethod} will turn this into a 
#' generic S3 function. The existing function will become the default, and the mock
#' function will be a method assigned for \code{class(mock)}.
#' If the existing function is already an S3 generic, then \code{mockMethod} will 
#' create a method for \code{class(mock)}, but note that this will override any existing
#' methods if the \code{mock} is imitating another class.
#' 
#' @examples
#' mockMethod(mock, "TestMethod") # returns NULL
#' mockMethod(mock, "TestMethod", return.value = 1) # returns 1
#' mockMethod(list(mock1, mock2), "TestMethod") # assigns "TestMethod" to both mock1 and 
#' mock2 
#' 
#' @return Returns NULL, but as a side effect the function is assigned to the first
#' \code{package} in the search path (excluding the base package). This should almost 
#' always be the package being tested.
#' 
#' @param mock The \code{Mock} object which the method is related to, or a list of 
#' \code{Mock}s.
#' @param method.name The method / function name.
#' @param return.value The (optional) value to be returned from future calls to 
#' method.name.
#' 
#' @seealso \code{\link{Mock}}
#' 
#' @export
mockMethod <- function(mock, method.name, return.value) {
	
	if (missing(return.value)) {
		return.value <- NULL
	}
	if (is_list_of_mocks(mock)) {
		multiple_mock_method(mock, method.name, return.value)
	} else {
		test.package <- search()[2]
		search.env <- parent.frame()
		if (exists(method.name, search.env)) {
			if (is_mock_method(method.name, mock, search.env)) {
				assign_method_to(mock, method.name, return.value)
			}
			if (!is_generic(method.name, search.env)) {
				setAs_S3generic(method.name = method.name, search.env = search.env)
			}
			assign_S3method(method.name = method.name, mock = mock, return.value = return.value, test.package = test.package)
		} else {
			assign_normal_function(mock = mock, method.name = method.name, return.value = return.value, test.package = test.package)
		}
	}
}


is_list_of_mocks <- function(mock) {
	is.list(mock) && !identical(names(mock), names(Mock()))
}

is_mock_method <- function(method.name, mock, search.env) {
	identical(body(get(method.name, search.env)), body(create_mock_call(mock)))
}

multiple_mock_method <- function(mock.list, method.name, return.value) {
	for (mock in mock.list) {
		mockMethod(mock, method.name, return.value)
	}
}

assign_method_to <- function(mock, method.name, return.value) {
	method.name <- remove_classname(method.name, mock)
	mock$method.returns[[method.name]] <- return.value
}

is_generic <- function(method.name, search.env) {
	return(suppressWarnings(utils:::findGeneric(method.name, search.env) != ""))
}

setAs_S3generic <- function(method.name, search.env) {
	
	existing.method.env <- get_env_containing(method.name, search.env)
	existing.method <- existing.method.env[[method.name]]
	
	generic.call <- paste0("function(mock, ...) UseMethod(\"", 
			method.name, "\")")
	generic.call <- parse(text = generic.call)
	assign(method.name, 
			eval(generic.call), pos = existing.method.env)
	assign(paste0(method.name, ".default"),
			existing.method, pos = existing.method.env)
}

assign_S3method <- function(method.name, mock, return.value, test.package) {
	mock.method.name <- paste(method.name, class(mock)[1], sep = ".")
	assign_method_to(mock, mock.method.name, return.value)
	assign(mock.method.name, 
			create_mock_call(mock), pos = test.package)
}

assign_normal_function <- function(mock, method.name, return.value, test.package) {
	assign_method_to(mock, method.name, return.value)
	assign(method.name, create_mock_call(mock), pos = test.package)
}

get_env_containing <- function(object, env = parent.frame()) {
	if (exists(object, env, inherits = FALSE)) {
		return(env)
	} else {
		get_env_containing(object, env = parent.env(env))
	}
}



#' Assert that the named method was called once and only once.
#' 
#' The \code{called_once} function is designed to be called from within \code{testthat}s
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
	methods <- ls(mock$method.returns)
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
	return(mock$call.results[[method]])
}

mock_arguments <- function(mock, method, instance) {
	calls <- mock_calls(mock, method)
	called.arguments <- calls[[instance]][-1]
	attributes(called.arguments) <- NULL
	return(called.arguments)
}


#' The function for recording activity on the mock
create_mock_call <- function(mock) {
	
	mock_call <- function(mock, ...) {
		call <- as.list(match.call())
		call.name <- as.character(call[[1]])
		if (!has_method(mock, call.name)) {
			stop(paste("Unexpected method call '", call.name, "'", sep = ""))
		}
		call.args <- lapply(call[-1], eval, env = parent.frame())
		add_call_results(mock, call.name, call.args)
		return(method_value(mock, call.name))
	}	
	return(mock_call)
}

add_call_results <- function(mock, method, arguments) {
	method <- remove_classname(method, mock)
	existing.calls <- mock_calls(mock, method)
	if (length(existing.calls)) {
		mock$call.results[[method]] <- c(list(existing.calls), list(arguments))
	} else {
		mock$call.results[[method]] <- list(arguments)
	}
}

method_value <- function(mock, method.name) {
	method.name <- remove_classname(method.name, mock)
	return(mock$method.returns[[method.name]])
}

#' Utility function for removing mock methods - does not remove generics
cleanMethods <- function(methods) {
	
	for (method in methods) {
		method.env <- get_env_containing(method)
		rm(list = as.character(methods(method)), envir = method.env)
	}
}











