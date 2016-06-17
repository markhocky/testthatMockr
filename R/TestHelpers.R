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
#' @importFrom testthat expectation
#' 
#' @export 
called_once <- function(method.name) {
	function(actual) {
		number.of.calls <- length(call_args_for(actual, method.name))
		expectation(isTRUE(number.of.calls == 1), 
				paste("expected one call, was called", number.of.calls, "times"), 
				"called once")
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
		expected.args <- c(list(actual), list(...))
		method.calls <- call_args_for(actual, method.name)
		if (length(method.calls) != 1) {
			expectation(FALSE, paste("was called ", length(method.calls), "times"))
		} else {
			actual.args <- method.calls[[1]] 
			expected_vs_actual(expected.args, actual.args, ignore.attributes = TRUE)
		}
	}
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
#' @export 
has_calls <- function(...) {
	test.calls <- as.list(match.call()[-1])
	test.calls <- eval_args(test.calls, parent.frame())
	function(actual) {
		mock.calls <- all_calls_on(actual)
		comparison.result <- compare_calls(mock.calls, test.calls)
		expectation(comparison.result$outcome, comparison.result$message, "calls match")
	}
}

eval_args <- function(test.calls, frame) {
	method.names <- character(length(test.calls))
	for (i in seq_along(test.calls)) {
		method.names[i] <- as.character(test.calls[[i]][1])
		test.call.args <- as.list(test.calls[[i]][-1])
		test.calls[[i]] <- lapply(test.call.args, eval, frame)
	}
	names(test.calls) <- method.names
	return(test.calls)
}

compare_calls <- function(actual.calls, expected.calls) {
	
	if (!all(names(expected.calls) %in% names(actual.calls))) {
		list(outcome = FALSE, 
				message = print_comparison(names(expected.calls), names(actual.calls)))
	}
	
	if (!all(expected.calls %in% actual.calls)) {
		list(outcome = FALSE, message = print_comparison(expected.calls, actual.calls))
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
		num.calls <- length(call_args_for(actual, method.name))
		expectation(isTRUE(num.calls == 0), 
				paste("expected no calls, was called", num.calls, "times"), 
				"not called")
	}
}

#' Checks expected and actual objects are matching
#' 
#' This assertion function is a more expressive version of the \code{equals} assertion
#' found in the \code{testthat} package.
#' The assertion first checks the compatibility of the expected and actual objects with
#' with \code{all.equal}, and if this throws an error, checks with \code{identical}. The 
#' optional \code{ignore.attributes} parameter (default \code{TRUE}) will strip the 
#' attributes from both objects before comparing.
#' In the case of a comparison failure, the \code{matchesObject} prints both the 
#' expected and actual objects using \code{str} (or failing that with \code{print}) so 
#' that you can see the actual differences between the objects.
#' 
#' @param expected the expected object configuration
#' @param ignore.attributes whether to strip attributes before testing for equality, 
#' defaults to TRUE.
#'
#' @family assertions
#' 
#' @importFrom utils capture.output str
#'  
#' @export 
matchesObject <- function(expected, ignore.attributes = TRUE) {
	function(actual) {
		expected_vs_actual(expected, actual, ignore.attributes)
	}
}

expected_vs_actual <- function(expected, actual, ignore.attributes = TRUE) {
	if (ignore.attributes && !(isS4(expected) | isS4(actual))) {
		attributes(expected) <- NULL
		attributes(actual) <- NULL
	}
	comparison <- tryCatch(
			all.equal(actual, expected),
			error = function(e) identical(actual, expected))
	expectation(isTRUE(comparison),
			print_comparison(expected, actual), "match")
}

print_comparison <- function(expected, actual) {
	paste("\nExpected:", print_object(expected), 
			"But got:", print_object(actual), sep = "\n")
}


print_object <- function(object) {
	
	output <- tryCatch(capture.output(str(object)), 
			error = function(e) capture.output(print(object)))
	return(paste(output, collapse = "\n"))
}



# Following are helpers for testthatMockr tests (see: inst/tests/testMocks.R)
setClass("testS4",
		representation(
				container = "list"
		))


setGeneric("TestS4generic",
		function(x, ...) {
			standardGeneric("TestS4generic")
		})


setGeneric("TestS4method",
		function(x, ...) {
			standardGeneric("TestS4method")
		})


setMethod("TestS4method",
		signature(x = "testS4"),
		function(x) {
			return("S4 method")
		})


