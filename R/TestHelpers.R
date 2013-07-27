#' Checks expected and actual objects are matching
#' 
#' This assertion function is a more expressive version of the \code{equals} assertion
#' found in the \code{testthat} package.
#' The assertion first checks the compatibility of the expected and actual objects with
#' \code{all.equal}, and if this throws an error, checks with \code{identical}. The 
#' optional \code{ignore.attributes} parameter (default TRUE) will strip the attributes
#' from both objects before comparing.
#' In the case of a comparison failure, the \code{matchesObject} prints both the 
#' expected and actual objects using \code{str} (or failing that with \code{print}) so 
#' that you can see the actual differences between the objects.
#' 
#' @family assertions
#' 
#' @param expected the expected object configuration
#' @param ignore.attributes whether to strip attributes before testing for equality, 
#' defaults to TRUE.
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
			print_comparison(expected, actual))
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

TestFunction <- function(x) {
	sum(x)
}

