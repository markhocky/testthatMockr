#' Checks expected and actual objects are matching
#'
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
	actual.str <- paste(print_object(actual), collapse = "\n")
	expected.str <- paste(print_object(expected), collapse = "\n")
	comparison <- tryCatch(
			all.equal(actual, expected),
			error = function(e) identical(actual, expected))
	expectation(isTRUE(comparison),
			paste("\nExpected:", expected.str, "But got:", actual.str, sep = "\n"))
}

print_object <- function(object) {
	
	output <- tryCatch(capture.output(str(object)), 
			error = function(e) capture.output(print(object)))
	return(output)
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
		function(x) {
			standardGeneric("TestS4method")
		})

		
setMethod("TestS4method",
		signature(x = "testS4"),
		function(x) {
			return("S4 method")
		})

