#'
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
	actual.str <- paste(capture.output(str(actual)), collapse = "\n")
	expected.str <- paste(capture.output(str(expected)), collapse = "\n")
	expectation(
			identical(actual, expected),
			paste("\nExpected:", expected.str, "But got:", actual.str, sep = "\n"))
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
			return("FAIL")
		})