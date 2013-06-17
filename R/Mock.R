#'
#'
Mock <- function(spec) {
	
	mock <- list()
	mock$call.results <- new.env()
	if (missing(spec)) {
		class(mock) <- "mock"
	} else {
		if (is.name(substitute(spec))) {
			spec <- class(spec)
		} else {
			spec <- as.character(spec)
		}
		class(mock) <- spec
	}
	return(mock)
}

mockMethod <- function(mock, method.name, return.value) {
	
	if (missing(return.value)) {
		return.value <- NULL
	}
	assign(method.name, createMockCall(return.value), pos = search()[2])
}

createMockCall <- function(return.value) {
	
	mock_call <- function(mock, ...) {
		call <- as.list(match.call())
		call.name <- as.character(call[[1]])
		call.args <- lapply(call[-1], eval, env = parent.frame())
		.add_call_results(mock, call.name, call.args)
		return(return.value)
	}	
	return(mock_call)
}

.add_call_results <- function(mock, method, arguments) {
	existing.calls <- mock_calls(mock, method)
	if (length(existing.calls)) {
		mock$call.results[[method]] <- c(list(existing.calls), list(arguments))
	} else {
		mock$call.results[[method]] <- list(arguments)
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

called_once <- function(method.name) {
	function(actual) {
		number.of.calls <- length(mock_calls(actual, method.name))
		expectation(number.of.calls == 1, 
				paste("Expected one call, was called", number.of.calls, "times"))
	}
}

called_once_with <- function(method.name, ...) {
	
	function(actual) {
		expected.args <- list(...)
		actual.args <- mock_calls(actual, method.name)
		if (length(actual.args) != 1) {
			expectation(FALSE, 
				paste("Was called ", length(actual.args), "times"))
		} else {
			actual.args <- mock_arguments(actual, method.name, 1)
			expected <- paste(capture.output(str(expected.args)), collapse = "\n")
			received <- paste(capture.output(str(actual.args)), collapse = "\n")
			expectation(identical(actual.args, expected.args), 
				paste("\nExpected: ", expected, "But got: ", received, sep = "\n"))
		}
	}
}














