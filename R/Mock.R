#'
#'
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
		class(mock) <- spec
	}
	return(mock)
}

mockMethod <- function(mock, method.name, return.value) {
	
	if (missing(return.value)) {
		return.value <- NULL
	}
	if (is.list(mock) && !identical(names(mock), names(Mock()))) {
		multiple_mock_method(mock, method.name, return.value)
	}
	assign_method_value(mock, method.name, return.value)
	assign(method.name, create_mock_call(mock), pos = search()[2])
}

multiple_mock_method <- function(mock.list, method.name, return.value) {
	for (mock in mock.list) {
		mockMethod(mock, method.name, return.value)
	}
}

assign_method_value <- function(mock, method.name, return.value) {
	mock$method.returns[[method.name]] <- return.value
}

method_value <- function(mock, method.name) {
	return(mock$method.returns[[method.name]])
}

has_method <- function(mock, method.name) {
	methods <- ls(mock$method.returns)
	return(method.name %in% methods)
}

create_mock_call <- function(mock) {
	
	mock_call <- function(mock, ...) {
		call <- as.list(match.call())
		call.name <- as.character(call[[1]])
		if (!has_method(mock, call.name)) {
			stop(paste("Unexpected method call '", call.name, "'", sep = ""))
		}
		call.args <- lapply(call[-1], eval, env = parent.frame())
		.add_call_results(mock, call.name, call.args)
		return(method_value(mock, call.name))
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
			expectation(FALSE, paste("Was called ", length(actual.args), "times"))
		} else {
			actual.args <- mock_arguments(actual, method.name, 1)
			expected_vs_actual(expected.args, actual.args, ignore.attributes = TRUE)
		}
	}
}














