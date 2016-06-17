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
#' @param spec a character string or name of the class to be imitated.
#' 
#' @seealso \code{mockMethod}
#' 
#' @importFrom methods is isClass new representation setClass setGeneric setMethod signature
#' 
#' @export 
Mock <- function(spec) {
	
	if (!missing(spec)) {
		spec.name <- define_spec_name(spec)
		mock <- create_mimic_mock(spec.name)
	} else {
		mock <- create_mock()
	}
	return(mock)
}

setClass("Mock", contains = "VIRTUAL")

# character vector of modified S3 methods
new_S3_generics <- character(0)

define_spec_name <- function(spec) {
	spec.name <- as.character(sys.call(-1L))[2]
	if (!isClass(spec.name)) {
		spec.name <- class(spec)[1]
	}
	return(spec.name)
}

create_mimic_mock <- function(spec.name) {
	spec <- c(spec.name, "Mock")
	name <- paste0(spec, collapse = "_")
	return(create_mock(name, spec))
}

create_mock <- function(name = "mock", contains = "Mock") {
	setClass(name,
			representation(
					methods = "environment", 
					calls = "environment"
			), 
			contains = contains)
	mock <- new(name)
	mock@methods <- new.env(emptyenv())
	mock@calls <- new.env(emptyenv())
	return(mock)
}


#' Assign a method to a Mock object
#' 
#' Creates a mock method which diverts the function call to a special method which then
#' records the behaviour for later testing. A return value may be specified if desired.
#' Note that the original function is not intended to be called.
#' 
#' @details
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
#' recorded in the \code{new_S3_generics} character vector located in the 
#' \code{testthatMockr} namespace) so that it can be removed safely 
#' (i.e. on call to \code{cleanMockMethods}.
#' 
#' @examples
#' \dontrun{
#' mock <- Mock()
#' mock2 <- Mock()
#' mockMethod(mock, "TestMethod") # returns NULL
#' # returns 1
#' mockMethod(mock, "TestMethod", return.value = 1)
#' # assigns "TestMethod" for mock and mock2
#' mockMethod(list(mock, mock2), "TestMethod") 
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
	setGeneric(method.name, eval(generic), where = "package:testthatMockr")
	return(get(method.name))
}

make_S3andS4_generics <- function(method.name, mock, method) {
	if(no_dots_args(method)) {
		method <- add_dots_arg(method)
	}
	if (!is_S3generic(method)) {
		setAs_S3generic(method.name, method)
	}
	setGeneric(method.name, method, where = "package:testthatMockr")
}

assign_S3_method <- function(method.name, mock) {
	assign(paste0(c(method.name, class(mock)), collapse = "."), create_mock_call(mock), 
			pos = "package:testthatMockr")
}

is_S3generic <- function(method) {
	any(grepl("UseMethod", body(method)))
}

setAs_S3generic <- function(method.name, method) {
	generic <- method
	body(generic) <- parse(text = paste0("UseMethod(\"", method.name, "\")"))
	assign(method.name, generic, pos = environment(method))
	assign(paste0(method.name, ".default"), method, pos = environment(method))
	newS3generics <- get("new_S3_generics", pos = "package:testthatMockr")
	assign("new_S3_generics", c(newS3generics, method.name), pos = "package:testthatMockr")
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
			signature(class(mock)),
			mock.method, 
			where = "package:testthatMockr")
	assign_method_to(mock, method.name, return.value)
}

assign_method_to <- function(mock, method.name, return.value) {
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
#' Creates the function to be used for the mock. A function is returned which is then
#' assigned to the appropriate mock function name for later calling.
#' 
#' @details
#' This utility function creates the function which will form the basis of the \code{Mock}
#' method assigned as part of the \code{mockMethod} usage.
#' The body of this function is assigned to the method, and used to record the method and
#' arguments when called on the \code{Mock} it is assigned to.
#' When a method is called on a \code{Mock} the name of the method is first checked to 
#' see if it is registered for this particular mock object. If not, an error is raised. If
#' yes, then the arguments passed in the call are stored in the \code{Mock}s call.results
#' environment for later checking.
#' Finally any return value stored with the \code{Mock} is returned.
#' 
#' @param mock the mock object to which the method is assigned
#' 
#' @return function the function to be used by the mock for calls henceforth
create_mock_call <- function(mock) {
	
	mock_call <- function(...) {
		call <- method_call()
		confirm_registered_method(call)
		record_results_of(call)
		return(method_value(call))
	}	
}

setClass("MockCall",
		representation(
				method = "character", 
				args = "list", 
				mock = "Mock"
		))


method_call <- function() {
	method.call <- as.list(sys.call(-1L))
	method.args <- lapply(method.call[-1], eval, env = parent.frame(2L))
	mock <- get_mock_from(method.args)
	method.name <- get_method_name(method.call, mock)
	
	call <- new("MockCall")
	call@method <- method.name
	call@args <- method.args
	call@mock <- mock
	return(call)
}

get_mock_from <- function(method.args) {
	mock.idx <- which(sapply(method.args, function(arg) is(arg, "Mock")))
	mock <- method.args[[mock.idx]]
	return(mock)
}

get_method_name <- function(call, mock) {
	method.name <- as.character(call[[1]])
	method.name <- remove_classname(method.name, mock)
	return(method.name)
}

remove_classname <- function(method, mock) {
	class.name <- paste0(".", class(mock)[1])
	if (grepl(class.name, method)) {
		return(substr(method, 1, regexpr(class.name, method) - 1))
	} else {
		return(method)
	}
}

confirm_registered_method <- function(call) {
	mock <- call@mock
	methods <- ls(mock@methods)
	if (!call@method %in% methods) {
		stop(paste("Unexpected method call '", call@method, "'", sep = ""))
	}
}

record_results_of <- function(call) {
	store_call(call@mock, call)
}

store_call <- function(mock, call) {
	ID <- as.character(length(ls(mock@calls)) + 1)
	assign(ID, call, mock@calls)
}

get_args <- function(call) {
	return(call@args)
}

get_method <- function(call) {
	return(call@method)
}

method_value <- function(call) {
	mock <- call@mock
	method.name <- call@method
	return(mock@methods[[method.name]])
}


#' Returns the calls on the mock of a specified method
#' 
#' This function is intended for use predominately in tests to check if a method was
#' or was not called on the mock. The arguments of each call are also returned so that
#' it is possible to check that a specific call arrangement has occured.
#' 
#' @return a named list of call arguments for specified method
#' 
#' @param mock the mock object which is holding the call record
#' @param method the name of the method to check (as a character)
#' 
#' @seealso all_calls_on
#' 
#' @export
call_args_for <- function(mock, method) {
	call.list <- all_calls_on(mock)
	call.list <- call.list[names(call.list) == method]
	return(call.list)
}


#' Returns all calls which have been made on the mock
#' 
#' The full call record of the mock is returned. This allows querying of the calls made
#' to the mock to confirm the activity it has been a part of. The call record is a named
#' list, each entry named after the method it was related to. Each entry is also a list
#' with the contents being the arguments of the call made. 
#' 
#' @return a named list of calls.
#' 
#' @param mock the mock object which is holding the call record.
#' 
#' @seealso call_args_for
#' 
#' @export
all_calls_on <- function(mock) {
	calls <- as.list(mock@calls)
	call.list <- lapply(calls, get_args)
	names(call.list) <- sapply(calls, get_method)
	return(call.list)
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
#' Fingers crossed, all will be well. Worst case you may need to restart your R session.
#' 
#' @importFrom utils getAnywhere
#' 
#' @export
cleanMockMethods <- function() {
	
	if (any(grepl("new_S3_generics", ls(pos = "package:testthatMockr")))) {
		for (method in get("new_S3_generics", pos = "package:testthatMockr")) {
			reassign_default(method)
		}
	}
	assign("new_S3_generics", character(0), pos = "package:testthatMockr")
}

reassign_default <- function(method) {
	
	default <- paste(method, "default", sep = ".")
	default.method <- try(getAnywhere(default)[1], silent = TRUE)
	if (is.function(default.method)) {
		assign(method, default.method, pos = environment(default.method))
	}
}






