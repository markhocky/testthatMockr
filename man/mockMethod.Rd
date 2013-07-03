\name{mockMethod}
\alias{mockMethod}
\title{Assign a method to a Mock object}
\usage{
  mockMethod(mock, method.name, return.value = NULL)
}
\arguments{
  \item{mock}{The \code{Mock} object which the method is
  related to, or a list of \code{Mock}s.}

  \item{method.name}{The method / function name.}

  \item{return.value}{The (optional) value to be returned
  from future calls to \code{method.name}. Defaults to
  NULL.}
}
\value{
  Returns NULL, but called for it's side effect of setting
  up the mock methods.
}
\description{
  The usefullness of \code{Mock} objects comes into full
  effect when they are assigned a specific "mock method".
  The associated \code{Mock} will make note of calls made
  to this method along with any arguments and record these
  for future reporting. The method is assigned to a
  particular \code{Mock} so calls of this method on another
  \code{Mock} object will throw and error.
}
\details{
  The same method can be applied to multiple \code{Mock}
  objects by passing in a list of \code{Mock}s as the first
  parameter. This is effectively the same as calling the
  \code{mockMethod} repeatedly on each different
  \code{Mock}.

  The optional \code{return.value} can be used to
  specifically control the output from the \code{Mock}, and
  different \code{return.value}s can be assigned to
  different \code{Mock} objects.

  At this stage of development, only methods which take the
  \code{Mock} as their first argument can be imitated.

  The \code{mockMethod} function will attempt to create a
  method / function specific to the mock without hurting
  any existing functions. If there are no existing
  functions of name \code{method.name}, then an S4 method
  (with associated generic) is created. If a function of
  that name exists, then \code{mockMethod} will turn this
  into generic S3 and S4 methods. The existing function
  will become the default, and the mock function will be a
  method assigned for signature "Mock". If the existing
  function is already an S3 or S4 generic, then
  \code{mockMethod} will create a method with signature of
  class "Mock".

  For existing functions, the new generic is assigned in
  the same environment as the existing function
  (effectively overwriting it), and \code{Mock} methods
  that are created are assigned in the \code{mockR} package
  environment. Note that this means that there must be
  write permission to the environment where the existing
  function resides. This will not always be the case, for
  example if trying to mock a non-exported function from a
  package. In these cases, a call to \link{unlockBinding}
  on the existing function prior to setting the mock method
  may work. This is potentially something to be included in
  future versions of \code{mockR}.
}
\examples{
mockMethod(mock, "TestMethod") # returns NULL
mockMethod(mock, "TestMethod", return.value = 1) # returns 1
mockMethod(list(mock1, mock2), "TestMethod") # assigns "TestMethod" for mock1 and mock2
}
\seealso{
  \code{\link{Mock}}
}

