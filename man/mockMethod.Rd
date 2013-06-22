\name{mockMethod}
\alias{mockMethod}
\title{Assign a method to a Mock object}
\usage{
  mockMethod(mock, method.name, return.value)
}
\arguments{
  \item{mock}{The \code{Mock} object which the method is
  related to, or a list of \code{Mock}s.}

  \item{method.name}{The method / function name.}

  \item{return.value}{The (optional) value to be returned
  from future calls to method.name.}
}
\value{
  Returns NULL, but as a side effect the function is
  assigned to the first \code{package} in the search path
  (excluding the base package). This should almost always
  be the package being tested.
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
  \code{mockMethod} repeatedly on each \code{Mock}.

  The optional \code{return.value} can be used to
  specifically control the output from the \code{Mock}, and
  different \code{return.value}s can be assigned to
  different \code{Mock} objects.
}
\examples{
mockMethod(mock, "TestMethod") # returns NULL
mockMethod(mock, "TestMethod", return.value = 1) # returns 1
mockMethod(list(mock1, mock2), "TestMethod") # assigns "TestMethod" to both mock1 and
mock2
}
\seealso{
  \code{\link{Mock}}
}

