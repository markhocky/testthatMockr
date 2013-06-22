\name{called_once_with}
\alias{called_once_with}
\title{Assert method call and arguments called with.}
\usage{
  called_once_with(method.name, ...)
}
\arguments{
  \item{method.name}{character string identifying the
  method of interest.}

  \item{...}{an arbitrary number of arguments to be checked
  against the records of the call.}
}
\description{
  Similar to \code{called_once} this function is called
  from within \code{testthat}s \code{expect_that}
  assertion. The call records of the \code{Mock} are
  checked to ensure that the method was only called once,
  and that the arguments supplied to the \code{Mock} during
  the call match those specified.
}
\details{
  Note that the method being checked must have first been
  assigned to the \code{Mock} with a call to
  \code{\link{mockMethod}}.
}
\examples{
expect_that(mock, called_once_with("TestMethod", arg1, arg2))
}
\seealso{
  \code{\link{called_once_with}}, \code{\link{mockMethod}}
}

