\name{called_once}
\alias{called_once}
\title{Assert that the named method was called once and only once.}
\usage{
  expect_that(mock, called_once("TestMethod"))
}
\arguments{
  \item{method.name}{character string identifying the
  method of interest.}
}
\description{
  The \code{called_once} function is designed to be called
  from within \code{test_that}s \code{expect_that}
  assertion. This checks the call records of the
  \code{Mock} to confirm that the method was called once,
  and only once. If not, the function will cause a test
  failure.
}
\details{
  Note that the method being checked must have first been
  assigned to the \code{Mock} with a call to
  \code{\link{mockMethod}}.
}
\seealso{
  \code{\link{called_once_with}}, \code{\link{mockMethod}}
}

