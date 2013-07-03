\name{Mock}
\alias{Mock}
\title{Create a Mock class to support unit testing.}
\usage{
  Mock(spec = NULL)
}
\arguments{
  \item{spec}{a character string naming the class to be
  imitated.}
}
\value{
  A \code{Mock} object ready for use in your
  \code{testthat} unit tests.
}
\description{
  The \code{Mock} object is designed to mimic another
  object's behaviour, and also provide functionality to
  record operations performed on it. The \code{Mock} can be
  used as a stub for an object yet to be developed, or it
  can be used to mimic an existing object. The \code{Mock}
  can be provided with the name of a class to mimic via the
  \code{spec} parameter. In this case the Mock will be
  assigned this class, which is particularly useful if the
  \code{Mock} is to be used in the slot of another S4
  object. The \code{spec} parameter is optional however,
  and if not provided the \code{Mock} will simply be
  assigned the class "Mock".
}
\details{
  The \code{Mock} contains two slots:
  \item{call.results}{an environment used to store calls
  made on the \code{Mock} along with any arguments passed
  in as part of that call.} \item{methods}{an environment
  containing the names of methods registered for the
  \code{Mock} along with the optional values to be returned
  from the call} These slots are not intended to be used
  directly, but rather are the containers used by the
  \code{Mock}s methods, and for testing.
}
\seealso{
  \link{mockMethod}
}

