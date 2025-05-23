% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/check_dataframeish.R
\name{check_dataframeish}
\alias{check_dataframeish}
\alias{assert_dataframeish}
\alias{test_dataframeish}
\title{check_dataframeish}
\usage{
check_dataframeish(obj)

assert_dataframeish(obj, .var.name = checkmate::vname(obj), add = NULL)

test_dataframeish(obj)
}
\arguments{
\item{obj}{object to test}

\item{.var.name}{character(1) Name of the checked object to print in assertions. Defaults to the heuristic implemented in vname.}

\item{add}{\link[checkmate:AssertCollection]{checkmate::AssertCollection} Collection to store assertion messages. See \link[checkmate:AssertCollection]{checkmate::AssertCollection}.}
}
\value{
see the \link{checkmate} package for more information on check/assert/test functions
}
\description{
Tests if an object has data.frame methods \code{select}, \code{filter}, \code{mutate}, and \code{collect} defined so that we can treat it
like a data.frame.
}
\section{Functions}{
\itemize{
\item \code{assert_dataframeish()}: checkmate assert function for check_dataframeish

\item \code{test_dataframeish()}: checkmate test function for check_dataframeish

}}
\examples{
check_dataframeish(list())
# FALSE
}
