% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/purrr-like.R
\name{prepend_object}
\alias{prepend_object}
\title{Prepending in a pipe, never unlisting}
\usage{
prepend_object(x, .l, name = NULL, before = 1)
}
\arguments{
\item{x}{Object to prepend. If the object is a list, then it is appended as-is, and not unlisted.}

\item{.l}{The list to append to.
Special case handling applies if .l does not exist: then an empty list is used.
This alleviates the need for an initial mylist <- list()}

\item{name}{Will be used as name of the object in the list}

\item{before}{Prepend before this index}
}
\value{
The list .l with x prepended
}
\description{
Prepend to a given list, while considering as a single object and not unlisting.
Argument order is reversed compared to base::append or purrr::prepend to allow a different pattern of use in a pipe.
}
\examples{
#' library(tibble)
library(magrittr)
library(dplyr)
results <- list(second=list(1,2), third=list(3))
list(-1, 1) \%>\%
  prepend_object(results, "first") ->
results
# results has length 3, containing three lists
}
