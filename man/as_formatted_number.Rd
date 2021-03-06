% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/stringr-like.R
\name{as_formatted_number}
\alias{as_formatted_number}
\title{Format numeric value for output}
\usage{
as_formatted_number(x, decimal_places = 1, remove_trailing_zeroes = T)
}
\arguments{
\item{x}{Numeric vector}

\item{decimal_places}{Decimal places to display}

\item{remove_trailing_zeroes}{If the required decimal places are less than decimal places,
should resulting trailing zeros be removed?}
}
\value{
Character vector
}
\description{
Vectorised conversion
}
\examples{
as_formatted_number(0.74167, 2) # gives "0.74"
}
