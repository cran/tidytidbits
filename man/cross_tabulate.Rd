% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dplyr-like.R
\name{cross_tabulate}
\alias{cross_tabulate}
\title{Create cross table from a tibble}
\usage{
cross_tabulate(.df, ..., useNA = c("no", "ifany", "always"), dnn = NULL)
}
\arguments{
\item{.df}{A data frame}

\item{...}{Factors to tabulate by: symbolic column names / language}

\item{useNA, dnn}{passed to \code{\link{table}()}}
}
\value{
Result from a call to \code{\link{table}()}
}
\description{
A wrapper of table() for convenient use in a dplyr pipeline:
Pass the factors to tabulate as symbols or expressions like you would in mutate().
useNA and dnn are passed to table().
}
\examples{
library(magrittr)
if (requireNamespace("survival", quietly = TRUE))
{
   survival::bladder1 \%>\%
      cross_tabulate(treatment, recur) \%>\%
      chisq.test()
}
}
