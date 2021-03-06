% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dplyr-like.R
\name{execute_if}
\alias{execute_if}
\alias{execute_if_else}
\title{Conditional execution in a pipeline}
\usage{
execute_if(.data, .predicate, .language)

execute_if_else(.data, .predicate, .language_true, .language_false)
}
\arguments{
\item{.data}{Data argument, typical "first" argument in dplyr verbs}

\item{.predicate}{Evaluated to boolean. If true, executes and returns language; otherwise, returns untouched .data}

\item{.language}{Language call to execute. Write is just as if you would without the execute_if:
Will be used as the right-hand side of "\%>\%" with all possible options of magrittr.}

\item{.language_true}{Execute if predicate it TRUE}

\item{.language_false}{Execute if predicate it FALSE}
}
\value{
Result of .language
}
\description{
A verb for a magrittr pipeline:
execute_if_else: The language is executed only if .predicate is true.
}
\examples{
library(magrittr)
library(dplyr)
library(tibble)
library(stringr)
convert_to_quartiles <- TRUE
tibble(score=c(1,2,3,4,1,2,3,4,2,3,2,3,4,3,3)) \%>\%
  mutate(do_something=1) \%>\%
  execute_if(convert_to_quartiles,
             mutate(score = cut(score, 4, labels = str_c("Quartile ", 1:4)))) \%>\%
  filter(score > 2)
}
