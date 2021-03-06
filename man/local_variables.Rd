% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utilities.R
\name{local_variables}
\alias{local_variables}
\alias{localVariables}
\alias{source_variables}
\alias{sourceVariables}
\title{"Variable generating" functions}
\usage{
local_variables(env = parent.frame())

localVariables(env = parent.frame())

source_variables(localVars)

sourceVariables(localVars)
}
\arguments{
\item{env}{Parent environment}

\item{localVars}{Result of function call exporting an environment}
}
\value{
Named vector of created local variables

The updated environment
}
\description{
A pair of functions that allows a "variable generating" function
and read this function's local vars into the environment of the caller.
}
\examples{
myVariableGeneratingFunction <- function()
{
  x <- 1
  y <- 2
  local_variables()
}
myMainFunction <- function()
{
  source_variables(myVariableGeneratingFunction())
  print(c(x, y))
}
}
