% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/rlang-like.R
\name{symbol_as_quosure}
\alias{symbol_as_quosure}
\title{Make quosure from symbol}
\usage{
symbol_as_quosure(x, env = caller_env())
}
\arguments{
\item{x}{Symbol}

\item{env}{Environment for the created quosure}
}
\value{
Quosure containing the symbol
}
\description{
Make quosure from symbol
}
