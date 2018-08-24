
#' A python / javascript-like "truthy" notion
#'
#' Values are truthy that are not null, NA, empty, 0, or FALSE.
#'
#' Note that this is per se not vectorised, because a non-empty list or vector is "truthy" as such.
#' @param x Any object, value or NULL
#'
#' @return logical
#' @export
truthy <- function(x)
{
  if (invalid(x))
    return(F)
  if (is.atomic(x))
  {
    if (is.character(x))
      return(b(as.logical(max(str_length(x)))))
    if (length(x) == 1)
      return(b(as.logical(x)))
    return(all(b(as.logical(x))))
  }
  else if (is.list(x))
  {
    return(length(x) != 0)
  }
  return(b(as.logical(x)))
}

#' @describeIn truthy x is not truthy
#' @export
falsy <- function(x)
{
  return (!truthy(x))
}

#' A notion of valid and invalid
#'
#' An object is valid if it is not null, not missing (NA), and is not an empty vector.
#' Note that this is per se not vectorised, because a non-empty list or vector is valid as such.
#' @param x Any object, value or NULL
#'
#' @return logical
#' @export
invalid <- function(x)
{
  return (is.null(x) || is.na(x) || (is.vector(x) && length(x) == 0))
}

#' @describeIn invalid x is not invalid
#' @export
valid <- function(x)
{
  return (!is.null(x) && !is.na(x) && !(is.vector(x) && length(x) == 0))
}

