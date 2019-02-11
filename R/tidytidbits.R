#' @importFrom grDevices cairo_pdf dev.off png
#' @importFrom methods substituteDirect
#' @importFrom stats complete.cases prop.test setNames
#' @importFrom utils write.table
#' @importFrom magrittr "%>%"
#' @importFrom magrittr "%<>%"
#' @rawNamespace import(rlang, except="flatten_raw")
#' @rawNamespace import(tibble, except="has_name")
#' @import dplyr
#' @rawNamespace import(purrr, except=c("prepend", "flatten", "flatten_int", "flatten_chr", "flatten_dbl", "flatten_lgl", "flatten_raw", "as_function", "splice", "invoke", "%||%", "%@%", "list_along", "rep_along", "modify"))
#' @import tidyr
#' @import stringr
#' @import forcats
NULL


## quiets concerns of R CMD check re: the .'s that appear in pipelines
if(getRversion() >= "2.15.1") utils::globalVariables(c(".", ".prop_test"))
