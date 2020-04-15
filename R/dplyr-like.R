#' Executing language as if in a pipeline
#'
#' Executes the given language as if it was part of a magrittr pipeline\cr
#' \code{... \%>\% .language}\cr
#' while .data is the lhs value provided to .language as parameter by magrittr.
#'
#' Note that language is evaluated as a quosure in its captured environment.
#' This is fine if this method is called as a secondary helper and .language is already a quosure;
#' otherwise you may want to explicitly set the quosure's environment to your caller's env.
#'
#' @param .data A data frame
#' @param .language Language
#'
#' @return Result of the executed language
#' @export
execute_in_pipeline <- function(.data, .language)
{
  # nice thing is that all !! unquotation for the environment from where we are called is done
  # by enquo either here or one level up
  .language <- enquo(.language)
  # the quosure stores the environment from where it originates
  pipeline_env <- get_env(.language)
  # Something like ". %>% ..." creates a magrittr functional sequence, which can be called
  # Prepare such a call in our caller's environment

  magrittr_call <- quo(`%>%`(., !!.language))
  magrittr_call <- quo_squash(magrittr_call)
  #alternative
  #magrittr_call <- parse_expr(str_c(". %>% ", quo_text(.language)))

  magrittr_call <- new_quosure(magrittr_call, env=pipeline_env)
  # Call magrittr::%>%, creating a functiona sequence
  fseq <- eval_tidy(magrittr_call)
  # Call the sequence with our .data parameter
  rlang::invoke(fseq, list(.data))
}

#' Conditional execution in a pipeline
#'
#' A verb for a magrittr pipeline:
#' execute_if_else: The language is executed only if .predicate is true.
#'
#' @param .data Data argument, typical "first" argument in dplyr verbs
#' @param .predicate Evaluated to boolean. If true, executes and returns language; otherwise, returns untouched .data
#' @param .language Language call to execute. Write is just as if you would without the execute_if:
#'    Will be used as the right-hand side of "\%>\%" with all possible options of magrittr.
#'
#' @return Result of .language
#' @export
#'
#' @examples
#' library(magrittr)
#' library(dplyr)
#' library(tibble)
#' library(stringr)
#' convert_to_quartiles <- TRUE
#' tibble(score=c(1,2,3,4,1,2,3,4,2,3,2,3,4,3,3)) %>%
#'   mutate(do_something=1) %>%
#'   execute_if(convert_to_quartiles,
#'              mutate(score = cut(score, 4, labels = str_c("Quartile ", 1:4)))) %>%
#'   filter(score > 2)
execute_if <- function(.data, .predicate, .language)
{
  .predicate <- enquo(.predicate)
  if (eval_tidy(.predicate, data = .data))
  {
    .language <- enquo(.language)
    execute_in_pipeline(.data, !!.language)
  }
  else
    .data
}

#' execute_if_else: Like execute_if, but with an additional field of language which is executed if predicate is false
#' @param .language_true Execute if predicate it TRUE
#' @param .language_false Execute if predicate it FALSE
#' @export
#'
#' @rdname execute_if
execute_if_else <- function(.data, .predicate, .language_true, .language_false)
{
  .predicate <- enquo(.predicate)
  if (eval_tidy(.predicate, data = .data))
    .language <- enquo(.language_true)
  else
    .language <- enquo(.language_false)

  execute_in_pipeline(.data, !!.language)
}

#' Add summary to tibble
#'
#' A verb for a dplyr pipeline:
#' Performs a call to \code{summarise()}, but does not reduce the data frame to one row per group,
#' instead, adds the resulting fields to every row belonging to that group,
#' returning the original frame with added/changed columns.
#' Effectively, this is like calling \code{summarise()}, and then calling \code{mutate()} with all the resulting columns.
#' @param .data Data argument, typical "first" argument in dplyr verbs
#' @param .language A call to \code{summarise()},
#'    or another method performing equivalent aggregation (potentially wrapping \code{summarise()})
#'
#' @return The tibble with added columns
#' @export
add_summary <- function(.data, .language)
{
  .language <- enquo(.language)

  grouping_vars <- group_vars(.data)
  # get summarised data
  summarised_data <- execute_in_pipeline(.data, !!.language)

  if (has_length(grouping_vars))
  {
    .data %<>% inner_join(summarised_data, by=grouping_vars, suffix=c("", "summary-"))
  }
  else
  {
    if (nrow(summarised_data) == 1)
    {
      # no grouping -> we have a summary tibble with one row
      summarised_columns <- colnames(summarised_data)[!colnames(summarised_data) %in% grouping_vars]
      for (col in summarised_columns)
      {
        .data %<>% mutate(!!col := summarised_data[[col]])
      }
    }
    else
    {
      # No grouping accessible, but apparently there was grouping applied by the summarising language.
      # This can be the case if some integrated method like count_by was used instead of summarise().
      # Perform a merge on common columns.
      # This fails in corner cases (generated columns with the same name as table columns), but sorry for now.
      .data %<>%
        inner_join(summarised_data,
                   # need explicit by to avoid printed message
                   by = intersect(tbl_vars(.), tbl_vars(summarised_data)),
                   suffix=c("", "summary-"))
    }
  }
  .data
}

#' Add summary to tibble
#'
#' A verb for a dplyr pipeline:
#' Groups the frame by ... in addition to the current grouping,
#' then calls \code{\link{add_summary}}, then returns the frame with the mutated summarising columns
#' in the same grouping state as it was before this function was called.
#' @param .data Data argument, typical "first" argument in dplyr verbs
#' @param .language A call to \code{summarise()},
#'    or another method performing equivalent aggregation (potentially wrapping \code{summarise()})
#' @param ... Parameters for group_by
#'
#' @return The tibble with added columns
#' @export
add_summary_by <- function(.data, .language, ...)
{
  .language <- enquo(.language)
  vars <- quos(...)
  previous_grouping <- groups(.data)
  .data %>%
    group_by(!!!vars, add = T) %>%
    add_summary(!!.language) %>%
    group_by(!!!previous_grouping, add = F)
}

#' Lump rows of a tibble
#'
#' A verb for a dplyr pipeline:
#' In the given data frame, take the .level column as a set of levels and the .count column
#' as corresponding counts. Return a data frame where the rows are lumped according to levels/counts
#' using the parameters n, prop, other_level, ties.method like for \code{\link{lump}()}.
#' The resulting row for other_level has \code{level=other level}, \code{count=sum(count of all lumped rows)}.
#' For the remaining columns, either a default concatenation is used, or you can provide
#' custom summarising statements via the summarising_statements parameter.
#' Provide a list named by the column you want to summarize, giving statements wrapped in quo(),
#' using syntax as you would for a call to summarise().
#'
#' @param .df A data frame
#' @param .level Column name (symbolic) containing a set of levels
#' @param .count Column name (symbolic) containing counts of the levels
#' @param summarising_statements The "lumped" rows need to have all their columns summarised into one row.
#'    This parameter is a vars() list of arguments as if used in a call to \code{\link{summarise}()},
#'    name is column name, value is language.
#'    If not provided for a column, a default summary will be used
#'    which takes the sum if numeric, concatenates text, or uses any() if logical.
#' @param remaining_levels Levels that should explicitly not be lumped
#' @inheritParams lump
#'
#' @seealso \code{\link{lump}}
#'
#' @return The lumped data frame
#' @export
lump_rows <- function(.df, .level, .count, summarising_statements = quos(),
                      n, prop, remaining_levels, other_level = "Other",
                      ties.method = c("min", "average", "first", "last", "random", "max"))
{
  default_summarise <- function(v)
  {
    if (is_list(v))
      list(unique(unlist(v, recursive = F)))
    else if (is.factor(v))
    {
      present_levels <- unique(v)
      if (length(present_levels) == 1)
        present_levels
      else
        str_c(present_levels, collapse = ", ")
    }
    else if (is_double(v) || is_integer(v))
      sum(v, na.rm = T)
    else if (is_character(v))
    {
      if (length(unique(v)) == 1)
        v[1]
      else
        str_c(str_replace_na(v), collapse = ", ")
    }
    else if (is_logical(v))
      any(v)
    else
      NA
  }
  .level <- enquo(.level)
  .count <- enquo(.count)
  with_other <- !is.null(other_level) && !is.na(other_level)

  original_level <- .df %>% pull(!!.level)
  original_level_chr <- as.character(original_level)
  .df %<>% mutate(!!quo_name(.level) := as.character(!!.level))

  need_default_summarise <- tidyselect::vars_select(colnames(.df),
                                                    -one_of(group_vars(.df)),
                                                    -!!.level, -!!.count,
                                                    -one_of(names(summarising_statements)))
  default_summarising_statements <- list()
  for (field_need_default in need_default_summarise)
    default_summarising_statements[[field_need_default]] <- quo(default_summarise(!!sym(field_need_default)))

  # generate lumping dictionary: level -> level if remaining, NA if lumped
  if (!missing(remaining_levels))
  {
    lumping_dict <- .df %>% pull(!!.level)
    lumping_dict <- set_names(if_else(lumping_dict %in% remaining_levels, lumping_dict, na_chr), lumping_dict)
  }
  else
  {
    lumping_dict <- lump(.df %>% pull(!!.level),
                         .df %>% pull(!!.count),
                         n = n,
                         prop = prop,
                         other_level = NA,
                         ties.method = ties.method)
  }

  # if no rows are lumped, just return
  if (!sum(is.na(lumping_dict)) > 1)
    return(.df)


  if (with_other)
  {
    .df %>%
      filter(!(!!.level) %in% lumping_dict) %>%
      summarise(!!quo_name(.level) := other_level,
                !!quo_name(.count) := sum(!!.count),
                !!!summarising_statements,
                !!!default_summarising_statements) ->
      other_row
  }

  .df %<>% filter((!!.level) %in% lumping_dict)

  if (with_other)
  {
    .df %<>% bind_rows(other_row)
  }

  if (is.factor(original_level))
  {
    new_level <- .df %>% pull(!!.level)
    new_level_levels <- c(original_level_chr[original_level_chr %in% unique(new_level)], other_level)
    #.df[[quo_name(.level)]] <- factor(new_level, levels = unique(new_level_levels))
  }
  .df
}

#' Create cross table from a tibble
#'
#' A wrapper of table() for convenient use in a dplyr pipeline:
#' Pass the factors to tabulate as symbols or expressions like you would in mutate().
#' useNA and dnn are passed to table().

#' @param .df A data frame
#' @param ... Factors to tabulate by: symbolic column names / language
#' @param useNA,dnn passed to \code{\link{table}()}
#'
#' @return Result from a call to \code{\link{table}()}
#' @export
#'
#' @examples
#' library(magrittr)
#' if (requireNamespace("survival", quietly = TRUE))
#' {
#'    survival::bladder1 %>%
#'       cross_tabulate(treatment, recur) %>%
#'       chisq.test()
#' }
cross_tabulate <- function(.df, ...,
                           useNA = c("no", "ifany", "always"),
                           dnn = NULL)
{
  factors <- quos(...)
  useNA <- match.arg(useNA)
  if (is_null(dnn))
    dnn <- if_else(names(factors) == "",
                   map_chr(factors, quo_name),
                   names(factors))
  names(factors) <- NULL

  table_args <- list(useNA = useNA,
                     dnn = dnn)
  args <- c(factors, table_args)

  call <- quo(table(!!!args))
  eval_tidy(call, data = .df)
}

#' An interlude in a magrittr pipeline
#'
#' The given language is executed, with the pronoun "." set to .df
#' (usually the data frame sent through the pipeline),
#' but the results are ignored, and the next line in the pipeline gets the unchanged data.
#' Any executed code is allowed to edit variables which already exist in the
#' calling environment. This is useful to store intermediate results.
#'
#' Note: Detection of the calling environment is not solved cleanly;
#' it cannot be excluded that it fails under specific circumstances.
#'
#' @param .df Data argument, typical "first" argument in dplyr verbs
#' @param .language Language
#'
#' @return Unchanged .df
#' @export
#'
#' @examples
#' library(tibble)
#' library(magrittr)
#' library(dplyr)
#' x <- c() # now x exists in the calling env
#' tibble(a=1, b=2) %>%
#'    mutate(b=a+3) %>%
#'    interlude(x <- .$b) %>%
#'    mutate(a=a+1)
#' # x is set to 4
interlude <- function(.df, .language)
{
  .language <- enquo(.language)
  if (quo_is_missing(.language))
    return(.df)

  expr <- quo_squash(.language)
  lang_env <- get_env(.language)
  exec_env <- child_env(lang_env)
  # Provide the "." binding
  env_bind(exec_env, !!!list(. = .df))

  # execute
  eval_bare(expr, exec_env)

  # If the language assigned variables, they are contained in exec_env.
  # First, find the frame that actually called interlude() (there are magrittr pipe frames in between)
  # Second, check if an assigned variable is bound in the actually calling environment
  # Third, if it already exists, assign the new value.
  stack <- call_stack()[-1]
  my_func_name <- as.character(match.call()[[1]])
  pipe_env <- NULL
  for (i in seq_along(stack))
  {
    if (str_detect(expr_text(stack[[i]]$expr), my_func_name))
    {
      pipe_env <- stack[[i+1]]$env
    }
  }

  if (!is_null(pipe_env))
  {
    for (name in env_names(exec_env))
    {
      if (name != "." && env_has(pipe_env, name))
      {
        assign(name, env_get(exec_env, name), envir = lang_env, inherits = T)
      }
    }
  }

  .df
}

#' Format numeric columns for display
#'
#' Combines \code{\link{mutate_at}()} and \code{\link{as_formatted_number}()}
#'
#' @param .tbl A data frame
#' @param .vars A vars() list of symbolic columns
#' @inheritParams as_formatted_number
#'
#' @return Value of mutate_at
#' @export
#'
#' @seealso \code{\link{format_p_values_at}}
#'
#' @examples
#' library(tibble)
#' library(magrittr)
#' library(dplyr)
#' tibble(a=c(0.1, 0.238546)) %>%
#'     format_numbers_at(vars(a))
format_numbers_at <- function(.tbl, .vars,
                              decimal_places = 1,
                              remove_trailing_zeroes = T)
{
  mutate_at(.tbl, .vars,
            ~as_formatted_number(.,
                                 decimal_places = decimal_places,
                                 remove_trailing_zeroes = remove_trailing_zeroes)
  )
}

#' Format numeric columns for display
#'
#' Combines \code{\link{mutate_at}()} and \code{\link{as_formatted_p_value}()}
#'
#' @param .tbl A data frame
#' @param .vars A vars() list of symbolic columns
#' @inheritParams as_formatted_p_value
#'
#' @return Value of mutate_at
#' @export
#'
#' @seealso \code{\link{format_numbers_at}}
#'
#' @examples
#' library(tibble)
#' library(magrittr)
#' library(dplyr)
#' tibble(p=c(0.05, 0.0001)) %>%
#'     format_numbers_at(vars(p))
format_p_values_at <- function(.tbl, .vars,
                               decimal_places = 3,
                               prefix = "p",
                               less_than_cutoff = 0.001,
                               remove_trailing_zeroes = T,
                               alpha = 0.05,
                               ns_replacement = NULL)
{
  mutate_at(.tbl, .vars, ~as_formatted_p_value(.,
                                               decimal_places = decimal_places,
                                               prefix = prefix,
                                               less_than_cutoff = less_than_cutoff,
                                               remove_trailing_zeroes = remove_trailing_zeroes,
                                               alpha = alpha,
                                               ns_replacement = ns_replacement)
  )
}

# Deprecate?
count_and_summarise <- function(.tbl,
                                ...,
                                columns = c("n", "rel", "percent"),
                                percentage_label_decimal_places = 1,
                                group_by = NULL)
{
  stopifnot(has_length(columns) && length(columns) <=3)
  additional_statements <- quos(...)
  previous_grouping <- groups(.tbl)
  .tbl %>%
    execute_if(has_length(group_by),
               group_by(!!!group_by)) %>%
    summarise(!!columns[[1]] := n(), !!!additional_statements) %>%
    execute_if(length(columns) >= 2,
               mutate(!!columns[[2]] := n/sum(n))
    ) %>%
    execute_if(length(columns) >= 3,
               mutate(!!columns[[3]] := as_percentage_label(!!sym(columns[[2]]), percentage_label_decimal_places))
    ) %>%
    execute_if(has_length(group_by),
               group_by(!!!group_by, add = F))
}

#' Count according to grouping
#'
#' Similar to \code{dplyr::\link{count}()}, but also adds the relative proportion and
#' a percent-formatted string of the relative proportion,
#' and allows to specify the column names.
#'
#' @param .tbl A data frame
#' @param ... Columns / expressions by which to group / which shall be used for counting.
#' @param column_names vector if size 1 to 3, giving the names of (in order if unnamed, or named with n, rel, percent)
#'     the column containing the count, the relative proportion, and the latter formatted as a percent label.
#'     If a name is not contained, it will not be added.
#' @param percentage_label_decimal_places Decimal precision of the percent label
#' @param add_grouping Shall a pre-existing grouping be preserved for counting (adding the newly specified grouping)?
#'     Default is yes, which differs from group_by.
#' @param na.rm Shall NA values be removed prior to counting?
#'
#' @return The counted data frame
#' @export
#'
#' @examples
#' library(magrittr)
#' if (requireNamespace("survival", quietly = TRUE))
#' {
#'    survival::aml %>%
#'    count_by(x)
#' }
count_by <- function(.tbl,
                     ...,
                     column_names = c("n", "rel", "percent"),
                     percentage_label_decimal_places = 1,
                     add_grouping = T,
                     na.rm = F)
{
  stopifnot(has_length(column_names))
  grouping <- quos(...)
  previous_grouping <- groups(.tbl)

  if (is_named(column_names))
  {
    renames <- c()
    removals <- c()
    if ("n" %in% names(column_names))
      renames[[column_names[["n"]] ]] <- "n"
    else
      removals <- c(removals, "n")

    if ("rel" %in% names(column_names))
      renames[[column_names[["rel"]] ]] <- ".rel.count.by."
    else
      removals <- c(removals, ".rel.count.by.")

    if ("percent" %in% names(column_names))
      renames[[column_names[["percent"]] ]] <- ".percent.count.by."
    else
      removals <- c(removals, ".percent.count.by.")
  }
  else
  {
    renames <- set_names( c("n", ".rel.count.by.", ".percent.count.by."),
                          column_names)
    renames <- renames[!is.na(names(renames))]
    removals <- renames[is.na(names(renames))]
  }

  if (na.rm)
  {
    grouping_lang <- keep(grouping, ~quo_is_call(.x))
    grouping_symbol <- keep(grouping, ~quo_is_symbol(.x))
    complete <- rep(T, nrow(.tbl))
    if (has_length(grouping_lang))
      complete <- complete & complete.cases(.tbl %>% transmute(!!!grouping_lang))
    if (has_length(grouping_symbol))
      complete <- complete & complete.cases(.tbl %>% select(!!!grouping_symbol))

    .tbl <- .tbl[complete, ]
  }

  # we use tally, which uses "n" as hard-coded column name
  if ("n" %in% colnames(.tbl))
  {
    .tbl %<>% select(-n)
  }

  .tbl %<>%
    group_by(!!!grouping, add = add_grouping) %>%
    tally() %>%
    mutate(.rel.count.by.=n/sum(n),
           .percent.count.by. = as_percentage_label(.data$.rel.count.by.,
                                                    percentage_label_decimal_places)
           ) %>%
    rename(!!!renames) %>%
    select(-one_of(removals))

  if (add_grouping)
  {
    .tbl %<>% group_by(!!!previous_grouping, add=F)
  }
  .tbl
}

#' Count by multiple variables
#'
#' @param .tbl A data frame
#' @param .vars A list of variables (created using vars()) for which \code{\link{count_by}} is to be called
#' @param .grouping Additional grouping to apply prior to counting
#' @param label_style Character vector containing one of "wide" and "long" or both.\itemize{
#'     \item "wide": Include labels in wide format, i.e., for each variable one column named as variable
#'     and giving the label for the corresponding count, but NA for all rows from different variables
#'     \item "long": Include two meta columns, one giving the variable that is counted (value from .vars),
#'     the second giving the label (which value/category of the variable is counted?).
#'     }
#' @param na_label If na.rm=F, label to use for counting NA values
#' @param long_label_column_names Character vector of size 2: If label_style contains "long",
#'    the names for the additional meta columns for variable and category
#' @inheritParams count_by
#'
#' @return A data frame concatenated from individual count_by results, with labels as per label_style.
#' @export
#'
#' @examples
#' library(magrittr)
#' library(datasets)
#' library(dplyr)
#' mtcars %>% count_at(vars(gear, cyl))
count_at <- function(.tbl,
                     .vars,
                     .grouping = vars(),
                     label_style = "long",
                     long_label_column_names = c("variable", "category"),
                     column_names = c("n", "rel", "percent"),
                     na_label = "missing",
                     percentage_label_decimal_places = 1,
                     add_grouping = T,
                     na.rm = F)
{
  label_style <- match.arg(label_style, c("wide", "long"), several.ok = T)
  labels <- list()
  if ("long" %in% label_style)
    labels <- append(labels, long_label_column_names)
  if ("wide" %in% label_style)
    labels <- append(labels, .vars)

  map_dfr(.vars, function(.var)
  {
    var_name <- quo_name(.var)
    na_replacement_list <- set_names(list(na_label), var_name)
    .tbl %>%
      mutate(!!var_name := if(is.factor(!!.var)) as.character(!!.var) else !!.var) %>%
      count_by(!!!.grouping, !!.var,
               column_names = column_names,
               percentage_label_decimal_places = percentage_label_decimal_places,
               add_grouping = add_grouping,
               na.rm = na.rm) %>%
      replace_na(na_replacement_list)
  }) %>%
    mutate(!!long_label_column_names[[1]] := map_chr(.vars, quo_name)[first_which_non_na_at(., !!!.vars)],
           !!long_label_column_names[[2]] := first_non_nas_at(., !!!.vars)) %>%
    select(!!!labels, !!!.grouping, !!!column_names)
}


# For use with a tibble in a pipe:
# Using one-group prop.test, adds confidence intervals (with given conf.level)
# for the proportion of x positive results in n trials,
# and the p value that the proportion is equal to p (default: 0.5)
# (to add the estimated proportion itself, use count_by)
#' Title
#'
#' @param .df A data frame
#' @param x The column/vector with the number of positive results
#' @param n The column/vector/constant with the number of trials
#' @param p Assumed proportion: Will add a p-value that the proportion is equal to p (default: 0.5)
#' @param CI_lower_name,CI_upper_name,p_name Column names of the added columns
#' @param alternative,conf.level,correct As for \code{\link{prop.test}}
#'
#' @return Data frame with columns added
#' @export
#'
#' @seealso \code{\link{count_by}()}
#'
#' @examples
#' library(magrittr)
#' if (requireNamespace("survival", quietly = TRUE))
#' {
#'    survival::aml %>%
#'    count_by(x) %>%
#'    add_prop_test(n, sum(n), rel)
#' }
add_prop_test <- function(.df, x, n, p = NULL,
                          CI_lower_name = "CI_lower",
                          CI_upper_name = "CI_upper",
                          p_name = "p",
                          alternative = c("two.sided", "less", "greater"),
                          conf.level = 0.95,
                          correct = TRUE)
{
  x <- enquo(x)
  n <- enquo(n)
  p <- enquo(p)
  if (quo_is_null(p))
    p <- 0.5

  mutate(.df,
         .prop_test=pmap(list(!!x,
                              !!n,
                              !!p),
                         function(x,n,p)
                         {
                           if (p == 0)
                             p <- .Machine$double.eps
                           prop.test(x, n, p,
                                     alternative = alternative,
                                     conf.level = conf.level,
                                     correct = correct)
                         }),
         !!CI_lower_name := map_dbl(.data$.prop_test, ~.$conf.int[[1]]),
         !!CI_upper_name := map_dbl(.data$.prop_test, ~.$conf.int[[2]]),
         !!p_name := map_dbl(.data$.prop_test, "p.value")
  ) %>%
    select(-.prop_test)
}



