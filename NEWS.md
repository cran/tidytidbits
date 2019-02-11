# tidytidbits 0.2.0

* added `count_at` to perform `count_by` for multiple variables and return the aggregated results in one tibble
* added `first_non_nas_at` and `first_which_non_na_at` to retrieve the first non na value, or which values are not na, using column selection like in `dplyr`'s `select`
* fix incompatibility with current release of purrr (now also exporting flatten_raw, as does rlang; extend exception list in NAMESPACE)

# tidytidbits 0.1.0

* initial release
