#' @keywords internal
"_PACKAGE"

## Package-wide imports -------------------------------------------------
## These packages are used extensively throughout the package functions.
## Importing their full namespaces avoids polluting every function with
## explicit :: calls on every tidyverse verb.

#' @import dplyr
#' @import tidyr
#' @import ggplot2
#' @import stringr
#' @import purrr
#' @importFrom rlang sym .data :=
#' @importFrom scales percent_format
#' @importFrom cli cli_text cli_bullets
#' @importFrom labelled unlabelled
#' @importFrom haven zap_labels
#' @importFrom stats sd weighted.mean
#' @importFrom utils combn write.csv
NULL

## Suppress R CMD check "no visible binding" NOTEs for dplyr/ggplot2 column
## names used inside data-masking verbs (mutate, filter, aes, etc.).
utils::globalVariables(c(
  # funcs-align.R
  "aligned", "aligned_drop_na", "aligned_keep_na",
  "count_response_uwtd", "count_response_wtd",
  "cumulative_issue_alignment_n",
  "plurality_response",
  "prop_cumulative_plurality", "prop_cumulative_plurality_drop_na",
  "prop_questions_aligned", "prop_response_wtd",
  "question", "question_plurality_rank",
  "response",
  # funcs-plot.R
  "ci_lwr", "ci_upr",
  "first_below",
  "g", "group_col", "group_label",
  "label",
  "mean_prop",
  "pct", "pct_missing", "pct_support",
  "percent_both_support",
  "prop_cumulative_plurality",
  "q1", "q2",
  "row_id",
  "se_prop",
  "tooltip",
  "val_label",
  "variable"
))
