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
NULL
