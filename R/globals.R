#' Internal object name mappings for NSE verbs
#'
#' Declares the column names touched by dplyr/tidyr pipelines so that R CMD
#' check recognises them as intended objects.
#'
#' @noRd
utils::globalVariables(c(
  ".data",
  ".input_row_id",
  ".lvl",
  ".offset",
  ".row_id",
  "angle",
  "column",
  "cum_w_prev",
  "data",
  "height",
  "interaction_size",
  "set",
  "total_height",
  "total_interaction",
  "var",
  "w",
  "x",
  "xend",
  "xmax",
  "xmin",
  "y_end",
  "y_start",
  "ymax",
  "ymin"
))
