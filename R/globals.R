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
  "delta",
  "edge_id",
  "id",
  "interaction_size",
  "label",
  "set",
  "total_height",
  "total_interaction",
  "var",
  "w",
  "x",
  "xend",
  "x_lab",
  "xmax",
  "xmin",
  "y",
  "y_end",
  "y_lab",
  "y_start",
  "yend",
  "ymax",
  "ymin"
))
