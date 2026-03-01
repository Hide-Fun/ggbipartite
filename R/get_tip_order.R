#' Get Tip Label Order from Tree Data
#'
#' Extracts tip labels from tree data and returns them ordered by `y` position
#' and then by label.
#'
#' @param x A tree-like object accepted by `.extract_tree_data()`.
#'
#' @return A character vector of unique tip labels.
#' @export
get_tip_order <- function(x) {
  tree_data <- .extract_tree_data(x)
  required_cols <- c("isTip", "label", "y")
  missing_cols <- setdiff(required_cols, names(tree_data))

  if (length(missing_cols) > 0) {
    stop(
      "Tree data is missing required columns: ",
      paste(missing_cols, collapse = ", "),
      "."
    )
  }

  tip_labels <- tree_data |>
    dplyr::filter(!is.na(.data$isTip) & .data$isTip) |>
    dplyr::filter(!is.na(.data$label), !is.na(.data$y)) |>
    dplyr::arrange(.data$y, .data$label) |>
    dplyr::distinct(.data$label) |>
    dplyr::pull(.data$label)

  if (length(tip_labels) == 0) {
    stop("No tips were found in tree data.")
  }

  tip_labels
}
