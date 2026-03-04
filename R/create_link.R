#' Build Link Coordinates Between Bipartite Boxes and Tree Tips
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' Summarises box coordinates into one y-value per id and joins them to tip
#' y-values extracted from tree data.
#'
#' @param box A data frame with `xmin`, `xmax`, `ymin`, `ymax`, and one id
#'   column (`row` or `column`).
#' @param ggtree A tree-like object accepted by `.extract_tree_data()`.
#' @param side One of `"row"` or `"column"` (preferred). Backward-compatible
#'   values `"left"`/`"right"` are also accepted.
#' @param x Numeric scalar used as segment start x.
#' @param xend Numeric scalar used as segment end x.
#' @param direction Backward-compatible alias for `side`.
#'
#' @return A data frame with columns `x`, `xend`, `y1`, and `y2`.
#' @export
create_link <- function(
  box,
  ggtree,
  side = c("row", "column", "left", "right"),
  x = 0,
  xend = 1,
  direction = NULL
) {
  if (!is.null(direction)) {
    side <- direction
  }

  side <- match.arg(side)
  side <- switch(
    side,
    left = "row",
    right = "column",
    side
  )

  if (!is.data.frame(box)) {
    stop("`box` must be a data frame.")
  }
  if (!is.numeric(x) || length(x) != 1 || !is.finite(x)) {
    stop("`x` must be a finite numeric scalar.")
  }
  if (!is.numeric(xend) || length(xend) != 1 || !is.finite(xend)) {
    stop("`xend` must be a finite numeric scalar.")
  }

  required_box_cols <- c("xmin", "xmax", "ymin", "ymax")
  missing_box_cols <- setdiff(required_box_cols, names(box))
  if (length(missing_box_cols) > 0) {
    stop(
      "`box` is missing required columns: ",
      paste(missing_box_cols, collapse = ", "),
      "."
    )
  }

  id_col <- if (identical(side, "row")) "row" else "column"
  if (!(id_col %in% names(box))) {
    stop(
      "`box` must contain `",
      id_col,
      "` when `side = \"",
      side,
      "\"`."
    )
  }

  box_summary <- box %>%
    tidyr::pivot_longer(
      cols = c("xmin", "xmax", "ymin", "ymax"),
      names_to = c("axis", "end"),
      names_pattern = "([xy])(min|max)"
    ) %>%
    tidyr::pivot_wider(
      names_from = "axis",
      values_from = "value"
    ) %>%
    dplyr::group_by(.data[[id_col]]) %>%
    dplyr::summarise(
      y1 = mean(.data$y, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      x = x,
      xend = xend
    )

  tree_data <- .extract_tree_data(ggtree)
  required_tree_cols <- c("isTip", "label", "y")
  missing_tree_cols <- setdiff(required_tree_cols, names(tree_data))
  if (length(missing_tree_cols) > 0) {
    stop(
      "Tree data is missing required columns: ",
      paste(missing_tree_cols, collapse = ", "),
      "."
    )
  }

  tip_summary <- tree_data %>%
    dplyr::filter(!is.na(.data$isTip) & .data$isTip) %>%
    dplyr::group_by(.data$label) %>%
    dplyr::summarise(
      y2 = mean(.data$y, na.rm = TRUE),
      .groups = "drop"
    )

  join_by <- stats::setNames("label", id_col)
  coords <- dplyr::left_join(box_summary, tip_summary, by = join_by)

  if (any(is.na(coords$y2))) {
    warning(
      "Some entries in `box` could not be matched to tree tip labels.",
      call. = FALSE
    )
  }

  coords
}
