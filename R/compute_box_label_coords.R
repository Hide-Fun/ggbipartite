#' Compute representative label coordinates from box rectangles
#'
#' Summarize rectangle coordinates (`xmin`, `xmax`, `ymin`, `ymax`) into one
#' representative label position per group. The x coordinate is taken from the
#' outer edge of each side (`xmin` for `"row"`, `xmax` for `"column"`), and the
#' y coordinate is computed as the mean of vertical centres.
#'
#' @param .box A data frame/tibble containing rectangle coordinates and grouping
#'   columns.
#' @param .by Character vector of one or more column names used to define label
#'   groups (for example `c("row", "family")` or `"column"`).
#' @param .side One of `"row"` or `"column"`. Chooses the bipartite side that
#'   `.box` represents. `"row"` anchors labels to `xmin`; `"column"` anchors to
#'   `xmax`.
#'
#' @return A tibble with grouping columns from `.by` plus:
#' \itemize{
#'   \item `x`: representative x coordinate on the selected outer edge.
#'   \item `y`: representative y coordinate (mean vertical centre).
#' }
#'
#' @examples
#' row_box <- tibble::tibble(
#'   row = c("A", "B"),
#'   xmin = c(0, 0),
#'   xmax = c(5, 5),
#'   ymin = c(0, 10),
#'   ymax = c(8, 16)
#' )
#'
#' compute_box_label_coords(
#'   .box = row_box,
#'   .by = "row",
#'   .side = "row"
#' )
#'
#' @importFrom dplyr all_of mutate summarise
#' @export
compute_box_label_coords <- function(
  .box,
  .by,
  .side = c("row", "column")
) {
  if (!is.data.frame(.box)) {
    stop("`.box` must be a data frame or tibble.")
  }

  if (!is.character(.by) || length(.by) == 0 || anyNA(.by) || any(.by == "")) {
    stop("`.by` must be a non-empty character vector of column names.")
  }

  .side <- match.arg(.side)
  .by <- unique(.by)

  required_cols <- c(.by, "xmin", "xmax", "ymin", "ymax")
  missing_cols <- setdiff(required_cols, names(.box))
  if (length(missing_cols) > 0) {
    stop(
      paste0(
        "`.box` is missing required columns: ",
        paste(missing_cols, collapse = ", "),
        "."
      )
    )
  }

  coord_cols <- c("xmin", "xmax", "ymin", "ymax")
  is_numeric_col <- vapply(.box[coord_cols], is.numeric, logical(1))
  if (!all(is_numeric_col)) {
    non_numeric_cols <- coord_cols[!is_numeric_col]
    stop(
      paste0(
        "Rectangle coordinate columns must be numeric: ",
        paste(non_numeric_cols, collapse = ", "),
        "."
      )
    )
  }

  if (anyNA(.box[coord_cols])) {
    stop("Rectangle coordinate columns cannot contain NA values.")
  }

  x_col <- if (.side == "row") "xmin" else "xmax"
  x_summary <- if (.side == "row") min else max

  .box |>
    tibble::as_tibble() |>
    dplyr::mutate(.y_center = (.data$ymin + .data$ymax) / 2) |>
    dplyr::summarise(
      x = x_summary(.data[[x_col]]),
      y = mean(.data$.y_center),
      .by = dplyr::all_of(.by)
    )
}
