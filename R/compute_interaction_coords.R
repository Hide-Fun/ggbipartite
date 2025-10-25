#' Compute interaction polygon coordinates for a bipartite layout
#'
#' Given per-row and per-column box layouts and an interaction-cell table, this
#' function constructs 4-vertex polygons (one per nonzero interaction cell)
#' describing the ribbon/rectangle that connects the corresponding row box to
#' the column box. Each polygon is returned as four points in counter-clockwise
#' (CCW) order along with its area.
#'
#' @param .box1 A data frame/tibble describing the row-side boxes. Must contain
#'   at least a `row` identifier, the x coordinate to join on, and the vertical
#'   span information required by `split_y_by_interaction()`.
#' @param .box2 A data frame/tibble describing the column-side boxes. Must
#'   contain at least a `column` identifier, the x coordinate to join on, and
#'   the matching vertical span information.
#' @param .interation_cell A data frame/tibble of interaction cells (retains the
#'   original argument spelling). Requires, at minimum, the `row`, `column`, and
#'   per-cell interaction size used for splitting.
#'
#' @section Required inputs:
#' \describe{
#'   \item{`.box1`}{A tibble/data frame describing the \emph{row-side} boxes.
#'     Must contain at least:
#'     \itemize{
#'       \item \code{row}: row identifier (key used to join with `.interation_cell`).
#'       \item \code{x}: the x coordinate of the row-side vertical edge.
#'       \item \code{xmin}/\code{xmax} (if present): \code{split_y_by_interaction()}
#'             may use these; this function passes \code{x_side = "xmax"} when
#'             splitting the row side.
#'       \item \code{y} or precomputed vertical extents per row are not required;
#'             they are produced by \code{split_y_by_interaction()}.
#'       \item \code{interaction_size}: will be dropped if present.
#'     }}
#'   \item{`.box2`}{A tibble/data frame describing the \emph{column-side} boxes.
#'     Must contain at least:
#'     \itemize{
#'       \item \code{column}: column identifier (key used to join).
#'       \item \code{x}: the x coordinate of the column-side vertical edge.
#'       \item \code{xmin}/\code{xmax} (if present): \code{split_y_by_interaction()}
#'             may use these; this function passes \code{x_side = "xmin"} when
#'             splitting the column side.
#'       \item \code{interaction_size}: will be dropped if present.
#'     }}
#'   \item{`.interation_cell`}{(sic) A tibble/data frame of interaction cells with
#'     at least \code{row}, \code{column}, and the per-cell "interaction size"
#'     that \code{split_y_by_interaction()} uses to subdivide vertical spans.
#'     (The original spelling \code{.interation_cell} is retained to match the
#'     calling code.)}
#' }
#'
#' @details
#' The workflow is:
#' \enumerate{
#'   \item Join `.box1` and `.interation_cell`, then call
#'         \code{split_y_by_interaction(x_side = "xmax", var = "row")} to obtain
#'         \code{y_start}/\code{y_end} for each \code{row}–\code{column} cell on
#'         the row side and duplicate the x coordinate into \code{x1}/\code{x2}.
#'   \item Join `.box2` and `.interation_cell`, then call
#'         \code{split_y_by_interaction(x_side = "xmin", var = "column")} to obtain
#'         the analogous \code{y3}/\code{y4} and \code{x3}/\code{x4} for the column side.
#'   \item Merge both sides, pivot the four vertex columns to long format, and
#'         reconstruct (x, y) pairs for the 4 vertices.
#'   \item Order each 4-point set counter-clockwise (\code{\link{order_ccw_df}}),
#'         compute its area via the shoelace formula
#'         (\code{\link{polygon_area_xy}}), then unnest.
#' }
#'
#' @return A tibble with columns:
#' \describe{
#'   \item{\code{row}}{Row identifier.}
#'   \item{\code{column}}{Column identifier.}
#'   \item{\code{x}, \code{y}}{Polygon vertex coordinates in CCW order (four rows
#'                              per \code{row}–\code{column} cell).}
#'   \item{\code{area}}{Polygon area (repeated across the four vertices of a cell).}
#' }
#'
#' @note
#' This function relies on an external helper \code{split_y_by_interaction()}
#' that must exist in scope and return \code{y_start}, \code{y_end}, and \code{x}.
#' No NA values are permitted in the polygon vertex coordinates.
#'
#' @examples
#' \dontrun{
#' # Minimal example (requires user-defined split_y_by_interaction()):
#' box1 <- tibble::tibble(row = 1:2, x = 0.0, xmin = 0.0, xmax = 0.4, interaction_size = c(NA, NA))
#' box2 <- tibble::tibble(column = 1:2, x = 1.0, xmin = 0.6, xmax = 1.0, interaction_size = c(NA, NA))
#' cells <- tibble::tibble(row = c(1,1,2,2), column = c(1,2,1,2), interaction_size = c(2,1,1,3))
#' coords <- compute_interaction_coords(box1, box2, cells)
#' }
#'
#' @importFrom dplyr select right_join full_join mutate arrange
#' @importFrom dplyr %>% rename
#' @importFrom tidyr pivot_longer nest unnest
#' @importFrom purrr map map_dbl
#' @export
compute_interaction_coords <- function(
  .box1,
  .box2,
  .interation_cell
) {
  row <- .box1 %>%
    dplyr::select(!interaction_size) %>%
    dplyr::right_join(
      .interation_cell,
      by = dplyr::intersect(names(.box1), names(.interation_cell))
    ) %>%
    split_y_by_interaction(x_side = "xmax", var = "row") %>%
    dplyr::select(row, column, y1 = y_start, y2 = y_end, x1 = x, x2 = x)

  column <- .box2 %>%
    dplyr::select(!interaction_size) %>%
    dplyr::right_join(
      .interation_cell,
      by = dplyr::intersect(names(.box2), names(.interation_cell))
    ) %>%
    split_y_by_interaction(x_side = "xmin", var = "column") %>%
    dplyr::select(row, column, y3 = y_start, y4 = y_end, x3 = x, x4 = x)

  interaction_coords <- dplyr::full_join(
    row,
    column,
    by = c("row", "column")
  ) %>%
    tidyr::pivot_longer(
      cols = tidyselect::matches("^(x|y)\\d+$"),
      names_to = c(".value", "set"),
      names_pattern = "(.)(.)"
    ) %>%
    dplyr::select(!set) %>%
    tidyr::nest(.by = c(row, column)) %>%
    dplyr::mutate(
      data = purrr::map(.data$data, order_ccw_df),
      area = purrr::map_dbl(.data$data, polygon_area_xy)
    ) %>%
    dplyr::arrange(dplyr::desc(.data$area)) %>%
    tidyr::unnest(c(data))

  return(interaction_coords)
}

#' Order a set of 2D points counter-clockwise (CCW)
#'
#' Given a data frame with exactly four points (\code{x}, \code{y}), compute the
#' centroid and return the points ordered by increasing polar angle around the
#' centroid (i.e., CCW order).
#'
#' @param df A data frame/tibble with columns \code{x} and \code{y} and exactly
#'   4 rows.
#'
#' @return The input data with rows reordered to CCW vertex order. All original
#' columns are preserved.
#'
#' @keywords internal
#' @importFrom dplyr mutate arrange select
order_ccw_df <- function(df) {
  stopifnot(nrow(df) == 4, all(c("x", "y") %in% names(df)))

  # Compute centroid
  cx <- mean(df$x)
  cy <- mean(df$y)

  # Sort by polar angle around the centroid (counter-clockwise)
  df %>%
    dplyr::mutate(angle = atan2(.data$y - cy, .data$x - cx)) %>%
    dplyr::arrange(.data$angle) %>%
    dplyr::select(!angle)
}

#' Polygon area via the shoelace formula
#'
#' Compute the signed area magnitude of a simple (non-self-intersecting) polygon
#' given its vertices in traversal order. The result is always non-negative.
#'
#' @param df A data frame/tibble with numeric columns \code{x} and \code{y} and
#'   at least 3 rows (vertices).
#'
#' @return A non-negative numeric scalar: the polygon area.
#'
#' @details
#' The area is computed as
#' \deqn{A = \frac{1}{2}\left|\sum_{i=1}^{n} (x_i y_{i+1} - y_i x_{i+1})\right|}
#' with \eqn{(x_{n+1}, y_{n+1}) = (x_1, y_1)}.
#'
#' @keywords internal
polygon_area_xy <- function(df) {
  # inputs: df with columns x, y; vertices must be in a non-self-intersecting ring order
  stopifnot(all(c("x", "y") %in% names(df)))
  stopifnot(nrow(df) >= 3)
  stopifnot(!anyNA(df$x), !anyNA(df$y))

  # Extract coordinates in given order
  x <- df$x
  y <- df$y

  # Shoelace formula (returns positive area)
  x_next <- c(x[-1], x[1]) # wrap to first
  y_next <- c(y[-1], y[1])
  abs(sum(x * y_next - y * x_next)) / 2
}


#' Split y-direction range by strictly positive interaction values (grouped by a single key) and add drawing coordinates
#'
#' @description
#' Splits the y-range `[ymin, ymax]` within groups defined by a single key column
#' (`var`), proportionally to strictly positive `interaction` values. Computes
#' `y_start`/`y_end` and adds `x`/`xend` for vertical segment drawing.
#'
#' @details
#' - **Required columns**: the grouping key `var`, `xmin`, `xmax`, `ymin`, `ymax`, `interaction`.
#'   An error is raised if any are missing.
#' - **Validation**: `interaction` must be strictly positive (`> 0`) and non-missing for all rows.
#' - **Grouping**: by the single provided key `var` only. Bounds (`xmin/xmax/ymin/ymax`)
#'   are assumed to be fixed upstream and therefore excluded from grouping.
#' - **Coordinates**: if `x_side == "xmin"`, set `x = xmin`; if `"xmax"`, set `x = xmax`;
#'   in both cases `xend = x` (vertical segments).
#'
#' @param df A `data.frame`/`tibble` containing the required columns.
#' @param x_side One of `c("xmin","xmax")`. Chooses which side provides the x coordinate.
#' @param var Character scalar giving the column name to group by.
#'
#' @return
#' A `tibble` with all original columns plus:
#' - `y_start`, `y_end`: start/end of the y-slice
#' - `x`, `xend`: x-coordinates for vertical segments (`xend == x`)
#' - `.input_row_id`: internal row ID to preserve global input order
#'
#' @examples
#' library(dplyr)
#'
#' df <- tibble::tibble(
#'   row = c(1,1,1,2,2),
#'   column = c(1,1,1,1,1),
#'   xmin = 0, xmax = 1,
#'   ymin = 0, ymax = 1,
#'   interaction = c(0.2, 0.3, 0.5, 0.6, 0.4)
#' )
#'
#' # Group by "row"
#' split_y_by_interaction(df, var = "row")
#'
#' # Group by "column"
#' split_y_by_interaction(df, var = "column")
#'
#' # Use xmax as x reference while grouping by "row"
#' split_y_by_interaction(df, x_side = "xmax", var = "row")
#'
#' @seealso [dplyr::group_by()], [dplyr::arrange()]
#' @export
split_y_by_interaction <- function(
  df,
  x_side = c("xmin", "xmax"),
  var
) {
  x_side <- match.arg(x_side)

  # Validate `var` argument
  if (
    missing(var) ||
      length(var) != 1L ||
      !is.character(var) ||
      is.na(var) ||
      var == ""
  ) {
    stop(
      "`var` must be a single non-empty character column name.",
      call. = FALSE
    )
  }

  # Check required columns exist
  required_cols <- c(var, "xmin", "xmax", "ymin", "ymax", "interaction")
  missing_cols <- setdiff(required_cols, names(df))
  if (length(missing_cols) > 0) {
    stop(
      paste0(
        "Missing required columns: ",
        paste(missing_cols, collapse = ", ")
      ),
      call. = FALSE
    )
  }

  # Validate: interaction must be strictly positive and non-missing
  if (any(is.na(df$interaction) | df$interaction <= 0)) {
    stop(
      "`interaction` must be strictly positive (> 0) and non-missing.",
      call. = FALSE
    )
  }

  out <- df %>%
    dplyr::mutate(.input_row_id = dplyr::row_number()) %>%
    # Group by the single key column
    dplyr::group_by(dplyr::across(dplyr::all_of(var))) %>%
    # Preserve input order within group
    dplyr::arrange(.input_row_id, .by_group = TRUE) %>%
    dplyr::mutate(
      total_height = ymax - ymin,
      total_interaction = sum(interaction),
      w = interaction / total_interaction,
      cum_w_prev = dplyr::lag(cumsum(w), default = 0),
      y_start = ymin + total_height * cum_w_prev,
      y_end = ymin + total_height * (cum_w_prev + w),
      x = if (x_side == "xmin") xmin else xmax,
      xend = x
    ) %>%
    dplyr::ungroup() %>%
    # Restore global input order exactly
    dplyr::arrange(.input_row_id) %>%
    dplyr::select(
      dplyr::all_of(names(df)),
      y_start,
      y_end,
      x,
      xend,
      .input_row_id
    )

  out
}
