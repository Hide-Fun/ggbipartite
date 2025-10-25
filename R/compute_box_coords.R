#' Compute stacked rectangle coordinates for 1D partitions
#'
#' Given a data frame with a numeric `.size` column and a categorical variable,
#' compute rectangle coordinates (`xmin`, `xmax`, `ymin`, `ymax`) for a vertical
#' stack of boxes whose heights equal `.size`. This is useful for drawing stacked
#' bars or custom block diagrams with `ggplot2::geom_rect()`.
#'
#' Boxes are stacked along the y-axis starting at `.y0`. If `.var` is a factor,
#' its level order governs the stacking order. If `.var` is not a factor, the
#' stacking order is the reverse of the first-appearance order of the values.
#' The returned rows are reordered back to the original input row order; the
#' factor handling only affects how the cumulative offsets are computed.
#'
#' @param .df A data frame or tibble containing at least:
#'   - a numeric, non-negative column for heights referenced by `.size` with no `NA`s;
#'   - a column referenced by `.var`.
#' @param .var Bare column name in `.df` giving the grouping/category whose
#'   values define the stacked boxes. Captured with tidy evaluation.
#' @param .size Bare column name in `.df` giving the height for each box
#'   (previously hard-coded `interaction_size`). Captured with tidy evaluation.
#' @param .width Positive numeric scalar. The common width of all boxes on
#'   the x-axis. Default: `1`.
#' @param .x0 Numeric scalar. The left edge (x) of all boxes. Default: `0`.
#' @param .y0 Numeric scalar. The baseline (y) from which stacking starts.
#'   Default: `0`.
#' @param .gap Non-negative numeric scalar. Vertical gap inserted between
#'   adjacent stacked boxes. Applied to the cumulative offset. Default: `0`.
#'
#' @details
#' Input validation ensures `.size` is numeric, non-negative, and contains
#' no missing values. Scalars `.width`, `.gap`, `.x0`, and `.y0` are checked
#' for appropriate types and domains.
#'
#' Internally, the function:
#' 1) captures `.var` and `.size` via `rlang::enquo()`;
#' 2) establishes stacking order (factor levels preserved; otherwise reverse
#'    first-appearance order);
#' 3) computes cumulative offsets with optional `.gap`;
#' 4) returns coordinates while restoring the original input row order.
#'
#' @return A tibble with the following columns, in the original row order:
#' \itemize{
#'   \item \code{.var} — the chosen grouping column, with its original name;
#'   \item \code{.size} — the input heights;
#'   \item \code{xmin}, \code{xmax}, \code{ymin}, \code{ymax} — rectangle
#'         boundaries suitable for \code{ggplot2::geom_rect()}.
#' }
#'
#' @seealso [ggplot2::geom_rect()], [rlang::enquo()]
#'
#' @importFrom rlang enquo
#' @importFrom rlang :=
#' @export
#'
#' @examples
#' library(tibble)
#' library(ggplot2)
#'
#' # Non-factor: levels become reverse of input order (A,B,C -> C,B,A) for stacking,
#' # but output order stays as A,B,C
#' df1 <- tibble(var = c("A","B","C"), interaction_size = c(3,1,2))
#' coords1 <- compute_box_coords(df1, .var = var, .size = interaction_size, .gap = 0.2)
#'
#' # Factor: existing levels are preserved for stacking
#' df2 <- tibble(
#'   var  = factor(c("low","mid","high"), levels = c("high","mid","low")),
#'   interaction_size = c(1,2,3)
#' )
#' coords2 <- compute_box_coords(df2, .var = var, .size = interaction_size, .gap = 0.1)
#'
#' ggplot(coords1) +
#'   geom_rect(aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = var)) +
#'   coord_fixed() + theme_minimal()
compute_box_coords <- function(
  .df,
  .var,
  .size,
  .width = 1,
  .x0 = 0,
  .y0 = 0,
  .gap = 0
) {
  # --- capture quosures ---
  var_quo <- rlang::enquo(.var)
  size_quo <- rlang::enquo(.size)

  # --- validations (size column) ---
  sizes <- dplyr::pull(.df, !!size_quo)
  if (!is.numeric(sizes)) {
    stop("`.size` must refer to a numeric column.")
  }
  if (any(is.na(sizes))) {
    stop("`.size` contains NA values.")
  }
  if (any(sizes < 0)) {
    stop("`.size` must be non-negative.")
  }
  if (!is.numeric(.width) || length(.width) != 1 || .width <= 0) {
    stop("`.width` must be a positive numeric scalar.")
  }
  if (!is.numeric(.gap) || length(.gap) != 1 || .gap < 0) {
    stop("`.gap` must be a non-negative numeric scalar.")
  }
  if (!is.numeric(.x0) || length(.x0) != 1) {
    stop("`.x0` must be a numeric scalar.")
  }
  if (!is.numeric(.y0) || length(.y0) != 1) {
    stop("`.y0` must be a numeric scalar.")
  }

  # Use dplyr verbs with explicit namespace
  df <- dplyr::as_tibble(.df) %>%
    dplyr::mutate(.row_id = dplyr::row_number()) %>%
    dplyr::rename(var := {{ var_quo }}, height := {{ size_quo }})

  # --- factor handling (affects stacking order only) ---
  if (is.factor(df$var)) {
    var_levels <- levels(df$var) # preserve
  } else {
    var_levels <- rev(unique(df$var)) # reverse of input order
    df$var <- factor(df$var, levels = var_levels)
  }

  # Build stacking order key: by factor level, then by original row order
  df_stack <- df %>%
    dplyr::mutate(.lvl = as.integer(var)) %>%
    dplyr::arrange(.lvl, .row_id)

  # Compute coordinates in stacking order
  coords_stack <- df_stack %>%
    dplyr::mutate(
      .offset = cumsum(dplyr::lag(height + .gap, default = 0)),
      ymin = .y0 + .offset,
      ymax = ymin + height,
      xmin = .x0,
      xmax = .x0 + .width
    ) %>%
    dplyr::select(.row_id, var, height, xmin, xmax, ymin, ymax)

  # Restore original input row order and original column names
  coords <- coords_stack %>%
    dplyr::arrange(.row_id) %>%
    dplyr::select(-.row_id) %>%
    dplyr::rename({{ var_quo }} := var, {{ size_quo }} := height)

  return(coords)
}
