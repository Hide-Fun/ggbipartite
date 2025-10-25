#' Calculate global layout parameters for paired boxes
#'
#' Compute overall layout parameters (widths, origins, and vertical gaps) for a
#' two-box layout derived from a matrix input. The total "size" used for layout
#' is the sum of all entries in `.mat`. When `.adjust_box_height` is `TRUE`,
#' vertical gaps for each box are adjusted so that the total heights of the two
#' boxes match (based on the matrix's row/column counts).
#'
#' @param .mat A numeric matrix. Its sum defines the base height used in layout.
#' @param .x0,.y0 Numeric scalars giving the origin (bottom-left) of the first
#'   box.
#' @param .gap Non-negative numeric scalar; the baseline vertical gap between
#'   items (rows for box 1, columns for box 2) before any adjustment.
#' @param .box_ratio Positive numeric scalar; the divisor used to derive the
#'   box width as `sum(.mat) / .box_ratio`.
#' @param .ratio Positive numeric scalar; the aspect-like divisor used to derive
#'   the overall width as `sum(.mat) / .ratio`.
#' @param .adjust_box_height Logical; if `TRUE`, per-box gaps are adjusted via
#'   [adjust_box_height()] so that the two boxes have equal total height.
#'
#' @return A named list with elements:
#' \itemize{
#'   \item `width`: overall width computed as `sum(.mat) / .ratio`.
#'   \item `box_width`: width of an individual box, `sum(.mat) / .box_ratio`.
#'   \item `box1`: numeric length-2 vector `(x0, y0)` for the first box origin.
#'   \item `box2`: numeric length-2 vector `(x, y)` for the second box origin,
#'         where `x = width - box_width` and `y = y0`.
#'   \item `gap1`: vertical gap used for box 1 (rows).
#'   \item `gap2`: vertical gap used for box 2 (columns).
#' }
#'
#' @details
#' Internally, the variable `.interaction` is used for `sum(.mat)` (total mass).
#' If `.adjust_box_height` is `TRUE`, gaps are computed by
#' [adjust_box_height()] to equalize the total height:
#' \deqn{\text{height}_\mathrm{box1} = \sum(\mathtt{.mat}) + (\mathtt{.nrow}-1)\times \mathtt{gap1}}
#' \deqn{\text{height}_\mathrm{box2} = \sum(\mathtt{.mat}) + (\mathtt{.ncol}-1)\times \mathtt{gap2}}
#'
#' @note
#' This function relies on [adjust_box_height()]. As currently written,
#' if the two computed heights are already equal, that helper emits a message
#' and does not return a list of gaps (i.e., returns `NULL`). Callers expecting
#' `gap1`/`gap2` should handle that case accordingly.
#'
#' @seealso [adjust_box_height()]
#'
#' @examples
#' m <- matrix(c(1, 2, 1,
#'               0, 1, 3), nrow = 3)
#' calc_global_params(
#'   .mat = m,
#'   .x0 = 0, .y0 = 0,
#'   .gap = 0.2,
#'   .box_ratio = 5,
#'   .ratio = 1 / 1.618,
#'   .adjust_box_height = FALSE
#' )
#'
#' # With adjustment (be aware of the equal-height note)
#' calc_global_params(
#'   .mat = m,
#'   .gap = 0.1,
#'   .adjust_box_height = TRUE
#' )
#'
#' @export
calc_global_params <- function(
  .mat,
  .x0 = 0,
  .y0 = 0,
  .gap = 0,
  .box_ratio = 5,
  .ratio = 1 / 1.618,
  .adjust_box_height = TRUE
) {
  # total mass used for layout
  .interaction <- sum(.mat)
  .nrow <- nrow(.mat)
  .ncol <- ncol(.mat)

  # overall width derived from ratio
  w <- .interaction / .ratio

  # width for each box
  w1 <- .interaction / .box_ratio

  # box origins
  box1 <- c(.x0, .y0)
  box2 <- c(w - w1, .y0)

  if (.adjust_box_height) {
    .gaps <- adjust_box_height(
      .interaction = .interaction,
      .nrow = .nrow,
      .ncol = .ncol,
      .gap = .gap
    )

    return(
      list(
        width = w,
        box_width = w1,
        box1 = box1,
        box2 = box2,
        gap1 = .gaps$.gap1,
        gap2 = .gaps$.gap2
      )
    )
  }

  list(
    width = w,
    box_width = w1,
    box1 = box1,
    box2 = box2,
    gap1 = .gap,
    gap2 = .gap
  )
}

#' Adjust per-box vertical gaps to equalize heights
#'
#' Given the total mass (`.interaction`), matrix dimensions, and a baseline gap,
#' compute per-box gaps (`.gap1`, `.gap2`) so that the total heights for the
#' two boxes are equal. If the heights are already equal, a message is emitted
#' and no value is returned.
#'
#' @param .interaction Numeric scalar; the total mass (typically `sum(.mat)`).
#' @param .nrow,.ncol Positive integers; number of rows/columns in `.mat`.
#' @param .gap Baseline gap used for whichever box is treated as fixed.
#'
#' @return A named list with elements:
#' \itemize{
#'   \item `.gap1`: gap for box 1 (rows).
#'   \item `.gap2`: gap for box 2 (columns).
#' }
#' If the two heights are already equal, the function only emits a message and
#' returns `NULL`.
#'
#' @examples
#' adjust_box_height(.interaction = 10, .nrow = 4, .ncol = 3, .gap = 0.2)
#'
#' @export
adjust_box_height <- function(.interaction, .nrow, .ncol, .gap) {
  box1_height <- .interaction + (.nrow - 1) * .gap
  box2_height <- .interaction + (.ncol - 1) * .gap

  if (box1_height == box2_height) {
    message("Both are already at the same height.")
  } else if (box1_height > box2_height) {
    height <- box1_height
    return(
      list(
        .gap1 = .gap,
        .gap2 = (height - .interaction) / (.ncol - 1)
      )
    )
  } else {
    height <- box2_height
    return(
      list(
        .gap1 = (height - .interaction) / (.nrow - 1),
        .gap2 = .gap
      )
    )
  }
}
