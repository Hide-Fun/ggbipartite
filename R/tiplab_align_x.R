#' Compute Shared X Position for Tip Labels
#'
#' Computes a right-aligned x-position for tip labels from the range of `df$x`
#' plus a small epsilon and optional offset.
#'
#' @param df A data frame containing numeric column `x`.
#' @param offset Numeric scalar added to the aligned x-position.
#'
#' @return A numeric scalar x-position for aligned labels.
#' @export
.tiplab_align_x <- function(df, offset = 0) {
  if (!is.data.frame(df) || !("x" %in% names(df))) {
    stop("`df` must be a data frame containing column `x`.")
  }
  if (!is.numeric(offset) || length(offset) != 1 || !is.finite(offset)) {
    stop("`offset` must be a finite numeric scalar.")
  }

  x_range <- range(df$x, na.rm = TRUE)
  if (!all(is.finite(x_range))) {
    stop("`df$x` must contain at least one finite value.")
  }

  max(x_range) + diff(x_range) / 200 + offset
}
