#' Prepare Marquee Label Data Without Alignment
#'
#' Filters tip rows and computes `x_lab` by nudging each tip's own x-position.
#'
#' @param df A data frame containing `isTip`, `x`, and `y`.
#' @param offset Numeric scalar added to the label x-position.
#'
#' @return A tip-only data frame with added `x_lab` column.
#' @export
.tiplab_marquee_data_unaligned <- function(df, offset = 0) {
  required_cols <- c("isTip", "x", "y")
  missing_cols <- setdiff(required_cols, names(df))
  if (length(missing_cols) > 0) {
    stop(
      "`df` is missing required columns: ",
      paste(missing_cols, collapse = ", "),
      "."
    )
  }
  if (!is.numeric(offset) || length(offset) != 1 || !is.finite(offset)) {
    stop("`offset` must be a finite numeric scalar.")
  }

  x_range <- range(df$x, na.rm = TRUE)
  if (!all(is.finite(x_range))) {
    stop("`df$x` must contain at least one finite value.")
  }
  eps <- diff(x_range) / 200

  tip_data <- df %>%
    dplyr::filter(!is.na(.data$isTip) & .data$isTip) %>%
    dplyr::mutate(
      x_lab = .data$x + eps + offset
    )

  tip_data
}
