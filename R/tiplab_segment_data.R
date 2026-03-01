#' Prepare Tip Segment Data for Aligned Labels
#'
#' Filters tip rows and adds `xend`/`yend` columns used by guide segments for
#' aligned labels.
#'
#' @param df A data frame containing at least `isTip`, `x`, and `y`.
#' @param offset Numeric scalar passed to `.tiplab_align_x()`.
#'
#' @return A tip-only data frame with added `xend` and `yend` columns.
#' @export
.tiplab_segment_data <- function(df, offset = 0) {
  required_cols <- c("isTip", "y")
  missing_cols <- setdiff(required_cols, names(df))
  if (length(missing_cols) > 0) {
    stop(
      "`df` is missing required columns: ",
      paste(missing_cols, collapse = ", "),
      "."
    )
  }

  tip_data <- df |>
    dplyr::filter(!is.na(.data$isTip) & .data$isTip) |>
    dplyr::mutate(
      xend = .tiplab_align_x(df = df, offset = offset),
      yend = .data$y
    )

  tip_data
}
