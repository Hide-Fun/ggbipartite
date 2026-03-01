#' Prepare Marquee Label Data for Aligned Labels
#'
#' Filters tip rows and adds an aligned `x_lab` column used by marquee labels.
#'
#' @param df A data frame containing at least `isTip`, `x`, and `y`.
#' @param offset Numeric scalar passed to `.tiplab_align_x()`.
#'
#' @return A tip-only data frame with added `x_lab` column.
#' @keywords internal
#' @noRd
.tiplab_marquee_data <- function(df, offset = 0) {
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
      x_lab = .tiplab_align_x(df = df, offset = offset)
    )

  tip_data
}
