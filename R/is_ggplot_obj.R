#' Detect ggplot Objects Across ggplot2 Versions
#'
#' Checks whether `ggplot2` exports `is_ggplot()` and falls back to
#' `is.ggplot()` for older versions.
#'
#' @param x An object to test.
#'
#' @return A logical scalar indicating whether `x` is a ggplot object.
#' @keywords internal
#' @noRd
.is_ggplot_obj <- function(x) {
  if ("is_ggplot" %in% getNamespaceExports("ggplot2")) {
    return(ggplot2::is_ggplot(x))
  }

  ggplot2::is.ggplot(x)
}
