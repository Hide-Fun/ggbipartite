#' Extract Y-axis Range from a ggplot Object
#'
#' Returns y-axis limits from a built ggplot object with compatibility for
#' multiple ggplot2 internals.
#'
#' @param p A ggplot object.
#'
#' @return A numeric vector of length 2 containing y-axis limits.
#' @export
get_yrange <- function(p) {
  if (!.is_ggplot_obj(p)) {
    stop("`p` must be a ggplot object.")
  }

  built_plot <- ggplot2::ggplot_build(p)
  panel_params <- built_plot$layout$panel_params[[1]]

  # ggplot2 >= 3.4
  if (!is.null(panel_params$y.range)) {
    return(panel_params$y.range)
  }

  # ggplot2 compatibility fallback
  if (!is.null(panel_params$y$range)) {
    return(panel_params$y$range)
  }

  panel_scale <- built_plot$layout$panel_scales_y[[1]]
  if (!is.null(panel_scale$range$range)) {
    return(panel_scale$range$range)
  }

  if (!is.null(p$data$y)) {
    return(range(p$data$y, na.rm = TRUE))
  }

  stop("Could not determine the y-axis range from the plot.")
}
