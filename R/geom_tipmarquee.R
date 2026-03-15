#' Draw Tip Labels with `marquee`
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' API-compatible wrapper around `ggtree::geom_tiplab()`-style arguments and
#' `marquee::geom_marquee()` rendering.
#'
#' @param mapping Set of aesthetic mappings.
#' @param data A data frame, `NULL`, or function evaluated by `ggplot2`.
#' @param hjust Horizontal justification passed to `marquee::geom_marquee()`.
#' @param align Logical scalar. If `TRUE`, tip labels are aligned to a shared
#'   right x-position and guide segments are drawn.
#' @param linetype Linetype for guide segments when `align = TRUE`.
#' @param linesize Line width for guide segments when `align = TRUE`.
#' @param geom Character scalar. Only `"text"` is supported for compatibility
#'   with `ggtree::geom_tiplab()`.
#' @param offset Numeric scalar added to the tip label x-position.
#' @param as_ylab Logical scalar from `ggtree::geom_tiplab()`. Only `FALSE` is
#'   supported.
#' @param stat Passed to [marquee::geom_marquee()].
#' @param position Passed to [marquee::geom_marquee()].
#' @param ... Additional arguments passed to [marquee::geom_marquee()].
#' @param size.unit Passed to [marquee::geom_marquee()].
#' @param na.rm Passed to [marquee::geom_marquee()].
#' @param show.legend Passed to [marquee::geom_marquee()].
#' @param inherit.aes Passed to [marquee::geom_marquee()].
#'
#' @return A list of ggplot2 layers.
#'
#' @examples
#' if (
#'   requireNamespace("ape", quietly = TRUE) &&
#'     requireNamespace("marquee", quietly = TRUE)
#' ) {
#'   tree <- ape::read.tree(text = "((A:1,B:1):1,C:2);")
#'
#'   ggtree::ggtree(tree) +
#'     geom_tipmarquee(align = TRUE, hjust = 0) +
#'     ggplot2::coord_cartesian(clip = "off")
#' }
#' @export
geom_tipmarquee <- function(
  mapping = NULL,
  data = NULL,
  hjust = 0,
  align = FALSE,
  linetype = "dotted",
  linesize = 0.5,
  geom = "text",
  offset = 0,
  as_ylab = FALSE,
  stat = "identity",
  position = "identity",
  ...,
  size.unit = "mm",
  na.rm = FALSE,
  show.legend = NA,
  inherit.aes = TRUE
) {
  if (!requireNamespace("marquee", quietly = TRUE)) {
    stop(
      "`geom_tipmarquee()` requires package `marquee`. ",
      "Install it with `install.packages(\"marquee\")`."
    )
  }
  if (!is.character(geom) || length(geom) != 1L || is.na(geom)) {
    stop("`geom` must be a single string.")
  }
  if (!identical(geom, "text")) {
    stop("`geom_tipmarquee()` only supports `geom = \"text\"`.")
  }
  if (!is.logical(as_ylab) || length(as_ylab) != 1L || is.na(as_ylab)) {
    stop("`as_ylab` must be a single TRUE/FALSE.")
  }
  if (isTRUE(as_ylab)) {
    stop("`as_ylab = TRUE` is not supported in `geom_tipmarquee()`.")
  }
  if (!is.logical(align) || length(align) != 1L || is.na(align)) {
    stop("`align` must be a single TRUE/FALSE.")
  }
  if (!is.numeric(offset) || length(offset) != 1L || !is.finite(offset)) {
    stop("`offset` must be a finite numeric scalar.")
  }
  if (!is.numeric(linesize) || length(linesize) != 1L || !is.finite(linesize)) {
    stop("`linesize` must be a finite numeric scalar.")
  }

  data_fn <- .resolve_layer_data(data = data)

  marquee_df <- if (isTRUE(align)) {
    function(df) {
      .tiplab_marquee_data(
        df = as.data.frame(data_fn(df)),
        offset = offset
      )
    }
  } else {
    function(df) {
      .tiplab_marquee_data_unaligned(
        df = as.data.frame(data_fn(df)),
        offset = offset
      )
    }
  }

  layers <- list()

  if (isTRUE(align)) {
    seg_df <- function(df) {
      .tiplab_segment_data(
        df = as.data.frame(data_fn(df)),
        offset = offset
      )
    }

    layers <- c(
      layers,
      list(
        ggtree::geom_segment2(
          data = seg_df,
          mapping = ggplot2::aes(x = x, xend = xend, y = y, yend = yend),
          inherit.aes = FALSE,
          linetype = linetype,
          linewidth = linesize
        )
      )
    )
  }

  if (is.null(mapping)) {
    mapping <- ggplot2::aes()
  }

  # utils::modifyList is exported; ggplot2::modifyList is not.
  marquee_mapping <- utils::modifyList(
    ggplot2::aes(x = x_lab, y = y, label = label),
    mapping
  )

  layers <- c(
    layers,
    list(
      marquee::geom_marquee(
        mapping = marquee_mapping,
        data = marquee_df,
        stat = stat,
        position = position,
        hjust = hjust,
        ...,
        size.unit = size.unit,
        na.rm = na.rm,
        show.legend = show.legend,
        inherit.aes = inherit.aes
      )
    )
  )

  layers
}
