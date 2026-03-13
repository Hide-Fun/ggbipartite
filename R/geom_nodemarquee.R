#' Compute Shared Left X Position for Node Labels
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' Computes a left-aligned x-position for node labels from the range of `df$x`
#' minus a small epsilon and optional offsets.
#'
#' @param df A data frame containing numeric column `x`.
#' @param offset Numeric scalar subtracted from the aligned x-position.
#' @param nudge_x Numeric scalar added to the aligned x-position.
#'
#' @return A numeric scalar x-position for left-aligned node labels.
#' @export
.nodelab_align_x_left <- function(df, offset = 0, nudge_x = 0) {
  if (!is.data.frame(df) || !("x" %in% names(df))) {
    stop("`df` must be a data frame containing column `x`.")
  }
  if (!is.numeric(offset) || length(offset) != 1L || !is.finite(offset)) {
    stop("`offset` must be a finite numeric scalar.")
  }
  if (!is.numeric(nudge_x) || length(nudge_x) != 1L || !is.finite(nudge_x)) {
    stop("`nudge_x` must be a finite numeric scalar.")
  }

  x_range <- range(df$x, na.rm = TRUE)
  if (!all(is.finite(x_range))) {
    stop("`df$x` must contain at least one finite value.")
  }

  min(x_range) - diff(x_range) / 200 - offset + nudge_x
}

.validate_node_arg <- function(node) {
  if (!is.character(node) || length(node) != 1L || is.na(node)) {
    stop("`node` must be one of \"internal\", \"external\", or \"all\".")
  }

  match.arg(node, c("internal", "external", "all"))
}

.validate_nudge_y_arg <- function(nudge_y) {
  if (!is.numeric(nudge_y) || length(nudge_y) != 1L || !is.finite(nudge_y)) {
    stop("`nudge_y` must be a finite numeric scalar.")
  }
}

.nodelab_x_eps <- function(df) {
  x_range <- range(df$x, na.rm = TRUE)
  if (!all(is.finite(x_range))) {
    stop("`df$x` must contain at least one finite value.")
  }

  diff(x_range) / 200
}

.filter_nodelab_rows <- function(df, node) {
  if (!is.data.frame(df)) {
    stop("`df` must be a data frame.")
  }
  # Drop tbl_tree subclasses to avoid tidytree::filter() side effects.
  df <- as.data.frame(df)

  required_cols <- c("x", "y")
  missing_cols <- setdiff(required_cols, names(df))
  if (length(missing_cols) > 0) {
    stop(
      "`df` is missing required columns: ",
      paste(missing_cols, collapse = ", "),
      "."
    )
  }

  node <- .validate_node_arg(node = node)

  if (node == "all") {
    return(df)
  }

  if (!("isTip" %in% names(df))) {
    stop(
      "`df` must contain `isTip` when `node` is \"internal\" or \"external\"."
    )
  }

  if (node == "internal") {
    keep_idx <- !is.na(df$isTip) & !df$isTip
    return(df[keep_idx, , drop = FALSE])
  }

  keep_idx <- !is.na(df$isTip) & df$isTip
  df[keep_idx, , drop = FALSE]
}

#' Prepare Node Segment Data for Label Guides
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' Filters rows by `node` and adds `xend`/`yend` columns used by guide
#' segments for node labels.
#'
#' @param df A data frame containing at least `x` and `y`.
#' @param offset Numeric scalar subtracted from x-position.
#' @param nudge_x Numeric scalar added to x-position.
#' @param nudge_y Numeric scalar added to label y-position.
#' @param node Character scalar, one of `"internal"`, `"external"`, or `"all"`.
#' @param align Logical scalar. If `TRUE`, all selected labels share a common
#'   left x-position.
#'
#' @return A filtered data frame with added `xend` and `yend` columns.
#' @export
.nodelab_segment_data <- function(
  df,
  offset = 0,
  nudge_x = 0,
  nudge_y = 0,
  node = "internal",
  align = FALSE
) {
  if (!is.logical(align) || length(align) != 1L || is.na(align)) {
    stop("`align` must be a single TRUE/FALSE.")
  }
  .validate_nudge_y_arg(nudge_y = nudge_y)

  node_data <- .filter_nodelab_rows(df = df, node = node)
  x_eps <- .nodelab_x_eps(df = df)

  node_data |>
    dplyr::mutate(
      xend = if (isTRUE(align)) {
        .nodelab_align_x_left(df = df, offset = offset, nudge_x = nudge_x)
      } else {
        .data$x - x_eps - offset + nudge_x
      },
      yend = .data$y + nudge_y
    )
}

#' Prepare Marquee Label Data for Aligned Node Labels
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' Filters rows by `node`, adds aligned `x_lab`, and applies y-nudging.
#'
#' @param df A data frame containing at least `x` and `y`.
#' @param offset Numeric scalar subtracted from x-position.
#' @param nudge_x Numeric scalar added to x-position.
#' @param nudge_y Numeric scalar added to label y-position.
#' @param node Character scalar, one of `"internal"`, `"external"`, or `"all"`.
#'
#' @return A filtered data frame with added `x_lab` and `y_lab` columns.
#' @export
.nodelab_marquee_data <- function(
  df,
  offset = 0,
  nudge_x = 0,
  nudge_y = 0,
  node = "internal"
) {
  .validate_nudge_y_arg(nudge_y = nudge_y)

  node_data <- .filter_nodelab_rows(df = df, node = node)

  node_data |>
    dplyr::mutate(
      x_lab = .nodelab_align_x_left(
        df = df,
        offset = offset,
        nudge_x = nudge_x
      ),
      y_lab = .data$y + nudge_y
    )
}

#' Prepare Marquee Label Data Without Alignment for Node Labels
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' Filters rows by `node`, computes per-row `x_lab`, and applies y-nudging.
#'
#' @param df A data frame containing at least `x` and `y`.
#' @param offset Numeric scalar subtracted from x-position.
#' @param nudge_x Numeric scalar added to x-position.
#' @param nudge_y Numeric scalar added to label y-position.
#' @param node Character scalar, one of `"internal"`, `"external"`, or `"all"`.
#'
#' @return A filtered data frame with added `x_lab` and `y_lab` columns.
#' @export
.nodelab_marquee_data_unaligned <- function(
  df,
  offset = 0,
  nudge_x = 0,
  nudge_y = 0,
  node = "internal"
) {
  .validate_nudge_y_arg(nudge_y = nudge_y)

  node_data <- .filter_nodelab_rows(df = df, node = node)
  x_eps <- .nodelab_x_eps(df = df)

  node_data |>
    dplyr::mutate(
      x_lab = .data$x - x_eps - offset + nudge_x,
      y_lab = .data$y + nudge_y
    )
}

.resolve_layer_data <- function(data) {
  if (is.null(data)) {
    return(function(df) df)
  }
  if (is.function(data)) {
    return(data)
  }
  if (is.data.frame(data)) {
    return(function(df) data)
  }

  stop("`data` must be `NULL`, a data frame, or a function.")
}

#' Draw Node Labels with `marquee`
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' API-compatible wrapper around `ggtree::geom_nodelab()`-style node selection
#' and `marquee::geom_marquee()` rendering. It can optionally align selected
#' node labels to a shared left x-position and draw guide segments.
#'
#' @param mapping Set of aesthetic mappings.
#' @param data A data frame, `NULL`, or function evaluated by `ggplot2`.
#' @param nudge_x Numeric scalar x-offset, compatible with
#'   `ggtree::geom_nodelab()`.
#' @param nudge_y Numeric scalar y-offset, compatible with
#'   `ggtree::geom_nodelab()`.
#' @param geom Character scalar. Only `"text"` is supported for compatibility
#'   with `ggtree::geom_nodelab()`.
#' @param hjust Horizontal justification passed to `marquee::geom_marquee()`.
#' @param node Character scalar, one of `"internal"`, `"external"`, or `"all"`.
#' @param align Logical scalar. If `TRUE`, selected labels share one x-position.
#' @param offset Numeric scalar subtracted from label x-position.
#' @param segment Logical scalar. If `TRUE`, draw guide segments.
#' @param segment_linetype Linetype for guide segments.
#' @param segment_linewidth Line width for guide segments.
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
#'   tree$node.label <- c("90", "100")
#'
#'   ggtree::ggtree(tree) +
#'     geom_nodemarquee(
#'       mapping = ggplot2::aes(label = label),
#'       hjust = 1
#'     ) +
#'     ggplot2::coord_cartesian(clip = "off")
#' }
#' @export
geom_nodemarquee <- function(
  mapping = NULL,
  data = NULL,
  nudge_x = 0,
  nudge_y = 0,
  geom = "text",
  hjust = 0.5,
  node = "internal",
  align = FALSE,
  offset = 0,
  segment = align,
  segment_linetype = "dotted",
  segment_linewidth = 0.5,
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
      "`geom_nodemarquee()` requires package `marquee`. ",
      "Install it with `install.packages(\"marquee\")`."
    )
  }

  if (!is.character(geom) || length(geom) != 1L || is.na(geom)) {
    stop("`geom` must be a single string.")
  }
  if (!identical(geom, "text")) {
    stop("`geom_nodemarquee()` only supports `geom = \"text\"`.")
  }

  if (!is.logical(align) || length(align) != 1L || is.na(align)) {
    stop("`align` must be a single TRUE/FALSE.")
  }
  if (!is.numeric(offset) || length(offset) != 1L || !is.finite(offset)) {
    stop("`offset` must be a finite numeric scalar.")
  }
  if (!is.numeric(nudge_x) || length(nudge_x) != 1L || !is.finite(nudge_x)) {
    stop("`nudge_x` must be a finite numeric scalar.")
  }
  .validate_nudge_y_arg(nudge_y = nudge_y)
  node <- .validate_node_arg(node = node)

  if (!is.numeric(hjust) || length(hjust) != 1L || !is.finite(hjust)) {
    stop("`hjust` must be a finite numeric scalar.")
  }

  if (!is.logical(segment) || length(segment) != 1L || is.na(segment)) {
    stop("`segment` must be a single TRUE/FALSE.")
  }
  if (
    !is.numeric(segment_linewidth) ||
      length(segment_linewidth) != 1L ||
      !is.finite(segment_linewidth) ||
      segment_linewidth < 0
  ) {
    stop("`segment_linewidth` must be a non-negative finite numeric scalar.")
  }

  data_fn <- .resolve_layer_data(data = data)

  marquee_df <- if (isTRUE(align)) {
    function(df) {
      .nodelab_marquee_data(
        df = data_fn(df),
        offset = offset,
        nudge_x = nudge_x,
        nudge_y = nudge_y,
        node = node
      )
    }
  } else {
    function(df) {
      .nodelab_marquee_data_unaligned(
        df = data_fn(df),
        offset = offset,
        nudge_x = nudge_x,
        nudge_y = nudge_y,
        node = node
      )
    }
  }

  layers <- list()

  if (isTRUE(segment)) {
    seg_df <- function(df) {
      .nodelab_segment_data(
        df = data_fn(df),
        offset = offset,
        nudge_x = nudge_x,
        nudge_y = nudge_y,
        node = node,
        align = align
      )
    }

    layers <- c(
      layers,
      list(
        ggtree::geom_segment2(
          data = seg_df,
          mapping = ggplot2::aes(x = x, xend = xend, y = y, yend = yend),
          inherit.aes = FALSE,
          linetype = segment_linetype,
          linewidth = segment_linewidth
        )
      )
    )
  }

  if (is.null(mapping)) {
    mapping <- ggplot2::aes()
  }

  # utils::modifyList is exported; ggplot2::modifyList is not.
  marquee_mapping <- utils::modifyList(
    ggplot2::aes(x = x_lab, y = y_lab),
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
