#' marquee_tiplab
#' @description
#' Use an already-built ggtree object as the base instead of a phylo.
#' This function probes aligned tip label positions by temporarily adding
#' geom_tiplab(align = ...) and then draws marquee labels (and guide segments).
#'
#' @param p_tree A `ggtree`/`ggplot` object containing the tree to annotate.
#' @param align Value passed to [ggtree::geom_tiplab()]'s `align` argument while
#'   probing tip positions (typically `TRUE` or a numeric offset).
#' @param label_map Data frame with columns `old` (original labels) and `new`
#'   (replacement labels) used to remap tip labels.
#' @param size Text size for marquee labels, forwarded to
#'   [marquee::geom_marquee()].
#' @param width_mm Width of each marquee label in millimetres.
#' @param show_guides Logical; draw guide line segments from tips to marquee
#'   labels when `TRUE`.
#' @param guide_linetype Linetype used for the guide segments.
#' @param guide_linesize Line width used for the guide segments.
#' @param x_start_nudge Horizontal nudge (as a fraction of the tree width) that
#'   offsets the marquee labels from the aligned tip positions.
#' @param y_start_nudge Vertical nudge (as a fraction of the tree height) that
#'   offsets the marquee labels.
#' @param guide_end_gap_ratio Ratio of the diagonal span that determines how far
#'   each guide line stops before reaching the label start.
#' @param ... Additional arguments passed to [marquee::geom_marquee()].
#'
#' @return A `ggplot` object with marquee labels applied to `p_tree`.
#' @export
marquee_tiplab <- function(
  p_tree,
  align,
  label_map,
  size,
  width_mm = 28,
  show_guides = TRUE,
  guide_linetype = "dotted",
  guide_linesize = 0.5,
  x_start_nudge = 0.1,
  y_start_nudge = 0,
  guide_end_gap_ratio = 0.03,
  ...
) {
  # ---- Basic checks (keep strict but practical) ----
  # Use ggplot2::is.ggplot to accept ggtree objects
  if (!ggplot2::is.ggplot(p_tree)) {
    stop("`p_tree` must be a ggtree/ggplot object.")
  }
  stopifnot(is.data.frame(label_map))
  stopifnot(all(c("old", "new") %in% names(label_map)))
  stopifnot(is.numeric(x_start_nudge), length(x_start_nudge) == 1)
  stopifnot(is.numeric(y_start_nudge), length(y_start_nudge) == 1)
  stopifnot(is.numeric(guide_end_gap_ratio), length(guide_end_gap_ratio) == 1)

  # Ensure base data has necessary columns (typical for ggtree)
  needed_cols <- c("isTip", "label", "x", "y")
  if (!isTRUE(all(needed_cols %in% names(p_tree$data)))) {
    stop(
      "`p_tree$data` must contain columns: ",
      paste(needed_cols, collapse = ", "),
      ". Provide a ggtree built from a phylo-like tree."
    )
  }

  # ---- Base plot (use the provided ggtree object verbatim) ----
  p_base <- p_tree

  # ---- Probe: add geom_tiplab to compute aligned coords ----
  # This does not change p_base; only used to compute positions
  p_probe <- p_base + ggtree::geom_tiplab(align = align)
  b <- ggplot2::ggplot_build(p_probe)

  # ---- Extract tiplab layer (must have label/x/y) ----
  layer_idx <- length(b$data)
  tiplab_df <- b$data[[layer_idx]]
  req_cols <- c("label", "x", "y")
  if (!all(req_cols %in% names(tiplab_df))) {
    idx <- which(vapply(
      b$data,
      function(d) all(req_cols %in% names(d)),
      logical(1)
    ))
    if (length(idx) == 0) {
      stop("Could not locate geom_tiplab layer in built data.")
    }
    tiplab_df <- b$data[[idx[[1]]]]
  }

  # ---- Tip source: original tip x ----
  tips_src <- p_base$data %>%
    dplyr::filter(.data$isTip) %>%
    dplyr::transmute(
      label = .data$label,
      x_tip = .data$x,
      y = .data$y
    )

  # ---- Spans for ratio-based nudges (use both base and tiplab coords) ----
  x_range <- range(c(p_base$data$x, tiplab_df$x), na.rm = TRUE)
  y_range <- range(c(p_base$data$y, tiplab_df$y), na.rm = TRUE)
  x_span <- diff(x_range)
  if (!is.finite(x_span) || x_span == 0) {
    x_span <- 1
  }
  y_span <- diff(y_range)
  if (!is.finite(y_span) || y_span == 0) {
    y_span <- 1
  }
  diag_span <- sqrt(x_span^2 + y_span^2)
  if (!is.finite(diag_span) || diag_span == 0) {
    diag_span <- 1
  }

  x_nudge_abs <- x_start_nudge * x_span
  y_nudge_abs <- y_start_nudge * y_span
  gap_abs <- guide_end_gap_ratio * diag_span

  # ---- Join coords; apply label map; compute nudged starts and guide ends ----
  tips_joined <- tiplab_df %>%
    dplyr::transmute(
      label = .data$label,
      x_label = .data$x,
      y = .data$y
    ) %>%
    dplyr::distinct() %>%
    dplyr::left_join(tips_src, by = c("label", "y")) %>%
    dplyr::left_join(label_map, by = c("label" = "old")) %>%
    dplyr::mutate(
      label_out = dplyr::if_else(is.na(.data$new), .data$label, .data$new),
      x_label_start = .data$x_label + x_nudge_abs,
      y_label_start = .data$y + y_nudge_abs,
      # Vector from tip to (nudged) label start
      dx = .data$x_label_start - .data$x_tip,
      dy = .data$y_label_start - .data$y,
      seg_len = sqrt(.data$dx^2 + .data$dy^2),
      # Unit vector (safe when seg_len == 0)
      ux = dplyr::if_else(.data$seg_len > 0, .data$dx / .data$seg_len, 0),
      uy = dplyr::if_else(.data$seg_len > 0, .data$dy / .data$seg_len, 0),
      # End the guide slightly before the label start to avoid overlap
      pad = pmin(gap_abs, .data$seg_len * 0.6),
      x_guide_end = .data$x_label_start - .data$ux * .data$pad,
      y_guide_end = .data$y_label_start - .data$uy * .data$pad
    )

  # ---- Compose plot ----
  # Keep user's original theme/layers; only ensure clip is off for marquee
  p_out <- p_base + ggplot2::coord_cartesian(clip = "off")

  if (isTRUE(show_guides)) {
    p_out <- p_out +
      ggplot2::geom_segment(
        data = tips_joined,
        ggplot2::aes(
          x = .data$x_tip,
          y = .data$y,
          xend = .data$x_guide_end,
          yend = .data$y_guide_end
        ),
        inherit.aes = FALSE,
        linetype = guide_linetype,
        linewidth = guide_linesize
      )
  }

  p_out <- p_out +
    marquee::geom_marquee(
      data = tips_joined,
      ggplot2::aes(
        x = .data$x_label_start,
        y = .data$y_label_start,
        label = .data$label_out
      ),
      size = size,
      inherit.aes = FALSE,
      width = grid::unit(width_mm, "mm"),
      show.legend = FALSE,
      ...
    )

  p_out
}
