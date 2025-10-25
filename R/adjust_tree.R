#' Adjust tree
#'
#' Scales a phylogenetic tree drawn by ggtree so that the vertical (y) extent
#' fits exactly into a target box (.box$ymin ~ .box$ymax). Tip labels are NOT
#' drawn. The horizontal (x) scale is expanded by `.adjust` relative to the
#' base uniform scale derived from the y-fit (aspect preserved, then widened).
#'
#' @param .phylo phylogenetic tree (`phylo`).
#' @param .box data.frame containing numeric `ymin` and `ymax` columns.
#' @param .adjust numeric scalar. Horizontal expansion factor (x only).
#' @param .tree_position character, one of c("left", "right").
#' @param ... other args passed through to ggtree::ggtree().
#'
#' @return A ggplot object (ggtree) with nodes/edges only (no tip labels),
#'         vertically fit to `.box`, and horizontally scaled by `.adjust`.
#'
#' @importFrom ggtree ggtree ggexpand
#' @importFrom ggplot2 scale_y_continuous scale_x_reverse
#' @importFrom dplyr mutate
#' @importFrom rlang .data
#' @export
adjust_tree <- function(
  .phylo,
  .box,
  .adjust = 0.95,
  .tree_position = c("left", "right"),
  ...
) {
  .tree_position <- match.arg(.tree_position)

  # -- validate inputs ---------------------------------------------------------
  # Validate .phylo
  if (!inherits(.phylo, "phylo")) {
    stop("`.phylo` must be a `phylo` object.")
  }
  # Validate .box
  if (!is.data.frame(.box)) {
    stop("`.box` must be a data frame containing `ymin` and `ymax` columns.")
  }
  required_cols <- c("ymin", "ymax")
  if (!all(required_cols %in% names(.box))) {
    stop("`.box` must contain `ymin` and `ymax` columns.")
  }
  # Validate .adjust
  if (!is.numeric(.adjust) || length(.adjust) != 1 || !is.finite(.adjust)) {
    stop("`.adjust` must be a finite numeric scalar.")
  }

  # -- initial draw (no tip labels) -------------------------------------------
  # Draw the tree without tip labels; only nodes/edges will be present.
  tree_plot <- ggtree::ggtree(.phylo, ...)

  # -- extract and validate tree data -----------------------------------------
  tree_data <- tree_plot$data
  if (!all(c("x", "y") %in% names(tree_data))) {
    stop("Tree data must contain `x` and `y` columns.")
  }

  # -- compute target box vertical span ---------------------------------------
  # Lock vertical extent to .box
  box_y_vals <- c(.box$ymin, .box$ymax)
  if (!all(is.finite(box_y_vals))) {
    stop("`.box$ymin` and `.box$ymax` must be finite numerics.")
  }
  box_range <- range(box_y_vals, na.rm = TRUE)
  box_span <- diff(box_range)

  # -- compute tree spans ------------------------------------------------------
  tree_y_vals <- tree_data$y
  if (!any(is.finite(tree_y_vals))) {
    stop("Tree data contains no finite `y` values.")
  }
  tree_range <- range(tree_y_vals, na.rm = TRUE)
  tree_span <- diff(tree_range)

  # Base uniform scale: fit y exactly into the box
  scale_factor_y <- if (
    is.finite(tree_span) && tree_span > .Machine$double.eps
  ) {
    box_span / tree_span
  } else {
    1
  }
  # Use the same base for x to preserve aspect, then widen by `.adjust`
  scale_factor_x_base <- scale_factor_y

  tree_x_vals <- tree_data$x
  if (!any(is.finite(tree_x_vals))) {
    stop("Tree data contains no finite `x` values.")
  }
  min_x <- min(tree_x_vals, na.rm = TRUE)
  min_y <- min(tree_y_vals, na.rm = TRUE)
  box_min <- box_range[1]

  # -- rescale coordinates -----------------------------------------------------
  # y is locked to the box; x is widened by `.adjust` relative to the base.
  scaled_data <- dplyr::mutate(
    tree_data,
    x = (.data$x - min_x) * scale_factor_x_base * .adjust,
    y = box_min + (.data$y - min_y) * scale_factor_y
  )

  # inputs assumed to exist: tree_data, min_x, min_y, scale_factor_x, scale_factor_y, box_min, .adjust
  # If max_y is not defined, compute it once outside mutate to avoid per-row recomputation:
  max_y <- max(tree_data$y, na.rm = TRUE)

  # Precompute box height and vertical center for symmetric scaling
  box_height <- (max_y - min_y) * scale_factor_y
  y_mid <- box_min + box_height / 2

  y0_vals <- box_min + (tree_data$y - min_y) * scale_factor_y
  adjusted_y <- y_mid + (y0_vals - y_mid) * .adjust

  scaled_data2 <- dplyr::mutate(
    tree_data,
    # Scale x as before (no change)
    x = (.data$x - min_x) * scale_factor_x_base,
    # Apply symmetric vertical adjustment without retaining helper column
    y = adjusted_y
  )
  tree_plot$data <- scaled_data2

  # -- fix y-limits to the target box; add gentle expansion -------------------
  tree_plot <- tree_plot +
    ggplot2::scale_y_continuous(limits = box_range) +
    ggtree::ggexpand(ratio = 0.05, direction = 1, side = "hv")

  # -- flip x if tree is on the right -----------------------------------------
  if (.tree_position == "right") {
    tree_plot <- tree_plot + ggplot2::scale_x_reverse()
  }

  tree_plot
}
