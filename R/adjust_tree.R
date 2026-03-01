#' Adjust tree
#'
#' Scales a phylogenetic tree drawn by ggtree so that the vertical (y) extent
#' fits exactly into a target box (.box$ymin ~ .box$ymax). Tip labels are NOT
#' drawn. The horizontal (x) scale is expanded by `.adjust` relative to the
#' base uniform scale derived from the y-fit (aspect preserved, then widened).
#' When `.adjust_tip_position = TRUE`, tree tip y positions are matched to the
#' box-centre y positions (`(ymin + ymax) / 2`) by label.
#'
#' @param .phylo phylogenetic tree (`phylo`).
#' @param .box data.frame containing numeric `ymin` and `ymax` columns.
#' @param .adjust numeric scalar. Horizontal expansion factor (x only).
#' @param .adjust_tip_position logical scalar. If `TRUE`, map tree tips to box
#'   centre y positions using matched labels (`row`/`column`/`label`).
#' @param .tree_position character, one of c("left", "right").
#' @param ... other args passed through to ggtree::ggtree().
#'
#' @return A ggplot object (ggtree) with nodes/edges only (no tip labels),
#'         vertically fit to `.box` (or tip-matched when requested), and
#'         horizontally scaled by `.adjust`.
#'
#' @importFrom ggtree ggtree ggexpand
#' @importFrom ggplot2 scale_y_continuous scale_x_reverse
#' @importFrom dplyr mutate filter arrange distinct summarise group_by
#' @importFrom rlang .data
#' @export
adjust_tree <- function(
  .phylo,
  .box,
  .adjust = 0.95,
  .adjust_tip_position = FALSE,
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
  if (
    !is.logical(.adjust_tip_position) ||
      length(.adjust_tip_position) != 1L ||
      is.na(.adjust_tip_position)
  ) {
    stop("`.adjust_tip_position` must be a single TRUE/FALSE.")
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

  y_scaled <- box_min + (tree_data$y - min_y) * scale_factor_y

  if (.adjust_tip_position) {
    candidate_cols <- intersect(c("row", "column", "label"), names(.box))
    if (length(candidate_cols) == 0) {
      stop(
        paste0(
          "When `.adjust_tip_position = TRUE`, `.box` must contain one of ",
          "`row`, `column`, or `label`."
        )
      )
    }

    tip_labels <- tree_data %>%
      dplyr::filter(!is.na(.data$isTip) & .data$isTip) %>%
      dplyr::filter(!is.na(.data$label)) %>%
      dplyr::distinct(.data$label) %>%
      dplyr::pull(.data$label) %>%
      as.character()

    match_counts <- vapply(
      candidate_cols,
      function(col) {
        sum(as.character(.box[[col]]) %in% tip_labels, na.rm = TRUE)
      },
      numeric(1)
    )
    label_col <- candidate_cols[[which.max(match_counts)]]

    if (max(match_counts) == 0) {
      stop(
        paste0(
          "Could not match `.box` labels to tree tips. ",
          "Check that tree tip labels and `.box` ids use the same values."
        )
      )
    }

    box_tip <- .box %>%
      dplyr::mutate(
        .label = as.character(.data[[label_col]]),
        y_target = (.data$ymin + .data$ymax) / 2
      ) %>%
      dplyr::filter(!is.na(.data$.label), is.finite(.data$y_target)) %>%
      dplyr::group_by(.data$.label) %>%
      dplyr::summarise(y_target = mean(.data$y_target), .groups = "drop")

    tree_tip <- tree_data %>%
      dplyr::filter(!is.na(.data$isTip) & .data$isTip) %>%
      dplyr::filter(!is.na(.data$label), is.finite(.data$y)) %>%
      dplyr::group_by(.data$label) %>%
      dplyr::summarise(y_tip = mean(.data$y), .groups = "drop")

    tip_map <- tree_tip %>%
      dplyr::mutate(label = as.character(.data$label)) %>%
      dplyr::left_join(
        box_tip,
        by = c("label" = ".label")
      )

    if (any(!is.finite(tip_map$y_target))) {
      missing_labels <- tip_map %>%
        dplyr::filter(!is.finite(.data$y_target)) %>%
        dplyr::arrange(.data$label) %>%
        dplyr::pull(.data$label)
      stop(
        paste0(
          "Missing `.box` matches for tip labels: ",
          paste(missing_labels, collapse = ", "),
          "."
        )
      )
    }

    if (nrow(tip_map) >= 2) {
      tip_map <- tip_map %>%
        dplyr::arrange(.data$y_tip, .data$label)

      y_map <- stats::approxfun(
        x = tip_map$y_tip,
        y = tip_map$y_target,
        method = "linear",
        ties = "ordered",
        rule = 2
      )
      y_scaled <- y_map(tree_data$y)
    } else if (nrow(tip_map) == 1) {
      y_scaled <- tree_data$y + (tip_map$y_target - tip_map$y_tip)
    }
  }

  # -- rescale coordinates -----------------------------------------------------
  # y is mapped to the box; x is widened by `.adjust` relative to the base.
  tree_plot$data <- dplyr::mutate(
    tree_data,
    x = (.data$x - min_x) * scale_factor_x_base * .adjust,
    y = y_scaled
  )

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
