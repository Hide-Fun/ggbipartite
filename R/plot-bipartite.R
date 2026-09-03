#' Compose a bipartite network and optional phylogenetic trees
#'
#' `r lifecycle::badge("experimental")`
#'
#' `plot_bipartite()` builds editable ggplot components from either raw
#' interaction data or a reusable [layout_bipartite()] result. Raw data are
#' normalized and laid out exactly once. When a layout is supplied, its
#' validated trees and fixed coordinates are reused without recomputation.
#'
#' The returned object is deliberately not a patchwork object. Its named
#' components can be inspected or edited before [as_patchwork()] assembles
#' them. The suggested package `patchwork` is required only when two or more
#' non-`NULL` panels must be assembled.
#'
#' With trees, every panel uses the same y scale and fills the same vertical
#' space. Panel widths change horizontal spacing without moving matched tips
#' and nodes vertically. Tree-free plots retain a fixed coordinate aspect
#' ratio. The composer does not change the stored layout or tree coordinates.
#'
#' @param data A matrix or long data frame accepted by [layout_bipartite()],
#'   or an existing `bipartite_layout` object.
#' @param row,column,weight Columns used for long-format raw data. Supply bare
#'   names or single strings. They must not be supplied when `data` is already
#'   a `bipartite_layout`.
#' @param widths Optional panel widths. Supply either one positive value per
#'   active component or a named positive vector using any of `row_tree`,
#'   `row_link`, `network`, `column_link`, and `column_tree`.
#' @param ... Additional arguments passed to [layout_bipartite()] for raw
#'   input. Additional arguments are not accepted for a precomputed layout.
#'
#' @return A `ggbipartite_plot` object with named `components`, the reusable
#'   `layout`, and resolved panel `widths`. Missing tree and link components
#'   are represented by `NULL`.
#'
#' @examples
#' interaction_matrix <- matrix(
#'   c(2, 0, 1, 3),
#'   nrow = 2,
#'   dimnames = list(c("host_a", "host_b"), c("otu_a", "otu_b"))
#' )
#' composed <- plot_bipartite(interaction_matrix)
#' composed$components$network
#'
#' precomputed <- layout_bipartite(interaction_matrix)
#' plot_bipartite(precomputed)
#'
#' @export
plot_bipartite <- function(
  data,
  row = NULL,
  column = NULL,
  weight = NULL,
  widths = NULL,
  ...
) {
  row_quo <- rlang::enquo(row)
  column_quo <- rlang::enquo(column)
  weight_quo <- rlang::enquo(weight)

  if (inherits(data, "bipartite_layout")) {
    validate_composer_layout_arguments(
      row = row_quo,
      column = column_quo,
      weight = weight_quo,
      dots_count = ...length()
    )
    layout <- data
    validate_bipartite_layout(layout)
  } else {
    layout <- rlang::inject(
      layout_bipartite(
        data = data,
        row = !!row_quo,
        column = !!column_quo,
        weight = !!weight_quo,
        ...
      )
    )
  }

  components <- build_bipartite_components(layout)
  resolved_widths <- resolve_bipartite_widths(widths, components)

  structure(
    list(
      components = components,
      layout = layout,
      widths = resolved_widths
    ),
    class = "ggbipartite_plot"
  )
}

#' Assemble a composed bipartite plot
#'
#' `as_patchwork()` assembles the current named components of a
#' `ggbipartite_plot`. This means users can edit individual components and then
#' reassemble them without recomputing the layout. A plot with only its network
#' component is returned directly as a ggplot and does not require `patchwork`.
#'
#' @param x A `ggbipartite_plot` returned by [plot_bipartite()].
#' @param widths Optional panel-width override using the same format as
#'   `plot_bipartite(widths = )`.
#'
#' @return A ggplot object for a single component, otherwise a patchwork
#'   composition.
#'
#' @export
as_patchwork <- function(x, widths = NULL) {
  validate_ggbipartite_plot(x)
  active <- active_bipartite_components(x$components)
  resolved_widths <- if (is.null(widths)) {
    resolve_bipartite_widths(x$widths, x$components)
  } else {
    resolve_bipartite_widths(widths, x$components)
  }

  if (length(active) == 1L) {
    return(active[[1L]])
  }
  require_patchwork()

  patchwork::wrap_plots(
    active,
    nrow = 1,
    widths = unname(resolved_widths[names(active)])
  )
}

#' @export
print.ggbipartite_plot <- function(x, ...) {
  print(as_patchwork(x), ...)
  invisible(x)
}

build_bipartite_components <- function(layout) {
  mode <- layout$params$interaction
  network <- ggplot2::ggplot()

  if (mode == "binary") {
    network <- network +
      geom_bipnet_interaction(
        layout = layout,
        interaction_type = "binary"
      ) +
      geom_bipnet_point(layout = layout, type = "row") +
      geom_bipnet_point(layout = layout, type = "column")
  } else {
    network <- network +
      geom_bipnet_interaction(
        layout = layout,
        interaction_type = "abundance"
      ) +
      geom_bipnet_box(layout = layout, type = "row") +
      geom_bipnet_box(layout = layout, type = "column")
  }

  network <- network +
    ggplot2::coord_fixed(clip = "off") +
    ggplot2::theme_void()

  row_tree <- extract_bipartite_tree_component(layout, "row")
  column_tree <- extract_bipartite_tree_component(layout, "column")
  row_link <- build_bipartite_link_component(layout, "row")
  column_link <- build_bipartite_link_component(layout, "column")

  components <- list(
    row_tree = row_tree,
    row_link = row_link,
    network = network,
    column_link = column_link,
    column_tree = column_tree
  )

  if (is.null(row_tree) && is.null(column_tree)) {
    return(components)
  }

  if (mode == "abundance") {
    # Connect to box edges instead of leaving a gap inside the network panel.
    components$network <- components$network +
      ggplot2::scale_x_continuous(expand = ggplot2::expansion(mult = 0))
  }

  # Matching data coordinates also need identical panel heights and expansion.
  y_limits <- range(c(layout$nodes$ymin, layout$nodes$ymax))
  lapply(components, function(component) {
    if (is.null(component)) {
      return(NULL)
    }
    suppressMessages(
      component +
        ggplot2::scale_y_continuous(
          limits = y_limits,
          expand = ggplot2::expansion(mult = 0.05)
        ) +
        ggplot2::coord_cartesian(clip = "off") +
        ggplot2::theme(plot.margin = ggplot2::margin(t = 5.5, b = 5.5))
    )
  })
}

extract_bipartite_tree_component <- function(layout, side) {
  tree <- layout$trees[[side]]
  if (is.null(tree)) {
    return(NULL)
  }
  if (!is.list(tree) || !"geometry" %in% names(tree)) {
    stop(
      "`layout$trees$",
      side,
      "` must contain a `geometry` component.",
      call. = FALSE
    )
  }
  if (!.is_ggplot_obj(tree$geometry)) {
    stop(
      "`layout$trees$",
      side,
      "$geometry` must be a ggplot object.",
      call. = FALSE
    )
  }
  tree$geometry + ggplot2::theme_void()
}

build_bipartite_link_component <- function(layout, side) {
  if (layout$params$interaction == "binary") {
    return(NULL)
  }

  links <- layout$tree_links
  side_links <- links[links$side == side, , drop = FALSE]
  if (nrow(side_links) == 0L) {
    return(NULL)
  }

  y_range <- range(c(layout$nodes$ymin, layout$nodes$ymax))
  ggplot2::ggplot(
    side_links,
    ggplot2::aes(
      x = .data$x,
      xend = .data$xend,
      y = .data$y,
      yend = .data$yend,
      group = .data$id
    )
  ) +
    ggplot2::geom_segment() +
    ggplot2::scale_x_continuous(
      limits = c(0, 1),
      expand = ggplot2::expansion(mult = 0)
    ) +
    ggplot2::scale_y_continuous(
      limits = y_range,
      expand = ggplot2::expansion(mult = 0)
    ) +
    ggplot2::theme_void()
}

validate_composer_layout_arguments <- function(
  row,
  column,
  weight,
  dots_count
) {
  selectors_are_null <- vapply(
    list(row, column, weight),
    rlang::quo_is_null,
    logical(1)
  )
  if (!all(selectors_are_null) || dots_count > 0L) {
    stop(
      "`row`, `column`, `weight`, and layout arguments in `...` must not ",
      "be supplied when `data` is already a `bipartite_layout`. Recreate ",
      "the layout to change its input or layout parameters.",
      call. = FALSE
    )
  }
  invisible(NULL)
}

resolve_bipartite_widths <- function(widths, components) {
  defaults <- c(
    row_tree = 1.5,
    row_link = 0.5,
    network = 4,
    column_link = 0.5,
    column_tree = 1.5
  )
  component_names <- names(defaults)
  active_names <- names(active_bipartite_components(components))

  if (is.null(widths)) {
    return(defaults)
  }
  if (
    !is.numeric(widths) ||
      length(widths) == 0L ||
      anyNA(widths) ||
      any(!is.finite(widths)) ||
      any(widths <= 0)
  ) {
    stop("`widths` must contain positive finite numbers.", call. = FALSE)
  }

  width_names <- names(widths)
  has_names <- !is.null(width_names) && all(nzchar(width_names))
  if (has_names) {
    unknown <- setdiff(width_names, component_names)
    duplicated_names <- unique(width_names[duplicated(width_names)])
    if (length(unknown) > 0L) {
      stop(
        "Unknown names in `widths`: ",
        paste(unknown, collapse = ", "),
        ".",
        call. = FALSE
      )
    }
    if (length(duplicated_names) > 0L) {
      stop("Names in `widths` must be unique.", call. = FALSE)
    }
    defaults[width_names] <- widths
    return(defaults)
  }

  if (!is.null(width_names) && any(nzchar(width_names))) {
    stop("`widths` must be fully named or fully unnamed.", call. = FALSE)
  }
  if (length(widths) != length(active_names)) {
    stop(
      "An unnamed `widths` vector must have one value per active ",
      "component (",
      length(active_names),
      ").",
      call. = FALSE
    )
  }
  defaults[active_names] <- widths
  defaults
}

active_bipartite_components <- function(components) {
  if (!is.list(components)) {
    stop("`x$components` must be a named list.", call. = FALSE)
  }
  expected <- c(
    "row_tree",
    "row_link",
    "network",
    "column_link",
    "column_tree"
  )
  if (!identical(names(components), expected)) {
    stop(
      "`x$components` must contain the ordered components: ",
      paste(expected, collapse = ", "),
      ".",
      call. = FALSE
    )
  }

  active <- components[!vapply(components, is.null, logical(1))]
  if (length(active) == 0L) {
    stop("`x$components` contains no drawable components.", call. = FALSE)
  }
  invalid <- !vapply(active, .is_ggplot_obj, logical(1))
  if (any(invalid)) {
    stop(
      "Every non-NULL component must be a ggplot object; invalid: ",
      paste(names(active)[invalid], collapse = ", "),
      ".",
      call. = FALSE
    )
  }
  active
}

validate_ggbipartite_plot <- function(x) {
  if (!inherits(x, "ggbipartite_plot")) {
    stop("`x` must be a `ggbipartite_plot` object.", call. = FALSE)
  }
  required <- c("components", "layout", "widths")
  missing <- setdiff(required, names(x))
  if (length(missing) > 0L) {
    stop(
      "`x` is missing components: ",
      paste(missing, collapse = ", "),
      ".",
      call. = FALSE
    )
  }
  validate_bipartite_layout(x$layout)
  active_bipartite_components(x$components)
  invisible(x)
}

is_patchwork_installed <- function() {
  requireNamespace("patchwork", quietly = TRUE)
}

require_patchwork <- function() {
  if (!is_patchwork_installed()) {
    stop(
      "Assembling multiple bipartite panels requires the suggested package ",
      "`patchwork`. Install it with `install.packages(\"patchwork\")`, or ",
      "inspect and draw individual panels from `x$components`.",
      call. = FALSE
    )
  }
  invisible(TRUE)
}
