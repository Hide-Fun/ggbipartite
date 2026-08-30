#' Geom for bipartite boxes (rows or columns)
#'
#' `GeomBipnetBox` draws the rectangular boxes for either the row set
#' (`type = "row"`) or the column set (`type = "column"`), using the rectangle
#' coordinates computed by `StatBipnet`.
#'
#' @section Required aesthetics:
#' The underlying stat requires `row`, `column`, and `count`. This geom itself
#' consumes `xmin`, `xmax`, `ymin`, and `ymax` (provided by the stat).
#'
#' @section Aesthetics:
#' In addition to the required aesthetics, this geom understands:
#' `fill`, `colour`, `linewidth`, `linetype`, and `alpha`.
#'
#' @keywords internal
GeomBipnetBox <- ggproto(
  "GeomBipnetBox",
  GeomRect,
  required_aes = c("xmin", "xmax", "ymin", "ymax"),
  default_aes = aes(
    fill = "grey",
    colour = "black",
    linewidth = 0.5,
    linetype = "solid",
    alpha = 1
  )
)

merge_bipnet_layout_mapping <- function(default_mapping, mapping) {
  if (is.null(mapping)) {
    return(default_mapping)
  }
  if (!inherits(mapping, "uneval")) {
    rlang::abort("`mapping` must be created by `ggplot2::aes()`.")
  }

  default_mapping[names(mapping)] <- mapping
  default_mapping
}

validate_bipnet_layout_layer <- function(
  layout,
  data,
  stat,
  stat_is_missing,
  tip_positions_row,
  tip_positions_column
) {
  validate_bipartite_layout(layout)

  if (!is.null(data)) {
    rlang::abort("`data` must be `NULL` when `layout` is supplied.")
  }
  if (!stat_is_missing && !identical(stat, "identity")) {
    rlang::abort(
      "`stat` must be `\"identity\"` when `layout` is supplied."
    )
  }
  if (!is.null(tip_positions_row) || !is.null(tip_positions_column)) {
    rlang::abort(
      paste0(
        "Tip positions are already included in `layout`; ",
        "do not supply them again."
      )
    )
  }

  invisible(layout)
}

validate_bipnet_layout_node_type <- function(type) {
  if (
    !is.character(type) ||
      length(type) != 1L ||
      is.na(type) ||
      !type %in% c("row", "column")
  ) {
    rlang::abort(
      "`type` must be either `\"row\"` or `\"column\"` with `layout`."
    )
  }
  type
}

validate_bipnet_layout_interaction_type <- function(type) {
  if (is.null(type)) {
    return(invisible(type))
  }
  if (
    !is.character(type) ||
      length(type) != 1L ||
      is.na(type) ||
      type != "interaction"
  ) {
    rlang::abort("`type` must be `\"interaction\"` with `layout`.")
  }
  invisible(type)
}

#' Draw bipartite boxes
#'
#' Convenience layer to draw row (`type = "row"`) or column
#' (`type = "column"`) rectangles from `stat_bipnet()`.
#'
#' @inheritParams ggplot2::layer
#' @inheritParams stat_bipnet
#' @param stat The stat to use. Defaults to `"bipnet"` (i.e., `StatBipnet`).
#' @param layout An optional `bipartite_layout` created by
#'   [layout_bipartite()]. When supplied, the precomputed node coordinates are
#'   drawn with the identity stat, `data` must be `NULL`, and plot-level
#'   aesthetics are not inherited.
#' @param type Required node side: `"row"` or `"column"`. The raw-data path
#'   also accepts the compatibility aliases `"box1"` and `"box2"`; the layout
#'   path accepts only the canonical names.
#' @param ... Additional aesthetics passed to the geom/stat.
#'
#' @return A ggplot2 layer using `GeomBipnetBox`.
#'
#' @examples
#' interaction_df <- tibble::tibble(
#'   host = LETTERS[1:4],
#'   otu1 = c(1, 0, 3, 2),
#'   otu2 = c(0, 2, 1, 0)
#' ) %>%
#'   tidyr::pivot_longer(
#'     cols = !host,
#'     names_to = "otu",
#'     values_to = "num_seq"
#'   )
#'
#' ggplot() +
#'   geom_bipnet_box(
#'     data = interaction_df,
#'     mapping = aes(row = host, column = otu, count = num_seq),
#'     type = "row",
#'     alpha = 0.5
#'   ) +
#'   coord_fixed()
#'
#' @seealso [geom_bipnet_interaction()], [stat_bipnet()]
#' @export
geom_bipnet_box <- function(
  mapping = NULL,
  data = NULL,
  stat = "bipnet",
  position = "identity",
  tip_positions_row = NULL,
  tip_positions_column = NULL,
  ...,
  na.rm = FALSE,
  show.legend = NA,
  inherit.aes = TRUE,
  layout = NULL,
  type = NULL
) {
  stat_is_missing <- missing(stat)

  if (!is.null(layout)) {
    validate_bipnet_layout_layer(
      layout = layout,
      data = data,
      stat = stat,
      stat_is_missing = stat_is_missing,
      tip_positions_row = tip_positions_row,
      tip_positions_column = tip_positions_column
    )
    type <- validate_bipnet_layout_node_type(type)
    node_data <- dplyr::filter(layout$nodes, .data$side == type)
    layout_mapping <- merge_bipnet_layout_mapping(
      ggplot2::aes(
        xmin = xmin,
        xmax = xmax,
        ymin = ymin,
        ymax = ymax
      ),
      mapping
    )

    return(ggplot2::layer(
      data = node_data,
      mapping = layout_mapping,
      stat = "identity",
      geom = GeomBipnetBox,
      position = position,
      show.legend = show.legend,
      inherit.aes = FALSE,
      params = list(
        na.rm = na.rm,
        ...
      )
    ))
  }

  ggplot2::layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomBipnetBox,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      type = type,
      tip_positions_row = tip_positions_row,
      tip_positions_column = tip_positions_column,
      na.rm = na.rm,
      ...
    )
  )
}

#' Geom for bipartite points (rows or columns)
#'
#' `GeomBipnetPoint` draws one point per row-side (`type = "row"`) or
#' column-side (`type = "column"`) element. The point position is the centre of
#' the rectangle coordinates computed by `StatBipnet`.
#'
#' @section Required aesthetics:
#' The underlying stat requires `row`, `column`, and `count`. This geom itself
#' consumes `xmin`, `xmax`, `ymin`, and `ymax` (provided by the stat) and
#' converts them to point centres.
#'
#' @section Aesthetics:
#' In addition to the required aesthetics, this geom understands:
#' `shape`, `size`, `fill`, `colour`, `stroke`, and `alpha`.
#'
#' @keywords internal
GeomBipnetPoint <- ggproto(
  "GeomBipnetPoint",
  GeomPoint,
  required_aes = c("xmin", "xmax", "ymin", "ymax"),
  default_aes = aes(
    shape = 21,
    size = 2.5,
    fill = "grey",
    colour = "black",
    stroke = 0.4,
    alpha = 1
  ),
  setup_data = function(data, params) {
    data$x <- (data$xmin + data$xmax) / 2
    data$y <- (data$ymin + data$ymax) / 2
    data
  }
)

#' Draw bipartite points
#'
#' Convenience layer to draw row (`type = "row"`) or column
#' (`type = "column"`) points for binary-data style visualisation.
#'
#' @inheritParams ggplot2::layer
#' @inheritParams stat_bipnet
#' @param stat The stat to use. Defaults to `"bipnet"` (i.e., `StatBipnet`).
#' @param layout An optional `bipartite_layout` created by
#'   [layout_bipartite()]. When supplied, the precomputed node coordinates are
#'   drawn with the identity stat, `data` must be `NULL`, and plot-level
#'   aesthetics are not inherited.
#' @param type Required node side: `"row"` or `"column"`. The raw-data path
#'   also accepts the compatibility aliases `"box1"` and `"box2"`; the layout
#'   path accepts only the canonical names.
#' @param ... Additional aesthetics passed to the geom/stat.
#'
#' @return A ggplot2 layer using `GeomBipnetPoint`.
#'
#' @examples
#' interaction_df <- tibble::tibble(
#'   host = LETTERS[1:4],
#'   otu1 = c(1, 0, 1, 1),
#'   otu2 = c(0, 1, 1, 0)
#' ) %>%
#'   tidyr::pivot_longer(
#'     cols = !host,
#'     names_to = "otu",
#'     values_to = "is_present"
#'   )
#'
#' ggplot(interaction_df, aes(row = host, column = otu, count = is_present)) +
#'   geom_bipnet_point(type = "row") +
#'   geom_bipnet_point(type = "column") +
#'   coord_fixed()
#'
#' @seealso [geom_bipnet_box()], [geom_bipnet_interaction()], [stat_bipnet()]
#' @export
geom_bipnet_point <- function(
  mapping = NULL,
  data = NULL,
  stat = "bipnet",
  position = "identity",
  tip_positions_row = NULL,
  tip_positions_column = NULL,
  ...,
  na.rm = FALSE,
  show.legend = NA,
  inherit.aes = TRUE,
  layout = NULL,
  type = NULL
) {
  stat_is_missing <- missing(stat)

  if (!is.null(layout)) {
    validate_bipnet_layout_layer(
      layout = layout,
      data = data,
      stat = stat,
      stat_is_missing = stat_is_missing,
      tip_positions_row = tip_positions_row,
      tip_positions_column = tip_positions_column
    )
    type <- validate_bipnet_layout_node_type(type)
    node_data <- dplyr::filter(layout$nodes, .data$side == type)
    layout_mapping <- merge_bipnet_layout_mapping(
      ggplot2::aes(
        xmin = xmin,
        xmax = xmax,
        ymin = ymin,
        ymax = ymax
      ),
      mapping
    )

    return(ggplot2::layer(
      data = node_data,
      mapping = layout_mapping,
      stat = "identity",
      geom = GeomBipnetPoint,
      position = position,
      show.legend = show.legend,
      inherit.aes = FALSE,
      params = list(
        na.rm = na.rm,
        ...
      )
    ))
  }

  ggplot2::layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomBipnetPoint,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      type = type,
      tip_positions_row = tip_positions_row,
      tip_positions_column = tip_positions_column,
      na.rm = na.rm,
      ...
    )
  )
}

#' Geom for bipartite interaction polygons
#'
#' `GeomBipnetInteraction` draws the polygons that connect the row and column
#' boxes, using the `interaction_coords` computed by `StatBipnet`.
#'
#' @section Required aesthetics:
#' The stat requires `row`, `column`, and `count`. This geom itself consumes
#' `x`, `y`, and `group` (provided by the stat).
#'
#' @section Aesthetics:
#' In addition to the required aesthetics, this geom understands:
#' `fill`, `colour`, `linewidth`, `linetype`, and `alpha`.
#'
#' @keywords internal
GeomBipnetInteraction <- ggproto(
  "GeomBipnetInteraction",
  GeomPolygon,
  required_aes = c("x", "y", "group"),
  default_aes = aes(
    fill = "grey",
    colour = "black",
    linewidth = 0.5,
    linetype = "solid",
    alpha = 1
  ),
  setup_data = function(data, params) {
    for (id_column in intersect(c("row", "column"), names(data))) {
      data[[id_column]] <- as.character(data[[id_column]])
    }
    data
  }
)

#' Geom for bipartite interaction segments (binary mode)
#'
#' `GeomBipnetInteractionBinary` draws one segment per row-column interaction.
#' It is used when `interaction_type = "binary"` in
#' [geom_bipnet_interaction()].
#'
#' @keywords internal
GeomBipnetInteractionBinary <- ggproto(
  "GeomBipnetInteractionBinary",
  GeomSegment,
  required_aes = c("x", "y", "xend", "yend"),
  default_aes = aes(
    colour = NA,
    linewidth = 0.5,
    linetype = "solid",
    alpha = 1
  ),
  optional_aes = c("fill"),
  setup_data = function(data, params) {
    if (!"colour" %in% names(data) || length(data$colour) != nrow(data)) {
      data$colour <- rep(NA_character_, nrow(data))
    }

    if ("fill" %in% names(data) && length(data$fill) == nrow(data)) {
      needs_colour <- is.na(data$colour)
      data$colour[needs_colour] <- data$fill[needs_colour]
    }
    data$colour[is.na(data$colour)] <- "black"
    data
  }
)

#' Draw bipartite interactions
#'
#' Convenience layer to draw interaction polygons (`interaction_type =
#' "abundance"`) or interaction segments (`interaction_type = "binary"`)
#' between row and column sides from `stat_bipnet(type = "interaction")`.
#'
#' @inheritParams ggplot2::layer
#' @inheritParams stat_bipnet
#' @param stat The stat to use. Defaults to `"bipnet"` (i.e., `StatBipnet`).
#' @param layout An optional `bipartite_layout` created by
#'   [layout_bipartite()]. When supplied, precomputed interaction coordinates
#'   are drawn with the identity stat, `data` must be `NULL`, and plot-level
#'   aesthetics are not inherited. If `interaction_type` is omitted, the mode
#'   is taken from `layout`.
#' @param type Use `NULL` (the default) or `"interaction"`. With `layout`, no
#'   row, column, or compatibility alias is accepted.
#' @param interaction_type One of `"abundance"` or `"binary"`.
#' @param ... Additional aesthetics passed to the geom/stat.
#'
#' @return A ggplot2 layer using `GeomBipnetInteraction` or
#'   `GeomBipnetInteractionBinary`.
#'
#' @examples
#' interaction_df <- tibble::tibble(
#'   host = LETTERS[1:4],
#'   otu1 = c(1, 0, 3, 2),
#'   otu2 = c(0, 2, 1, 0)
#' ) %>%
#'   tidyr::pivot_longer(
#'     cols = !host,
#'     names_to = "otu",
#'     values_to = "num_seq"
#'   )
#'
#' ggplot(
#'   data = interaction_df,
#'   mapping = aes(row = host, column = otu, count = num_seq)
#' ) +
#'   geom_bipnet_box(type = "row") +
#'   geom_bipnet_box(type = "column") +
#'   geom_bipnet_interaction(type = "interaction", alpha = 0.7) +
#'   coord_fixed()
#'
#' ggplot(
#'   data = interaction_df,
#'   mapping = aes(row = host, column = otu, count = as.integer(num_seq > 0))
#' ) +
#'   geom_bipnet_point(type = "row") +
#'   geom_bipnet_point(type = "column") +
#'   geom_bipnet_interaction(
#'     type = "interaction",
#'     interaction_type = "binary",
#'     linewidth = 0.3
#'   ) +
#'   coord_fixed()
#'
#' @seealso [geom_bipnet_box()], [geom_bipnet_point()], [stat_bipnet()]
#' @export
geom_bipnet_interaction <- function(
  mapping = NULL,
  data = NULL,
  stat = "bipnet",
  position = "identity",
  interaction_type = c("abundance", "binary"),
  tip_positions_row = NULL,
  tip_positions_column = NULL,
  ...,
  na.rm = FALSE,
  show.legend = NA,
  inherit.aes = TRUE,
  layout = NULL,
  type = NULL
) {
  stat_is_missing <- missing(stat)
  interaction_type_is_missing <- missing(interaction_type)

  if (!is.null(layout)) {
    validate_bipnet_layout_layer(
      layout = layout,
      data = data,
      stat = stat,
      stat_is_missing = stat_is_missing,
      tip_positions_row = tip_positions_row,
      tip_positions_column = tip_positions_column
    )
    validate_bipnet_layout_interaction_type(type)

    layout_mode <- layout$params$interaction
    interaction_type <- if (interaction_type_is_missing) {
      layout_mode
    } else {
      match.arg(interaction_type)
    }
    if (!identical(interaction_type, layout_mode)) {
      rlang::abort(
        paste0(
          "`interaction_type` must match `layout$params$interaction` ",
          "(`\"", layout_mode, "\"`)."
        )
      )
    }

    if (interaction_type == "binary") {
      geom <- GeomBipnetInteractionBinary
      default_mapping <- ggplot2::aes(
        x = x,
        y = y,
        xend = xend,
        yend = yend
      )
    } else {
      geom <- GeomBipnetInteraction
      default_mapping <- ggplot2::aes(
        x = x,
        y = y,
        group = edge_id
      )
    }
    layout_mapping <- merge_bipnet_layout_mapping(
      default_mapping,
      mapping
    )

    return(ggplot2::layer(
      data = layout$interactions,
      mapping = layout_mapping,
      stat = "identity",
      geom = geom,
      position = position,
      show.legend = show.legend,
      inherit.aes = FALSE,
      params = list(
        na.rm = na.rm,
        ...
      )
    ))
  }

  interaction_type <- match.arg(interaction_type)
  geom <- if (interaction_type == "binary") {
    GeomBipnetInteractionBinary
  } else {
    GeomBipnetInteraction
  }

  ggplot2::layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = geom,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      type = type,
      interaction_type = interaction_type,
      tip_positions_row = tip_positions_row,
      tip_positions_column = tip_positions_column,
      na.rm = na.rm,
      ...
    )
  )
}
