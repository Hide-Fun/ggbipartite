#' Geom for bipartite boxes (rows or columns)
#'
#' `GeomBipnetBox` draws the rectangular boxes for either the row set
#' (`type = "box1"`) or the column set (`type = "box2"`), using the rectangle
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
  # required_aes = c("x", "y", "xmin", "xmax", "ymin", "ymax"),
  required_aes = c("xmin", "xmax", "ymin", "ymax"),
  default_aes = aes(
    fill = "grey",
    colour = "black",
    linewidth = 0.5,
    linetype = "solid",
    alpha = 1
  )
)

#' Draw bipartite boxes
#'
#' Convenience layer to draw row (`type = "box1"`) or column
#' (`type = "box2"`) rectangles from `stat_bipnet()`.
#'
#' @inheritParams ggplot2::layer
#' @inheritParams stat_bipnet
#' @param stat The stat to use. Defaults to `"bipnet"` (i.e., `StatBipnet`).
#' @param ... Additional aesthetics passed to the geom/stat.
#'
#' @return A ggplot2 layer using `GeomBipnetBox`.
#'
#' @examples
#' interaction_df <- tibble::tibble(
#'   host = LETTERS[1:4],
#'   otu1 = c(1, 0, 3, 2),
#'   otu2 = c(0, 2, 1, 0)
#' ) |>
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
#'     type = "box1",
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
  ...,
  na.rm = FALSE,
  show.legend = NA,
  inherit.aes = TRUE
) {
  ggplot2::layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomBipnetBox,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
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
  # required_aes = c("x", "y", "xmin", "xmax", "ymin", "ymax"),
  required_aes = c("x", "y", "group"),
  default_aes = aes(
    fill = "grey",
    colour = "black",
    linewidth = 0.5,
    linetype = "solid",
    alpha = 1
  ),
  setup_data = function(data, params) {
    transform(
      data,
      row = as.character(row),
      column = as.character(column)
    )
  }
)

#' Draw bipartite interaction polygons
#'
#' Convenience layer to draw interaction polygons between row and column boxes
#' from `stat_bipnet(type = "interaction")`.
#'
#' @inheritParams ggplot2::layer
#' @inheritParams stat_bipnet
#' @param stat The stat to use. Defaults to `"bipnet"` (i.e., `StatBipnet`).
#' @param ... Additional aesthetics passed to the geom/stat.
#'
#' @return A ggplot2 layer using `GeomBipnetInteraction`.
#'
#' @examples
#' interaction_df <- tibble::tibble(
#'   host = LETTERS[1:4],
#'   otu1 = c(1, 0, 3, 2),
#'   otu2 = c(0, 2, 1, 0)
#' ) |>
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
#'   geom_bipnet_box(type = "box1") +
#'   geom_bipnet_box(type = "box2") +
#'   geom_bipnet_interaction(type = "interaction", alpha = 0.7) +
#'   coord_fixed()
#'
#' @seealso [geom_bipnet_box()], [stat_bipnet()]
#' @export
geom_bipnet_interaction <- function(
  mapping = NULL,
  data = NULL,
  stat = "bipnet",
  position = "identity",
  ...,
  na.rm = FALSE,
  show.legend = NA,
  inherit.aes = TRUE
) {
  ggplot2::layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomBipnetInteraction,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}
