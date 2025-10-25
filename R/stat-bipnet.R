#' Stat for bipartite “box–interaction” layout
#'
#' `StatBipnet` computes the coordinates required to draw two sets of
#' rectangles (one for rows, one for columns) and the interaction polygons
#' that connect them. It expects long-format input carrying `row`, `column`,
#' and `count`, internally pivots to a matrix, and then delegates the geometry
#' calculation to `construct_bn_coordination()`.
#'
#' @section Required aesthetics:
#' \describe{
#'   \item{row}{Row key (character or factor).}
#'   \item{column}{Column key (character or factor).}
#'   \item{count}{Non-negative numeric weight for the row–column interaction.}
#' }
#'
#' @section Output by `type`:
#' \describe{
#'   \item{`"box1"`}{Rectangles for the row set with columns
#'     `xmin`, `xmax`, `ymin`, `ymax`, plus `row`.}
#'   \item{`"box2"`}{Rectangles for the column set with columns
#'     `xmin`, `xmax`, `ymin`, `ymax`, plus `column`.}
#'   \item{`"interaction"`}{Polygons spanning the two boxes with columns
#'     `x`, `y`, `row`, `column`, and a `group` id per interaction.}
#' }
#'
#' @details
#' If `gap` is not provided, a robust default is chosen as
#' `sum(count, na.rm = TRUE) / 10`, falling back to `1` when the sum is not
#' finite or non-positive. When `adjust_box_height = TRUE`, the vertical
#' extents of boxes are adjusted to reflect totals within each partition.
#'
#' The function `construct_bn_coordination()` must be available in scope and is
#' expected to return a list with elements `box1`, `box2`, and
#' `interaction_coords` containing the columns described above.
#'
#' @section Parameters handled:
#' `StatBipnet` honours the arguments accepted by [stat_bipnet()], including
#' `type`, `metadata_row`, `metadata_column`, `gap`, `box_ratio`, `ratio`,
#' `adjust_box_height`, and `na.rm`. These are validated in
#' `StatBipnet$setup_params()` before being forwarded to
#' `construct_bn_coordination()`.
#'
#' @return A data frame of coordinates appropriate for the selected `type`.
#' @keywords internal
StatBipnet <- ggplot2::ggproto(
  "StatBipnet",
  Stat,
  required_aes = c("row", "column", "count"),
  # Validate params and derive data-dependent defaults
  setup_params = function(data, params) {
    # Derive default gap if missing (robust to sign/outliers)
    if (is.null(params$gap)) {
      total <- sum(data$count, na.rm = TRUE)
      params$gap <- if (is.finite(total) && total > 0) total / 10 else 1
      message("Picking gap of ", signif(params$gap, 3))
    }
    if (is.null(params$adjust_box_height)) {
      params$adjust_box_height <- TRUE # default
    }
    if (
      !is.logical(params$adjust_box_height) ||
        length(params$adjust_box_height) != 1L ||
        is.na(params$adjust_box_height)
    ) {
      rlang::abort("`adjust_box_height` must be a single TRUE/FALSE.")
    }
    params
  },
  compute_panel = function(
    data,
    scales,
    type = NULL,
    row_nm = NULL,
    column_nm = NULL,
    metadata_row = NULL,
    metadata_column = NULL,
    gap = NULL,
    box_ratio = 5,
    ratio = 1 / 1.618,
    adjust_box_height = TRUE,
    na.rm = FALSE
  ) {
    # Convert to matrix.
    mat <- data.frame(
      row = data$row,
      column = data$column,
      count = data$count
    ) %>%
      tidyr::pivot_wider(
        names_from = "column",
        names_sort = TRUE,
        values_from = "count",
        values_fill = 0
      ) |>
      dplyr::arrange(row) %>%
      tibble::column_to_rownames(
        var = "row"
      ) |>
      as.matrix()

    bn_coords <- construct_bn_coordination(
      .mat = mat,
      .row = row_nm,
      .column = column_nm,
      .metadata_row = metadata_row,
      .metadata_column = metadata_column,
      .x0 = 0,
      .y0 = 0,
      .gap = gap,
      .box_ratio = box_ratio,
      .ratio = ratio,
      .adjust_box_height = adjust_box_height
    )
    if (type == "box1") {
      out <- bn_coords$box1
      out$group <- out$row
    } else if (type == "box2") {
      out <- bn_coords$box2
      out$group <- out$column
    } else if (type == "interaction") {
      out <- bn_coords$interaction_coords
      out$group <- as.factor(paste0(out$row, out$column))
    }
    out
  }
)

#' Layer constructor for the bipartite stat
#'
#' `stat_bipnet()` adds the computed coordinates for a bipartite
#' “box–interaction” layout to a plot. Use together with
#' [geom_bipnet_box()] (for `"box1"`/`"box2"`) and
#' [geom_bipnet_interaction()] (for `"interaction"`).
#'
#' @inheritParams ggplot2::layer
#' @param type One of `"box1"`, `"box2"`, or `"interaction"`.
#' @param metadata_row Optional data frame with row metadata used by the stat.
#' @param metadata_column Optional data frame with column metadata used by the stat.
#' @param gap Spacing between the two partitions (see Details of `StatBipnet`).
#' @param box_ratio Width of boxes relative to the interaction band.
#' @param ratio Overall aspect ratio of the layout.
#' @param adjust_box_height If `TRUE`, scale box heights by totals.
#' @param geom Geom to use. Defaults to `"polygon"` for convenience.
#' @param na.rm Logical indicating whether to drop `NA`s.
#' @param show.legend Passed to [ggplot2::layer()].
#' @param inherit.aes Should default aesthetics be inherited.
#' @param ... Additional parameters passed to the stat/geom.
#'
#' @section Aesthetics:
#' The stat requires mappings `row`, `column`, and `count`. Other aesthetics are
#' handled by the chosen geom (e.g., `fill`, `colour`, `alpha`).
#'
#' @return A ggplot2 layer using `StatBipnet`.
#'
#' @examples
#' # Minimal example
#' library(ggplot2)
#' library(tidyr)
#' library(tibble)
#'
#' interaction_df <- tibble::tibble(
#'   host = LETTERS[1:4],
#'   otu1 = c(1, 0, 3, 2),
#'   otu2 = c(0, 2, 1, 0)
#' ) |>
#'   pivot_longer(!host, names_to = "otu", values_to = "num_seq")
#'
#' ggplot(interaction_df, aes(row = host, column = otu, count = num_seq)) +
#'   geom_bipnet_box(type = "box1") +
#'   geom_bipnet_box(type = "box2") +
#'   geom_bipnet_interaction(type = "interaction") +
#'   coord_fixed()
#'
#' @export
stat_bipnet <- function(
  mapping = NULL,
  data = NULL,
  type = "interaction",
  row_nm = NULL,
  column_nm = NULL,
  metadata_row = NULL,
  metadata_column = NULL,
  gap = NULL,
  box_ratio = 5,
  ratio = 1 / 1.618,
  adjust_box_height = TRUE,
  geom = "polygon",
  position = "identity",
  na.rm = FALSE,
  show.legend = NA,
  inherit.aes = FALSE,
  ...
) {
  ggplot2::layer(
    stat = StatBipnet,
    geom = geom,
    data = data,
    mapping = mapping,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      type = type,
      row_nm = row_nm,
      column_nm = column_nm,
      metadata_row = metadata_row,
      metadata_column = metadata_column,
      gap = gap,
      box_ratio = box_ratio,
      ratio = ratio,
      adjust_box_height = adjust_box_height,
      na.rm = na.rm,
      ...
    )
  )
}
