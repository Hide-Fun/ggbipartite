is_ggplot_object <- function(x) {
  if ("is_ggplot" %in% getNamespaceExports("ggplot2")) {
    return(ggplot2::is_ggplot(x))
  }

  ggplot2::is.ggplot(x)
}

resolve_internal_function <- function(fn_name) {
  if (
    exists(
      fn_name,
      mode = "function",
      inherits = TRUE
    )
  ) {
    return(
      get(
        fn_name,
        mode = "function",
        inherits = TRUE
      )
    )
  }

  if (!("ggbipartite" %in% loadedNamespaces())) {
    requireNamespace("ggbipartite", quietly = TRUE)
  }

  if ("ggbipartite" %in% loadedNamespaces()) {
    ns <- asNamespace("ggbipartite")
    if (
      exists(
        fn_name,
        envir = ns,
        mode = "function",
        inherits = FALSE
      )
    ) {
      return(
        get(
          fn_name,
          envir = ns,
          mode = "function",
          inherits = FALSE
        )
      )
    }
  }

  stop(
    paste0(
      "`",
      fn_name,
      "()` could not be found. ",
      "Load ggbipartite with `library(ggbipartite)` or source the required ",
      "R files (including `R/bipartite_network.R` and ",
      "`R/compute_interaction_coords.R`)."
    )
  )
}

extract_tip_positions <- function(x, arg_name) {
  if (is.null(x)) {
    return(NULL)
  }

  if (is.data.frame(x)) {
    required_cols <- c("label", "y")
    missing_cols <- setdiff(required_cols, names(x))
    if (length(missing_cols) > 0) {
      stop(
        paste0(
          "`",
          arg_name,
          "` data frame is missing columns: ",
          paste(missing_cols, collapse = ", "),
          "."
        )
      )
    }
    tip_df <- x
  } else if (inherits(x, "phylo")) {
    tip_df <- ggtree::ggtree(x)$data
  } else if (is_ggplot_object(x)) {
    tip_df <- x$data
  } else if (isS4(x) && "data" %in% methods::slotNames(x)) {
    tip_df <- x@data
  } else {
    stop(
      paste0(
        "`",
        arg_name,
        "` must be NULL, a data frame with `label`/`y`, `phylo`, or ggtree ",
        "object."
      )
    )
  }

  if (!is.data.frame(tip_df)) {
    stop("Extracted tip data must be a data frame.")
  }

  if (all(c("label", "y") %in% names(tip_df))) {
    out <- tip_df |>
      dplyr::mutate(
        label = as.character(.data$label),
        y = as.numeric(.data$y)
      ) |>
      dplyr::filter(!is.na(.data$label), is.finite(.data$y)) |>
      dplyr::group_by(.data$label) |>
      dplyr::summarise(y = mean(.data$y), .groups = "drop")
  } else {
    required_cols <- c("isTip", "label", "y")
    missing_cols <- setdiff(required_cols, names(tip_df))
    if (length(missing_cols) > 0) {
      stop(
        paste0(
          "Extracted tip data is missing columns: ",
          paste(missing_cols, collapse = ", "),
          "."
        )
      )
    }

    out <- tip_df |>
      dplyr::filter(!is.na(.data$isTip) & .data$isTip) |>
      dplyr::mutate(
        label = as.character(.data$label),
        y = as.numeric(.data$y)
      ) |>
      dplyr::filter(!is.na(.data$label), is.finite(.data$y)) |>
      dplyr::group_by(.data$label) |>
      dplyr::summarise(y = mean(.data$y), .groups = "drop")
  }

  if (nrow(out) == 0) {
    stop(paste0("`", arg_name, "` does not contain usable tip positions."))
  }

  out
}

prepare_interaction_cells <- function(data) {
  data |>
    dplyr::transmute(
      row = as.character(.data$row),
      column = as.character(.data$column),
      interaction = as.numeric(.data$count)
    ) |>
    dplyr::group_by(.data$row, .data$column) |>
    dplyr::summarise(
      interaction = sum(.data$interaction, na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::filter(is.finite(.data$interaction), .data$interaction > 0)
}

align_box_to_tip_positions <- function(box_df, id_col, tip_positions, side_name) {
  if (is.null(tip_positions)) {
    return(box_df)
  }

  if (!(id_col %in% names(box_df))) {
    stop(
      paste0(
        "`",
        id_col,
        "` is missing in ",
        side_name,
        " box coordinates."
      )
    )
  }

  current_centres <- box_df |>
    dplyr::transmute(
      id = as.character(.data[[id_col]]),
      y_center = (.data$ymin + .data$ymax) / 2
    ) |>
    dplyr::group_by(.data$id) |>
    dplyr::summarise(y_center = mean(.data$y_center), .groups = "drop")

  shift_tbl <- current_centres |>
    dplyr::left_join(
      tip_positions |>
        dplyr::transmute(id = as.character(.data$label), y_target = .data$y),
      by = "id"
    )

  if (any(!is.finite(shift_tbl$y_target))) {
    missing_ids <- shift_tbl |>
      dplyr::filter(!is.finite(.data$y_target)) |>
      dplyr::arrange(.data$id) |>
      dplyr::pull(.data$id)
    stop(
      paste0(
        "Missing tip y positions for ",
        side_name,
        " ids: ",
        paste(missing_ids, collapse = ", "),
        "."
      )
    )
  }

  shift_tbl <- shift_tbl |>
    dplyr::mutate(delta = .data$y_target - .data$y_center) |>
    dplyr::select(id, delta)

  box_df |>
    dplyr::mutate(id = as.character(.data[[id_col]])) |>
    dplyr::left_join(shift_tbl, by = "id") |>
    dplyr::mutate(
      ymin = .data$ymin + .data$delta,
      ymax = .data$ymax + .data$delta
    ) |>
    dplyr::select(-id, -delta)
}

adjust_bn_coords_to_tip_positions <- function(
  .bn_coords,
  .tip_positions_row,
  .tip_positions_column,
  .interaction_cells
) {
  if (is.null(.tip_positions_row) && is.null(.tip_positions_column)) {
    return(.bn_coords)
  }

  .bn_coords$box1 <- align_box_to_tip_positions(
    box_df = .bn_coords$box1,
    id_col = "row",
    tip_positions = .tip_positions_row,
    side_name = "row"
  )
  .bn_coords$box2 <- align_box_to_tip_positions(
    box_df = .bn_coords$box2,
    id_col = "column",
    tip_positions = .tip_positions_column,
    side_name = "column"
  )

  if (nrow(.interaction_cells) == 0) {
    .bn_coords$interaction_coords <- .bn_coords$interaction_coords[0, ]
    return(.bn_coords)
  }

  compute_interaction_coords_fn <- resolve_internal_function(
    "compute_interaction_coords"
  )

  interaction_coords <- compute_interaction_coords_fn(
    .box1 = .bn_coords$box1,
    .box2 = .bn_coords$box2,
    .interation_cell = .interaction_cells
  ) |>
    dplyr::mutate(
      row = as.character(.data$row),
      column = as.character(.data$column)
    )

  original_interaction <- .bn_coords$interaction_coords |>
    dplyr::mutate(
      row = as.character(.data$row),
      column = as.character(.data$column)
    )

  extra_cols <- setdiff(
    names(original_interaction),
    c("row", "column", "x", "y", "area", "group")
  )
  if (length(extra_cols) > 0) {
    metadata_lookup <- original_interaction |>
      dplyr::distinct(
        .data$row,
        .data$column,
        .keep_all = TRUE
      ) |>
      dplyr::select(dplyr::all_of(c("row", "column", extra_cols)))
    interaction_coords <- interaction_coords |>
      dplyr::left_join(metadata_lookup, by = c("row", "column"))
  }

  .bn_coords$interaction_coords <- interaction_coords
  .bn_coords
}

compute_binary_interaction_coords <- function(.bn_coords) {
  row_points <- .bn_coords$box1 |>
    dplyr::transmute(
      row = as.character(row),
      x = (xmin + xmax) / 2,
      y = (ymin + ymax) / 2
    )

  column_points <- .bn_coords$box2 |>
    dplyr::transmute(
      column = as.character(column),
      xend = (xmin + xmax) / 2,
      yend = (ymin + ymax) / 2
    )

  interaction_cells <- .bn_coords$interaction_coords |>
    dplyr::mutate(
      row = as.character(row),
      column = as.character(column)
    )

  drop_cols <- intersect(
    c("x", "y", "area", "group"),
    names(interaction_cells)
  )
  if (length(drop_cols) > 0) {
    interaction_cells <- interaction_cells |>
      dplyr::select(-dplyr::all_of(drop_cols))
  }

  interaction_cells |>
    dplyr::distinct(row, column, .keep_all = TRUE) |>
    dplyr::left_join(row_points, by = "row") |>
    dplyr::left_join(column_points, by = "column")
}

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
#' `adjust_box_height`, `tip_positions_row`, `tip_positions_column`, and
#' `na.rm`. These are validated in
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
      params$adjust_box_height <- FALSE # default
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
    interaction_type = "abundance",
    adjust_box_height = FALSE,
    tip_positions_row = NULL,
    tip_positions_column = NULL,
    na.rm = FALSE
  ) {
    interaction_type <- match.arg(
      interaction_type,
      choices = c("abundance", "binary")
    )

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

    construct_bn_coordination_fn <- resolve_internal_function(
      "construct_bn_coordination"
    )

    bn_coords <- construct_bn_coordination_fn(
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

    tip_positions_row <- extract_tip_positions(
      tip_positions_row,
      arg_name = "tip_positions_row"
    )
    tip_positions_column <- extract_tip_positions(
      tip_positions_column,
      arg_name = "tip_positions_column"
    )

    interaction_cells <- prepare_interaction_cells(data)
    bn_coords <- adjust_bn_coords_to_tip_positions(
      .bn_coords = bn_coords,
      .tip_positions_row = tip_positions_row,
      .tip_positions_column = tip_positions_column,
      .interaction_cells = interaction_cells
    )

    if (type == "box1") {
      out <- bn_coords$box1
      out$group <- out$row
    } else if (type == "box2") {
      out <- bn_coords$box2
      out$group <- out$column
    } else if (type == "interaction") {
      if (interaction_type == "binary") {
        out <- compute_binary_interaction_coords(bn_coords)
      } else {
        out <- bn_coords$interaction_coords
      }
      out$group <- as.factor(paste0(out$row, out$column))
    } else {
      rlang::abort("`type` must be one of 'box1', 'box2', or 'interaction'.")
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
#' @param row_nm A single string giving the key column name in
#'   `metadata_row` used to join row metadata.
#' @param column_nm A single string giving the key column name in
#'   `metadata_column` used to join column metadata.
#' @param metadata_row Optional data frame with row metadata used by the stat.
#' @param metadata_column Optional data frame with column metadata used by the stat.
#' @param gap Spacing between the two partitions (see Details of `StatBipnet`).
#' @param box_ratio Width of boxes relative to the interaction band.
#' @param ratio Overall aspect ratio of the layout.
#' @param interaction_type One of `"abundance"` or `"binary"` for interaction
#'   rendering.
#' @param adjust_box_height If `TRUE`, scale box heights by totals.
#' @param tip_positions_row Optional row-side tip positions. Accepts either:
#'   (1) a data frame with columns `label` and `y`, or (2) a tree object
#'   (`phylo`/ggtree) from which tip positions are extracted.
#' @param tip_positions_column Optional column-side tip positions. Accepts the
#'   same formats as `tip_positions_row`.
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
  interaction_type = c("abundance", "binary"),
  adjust_box_height = FALSE,
  tip_positions_row = NULL,
  tip_positions_column = NULL,
  geom = "polygon",
  position = "identity",
  na.rm = FALSE,
  show.legend = NA,
  inherit.aes = FALSE,
  ...
) {
  interaction_type <- match.arg(interaction_type)
  if (
    type == "interaction" &&
      interaction_type == "binary" &&
      identical(geom, "polygon")
  ) {
    geom <- "segment"
  }

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
      interaction_type = interaction_type,
      adjust_box_height = adjust_box_height,
      tip_positions_row = tip_positions_row,
      tip_positions_column = tip_positions_column,
      na.rm = na.rm,
      ...
    )
  )
}
