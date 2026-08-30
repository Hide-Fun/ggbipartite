#' Compute a reusable bipartite layout
#'
#' `layout_bipartite()` validates and normalizes a bipartite interaction input
#' once, then computes coordinates shared by all ggbipartite layers. Matrix
#' inputs use their dimnames as identifiers. Long data inputs require explicit
#' `row`, `column`, and `weight` columns.
#'
#' @param data A numeric matrix with non-empty, unique dimnames, or a data
#'   frame in long format.
#' @param row,column,weight Columns in a long-format `data` frame. Supply bare
#'   names or single strings. These arguments must be `NULL` for matrix input.
#' @param interaction One of `"abundance"` or `"binary"`. Binary mode treats
#'   every positive weight as presence; the mode is never inferred from values.
#' @param row_tree,column_tree Optional `phylo` objects for the row and column
#'   partitions.
#' @param metadata_row,metadata_column Optional node metadata data frames.
#' @param metadata_row_key,metadata_column_key Single strings identifying the
#'   metadata key columns. For long data, these default to the corresponding
#'   `row` or `column` column name. For matrix data, they default to `"row"`
#'   and `"column"`.
#' @param duplicate Optional function used to aggregate duplicate long-format
#'   row-column cells. The default, `NULL`, rejects duplicate cells.
#' @param unmatched_data What to do with data IDs absent from a supplied tree:
#'   `"error"` (the default) or `"drop"`.
#' @param unmatched_tree What to do with tree tips absent from the data:
#'   `"error"` (the default) or `"prune"`. Pruning requires the suggested
#'   package `ape`.
#' @param x0,y0 Finite numeric scalars defining the network origin.
#' @param gap A finite non-negative gap between nodes. When `NULL`, a value of
#'   one tenth of the total interaction weight is used.
#' @param box_ratio A positive finite divisor controlling node-box width.
#' @param ratio A positive finite divisor controlling total network width.
#' @param adjust_box_height Whether to adjust side-specific gaps so both
#'   partitions have the same total height.
#'
#' @return A `bipartite_layout` object with stable components `nodes`,
#'   `interactions`, `tree_links`, `params`, and `trees`.
#'
#' @examples
#' interaction_matrix <- matrix(
#'   c(2, 0, 1, 3),
#'   nrow = 2,
#'   dimnames = list(c("host_a", "host_b"), c("otu_a", "otu_b"))
#' )
#' layout <- layout_bipartite(interaction_matrix)
#' layout
#'
#' interaction_data <- tibble::tibble(
#'   host = c("host_a", "host_a", "host_b"),
#'   otu = c("otu_a", "otu_b", "otu_b"),
#'   abundance = c(2, 1, 3)
#' )
#' layout_bipartite(
#'   interaction_data,
#'   row = host,
#'   column = otu,
#'   weight = abundance
#' )
#'
#' @export
layout_bipartite <- function(
  data,
  row = NULL,
  column = NULL,
  weight = NULL,
  interaction = c("abundance", "binary"),
  row_tree = NULL,
  column_tree = NULL,
  metadata_row = NULL,
  metadata_column = NULL,
  metadata_row_key = NULL,
  metadata_column_key = NULL,
  duplicate = NULL,
  unmatched_data = c("error", "drop"),
  unmatched_tree = c("error", "prune"),
  x0 = 0,
  y0 = 0,
  gap = NULL,
  box_ratio = 5,
  ratio = 1 / 1.618,
  adjust_box_height = FALSE
) {
  interaction <- match.arg(interaction)
  unmatched_data <- match.arg(unmatched_data)
  unmatched_tree <- match.arg(unmatched_tree)

  row_quo <- rlang::enquo(row)
  column_quo <- rlang::enquo(column)
  weight_quo <- rlang::enquo(weight)

  normalized <- normalize_bipartite_input(
    data = data,
    row = row_quo,
    column = column_quo,
    weight = weight_quo,
    duplicate = duplicate
  )
  interaction_matrix <- normalized$matrix

  if (interaction == "binary") {
    interaction_matrix[interaction_matrix > 0] <- 1
  }

  row_match <- reconcile_layout_tree(
    tree = row_tree,
    ids = rownames(interaction_matrix),
    side = "row",
    unmatched_data = unmatched_data,
    unmatched_tree = unmatched_tree
  )
  column_match <- reconcile_layout_tree(
    tree = column_tree,
    ids = colnames(interaction_matrix),
    side = "column",
    unmatched_data = unmatched_data,
    unmatched_tree = unmatched_tree
  )

  interaction_matrix <- interaction_matrix[
    row_match$order,
    column_match$order,
    drop = FALSE
  ]
  validate_nonzero_layout_sides(interaction_matrix)

  validate_layout_scalar(x0, "x0", lower = -Inf)
  validate_layout_scalar(y0, "y0", lower = -Inf)
  validate_layout_scalar(box_ratio, "box_ratio", lower = 0, strict = TRUE)
  validate_layout_scalar(ratio, "ratio", lower = 0, strict = TRUE)
  validate_layout_flag(adjust_box_height, "adjust_box_height")

  if (is.null(gap)) {
    gap <- sum(interaction_matrix) / 10
  }
  validate_layout_scalar(gap, "gap", lower = 0)

  row_key <- metadata_row_key %||% normalized$row_name
  column_key <- metadata_column_key %||% normalized$column_name
  row_metadata <- prepare_layout_metadata(
    metadata = metadata_row,
    key = row_key,
    side = "row"
  )
  column_metadata <- prepare_layout_metadata(
    metadata = metadata_column,
    key = column_key,
    side = "column"
  )

  row_order <- row_match$order
  column_order <- column_match$order

  # Legacy coordinate helpers stack non-factor IDs in reverse input order.
  # Reversing here preserves the canonical input/tree order from bottom to top.
  coordinate_matrix <- interaction_matrix[
    rev(row_order),
    rev(column_order),
    drop = FALSE
  ]
  coordinates <- construct_bn_coordination(
    .mat = coordinate_matrix,
    .row = "row",
    .column = "column",
    .x0 = x0,
    .y0 = y0,
    .gap = gap,
    .box_ratio = box_ratio,
    .ratio = ratio,
    .adjust_box_height = adjust_box_height
  )

  nodes <- build_layout_nodes(
    coordinates = coordinates,
    row_order = row_order,
    column_order = column_order,
    row_metadata = row_metadata,
    column_metadata = column_metadata
  )
  interactions <- build_layout_interactions(
    coordinates = coordinates,
    interaction_matrix = interaction_matrix,
    interaction = interaction,
    row_metadata = row_metadata,
    column_metadata = column_metadata
  )

  global_params <- calc_global_params(
    .mat = coordinate_matrix,
    .x0 = x0,
    .y0 = y0,
    .gap = gap,
    .box_ratio = box_ratio,
    .ratio = ratio,
    .adjust_box_height = adjust_box_height
  )
  tree_components <- build_layout_trees(
    row_original = row_tree,
    column_original = column_tree,
    row_tree = row_match$tree,
    column_tree = column_match$tree,
    nodes = nodes,
    interaction = interaction
  )

  params <- list(
    interaction = interaction,
    origin = c(x = x0, y = y0),
    gap = c(
      row = global_params$gap_row,
      column = global_params$gap_column
    ),
    box_width = global_params$box_width,
    width = global_params$width,
    box_ratio = box_ratio,
    ratio = ratio,
    adjust_box_height = adjust_box_height,
    row_order = row_order,
    column_order = column_order,
    unmatched_data = unmatched_data,
    unmatched_tree = unmatched_tree
  )

  new_bipartite_layout(
    nodes = nodes,
    interactions = interactions,
    tree_links = tree_components$links,
    params = params,
    trees = tree_components$trees
  )
}

#' @export
print.bipartite_layout <- function(x, ...) {
  validate_bipartite_layout(x)
  row_count <- sum(x$nodes$side == "row")
  column_count <- sum(x$nodes$side == "column")
  edge_count <- length(unique(x$interactions$edge_id))
  tree_count <- sum(!vapply(x$trees, is.null, logical(1)))

  cat("<bipartite_layout>\n")
  cat("  mode: ", x$params$interaction, "\n", sep = "")
  cat(
    "  nodes: ", row_count, " row / ", column_count, " column\n",
    sep = ""
  )
  cat("  interactions: ", edge_count, "\n", sep = "")
  cat("  trees: ", tree_count, "\n", sep = "")
  invisible(x)
}

new_bipartite_layout <- function(
  nodes,
  interactions,
  tree_links,
  params,
  trees
) {
  structure(
    list(
      nodes = nodes,
      interactions = interactions,
      tree_links = tree_links,
      params = params,
      trees = trees
    ),
    class = "bipartite_layout"
  )
}

validate_bipartite_layout <- function(x) {
  if (!inherits(x, "bipartite_layout")) {
    stop("`layout` must be a `bipartite_layout` object.", call. = FALSE)
  }

  required_components <- c(
    "nodes",
    "interactions",
    "tree_links",
    "params",
    "trees"
  )
  missing_components <- setdiff(required_components, names(x))
  if (length(missing_components) > 0L) {
    stop(
      "`layout` is missing components: ",
      paste(missing_components, collapse = ", "),
      ".",
      call. = FALSE
    )
  }

  required_nodes <- c(
    "side",
    "id",
    "order",
    "interaction_size",
    "xmin",
    "xmax",
    "ymin",
    "ymax",
    "x",
    "y"
  )
  required_interactions <- c(
    "edge_id",
    "row",
    "column",
    "weight",
    "x",
    "y"
  )
  required_links <- c("side", "id", "x", "xend", "y", "yend")

  validate_layout_columns(x$nodes, required_nodes, "layout$nodes")
  validate_layout_columns(
    x$interactions,
    required_interactions,
    "layout$interactions"
  )
  validate_layout_columns(x$tree_links, required_links, "layout$tree_links")

  if (!identical(sort(unique(x$nodes$side)), c("column", "row"))) {
    stop(
      "`layout$nodes$side` must contain both `row` and `column`.",
      call. = FALSE
    )
  }
  if (!x$params$interaction %in% c("abundance", "binary")) {
    stop("`layout$params$interaction` is invalid.", call. = FALSE)
  }

  invisible(x)
}

validate_layout_columns <- function(data, required, arg_name) {
  if (!is.data.frame(data)) {
    stop("`", arg_name, "` must be a data frame.", call. = FALSE)
  }
  missing_columns <- setdiff(required, names(data))
  if (length(missing_columns) > 0L) {
    stop(
      "`", arg_name, "` is missing columns: ",
      paste(missing_columns, collapse = ", "),
      ".",
      call. = FALSE
    )
  }
}

normalize_bipartite_input <- function(data, row, column, weight, duplicate) {
  selectors_are_null <- vapply(
    list(row, column, weight),
    rlang::quo_is_null,
    logical(1)
  )

  if (is.matrix(data)) {
    if (!all(selectors_are_null)) {
      stop(
        "`row`, `column`, and `weight` must be `NULL` for matrix input.",
        call. = FALSE
      )
    }
    if (!is.null(duplicate)) {
      stop("`duplicate` is only used with long data input.", call. = FALSE)
    }

    return(list(
      matrix = validate_layout_matrix(data),
      row_name = "row",
      column_name = "column"
    ))
  }

  if (!is.data.frame(data)) {
    stop("`data` must be a matrix or data frame.", call. = FALSE)
  }
  if (!all(!selectors_are_null)) {
    stop(
      "Long data input requires explicit `row`, `column`, and `weight` ",
      "columns.",
      call. = FALSE
    )
  }

  row_name <- resolve_layout_column(row, data, "row")
  column_name <- resolve_layout_column(column, data, "column")
  weight_name <- resolve_layout_column(weight, data, "weight")

  if (length(unique(c(row_name, column_name, weight_name))) != 3L) {
    stop("`row`, `column`, and `weight` must select distinct columns.")
  }

  row_ids <- normalize_layout_ids(
    data[[row_name]],
    paste0("data$", row_name),
    unique_required = FALSE
  )
  column_ids <- normalize_layout_ids(
    data[[column_name]],
    paste0("data$", column_name),
    unique_required = FALSE
  )
  weights <- data[[weight_name]]
  validate_layout_weights(weights, paste0("data$", weight_name))

  cells <- tibble::tibble(
    row = row_ids,
    column = column_ids,
    weight = as.numeric(weights)
  )
  row_order <- unique(row_ids)
  column_order <- unique(column_ids)
  cells <- aggregate_layout_cells(cells, duplicate)

  interaction_matrix <- matrix(
    0,
    nrow = length(row_order),
    ncol = length(column_order),
    dimnames = list(row_order, column_order)
  )
  cell_indices <- cbind(
    match(cells$row, row_order),
    match(cells$column, column_order)
  )
  interaction_matrix[cell_indices] <- cells$weight
  validate_nonzero_layout_sides(interaction_matrix)

  list(
    matrix = interaction_matrix,
    row_name = row_name,
    column_name = column_name
  )
}

validate_layout_matrix <- function(data) {
  if (length(dim(data)) != 2L || any(dim(data) == 0L)) {
    stop("`data` must be a non-empty two-dimensional matrix.", call. = FALSE)
  }
  if (!typeof(data) %in% c("integer", "double")) {
    stop("`data` must be a numeric matrix.", call. = FALSE)
  }

  validate_layout_weights(as.vector(data), "data")
  row_ids <- normalize_layout_ids(rownames(data), "rownames(data)")
  column_ids <- normalize_layout_ids(colnames(data), "colnames(data)")

  output <- matrix(
    as.numeric(data),
    nrow = nrow(data),
    ncol = ncol(data),
    dimnames = list(row_ids, column_ids)
  )
  validate_nonzero_layout_sides(output)
  output
}

resolve_layout_column <- function(column, data, arg_name) {
  expression <- rlang::get_expr(column)
  column_name <- if (is.symbol(expression)) {
    rlang::as_string(expression)
  } else if (is.character(expression) && length(expression) == 1L) {
    expression
  } else {
    stop(
      "`", arg_name, "` must be a bare column name or a single string.",
      call. = FALSE
    )
  }

  if (!column_name %in% names(data)) {
    stop(
      "`", arg_name, "` refers to missing column `", column_name, "`.",
      call. = FALSE
    )
  }
  column_name
}

normalize_layout_ids <- function(ids, arg_name, unique_required = TRUE) {
  if (is.null(ids)) {
    stop("`", arg_name, "` must be supplied.", call. = FALSE)
  }

  normalized <- as.character(ids)
  if (anyNA(normalized) || any(normalized == "")) {
    stop(
      "`", arg_name, "` must not contain missing or empty IDs.",
      call. = FALSE
    )
  }

  duplicated_ids <- unique(normalized[duplicated(normalized)])
  if (unique_required && length(duplicated_ids) > 0L) {
    stop(
      "`", arg_name, "` must be unique; duplicated IDs: ",
      paste(duplicated_ids, collapse = ", "),
      ".",
      call. = FALSE
    )
  }
  normalized
}

validate_layout_weights <- function(weights, arg_name) {
  if (
    !is.numeric(weights) ||
      !typeof(weights) %in% c("integer", "double")
  ) {
    stop("`", arg_name, "` must be numeric.", call. = FALSE)
  }
  if (any(!is.finite(weights))) {
    stop("`", arg_name, "` must contain only finite values.", call. = FALSE)
  }
  if (any(weights < 0)) {
    stop("`", arg_name, "` must not contain negative values.", call. = FALSE)
  }
  invisible(weights)
}

validate_nonzero_layout_sides <- function(interaction_matrix) {
  if (nrow(interaction_matrix) == 0L || ncol(interaction_matrix) == 0L) {
    stop("The normalized interaction matrix must have two non-empty sides.")
  }

  zero_rows <- rownames(interaction_matrix)[rowSums(interaction_matrix) == 0]
  zero_columns <- colnames(interaction_matrix)[
    colSums(interaction_matrix) == 0
  ]
  problems <- character()
  if (length(zero_rows) > 0L) {
    problems <- c(
      problems,
      paste0("zero-sum row IDs: ", paste(zero_rows, collapse = ", "))
    )
  }
  if (length(zero_columns) > 0L) {
    problems <- c(
      problems,
      paste0("zero-sum column IDs: ", paste(zero_columns, collapse = ", "))
    )
  }
  if (length(problems) > 0L) {
    stop(
      "Every node must have positive total interaction; ",
      paste(problems, collapse = "; "),
      ".",
      call. = FALSE
    )
  }
  invisible(interaction_matrix)
}

aggregate_layout_cells <- function(cells, duplicate) {
  if (!is.null(duplicate) && !is.function(duplicate)) {
    stop("`duplicate` must be `NULL` or a function.", call. = FALSE)
  }

  keys <- cells[c("row", "column")]
  is_duplicate <- duplicated(keys) | duplicated(keys, fromLast = TRUE)
  if (!any(is_duplicate)) {
    return(cells)
  }

  duplicate_keys <- unique(keys[is_duplicate, , drop = FALSE])
  duplicate_labels <- paste0(
    "(`",
    duplicate_keys$row,
    "`, `",
    duplicate_keys$column,
    "`)"
  )
  if (is.null(duplicate)) {
    stop(
      "`data` contains duplicate row-column cells: ",
      paste(duplicate_labels, collapse = ", "),
      ". Supply `duplicate` to aggregate them explicitly.",
      call. = FALSE
    )
  }
  first_cells <- !duplicated(keys)
  unique_cells <- cells[first_cells, c("row", "column"), drop = FALSE]
  aggregated <- vapply(
    seq_len(nrow(unique_cells)),
    function(index) {
      selected <- cells$row == unique_cells$row[[index]] &
        cells$column == unique_cells$column[[index]]
      result <- duplicate(cells$weight[selected])
      if (
        !typeof(result) %in% c("integer", "double") ||
          length(result) != 1L ||
          !is.finite(result) ||
          result < 0
      ) {
        stop(
          "`duplicate` must return one finite, non-negative number per cell.",
          call. = FALSE
        )
      }
      as.numeric(result)
    },
    numeric(1)
  )

  tibble::tibble(
    row = unique_cells$row,
    column = unique_cells$column,
    weight = aggregated
  )
}

reconcile_layout_tree <- function(
  tree,
  ids,
  side,
  unmatched_data,
  unmatched_tree
) {
  if (is.null(tree)) {
    return(list(tree = NULL, order = ids))
  }
  if (!inherits(tree, "phylo")) {
    stop("`", side, "_tree` must be a `phylo` object.", call. = FALSE)
  }

  tip_ids <- normalize_layout_ids(
    tree$tip.label,
    paste0(side, "_tree$tip.label")
  )
  data_only <- setdiff(ids, tip_ids)
  tree_only <- setdiff(tip_ids, ids)
  errors <- character()
  if (length(data_only) > 0L && unmatched_data == "error") {
    errors <- c(
      errors,
      paste0("data IDs absent from tree: ", paste(data_only, collapse = ", "))
    )
  }
  if (length(tree_only) > 0L && unmatched_tree == "error") {
    errors <- c(
      errors,
      paste0("tree tips absent from data: ", paste(tree_only, collapse = ", "))
    )
  }
  if (length(errors) > 0L) {
    stop(
      "ID mismatch for the ", side, " side; ",
      paste(errors, collapse = "; "),
      ".",
      call. = FALSE
    )
  }

  retained_ids <- ids[ids %in% tip_ids]
  if (length(retained_ids) == 0L) {
    stop(
      "No ", side, " IDs remain after tree reconciliation.",
      call. = FALSE
    )
  }

  matched_tree <- tree
  if (length(tree_only) > 0L) {
    if (!requireNamespace("ape", quietly = TRUE)) {
      stop(
        "Pruning unmatched tree tips requires the suggested package `ape`.",
        call. = FALSE
      )
    }
    matched_tree <- ape::drop.tip(matched_tree, tree_only)
  }

  tip_order <- if (length(matched_tree$tip.label) == 1L) {
    as.character(matched_tree$tip.label)
  } else {
    suppressMessages(as.character(get_tip_order(matched_tree)))
  }
  tip_order <- tip_order[tip_order %in% retained_ids]
  if (length(tip_order) == 0L) {
    stop("Internal error: tree reconciliation lost retained IDs.")
  }

  list(tree = matched_tree, order = tip_order)
}

prepare_layout_metadata <- function(metadata, key, side) {
  if (is.null(metadata)) {
    return(NULL)
  }
  if (
    !is.character(key) ||
      length(key) != 1L ||
      is.na(key) ||
      key == ""
  ) {
    stop(
      "`metadata_", side, "_key` must be a single non-empty string.",
      call. = FALSE
    )
  }

  normalized <- normalize_metadata_key(
    metadata = metadata,
    key = key,
    metadata_arg = paste0("metadata_", side),
    key_arg = paste0("metadata_", side, "_key")
  )
  value_columns <- setdiff(names(normalized), key)
  if (length(value_columns) > 0L) {
    names(normalized)[match(value_columns, names(normalized))] <- paste0(
      side,
      "_",
      value_columns
    )
  }
  names(normalized)[names(normalized) == key] <- "id"
  tibble::as_tibble(normalized)
}

matrix_to_layout_cells <- function(interaction_matrix) {
  indices <- which(interaction_matrix > 0, arr.ind = TRUE)
  if (nrow(indices) == 0L) {
    return(tibble::tibble(
      row = character(),
      column = character(),
      interaction = numeric()
    ))
  }

  tibble::tibble(
    row = rownames(interaction_matrix)[indices[, "row"]],
    column = colnames(interaction_matrix)[indices[, "col"]],
    interaction = interaction_matrix[indices]
  )
}

build_layout_nodes <- function(
  coordinates,
  row_order,
  column_order,
  row_metadata,
  column_metadata
) {
  row_nodes <- coordinates$row_box |>
    dplyr::transmute(
      side = "row",
      id = as.character(.data$row),
      order = match(.data$id, row_order),
      interaction_size = as.numeric(.data$interaction_size),
      xmin = as.numeric(.data$xmin),
      xmax = as.numeric(.data$xmax),
      ymin = as.numeric(.data$ymin),
      ymax = as.numeric(.data$ymax),
      x = (.data$xmin + .data$xmax) / 2,
      y = (.data$ymin + .data$ymax) / 2
    ) |>
    dplyr::arrange(.data$order)
  column_nodes <- coordinates$column_box |>
    dplyr::transmute(
      side = "column",
      id = as.character(.data$column),
      order = match(.data$id, column_order),
      interaction_size = as.numeric(.data$interaction_size),
      xmin = as.numeric(.data$xmin),
      xmax = as.numeric(.data$xmax),
      ymin = as.numeric(.data$ymin),
      ymax = as.numeric(.data$ymax),
      x = (.data$xmin + .data$xmax) / 2,
      y = (.data$ymin + .data$ymax) / 2
    ) |>
    dplyr::arrange(.data$order)

  if (!is.null(row_metadata)) {
    row_nodes <- dplyr::left_join(row_nodes, row_metadata, by = "id")
  }
  if (!is.null(column_metadata)) {
    column_nodes <- dplyr::left_join(
      column_nodes,
      column_metadata,
      by = "id"
    )
  }
  dplyr::bind_rows(row_nodes, column_nodes)
}

build_layout_interactions <- function(
  coordinates,
  interaction_matrix,
  interaction,
  row_metadata,
  column_metadata
) {
  edge_lookup <- matrix_to_layout_cells(interaction_matrix) |>
    dplyr::rename(weight = tidyselect::all_of("interaction")) |>
    dplyr::mutate(edge_id = paste0("edge-", dplyr::row_number())) |>
    dplyr::select(tidyselect::all_of(
      c("edge_id", "row", "column", "weight")
    ))

  if (interaction == "binary") {
    interactions <- compute_binary_interaction_coords(coordinates) |>
      dplyr::mutate(
        row = as.character(.data$row),
        column = as.character(.data$column)
      ) |>
      dplyr::select(tidyselect::all_of(
        c("row", "column", "x", "y", "xend", "yend")
      ))
  } else {
    interactions <- coordinates$interaction_coords |>
      dplyr::mutate(
        row = as.character(.data$row),
        column = as.character(.data$column)
      ) |>
      dplyr::select(
        tidyselect::all_of(c("row", "column", "x", "y")),
        tidyselect::any_of("area")
      )
  }

  interactions <- interactions |>
    dplyr::left_join(edge_lookup, by = c("row", "column")) |>
    dplyr::group_by(.data$edge_id) |>
    dplyr::mutate(
      vertex = dplyr::row_number(),
      group = .data$edge_id
    ) |>
    dplyr::ungroup() |>
    dplyr::relocate(tidyselect::all_of(
      c("edge_id", "row", "column", "weight", "vertex", "group")
    ))

  if (anyNA(interactions$edge_id)) {
    stop("Internal error: interaction coordinates lost an edge ID.")
  }
  if (!is.null(row_metadata)) {
    interactions <- dplyr::left_join(
      interactions,
      row_metadata,
      by = c("row" = "id")
    )
  }
  if (!is.null(column_metadata)) {
    interactions <- dplyr::left_join(
      interactions,
      column_metadata,
      by = c("column" = "id")
    )
  }
  interactions
}

build_layout_trees <- function(
  row_original,
  column_original,
  row_tree,
  column_tree,
  nodes,
  interaction
) {
  empty_links <- tibble::tibble(
    side = character(),
    id = character(),
    x = numeric(),
    xend = numeric(),
    y = numeric(),
    yend = numeric()
  )
  row_component <- build_layout_tree_side(
    original = row_original,
    tree = row_tree,
    nodes = dplyr::filter(nodes, .data$side == "row"),
    side = "row",
    interaction = interaction
  )
  column_component <- build_layout_tree_side(
    original = column_original,
    tree = column_tree,
    nodes = dplyr::filter(nodes, .data$side == "column"),
    side = "column",
    interaction = interaction
  )

  links <- dplyr::bind_rows(
    row_component$links %||% empty_links,
    column_component$links %||% empty_links
  )
  list(
    trees = list(row = row_component$tree, column = column_component$tree),
    links = links
  )
}

build_layout_tree_side <- function(
  original,
  tree,
  nodes,
  side,
  interaction
) {
  if (is.null(tree)) {
    return(list(tree = NULL, links = NULL))
  }

  position <- if (side == "row") "left" else "right"
  tree_box <- nodes
  tree_box[[side]] <- tree_box$id
  tree_plot <- if (length(tree$tip.label) == 1L) {
    build_singleton_layout_tree(tree, nodes, side)
  } else {
    suppressMessages(
      adjust_tree(
        .phylo = tree,
        .box = tree_box,
        .adjust_tip_position = interaction == "binary",
        .tree_position = position
      )
    )
  }
  tips <- suppressMessages(
    extract_tip_positions(tree_plot, paste0(side, "_tree"))
  ) |>
    dplyr::transmute(id = as.character(.data$label), tree_y = .data$y)
  links <- NULL

  if (interaction == "abundance") {
    links <- nodes |>
      dplyr::transmute(id = .data$id, node_y = .data$y) |>
      dplyr::left_join(tips, by = "id")
    if (anyNA(links$tree_y)) {
      stop("Internal error: tree links could not match all node IDs.")
    }

    if (side == "row") {
      links <- links |>
        dplyr::transmute(
          side = side,
          id = .data$id,
          x = 0,
          xend = 1,
          y = .data$tree_y,
          yend = .data$node_y
        )
    } else {
      links <- links |>
        dplyr::transmute(
          side = side,
          id = .data$id,
          x = 0,
          xend = 1,
          y = .data$node_y,
          yend = .data$tree_y
        )
    }
  }

  list(
    tree = list(
      original = original,
      validated = tree,
      geometry = tree_plot
    ),
    links = links
  )
}

build_singleton_layout_tree <- function(tree, nodes, side) {
  tip_label <- as.character(tree$tip.label[[1L]])
  tip_index <- match(tip_label, nodes$id)
  if (is.na(tip_index)) {
    stop("Internal error: singleton tree tip did not match a node ID.")
  }

  branch_length <- tree$edge.length
  if (
    is.null(branch_length) ||
      length(branch_length) != 1L ||
      !is.finite(branch_length)
  ) {
    branch_length <- 1
  }
  branch_length <- as.numeric(branch_length)
  tip_y <- nodes$y[[tip_index]]
  y_limits <- range(c(nodes$ymin, nodes$ymax))

  tree_data <- tibble::tibble(
    node = c(2L, 1L),
    parent = c(2L, 2L),
    isTip = c(FALSE, TRUE),
    label = c(NA_character_, tip_label),
    x = c(0, branch_length),
    y = c(tip_y, tip_y)
  )
  edge_data <- tibble::tibble(
    x = 0,
    xend = branch_length,
    y = tip_y,
    yend = tip_y
  )
  tree_plot <- ggplot2::ggplot(tree_data) +
    ggplot2::geom_segment(
      data = edge_data,
      mapping = ggplot2::aes(
        x = .data$x,
        xend = .data$xend,
        y = .data$y,
        yend = .data$yend
      ),
      inherit.aes = FALSE
    ) +
    ggplot2::scale_y_continuous(
      limits = y_limits,
      expand = ggplot2::expansion(mult = 0.05)
    )

  if (side == "column") {
    tree_plot <- tree_plot + ggplot2::scale_x_reverse()
  }
  tree_plot
}

validate_layout_scalar <- function(
  value,
  arg_name,
  lower,
  strict = FALSE
) {
  if (
    !is.numeric(value) ||
      length(value) != 1L ||
      is.na(value) ||
      !is.finite(value)
  ) {
    stop("`", arg_name, "` must be a finite numeric scalar.", call. = FALSE)
  }
  is_invalid <- if (strict) value <= lower else value < lower
  if (is_invalid) {
    comparison <- if (strict) "greater than" else "at least"
    stop(
      "`", arg_name, "` must be ", comparison, " ", lower, ".",
      call. = FALSE
    )
  }
  invisible(value)
}

validate_layout_flag <- function(value, arg_name) {
  if (!is.logical(value) || length(value) != 1L || is.na(value)) {
    stop("`", arg_name, "` must be a single TRUE/FALSE.", call. = FALSE)
  }
  invisible(value)
}

`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}
