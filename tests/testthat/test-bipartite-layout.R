layout_contract_matrix <- function() {
  matrix(
    c(
      2,
      0,
      1,
      3,
      2,
      1
    ),
    nrow = 3,
    byrow = TRUE,
    dimnames = list(
      c("row_a", "row_b", "row_c"),
      c("column_a", "column_b")
    )
  )
}

quiet_layout_bipartite <- function(...) {
  suppressMessages(suppressWarnings(layout_bipartite(...)))
}

expect_finite_layout_coordinates <- function(layout) {
  node_coordinates <- c("xmin", "xmax", "ymin", "ymax", "x", "y")
  interaction_coordinates <- intersect(
    c("x", "y", "xend", "yend", "area"),
    names(layout$interactions)
  )
  link_coordinates <- c("x", "xend", "y", "yend")

  expect_true(all(is.finite(unlist(layout$nodes[node_coordinates]))))
  expect_true(all(is.finite(unlist(
    layout$interactions[interaction_coordinates]
  ))))
  expect_true(all(is.finite(unlist(layout$tree_links[link_coordinates]))))
}

test_that("layout exposes the stable v0.10 schema", {
  layout <- layout_bipartite(layout_contract_matrix(), gap = 0.25)

  expect_s3_class(layout, "bipartite_layout")
  expect_true(all(
    c(
      "nodes",
      "interactions",
      "tree_links",
      "params",
      "trees"
    ) %in%
      names(layout)
  ))
  expect_s3_class(layout$nodes, "tbl_df")
  expect_s3_class(layout$interactions, "tbl_df")
  expect_s3_class(layout$tree_links, "tbl_df")
  expect_true(all(
    c(
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
    ) %in%
      names(layout$nodes)
  ))
  expect_true(all(
    c(
      "edge_id",
      "row",
      "column",
      "weight",
      "vertex",
      "group",
      "x",
      "y",
      "area"
    ) %in%
      names(layout$interactions)
  ))
  expect_true(all(
    c(
      "side",
      "id",
      "x",
      "xend",
      "y",
      "yend"
    ) %in%
      names(layout$tree_links)
  ))
  expect_true(all(c("row", "column") %in% names(layout$trees)))
  expect_null(layout$trees$row)
  expect_null(layout$trees$column)
  expect_equal(sort(unique(layout$nodes$side)), c("column", "row"))
  expect_equal(
    length(unique(layout$interactions$edge_id)),
    sum(layout_contract_matrix() > 0)
  )
  edge_vertex_counts <- table(layout$interactions$edge_id)
  expect_true(all(edge_vertex_counts == 4L))
  expect_equal(layout$params$interaction, "abundance")
  expect_equal(layout$params$origin, c(x = 0, y = 0))
  expect_equal(layout$params$gap, c(row = 0.25, column = 0.25))
  expect_finite_layout_coordinates(layout)
})

test_that("matrix and long inputs use explicit dispatch", {
  interaction_matrix <- layout_contract_matrix()
  matrix_layout <- layout_bipartite(interaction_matrix)

  expect_equal(matrix_layout$params$row_order, rownames(interaction_matrix))
  expect_equal(
    matrix_layout$params$column_order,
    colnames(interaction_matrix)
  )
  expect_error(
    layout_bipartite(interaction_matrix, row = row),
    "must be `NULL` for matrix input",
    fixed = TRUE
  )
  expect_error(
    layout_bipartite(interaction_matrix, duplicate = sum),
    "only used with long data input",
    fixed = TRUE
  )

  interaction_data <- tibble::tibble(
    host = c("row_b", "row_a", "row_c", "row_b"),
    partner = c("column_b", "column_a", "column_a", "column_a"),
    abundance = c(3, 2, 2, 1)
  )
  bare_layout <- layout_bipartite(
    interaction_data,
    row = host,
    column = partner,
    weight = abundance
  )
  string_layout <- layout_bipartite(
    interaction_data,
    row = "host",
    column = "partner",
    weight = "abundance"
  )

  expect_equal(bare_layout, string_layout)
  expect_equal(bare_layout$params$row_order, c("row_b", "row_a", "row_c"))
  expect_equal(
    bare_layout$params$column_order,
    c("column_b", "column_a")
  )
  expect_error(
    layout_bipartite(interaction_data),
    "requires explicit `row`, `column`, and `weight`",
    fixed = TRUE
  )
  expect_error(
    layout_bipartite(
      interaction_data,
      row = host,
      column = partner
    ),
    "requires explicit `row`, `column`, and `weight`",
    fixed = TRUE
  )
  expect_error(
    layout_bipartite(
      interaction_data,
      row = missing_host,
      column = partner,
      weight = abundance
    ),
    "missing column `missing_host`",
    fixed = TRUE
  )
  expect_error(
    layout_bipartite(
      interaction_data,
      row = host,
      column = host,
      weight = abundance
    ),
    "must select distinct columns",
    fixed = TRUE
  )
  expect_error(layout_bipartite(1:4), "matrix or data frame", fixed = TRUE)
})

test_that("IDs are non-empty, unique, and normalized without guessing", {
  interaction_matrix <- layout_contract_matrix()
  unnamed_matrix <- unname(interaction_matrix)
  empty_id_matrix <- interaction_matrix
  duplicate_id_matrix <- interaction_matrix
  missing_id_matrix <- interaction_matrix
  rownames(empty_id_matrix)[[1]] <- ""
  rownames(duplicate_id_matrix)[[2]] <- rownames(duplicate_id_matrix)[[1]]
  rownames(missing_id_matrix)[[1]] <- NA_character_

  expect_error(
    layout_bipartite(unnamed_matrix),
    "`rownames(data)` must be supplied.",
    fixed = TRUE
  )
  expect_error(
    layout_bipartite(empty_id_matrix),
    "must not contain missing or empty IDs",
    fixed = TRUE
  )
  expect_error(
    layout_bipartite(duplicate_id_matrix),
    "must be unique; duplicated IDs: row_a",
    fixed = TRUE
  )
  expect_error(
    layout_bipartite(missing_id_matrix),
    "must not contain missing or empty IDs",
    fixed = TRUE
  )

  interaction_data <- tibble::tibble(
    host = factor(c("A", " a ")),
    partner = c(10L, 20L),
    abundance = c(1L, 2L)
  )
  layout <- layout_bipartite(
    interaction_data,
    row = host,
    column = partner,
    weight = abundance
  )
  row_ids <- layout$nodes$id[layout$nodes$side == "row"]
  column_ids <- layout$nodes$id[layout$nodes$side == "column"]

  expect_type(layout$nodes$id, "character")
  expect_equal(row_ids, c("A", " a "))
  expect_equal(column_ids, c("10", "20"))

  interaction_data$host <- as.character(interaction_data$host)
  interaction_data$host[[1]] <- ""
  expect_error(
    layout_bipartite(
      interaction_data,
      row = host,
      column = partner,
      weight = abundance
    ),
    "must not contain missing or empty IDs",
    fixed = TRUE
  )
})

test_that("weights must be finite, numeric, and non-negative", {
  interaction_matrix <- layout_contract_matrix()
  non_numeric_matrix <- matrix(
    as.character(interaction_matrix),
    nrow = nrow(interaction_matrix),
    dimnames = dimnames(interaction_matrix)
  )

  expect_error(
    layout_bipartite(non_numeric_matrix),
    "`data` must be a numeric matrix.",
    fixed = TRUE
  )
  for (bad_weight in list(NA_real_, Inf, -1)) {
    invalid_matrix <- interaction_matrix
    invalid_matrix[[1]] <- bad_weight
    expected <- if (is.finite(bad_weight) && bad_weight < 0) {
      "must not contain negative values"
    } else {
      "must contain only finite values"
    }
    expect_error(layout_bipartite(invalid_matrix), expected, fixed = TRUE)
  }

  interaction_data <- tibble::tibble(
    host = c("r1", "r2"),
    partner = c("c1", "c2"),
    abundance = c("1", "2")
  )
  expect_error(
    layout_bipartite(
      interaction_data,
      row = host,
      column = partner,
      weight = abundance
    ),
    "`data$abundance` must be numeric.",
    fixed = TRUE
  )

  interaction_data$abundance <- as.Date(c("2020-01-01", "2020-01-02"))
  expect_error(
    layout_bipartite(
      interaction_data,
      row = host,
      column = partner,
      weight = abundance
    ),
    "`data$abundance` must be numeric.",
    fixed = TRUE
  )
})

test_that("zero cells are absent but zero-sum nodes are rejected", {
  interaction_matrix <- layout_contract_matrix()
  layout <- layout_bipartite(interaction_matrix)
  edges <- dplyr::distinct(
    layout$interactions,
    .data$row,
    .data$column,
    .data$weight
  )

  expect_false(any(edges$row == "row_a" & edges$column == "column_b"))

  zero_row_matrix <- interaction_matrix
  zero_row_matrix["row_a", ] <- 0
  expect_error(
    layout_bipartite(zero_row_matrix),
    "zero-sum row IDs: row_a",
    fixed = TRUE
  )

  zero_column_matrix <- interaction_matrix
  zero_column_matrix[, "column_a"] <- 0
  expect_error(
    layout_bipartite(zero_column_matrix),
    "zero-sum column IDs: column_a",
    fixed = TRUE
  )

  all_zero_matrix <- interaction_matrix * 0
  expect_error(
    layout_bipartite(all_zero_matrix),
    "Every node must have positive total interaction",
    fixed = TRUE
  )
})

test_that("duplicate long cells require explicit aggregation", {
  interaction_data <- tibble::tibble(
    host = c("r1", "r1", "r2"),
    partner = c("c1", "c1", "c2"),
    abundance = c(1, 2, 4)
  )

  expect_error(
    layout_bipartite(
      interaction_data,
      row = host,
      column = partner,
      weight = abundance
    ),
    "duplicate row-column cells: (`r1`, `c1`)",
    fixed = TRUE
  )

  layout <- layout_bipartite(
    interaction_data,
    row = host,
    column = partner,
    weight = abundance,
    duplicate = sum
  )
  edges <- dplyr::distinct(
    layout$interactions,
    .data$row,
    .data$column,
    .data$weight
  )
  expect_equal(edges$weight[edges$row == "r1"], 3)
  expect_equal(nrow(edges), 2L)

  expect_error(
    layout_bipartite(
      interaction_data,
      row = host,
      column = partner,
      weight = abundance,
      duplicate = "sum"
    ),
    "must be `NULL` or a function",
    fixed = TRUE
  )
  expect_error(
    layout_bipartite(
      interaction_data,
      row = host,
      column = partner,
      weight = abundance,
      duplicate = function(x) c(min(x), max(x))
    ),
    "must return one finite, non-negative number",
    fixed = TRUE
  )
  expect_error(
    layout_bipartite(
      interaction_data,
      row = host,
      column = partner,
      weight = abundance,
      duplicate = function(x) as.Date("2020-01-01")
    ),
    "must return one finite, non-negative number",
    fixed = TRUE
  )

  unique_data <- dplyr::distinct(
    interaction_data,
    .data$host,
    .data$partner,
    .keep_all = TRUE
  )
  expect_error(
    layout_bipartite(
      unique_data,
      row = host,
      column = partner,
      weight = abundance,
      duplicate = "sum"
    ),
    "must be `NULL` or a function",
    fixed = TRUE
  )
})

test_that("binary mode treats positive weights as presence only when asked", {
  interaction_matrix <- layout_contract_matrix()
  abundance_layout <- layout_bipartite(interaction_matrix)
  binary_layout <- layout_bipartite(
    interaction_matrix,
    interaction = "binary"
  )
  binary_edges <- binary_layout$interactions

  expect_equal(abundance_layout$params$interaction, "abundance")
  expect_equal(binary_layout$params$interaction, "binary")
  expect_true(all(binary_edges$weight == 1))
  expect_equal(nrow(binary_edges), sum(interaction_matrix > 0))
  expect_named(
    binary_edges,
    c(
      "edge_id",
      "row",
      "column",
      "weight",
      "vertex",
      "group",
      "x",
      "y",
      "xend",
      "yend"
    )
  )

  row_nodes <- dplyr::filter(binary_layout$nodes, .data$side == "row")
  column_nodes <- dplyr::filter(binary_layout$nodes, .data$side == "column")
  expect_equal(
    row_nodes$interaction_size,
    as.numeric(rowSums(interaction_matrix > 0))
  )
  expect_equal(
    column_nodes$interaction_size,
    as.numeric(colSums(interaction_matrix > 0))
  )
  expect_finite_layout_coordinates(binary_layout)
})

test_that("binary edge order is independent of polygon area rounding", {
  interaction_matrix <- matrix(
    1,
    nrow = 4,
    ncol = 3,
    dimnames = list(c("d", "b", "c", "a"), c("z", "x", "y"))
  )

  for (gap in c(0, 0.1, 0.5)) {
    layout <- layout_bipartite(
      interaction_matrix,
      interaction = "binary",
      gap = gap
    )
    edges <- layout$interactions

    expect_identical(edges$edge_id, paste0("edge-", seq_len(12)))
    expect_identical(edges$row, rep(rownames(interaction_matrix), 3))
    expect_identical(edges$column, rep(colnames(interaction_matrix), each = 4))
  }
})

test_that("metadata keys are unique and value names are side-prefixed", {
  interaction_matrix <- matrix(
    c(1, 2, 3, 4),
    nrow = 2,
    byrow = TRUE,
    dimnames = list(c("1", "2"), c("10", "20"))
  )
  row_metadata <- tibble::tibble(
    taxon = 1:2,
    group = c("r-a", "r-b"),
    side = c("left-a", "left-b")
  )
  column_metadata <- tibble::tibble(
    taxon = factor(c("10", "20")),
    group = c("c-a", "c-b"),
    side = c("right-a", "right-b")
  )
  layout <- layout_bipartite(
    interaction_matrix,
    metadata_row = row_metadata,
    metadata_column = column_metadata,
    metadata_row_key = "taxon",
    metadata_column_key = "taxon"
  )

  expect_true(all(
    c(
      "row_group",
      "row_side",
      "column_group",
      "column_side"
    ) %in%
      names(layout$nodes)
  ))
  expect_true(all(
    c(
      "row_group",
      "row_side",
      "column_group",
      "column_side"
    ) %in%
      names(layout$interactions)
  ))
  row_nodes <- dplyr::filter(layout$nodes, .data$side == "row")
  column_nodes <- dplyr::filter(layout$nodes, .data$side == "column")
  expect_equal(row_nodes$row_group, c("r-a", "r-b"))
  expect_equal(column_nodes$column_group, c("c-a", "c-b"))
  expect_false(anyNA(layout$interactions$row_group))
  expect_false(anyNA(layout$interactions$column_group))
  edge_vertex_counts <- table(layout$interactions$edge_id)
  expect_true(all(edge_vertex_counts == 4L))

  duplicate_metadata <- tibble::tibble(
    taxon = c(1, 1),
    group = c("a", "b")
  )
  expect_error(
    layout_bipartite(
      interaction_matrix,
      metadata_row = duplicate_metadata,
      metadata_row_key = "taxon"
    ),
    "must be unique; duplicated IDs: 1",
    fixed = TRUE
  )
  expect_error(
    layout_bipartite(
      interaction_matrix,
      metadata_row = row_metadata,
      metadata_row_key = "missing"
    ),
    "must contain the key column `missing`",
    fixed = TRUE
  )

  duplicate_names <- data.frame(
    taxon = c("1", "2"),
    group = c("a", "b"),
    group = c("x", "y"),
    check.names = FALSE
  )
  expect_error(
    layout_bipartite(
      interaction_matrix,
      metadata_row = duplicate_names,
      metadata_row_key = "taxon"
    ),
    "must have unique column names; duplicated names: group",
    fixed = TRUE
  )
})

test_that("long data selectors also define default metadata keys", {
  interaction_data <- tibble::tibble(
    host = c("r1", "r2"),
    partner = c("c1", "c2"),
    abundance = c(2, 3)
  )
  row_metadata <- tibble::tibble(
    host = c("r1", "r2"),
    guild = c("a", "b")
  )
  column_metadata <- tibble::tibble(
    partner = c("c1", "c2"),
    guild = c("x", "y")
  )
  layout <- layout_bipartite(
    interaction_data,
    row = host,
    column = partner,
    weight = abundance,
    metadata_row = row_metadata,
    metadata_column = column_metadata
  )

  expect_true(all(c("row_guild", "column_guild") %in% names(layout$nodes)))
  expect_false(anyNA(layout$interactions$row_guild))
  expect_false(anyNA(layout$interactions$column_guild))
})

test_that("tree mismatches error by default and support explicit drop", {
  skip_if_not_installed("ape")

  interaction_matrix <- rbind(
    layout_contract_matrix(),
    row_extra = c(1, 1)
  )
  row_tree <- ape::read.tree(
    text = "((row_c:1,row_a:1):1,row_b:1);"
  )

  expect_error(
    quiet_layout_bipartite(interaction_matrix, row_tree = row_tree),
    "data IDs absent from tree: row_extra",
    fixed = TRUE
  )

  layout <- quiet_layout_bipartite(
    interaction_matrix,
    row_tree = row_tree,
    unmatched_data = "drop"
  )
  expected_order <- suppressMessages(suppressWarnings(get_tip_order(row_tree)))
  row_nodes <- dplyr::filter(layout$nodes, .data$side == "row")

  expect_equal(layout$params$row_order, expected_order)
  expect_equal(row_nodes$id, expected_order)
  expect_false("row_extra" %in% layout$interactions$row)
  expect_equal(layout$params$unmatched_data, "drop")
})

test_that("tree-only tips can be pruned explicitly", {
  skip_if_not_installed("ape")

  interaction_matrix <- layout_contract_matrix()
  row_tree <- ape::read.tree(
    text = "(((row_c:1,row_a:1):1,row_b:1):1,tree_only:1);"
  )
  row_tree_copy <- row_tree

  expect_error(
    quiet_layout_bipartite(interaction_matrix, row_tree = row_tree),
    "tree tips absent from data: tree_only",
    fixed = TRUE
  )

  layout <- quiet_layout_bipartite(
    interaction_matrix,
    row_tree = row_tree,
    unmatched_tree = "prune"
  )
  expected_order <- suppressMessages(suppressWarnings(
    get_tip_order(layout$trees$row$validated)
  ))

  expect_identical(row_tree, row_tree_copy)
  expect_identical(layout$trees$row$original, row_tree)
  expect_setequal(
    layout$trees$row$validated$tip.label,
    rownames(interaction_matrix)
  )
  expect_equal(layout$params$row_order, expected_order)
  expect_equal(layout$params$unmatched_tree, "prune")
  expect_equal(layout$tree_links$side, rep("row", nrow(interaction_matrix)))
  expect_setequal(layout$tree_links$id, rownames(interaction_matrix))
  expect_finite_layout_coordinates(layout)
})

test_that("tree tip order controls node order and binary alignment", {
  skip_if_not_installed("ape")

  interaction_matrix <- layout_contract_matrix()
  row_tree <- ape::read.tree(
    text = "((row_c:1,row_a:1):1,row_b:1);"
  )
  expected_order <- suppressMessages(suppressWarnings(get_tip_order(row_tree)))

  abundance_layout <- quiet_layout_bipartite(
    interaction_matrix,
    row_tree = row_tree
  )
  binary_layout <- quiet_layout_bipartite(
    interaction_matrix,
    row_tree = row_tree,
    interaction = "binary"
  )

  for (layout in list(abundance_layout, binary_layout)) {
    row_nodes <- dplyr::filter(layout$nodes, .data$side == "row")
    expect_equal(layout$params$row_order, expected_order)
    expect_equal(row_nodes$id, expected_order)
    expect_true(all(diff(row_nodes$y) > 0))
    expect_true(all(
      c(
        "original",
        "validated",
        "geometry"
      ) %in%
        names(layout$trees$row)
    ))
  }

  binary_nodes <- dplyr::filter(
    binary_layout$nodes,
    .data$side == "row"
  ) |>
    dplyr::transmute(id = .data$id, node_y = .data$y)
  tree_data <- suppressMessages(as.data.frame(
    binary_layout$trees$row$geometry$data
  ))
  binary_tips <- tree_data[
    !is.na(tree_data$isTip) & tree_data$isTip,
    c("label", "y"),
    drop = FALSE
  ] |>
    dplyr::transmute(id = as.character(.data$label), tree_y = .data$y)
  aligned <- dplyr::left_join(binary_nodes, binary_tips, by = "id")

  expect_equal(aligned$node_y, aligned$tree_y)
  expect_equal(nrow(abundance_layout$tree_links), nrow(interaction_matrix))
  expect_equal(nrow(binary_layout$tree_links), 0L)
})

test_that("binary tree alignment preserves origin translation", {
  skip_if_not_installed("ape")

  interaction_matrix <- layout_contract_matrix()
  row_tree <- ape::read.tree(
    text = "((row_c:1,row_a:1):1,row_b:1);"
  )
  baseline <- quiet_layout_bipartite(
    interaction_matrix,
    row_tree = row_tree,
    interaction = "binary",
    x0 = 3,
    y0 = -7,
    gap = 0.4
  )
  translated <- quiet_layout_bipartite(
    interaction_matrix,
    row_tree = row_tree,
    interaction = "binary",
    x0 = 103,
    y0 = 18,
    gap = 0.4
  )

  for (coordinate in c("xmin", "xmax", "x")) {
    expect_equal(
      translated$nodes[[coordinate]],
      baseline$nodes[[coordinate]] + 100
    )
  }
  for (coordinate in c("ymin", "ymax", "y")) {
    expect_equal(
      translated$nodes[[coordinate]],
      baseline$nodes[[coordinate]] + 25
    )
  }
  baseline_edges <- dplyr::arrange(baseline$interactions, .data$edge_id)
  translated_edges <- dplyr::arrange(translated$interactions, .data$edge_id)
  expect_equal(translated_edges$x, baseline_edges$x + 100)
  expect_equal(translated_edges$xend, baseline_edges$xend + 100)
  expect_equal(translated_edges$y, baseline_edges$y + 25)
  expect_equal(translated_edges$yend, baseline_edges$yend + 25)

  baseline_tree <- as.data.frame(baseline$trees$row$geometry$data)
  translated_tree <- as.data.frame(translated$trees$row$geometry$data)
  expect_equal(translated_tree$y, baseline_tree$y + 25)
})

test_that("origin translation preserves geometry and area", {
  interaction_matrix <- layout_contract_matrix()
  baseline <- layout_bipartite(
    interaction_matrix,
    x0 = 3,
    y0 = -7,
    gap = 0.4
  )
  translated <- layout_bipartite(
    interaction_matrix,
    x0 = 103,
    y0 = 18,
    gap = 0.4
  )

  for (coordinate in c("xmin", "xmax", "x")) {
    expect_equal(
      translated$nodes[[coordinate]],
      baseline$nodes[[coordinate]] + 100
    )
  }
  for (coordinate in c("ymin", "ymax", "y")) {
    expect_equal(
      translated$nodes[[coordinate]],
      baseline$nodes[[coordinate]] + 25
    )
  }
  expect_equal(translated$interactions$x, baseline$interactions$x + 100)
  expect_equal(translated$interactions$y, baseline$interactions$y + 25)
  expect_equal(translated$interactions$area, baseline$interactions$area)
  expect_equal(translated$params$origin, c(x = 103, y = 18))
  expect_equal(min(translated$nodes$ymin), 18)
  expect_equal(
    min(translated$nodes$xmin[translated$nodes$side == "row"]),
    103
  )
  expect_finite_layout_coordinates(translated)

  expect_error(layout_bipartite(interaction_matrix, x0 = Inf), "finite")
  expect_error(layout_bipartite(interaction_matrix, gap = -1), "at least 0")
})

test_that("equal-height and singleton layouts keep finite centered boxes", {
  equal_matrix <- matrix(
    c(1, 2, 3, 4),
    nrow = 2,
    dimnames = list(c("r1", "r2"), c("c1", "c2"))
  )
  row_singleton <- matrix(
    c(1, 2),
    nrow = 1,
    dimnames = list("r1", c("c1", "c2"))
  )
  column_singleton <- matrix(
    c(1, 2),
    ncol = 1,
    dimnames = list(c("r1", "r2"), "c1")
  )
  one_by_one <- matrix(
    2,
    nrow = 1,
    dimnames = list("r1", "c1")
  )

  equal_layout <- layout_bipartite(
    equal_matrix,
    gap = 0.5,
    adjust_box_height = TRUE
  )
  row_layout <- layout_bipartite(
    row_singleton,
    gap = 0.5,
    adjust_box_height = TRUE
  )
  column_layout <- layout_bipartite(
    column_singleton,
    gap = 0.5,
    adjust_box_height = TRUE
  )
  one_layout <- layout_bipartite(
    one_by_one,
    gap = 0.5,
    adjust_box_height = TRUE
  )

  expect_equal(equal_layout$params$gap, c(row = 0.5, column = 0.5))
  expect_equal(row_layout$params$gap, c(row = 0.5, column = 0))
  expect_equal(column_layout$params$gap, c(row = 0, column = 0.5))
  expect_equal(one_layout$params$gap, c(row = 0.5, column = 0.5))

  side_center <- function(nodes, side) {
    side_nodes <- dplyr::filter(nodes, .data$side == side)
    (min(side_nodes$ymin) + max(side_nodes$ymax)) / 2
  }
  expect_equal(
    side_center(row_layout$nodes, "row"),
    side_center(row_layout$nodes, "column")
  )
  expect_equal(
    side_center(column_layout$nodes, "row"),
    side_center(column_layout$nodes, "column")
  )
  expect_equal(
    side_center(one_layout$nodes, "row"),
    side_center(one_layout$nodes, "column")
  )

  for (layout in list(
    equal_layout,
    row_layout,
    column_layout,
    one_layout
  )) {
    expect_true(nrow(layout$interactions) > 0L)
    expect_finite_layout_coordinates(layout)
    expect_true(all(is.finite(layout$params$gap)))
  }
})

test_that("tree pruning supports a singleton validated tree", {
  skip_if_not_installed("ape")

  interaction_matrix <- matrix(
    c(1, 2),
    nrow = 1,
    dimnames = list("r1", c("c1", "c2"))
  )
  row_tree <- ape::read.tree(text = "(r1:1,extra:1);")

  for (mode in c("abundance", "binary")) {
    layout <- layout_bipartite(
      interaction_matrix,
      interaction = mode,
      row_tree = row_tree,
      unmatched_tree = "prune",
      gap = 0.5
    )

    expect_equal(layout$params$row_order, "r1")
    expect_equal(layout$trees$row$validated$tip.label, "r1")
    expect_s3_class(layout$trees$row$geometry, "ggplot")

    tree_data <- layout$trees$row$geometry$data
    tree_tip <- tree_data[tree_data$isTip, , drop = FALSE]
    row_node <- layout$nodes[
      layout$nodes$side == "row" & layout$nodes$id == "r1",
      ,
      drop = FALSE
    ]
    expect_equal(tree_tip$label, "r1")
    expect_equal(tree_tip$y, row_node$y)
    expect_true(all(is.finite(tree_data$x)))
    expect_true(all(is.finite(tree_data$y)))
    expect_no_error(ggplot2::ggplot_build(layout$trees$row$geometry))

    row_links <- layout$tree_links[layout$tree_links$side == "row", ]
    if (mode == "abundance") {
      expect_equal(nrow(row_links), 1L)
    } else {
      expect_equal(nrow(row_links), 0L)
    }
  }
})

test_that("tree reconciliation reports an empty data-tree intersection", {
  skip_if_not_installed("ape")

  interaction_matrix <- matrix(
    c(1, 2),
    nrow = 1,
    dimnames = list("r1", c("c1", "c2"))
  )
  disjoint_tree <- ape::read.tree(text = "(tree_a:1,tree_b:1);")

  expect_error(
    layout_bipartite(
      interaction_matrix,
      row_tree = disjoint_tree,
      unmatched_data = "drop",
      unmatched_tree = "prune"
    ),
    "No row IDs remain after tree reconciliation"
  )
})

test_that("singleton tree geometry preserves an explicit zero branch", {
  skip_if_not_installed("ape")

  interaction_matrix <- matrix(
    c(1, 2),
    nrow = 1,
    dimnames = list("r1", c("c1", "c2"))
  )
  row_tree <- ape::read.tree(text = "(r1:0,extra:1);")
  layout <- layout_bipartite(
    interaction_matrix,
    interaction = "binary",
    row_tree = row_tree,
    unmatched_tree = "prune"
  )

  expect_equal(layout$trees$row$validated$edge.length, 0)
  expect_equal(layout$trees$row$geometry$data$x, c(0, 0))
  expect_no_error(ggplot2::ggplot_build(layout$trees$row$geometry))
})

test_that("layout construction does not modify its inputs", {
  skip_if_not_installed("ape")

  interaction_matrix <- layout_contract_matrix()
  interaction_data <- tibble::tibble(
    host = c("row_a", "row_b", "row_c"),
    partner = c("column_a", "column_b", "column_a"),
    abundance = c(2, 3, 1)
  )
  row_metadata <- tibble::tibble(
    host = c("row_a", "row_b", "row_c"),
    group = c("a", "b", "c")
  )
  row_tree <- ape::read.tree(
    text = "(((row_c:1,row_a:1):1,row_b:1):1,tree_only:1);"
  )
  matrix_copy <- interaction_matrix
  data_copy <- interaction_data
  metadata_copy <- row_metadata
  tree_copy <- row_tree

  layout_bipartite(interaction_matrix)
  quiet_layout_bipartite(
    interaction_data,
    row = host,
    column = partner,
    weight = abundance,
    metadata_row = row_metadata,
    row_tree = row_tree,
    unmatched_tree = "prune"
  )

  expect_identical(interaction_matrix, matrix_copy)
  expect_identical(interaction_data, data_copy)
  expect_identical(row_metadata, metadata_copy)
  expect_identical(row_tree, tree_copy)
})
