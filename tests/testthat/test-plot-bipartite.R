test_that("composer reuses a layout and needs no patchwork for one panel", {
  layout <- layout_bipartite(fixture_bipartite_matrix(), gap = 0.25)
  composed <- plot_bipartite(layout)

  expect_s3_class(composed, "ggbipartite_plot")
  expect_identical(composed$layout, layout)
  expect_named(
    composed$components,
    c(
      "row_tree",
      "row_link",
      "network",
      "column_link",
      "column_tree"
    )
  )
  expect_null(composed$components$row_tree)
  expect_null(composed$components$row_link)
  expect_s3_class(composed$components$network, "ggplot")
  expect_null(composed$components$column_link)
  expect_null(composed$components$column_tree)
  network_data <- ggplot2::ggplot_build(composed$components$network)$data
  expect_length(network_data, 3L)
  expect_true(all(vapply(network_data, nrow, integer(1)) > 0L))

  local_mocked_bindings(
    is_patchwork_installed = function() FALSE,
    .package = "ggbipartite"
  )
  expect_s3_class(as_patchwork(composed), "ggplot")
})

test_that("composer normalizes raw long data exactly once", {
  interaction_long <- fixture_bipartite_long()
  original_layout <- layout_bipartite
  call_count <- 0L
  local_mocked_bindings(
    layout_bipartite = function(...) {
      call_count <<- call_count + 1L
      original_layout(...)
    },
    .package = "ggbipartite"
  )

  composed <- plot_bipartite(
    interaction_long,
    row = row,
    column = column,
    weight = count,
    gap = 0.25
  )

  expect_equal(call_count, 1L)
  expect_s3_class(composed$layout, "bipartite_layout")
  expect_equal(composed$layout$params$interaction, "abundance")
})

test_that("abundance composer represents zero, one, and two trees", {
  skip_if_not_installed("ape")
  row_tree <- ape::read.tree(
    text = "(row_a:1,(row_b:1,row_c:1):0.5);"
  )
  column_tree <- ape::read.tree(
    text = "(column_a:1,column_b:1);"
  )

  one_tree <- plot_bipartite(
    fixture_bipartite_matrix(),
    row_tree = row_tree,
    gap = 0.25
  )
  expect_s3_class(one_tree$components$row_tree, "ggplot")
  expect_s3_class(one_tree$components$row_link, "ggplot")
  expect_null(one_tree$components$column_link)
  expect_null(one_tree$components$column_tree)
  expect_setequal(
    one_tree$layout$tree_links$id,
    row_tree$tip.label
  )

  two_trees <- plot_bipartite(
    fixture_bipartite_matrix(),
    row_tree = row_tree,
    column_tree = column_tree,
    gap = 0.25
  )
  expect_true(
    all(vapply(two_trees$components, inherits, logical(1), "ggplot"))
  )
  expect_setequal(
    unique(two_trees$layout$tree_links$side),
    c("row", "column")
  )
})

test_that("binary composer uses direct tree alignment without link panels", {
  skip_if_not_installed("ape")
  row_tree <- ape::read.tree(
    text = "(row_a:1,(row_b:1,row_c:1):0.5);"
  )
  column_tree <- ape::read.tree(
    text = "(column_a:1,column_b:1);"
  )

  composed <- plot_bipartite(
    fixture_bipartite_matrix(),
    interaction = "binary",
    row_tree = row_tree,
    column_tree = column_tree,
    gap = 0.25
  )

  expect_s3_class(composed$components$row_tree, "ggplot")
  expect_null(composed$components$row_link)
  expect_s3_class(composed$components$network, "ggplot")
  expect_null(composed$components$column_link)
  expect_s3_class(composed$components$column_tree, "ggplot")
  expect_equal(nrow(composed$layout$tree_links), 0L)
  network_data <- ggplot2::ggplot_build(composed$components$network)$data
  expect_equal(nrow(network_data[[1L]]), 5L)

  row_tree_data <- as.data.frame(
    composed$layout$trees$row$geometry$data
  )
  row_tips <- row_tree_data |>
    dplyr::filter(.data$isTip) |>
    dplyr::transmute(id = as.character(.data$label), tree_y = .data$y)
  row_nodes <- composed$layout$nodes |>
    dplyr::filter(.data$side == "row") |>
    dplyr::transmute(id = .data$id, node_y = .data$y)
  aligned <- dplyr::left_join(row_nodes, row_tips, by = "id")
  expect_equal(aligned$node_y, aligned$tree_y)
})

test_that("components remain editable and can be reassembled", {
  skip_if_not_installed("ape")
  skip_if_not_installed("patchwork")
  row_tree <- ape::read.tree(
    text = "(row_a:1,(row_b:1,row_c:1):0.5);"
  )
  composed <- plot_bipartite(
    fixture_bipartite_matrix(),
    row_tree = row_tree,
    gap = 0.25
  )

  original_layout <- composed$layout
  composed$components$network <- composed$components$network +
    ggplot2::theme(panel.background = ggplot2::element_rect(fill = "ivory"))
  assembled <- as_patchwork(
    composed,
    widths = c(row_tree = 2, row_link = 0.75, network = 5)
  )

  expect_s3_class(assembled, "patchwork")
  expect_identical(composed$layout, original_layout)
})

test_that("multiple panels report an actionable patchwork error", {
  skip_if_not_installed("ape")
  row_tree <- ape::read.tree(
    text = "(row_a:1,(row_b:1,row_c:1):0.5);"
  )
  composed <- plot_bipartite(
    fixture_bipartite_matrix(),
    row_tree = row_tree,
    gap = 0.25
  )
  local_mocked_bindings(
    is_patchwork_installed = function() FALSE,
    .package = "ggbipartite"
  )

  expect_error(
    as_patchwork(composed),
    "install.packages.*patchwork"
  )
})

test_that("precomputed layouts reject layout-changing arguments", {
  layout <- layout_bipartite(fixture_bipartite_matrix(), gap = 0.25)

  expect_error(
    plot_bipartite(layout, gap = 1),
    "must not be supplied"
  )
  expect_error(
    plot_bipartite(layout, row = row),
    "must not be supplied"
  )
  expect_error(
    plot_bipartite(layout, widths = c(network = 0)),
    "positive finite"
  )
  expect_error(
    plot_bipartite(layout, widths = c(1, 2)),
    "one value per active component"
  )
})
