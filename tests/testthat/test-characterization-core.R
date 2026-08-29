test_that("named matrices have a stable long representation", {
  interaction_matrix <- fixture_bipartite_matrix()

  interaction_long <- to_longer(interaction_matrix, .rowname = "row")

  expect_s3_class(interaction_long, "tbl_df")
  expect_named(interaction_long, c("row", "column", "interaction"))
  expect_equal(nrow(interaction_long), 5L)
  expect_equal(sum(interaction_long$interaction), sum(interaction_matrix))
  expect_false(any(interaction_long$interaction == 0))
  expect_equal(levels(interaction_long$row), rownames(interaction_matrix))
  expect_equal(levels(interaction_long$column), colnames(interaction_matrix))
})

test_that("network summaries agree with matrix margins", {
  interaction_matrix <- fixture_bipartite_matrix()

  network <- bipartite_network(interaction_matrix)

  expect_named(network, c("rsf", "csf", "ilf"))
  expect_equal(network$rsf$row, rownames(interaction_matrix))
  expect_equal(
    network$rsf$interaction_size,
    unname(rowSums(interaction_matrix))
  )
  expect_equal(network$csf$column, colnames(interaction_matrix))
  expect_equal(
    network$csf$interaction_size,
    unname(colSums(interaction_matrix))
  )
  expect_equal(sum(network$ilf$interaction), sum(interaction_matrix))
})

test_that("global parameters preserve origins and compatibility aliases", {
  interaction_matrix <- fixture_bipartite_matrix()

  params <- calc_global_params(
    .mat = interaction_matrix,
    .x0 = 2,
    .y0 = 5,
    .gap = 0.25,
    .adjust_box_height = FALSE
  )

  expect_equal(params$row_box, c(2, 5))
  expect_equal(params$column_box[[2]], 5)
  expect_equal(params$column_box[[1]], 2 + params$width - params$box_width)
  expect_identical(params$box1, params$row_box)
  expect_identical(params$box2, params$column_box)
  expect_identical(params$gap1, params$gap_row)
  expect_identical(params$gap2, params$gap_column)
})

test_that("coordinate construction returns one box per named node", {
  coords <- fixture_bipartite_coords(
    x0 = 2,
    y0 = 5,
    with_metadata = TRUE
  )

  expect_named(
    coords,
    c("row_box", "column_box", "interaction_coords", "box1", "box2")
  )
  expect_equal(nrow(coords$row_box), 3L)
  expect_equal(nrow(coords$column_box), 2L)
  expect_equal(sort(coords$row_box$row), c("row_a", "row_b", "row_c"))
  expect_equal(
    sort(coords$column_box$column),
    c("column_a", "column_b")
  )
  expect_gte(min(coords$row_box$xmin), 2)
  expect_gte(min(coords$row_box$ymin), 5)
  expect_true(all(is.finite(coords$interaction_coords$x)))
  expect_true(all(is.finite(coords$interaction_coords$y)))
  expect_equal(
    table(coords$interaction_coords$row, coords$interaction_coords$column)[
      table(
        coords$interaction_coords$row,
        coords$interaction_coords$column
      ) > 0
    ],
    rep(4L, 5)
  )
  expect_true("row_group" %in% names(coords$row_box))
  expect_true("column_group" %in% names(coords$column_box))
})

test_that("box labels use the outer edge and vertical centre", {
  coords <- fixture_bipartite_coords()

  row_labels <- compute_box_label_coords(
    .box = coords$row_box,
    .by = "row",
    .side = "row"
  )
  column_labels <- compute_box_label_coords(
    .box = coords$column_box,
    .by = "column",
    .side = "column"
  )

  expect_equal(row_labels$x, coords$row_box$xmin)
  expect_equal(
    row_labels$y,
    (coords$row_box$ymin + coords$row_box$ymax) / 2
  )
  expect_equal(column_labels$x, coords$column_box$xmax)
  expect_equal(
    column_labels$y,
    (coords$column_box$ymin + coords$column_box$ymax) / 2
  )
})
