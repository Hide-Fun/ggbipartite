test_that("duplicate metadata keys are rejected before joining", {
  interaction_matrix <- matrix(
    c(1, 2, 3, 4),
    nrow = 2,
    dimnames = list(c("1", "2"), c("10", "20"))
  )

  expect_error(
    construct_bn_coordination(
      .mat = interaction_matrix,
      .row = "taxon_id",
      .column = "taxon_id",
      .metadata_row = tibble::tibble(taxon_id = c(1, 1)),
      .adjust_box_height = FALSE
    ),
    "`.metadata_row$taxon_id` must be unique; duplicated IDs: 1.",
    fixed = TRUE
  )

  expect_error(
    construct_bn_coordination(
      .mat = interaction_matrix,
      .row = "taxon_id",
      .column = "taxon_id",
      .metadata_column = tibble::tibble(taxon_id = factor(c("10", "10"))),
      .adjust_box_height = FALSE
    ),
    "`.metadata_column$taxon_id` must be unique; duplicated IDs: 10.",
    fixed = TRUE
  )
})

test_that("numeric and factor metadata IDs are normalized to character", {
  interaction_matrix <- matrix(
    c(1, 2, 3, 4),
    nrow = 2,
    dimnames = list(c("1", "2"), c("10", "20"))
  )
  row_metadata <- tibble::tibble(
    taxon_id = 1:2,
    row_group = c("r-a", "r-b")
  )
  column_metadata <- tibble::tibble(
    taxon_id = factor(c("10", "20")),
    column_group = c("c-a", "c-b")
  )

  coordinates <- construct_bn_coordination(
    .mat = interaction_matrix,
    .row = "taxon_id",
    .column = "taxon_id",
    .metadata_row = row_metadata,
    .metadata_column = column_metadata,
    .adjust_box_height = FALSE
  )

  expect_type(coordinates$row_box$row, "character")
  expect_type(coordinates$column_box$column, "character")
  expect_type(coordinates$interaction_coords$row, "character")
  expect_type(coordinates$interaction_coords$column, "character")
  expect_equal(coordinates$row_box$row_group, c("r-a", "r-b"))
  expect_equal(coordinates$column_box$column_group, c("c-a", "c-b"))
  expect_false(anyNA(coordinates$interaction_coords$row_group))
  expect_false(anyNA(coordinates$interaction_coords$column_group))
  expect_equal(nrow(coordinates$interaction_coords), 16L)
})

test_that("custom origins translate every coordinate without changing area", {
  interaction_matrix <- matrix(
    c(2, 1, 3, 4),
    nrow = 2,
    byrow = TRUE,
    dimnames = list(c("r1", "r2"), c("c1", "c2"))
  )
  baseline <- construct_bn_coordination(
    .mat = interaction_matrix,
    .row = "row",
    .column = "column",
    .adjust_box_height = FALSE
  )
  translated <- construct_bn_coordination(
    .mat = interaction_matrix,
    .row = "row",
    .column = "column",
    .x0 = 100,
    .y0 = -25,
    .adjust_box_height = FALSE
  )

  for (box_name in c("row_box", "column_box")) {
    expect_equal(translated[[box_name]]$xmin, baseline[[box_name]]$xmin + 100)
    expect_equal(translated[[box_name]]$xmax, baseline[[box_name]]$xmax + 100)
    expect_equal(translated[[box_name]]$ymin, baseline[[box_name]]$ymin - 25)
    expect_equal(translated[[box_name]]$ymax, baseline[[box_name]]$ymax - 25)
  }

  expect_equal(
    translated$interaction_coords$x,
    baseline$interaction_coords$x + 100
  )
  expect_equal(
    translated$interaction_coords$y,
    baseline$interaction_coords$y - 25
  )
  expect_equal(
    translated$interaction_coords$area,
    baseline$interaction_coords$area
  )
})

test_that("interaction coordinates use explicit keys and optional box sizes", {
  row_box <- tibble::tibble(
    row = c("r1", "r2"),
    xmin = 0,
    xmax = 0.5,
    ymin = c(0, 2),
    ymax = c(2, 3),
    source = "box"
  )
  column_box <- tibble::tibble(
    column = c("c1", "c2"),
    xmin = 2,
    xmax = 2.5,
    ymin = c(0, 2),
    ymax = c(2, 3),
    source = "box"
  )
  interaction_cells <- tibble::tibble(
    row = c("r1", "r1", "r2"),
    column = c("c1", "c2", "c1"),
    interaction = c(1, 1, 1),
    source = "cell"
  )

  coordinates <- compute_interaction_coords(
    .row_box = row_box,
    .column_box = column_box,
    .interation_cell = interaction_cells
  )

  expect_equal(nrow(coordinates), 12L)
  expect_equal(
    nrow(dplyr::distinct(coordinates, .data$row, .data$column)),
    3L
  )
  expect_true(all(is.finite(coordinates$x)))
  expect_true(all(is.finite(coordinates$y)))
  expect_true(all(is.finite(coordinates$area)))
})
