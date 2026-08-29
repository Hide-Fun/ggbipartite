test_that("equal-height layouts preserve finite baseline gaps", {
  gaps <- adjust_box_height(
    .interaction = 10,
    .nrow = 2,
    .ncol = 2,
    .gap = 0.25
  )

  expect_equal(gaps$.gap_row, 0.25)
  expect_equal(gaps$.gap_column, 0.25)
  expect_true(all(is.finite(unlist(gaps))))

  params <- calc_global_params(
    .mat = matrix(1:4, nrow = 2),
    .gap = 0.25,
    .adjust_box_height = TRUE
  )
  expect_equal(params$gap_row, 0.25)
  expect_equal(params$gap_column, 0.25)
})

test_that("singleton layouts return finite gaps and coordinates", {
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

  row_gaps <- adjust_box_height(
    .interaction = sum(row_singleton),
    .nrow = nrow(row_singleton),
    .ncol = ncol(row_singleton),
    .gap = 0.5
  )
  column_gaps <- adjust_box_height(
    .interaction = sum(column_singleton),
    .nrow = nrow(column_singleton),
    .ncol = ncol(column_singleton),
    .gap = 0.5
  )

  expect_equal(row_gaps$.gap_column, 0)
  expect_equal(column_gaps$.gap_row, 0)
  expect_true(all(is.finite(unlist(row_gaps))))
  expect_true(all(is.finite(unlist(column_gaps))))

  row_layout <- construct_bn_coordination(
    .mat = row_singleton,
    .row = NULL,
    .column = NULL,
    .gap = 0.5,
    .adjust_box_height = TRUE
  )
  column_layout <- construct_bn_coordination(
    .mat = column_singleton,
    .row = NULL,
    .column = NULL,
    .gap = 0.5,
    .adjust_box_height = TRUE
  )

  coordinate_columns <- c("xmin", "xmax", "ymin", "ymax")
  expect_true(all(is.finite(unlist(
    row_layout$row_box[coordinate_columns]
  ))))
  expect_true(all(is.finite(unlist(
    row_layout$column_box[coordinate_columns]
  ))))
  expect_true(all(is.finite(row_layout$interaction_coords$x)))
  expect_true(all(is.finite(row_layout$interaction_coords$y)))

  expect_true(all(is.finite(unlist(
    column_layout$row_box[coordinate_columns]
  ))))
  expect_true(all(is.finite(unlist(
    column_layout$column_box[coordinate_columns]
  ))))
  expect_true(all(is.finite(column_layout$interaction_coords$x)))
  expect_true(all(is.finite(column_layout$interaction_coords$y)))

  box_height <- function(box) {
    max(box$ymax) - min(box$ymin)
  }
  expect_equal(
    box_height(row_layout$row_box),
    box_height(row_layout$column_box)
  )
  expect_equal(
    box_height(column_layout$row_box),
    box_height(column_layout$column_box)
  )
})
