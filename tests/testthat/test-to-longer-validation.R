test_that("to_longer preserves default and custom row identifier columns", {
  interaction_matrix <- matrix(
    c(1, 0, 2, 3),
    nrow = 2,
    byrow = TRUE,
    dimnames = list(c("r1", "r2"), c("c1", "c2"))
  )

  default_result <- to_longer(interaction_matrix)
  custom_result <- to_longer(interaction_matrix, .rowname = "species")

  expect_named(default_result, c("row", "column", "interaction"))
  expect_equal(levels(default_result$row), c("r1", "r2"))
  expect_equal(levels(default_result$column), c("c1", "c2"))
  expect_named(custom_result, c("species", "column", "interaction"))
  expect_equal(as.character(custom_result$species), c("r1", "r2", "r2"))
  expect_equal(levels(custom_result$species), c("r1", "r2"))
})

test_that("to_longer requires an explicit row identifier column", {
  interaction_matrix <- matrix(
    1,
    dimnames = list("r1", "c1")
  )

  expect_error(
    to_longer(interaction_matrix, .rowname = NULL),
    "row identifiers are required"
  )
})

test_that("to_longer rejects missing, empty, and duplicate dimension names", {
  unnamed_matrix <- matrix(1:4, nrow = 2)
  missing_column_names <- unnamed_matrix
  rownames(missing_column_names) <- c("r1", "r2")

  empty_row_name <- unnamed_matrix
  dimnames(empty_row_name) <- list(c("", "r2"), c("c1", "c2"))

  duplicate_column_name <- unnamed_matrix
  dimnames(duplicate_column_name) <- list(
    c("r1", "r2"),
    c("c1", "c1")
  )

  expect_error(to_longer(unnamed_matrix), "row names; none were supplied")
  expect_error(
    to_longer(missing_column_names),
    "column names; none were supplied"
  )
  expect_error(to_longer(empty_row_name), "empty row names at positions: 1")
  expect_error(
    to_longer(duplicate_column_name),
    "duplicate column names: c1"
  )
})

test_that("to_longer rejects invalid weights and zero-sum nodes", {
  make_matrix <- function(values) {
    matrix(
      values,
      nrow = 2,
      byrow = TRUE,
      dimnames = list(c("r1", "r2"), c("c1", "c2"))
    )
  }

  expect_error(
    to_longer(make_matrix(c(1, NA, 2, 3))),
    "only finite"
  )
  expect_error(
    to_longer(make_matrix(c(1, -1, 2, 3))),
    "negative interaction"
  )
  expect_error(
    to_longer(make_matrix(c(1, 0, 0, 0))),
    "zero-sum row IDs: r2; zero-sum column IDs: c2"
  )
})
