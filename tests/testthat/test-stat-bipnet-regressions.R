test_that("interaction groups are unique for adversarial identifier pairs", {
  interactions <- data.frame(
    row = c("a", "ab"),
    column = c("bc", "c"),
    count = c(1, 2)
  )

  result <- ggbipartite:::StatBipnet$compute_panel(
    data = interactions,
    scales = NULL,
    type = "interaction",
    gap = 0
  )

  edge_groups <- unique(result[c("row", "column", "group")])
  vertex_counts <- stats::aggregate(
    seq_len(nrow(result)),
    result[c("row", "column")],
    length
  )

  expect_equal(nrow(edge_groups), 2L)
  expect_equal(length(unique(edge_groups$group)), 2L)
  expect_equal(vertex_counts$x, c(4L, 4L))
})

test_that("duplicate long-format cells fail before reshaping", {
  interactions <- data.frame(
    row = c("A", "A", "B"),
    column = c("X", "X", "Y"),
    count = c(1, 2, 3)
  )

  expect_error(
    ggbipartite:::StatBipnet$compute_panel(
      data = interactions,
      scales = NULL,
      type = "interaction",
      gap = 0
    ),
    "duplicate row-column cells: \\(`A`, `X`\\)"
  )
})

test_that("tip extraction excludes internal nodes before matching labels", {
  tree_data <- data.frame(
    label = c("A", "A", "B"),
    y = c(1, 99, 2),
    isTip = c(TRUE, FALSE, TRUE)
  )

  result <- ggbipartite:::extract_tip_positions(
    tree_data,
    arg_name = "tip_positions_row"
  )

  expect_equal(result$label, c("A", "B"))
  expect_equal(result$y, c(1, 2))
})

test_that("the legacy stat rejects ambiguous interaction inputs", {
  compute_interactions <- function(interactions) {
    ggbipartite:::StatBipnet$compute_panel(
      data = interactions,
      scales = NULL,
      type = "interaction",
      gap = 0
    )
  }

  expect_error(
    compute_interactions(data.frame(
      row = c("A", "B"),
      column = c("X", "Y"),
      count = c(1, Inf)
    )),
    "only finite"
  )
  expect_error(
    compute_interactions(data.frame(
      row = c("A", "B"),
      column = c("X", "Y"),
      count = c(1, -1)
    )),
    "negative values"
  )
  expect_error(
    compute_interactions(data.frame(
      row = c("A", "B"),
      column = c("X", "Y"),
      count = c(1, 0)
    )),
    "zero-sum row IDs: B; zero-sum column IDs: Y"
  )
  expect_error(
    compute_interactions(data.frame(
      row = c("A", ""),
      column = c("X", "Y"),
      count = c(1, 1)
    )),
    "missing or empty IDs"
  )
})
