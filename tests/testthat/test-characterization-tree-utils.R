test_that("tree utilities preserve tip identity and create finite links", {
  tree_plot <- fixture_row_tree_plot()
  coords <- fixture_bipartite_coords()

  tip_order <- get_tip_order(tree_plot)
  links <- create_link(
    box = coords$row_box,
    ggtree = tree_plot,
    side = "row",
    x = 0,
    xend = 1
  )

  expect_setequal(tip_order, rownames(fixture_bipartite_matrix()))
  expect_equal(nrow(links), 3L)
  expect_true(all(c("row", "y1", "x", "xend", "y2") %in% names(links)))
  expect_true(all(is.finite(links$y1)))
  expect_true(all(is.finite(links$y2)))
})

test_that("get_yrange extracts a finite increasing panel range", {
  plot <- ggplot2::ggplot(
    data.frame(x = 1:3, y = c(2, 4, 8)),
    ggplot2::aes(x = x, y = y)
  ) +
    ggplot2::geom_point()

  y_range <- get_yrange(plot)

  expect_type(y_range, "double")
  expect_length(y_range, 2L)
  expect_true(all(is.finite(y_range)))
  expect_lt(y_range[[1]], y_range[[2]])
  expect_lte(y_range[[1]], 2)
  expect_gte(y_range[[2]], 8)
})

test_that("label formatters retain support labels and style taxa", {
  styled <- style_sciname(c("Cremastra aphylla", "90"))
  supports <- format_node_support(c("90/96", "55", NA_character_))

  expect_match(styled[[1]], "Cremastra")
  expect_match(styled[[1]], "aphylla")
  expect_identical(styled[[2]], "90")
  expect_identical(supports[[1]], "90/96")
  expect_identical(supports[[2]], "")
  expect_true(is.na(supports[[3]]))
})
