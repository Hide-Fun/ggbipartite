test_that("abundance layers build finite box and polygon coordinates", {
  interaction_long <- fixture_bipartite_long()

  plot <- ggplot2::ggplot(
    interaction_long,
    ggplot2::aes(row = row, column = column, count = count)
  ) +
    geom_bipnet_box(type = "row", gap = 0.25) +
    geom_bipnet_box(type = "column", gap = 0.25) +
    geom_bipnet_interaction(gap = 0.25)

  layer_data <- ggplot2::ggplot_build(plot)$data

  expect_length(layer_data, 3L)
  expect_equal(nrow(layer_data[[1]]), 3L)
  expect_equal(nrow(layer_data[[2]]), 2L)
  expect_equal(nrow(layer_data[[3]]), 20L)
  expect_true(
    all(c("xmin", "xmax", "ymin", "ymax") %in% names(layer_data[[1]]))
  )
  expect_true(all(c("x", "y", "group") %in% names(layer_data[[3]])))
  expect_true(all(is.finite(layer_data[[3]]$x)))
  expect_true(all(is.finite(layer_data[[3]]$y)))
  expect_equal(as.vector(table(layer_data[[3]]$group)), rep(4L, 5))
})

test_that("binary interaction layers return one segment per nonzero cell", {
  interaction_long <- fixture_bipartite_long()

  plot <- ggplot2::ggplot(
    interaction_long,
    ggplot2::aes(row = row, column = column, count = count)
  ) +
    geom_bipnet_interaction(
      interaction_type = "binary",
      gap = 0.25
    )

  layer_data <- ggplot2::ggplot_build(plot)$data[[1]]

  expect_equal(nrow(layer_data), 5L)
  expect_true(all(c("x", "y", "xend", "yend") %in% names(layer_data)))
  expect_true(all(is.finite(layer_data$x)))
  expect_true(all(is.finite(layer_data$y)))
  expect_true(all(is.finite(layer_data$xend)))
  expect_true(all(is.finite(layer_data$yend)))
})

test_that("stat compatibility aliases select the same box sides", {
  interaction_long <- fixture_bipartite_long()
  mapping <- ggplot2::aes(row = row, column = column, count = count)

  row_plot <- ggplot2::ggplot() +
    stat_bipnet(
      mapping = mapping,
      data = interaction_long,
      type = "row",
      geom = "rect",
      gap = 0.25
    )
  box1_plot <- ggplot2::ggplot() +
    stat_bipnet(
      mapping = mapping,
      data = interaction_long,
      type = "box1",
      geom = "rect",
      gap = 0.25
    )

  row_data <- ggplot2::ggplot_build(row_plot)$data[[1]]
  box1_data <- ggplot2::ggplot_build(box1_plot)$data[[1]]

  expect_equal(
    row_data[c("row", "xmin", "xmax", "ymin", "ymax")],
    box1_data[c("row", "xmin", "xmax", "ymin", "ymax")]
  )
})
