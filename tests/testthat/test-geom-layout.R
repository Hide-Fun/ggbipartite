make_geom_test_layout <- function(interaction = "abundance") {
  ggbipartite:::layout_bipartite(
    fixture_bipartite_matrix(),
    interaction = interaction,
    gap = 0.25
  )
}

test_that("layout node layers use identity coordinates for one side", {
  layout <- make_geom_test_layout()

  box_layer <- geom_bipnet_box(
    layout = layout,
    type = "row",
    mapping = ggplot2::aes(fill = id)
  )
  point_layer <- geom_bipnet_point(
    layout = layout,
    type = "column"
  )

  expect_s3_class(box_layer$stat, "StatIdentity")
  expect_s3_class(point_layer$stat, "StatIdentity")
  expect_false(box_layer$inherit.aes)
  expect_false(point_layer$inherit.aes)
  expect_equal(box_layer$data$side, rep("row", 3L))
  expect_equal(point_layer$data$side, rep("column", 2L))
  expect_setequal(
    names(box_layer$mapping),
    c("xmin", "xmax", "ymin", "ymax", "fill")
  )

  plot <- ggplot2::ggplot() + box_layer + point_layer
  built <- ggplot2::ggplot_build(plot)$data

  expect_equal(nrow(built[[1]]), 3L)
  expect_equal(nrow(built[[2]]), 2L)
  expect_true(all(is.finite(built[[1]]$xmin)))
  expect_true(all(is.finite(built[[1]]$ymax)))
  expect_true(all(is.finite(built[[2]]$x)))
  expect_true(all(is.finite(built[[2]]$y)))
})

test_that("layout mappings override only requested default aesthetics", {
  layout <- make_geom_test_layout()

  layer <- geom_bipnet_box(
    layout = layout,
    type = "row",
    mapping = ggplot2::aes(xmin = xmax, colour = id)
  )

  expect_identical(rlang::as_label(layer$mapping$xmin), "xmax")
  expect_identical(rlang::as_label(layer$mapping$xmax), "xmax")
  expect_identical(rlang::as_label(layer$mapping$ymin), "ymin")
  expect_identical(rlang::as_label(layer$mapping$ymax), "ymax")
  expect_identical(rlang::as_label(layer$mapping$colour), "id")
})

test_that("layout layers do not inherit plot data or aesthetics", {
  layout <- make_geom_test_layout()
  external_data <- data.frame(unrelated = 1)

  plot <- ggplot2::ggplot(
    external_data,
    ggplot2::aes(x = missing_coordinate)
  ) +
    geom_bipnet_box(layout = layout, type = "row")

  expect_no_error(built <- ggplot2::ggplot_build(plot))
  expect_equal(nrow(built$data[[1]]), 3L)
})

test_that("abundance layout interactions draw one polygon per edge", {
  layout <- make_geom_test_layout("abundance")

  layer <- geom_bipnet_interaction(layout = layout)
  built <- ggplot2::ggplot_build(
    ggplot2::ggplot() + layer
  )$data[[1]]

  expect_s3_class(layer$stat, "StatIdentity")
  expect_false(layer$inherit.aes)
  expect_equal(nrow(built), 20L)
  expect_equal(length(unique(built$group)), 5L)
  expect_equal(as.vector(table(built$group)), rep(4L, 5L))
  expect_true(all(is.finite(built$x)))
  expect_true(all(is.finite(built$y)))
})

test_that("binary layout interactions select segment geometry by default", {
  layout <- make_geom_test_layout("binary")

  layer <- geom_bipnet_interaction(layout = layout)
  built <- ggplot2::ggplot_build(
    ggplot2::ggplot() + layer
  )$data[[1]]

  expect_s3_class(layer$geom, "GeomBipnetInteractionBinary")
  expect_equal(nrow(built), 5L)
  expect_true(all(c("x", "y", "xend", "yend") %in% names(built)))
  expect_true(all(is.finite(built$x)))
  expect_true(all(is.finite(built$y)))
  expect_true(all(is.finite(built$xend)))
  expect_true(all(is.finite(built$yend)))
})

test_that("layout layer conflicts are rejected explicitly", {
  abundance_layout <- make_geom_test_layout("abundance")
  binary_layout <- make_geom_test_layout("binary")

  expect_error(
    geom_bipnet_box(layout = abundance_layout),
    "`type` must be either `\"row\"` or `\"column\"`"
  )
  expect_error(
    geom_bipnet_point(layout = abundance_layout, type = "interaction"),
    "`type` must be either `\"row\"` or `\"column\"`"
  )
  expect_error(
    geom_bipnet_box(
      data = data.frame(id = "external"),
      layout = abundance_layout,
      type = "row"
    ),
    "`data` must be `NULL`"
  )
  expect_error(
    geom_bipnet_box(
      layout = abundance_layout,
      type = "row",
      stat = "bipnet"
    ),
    "`stat` must be `\"identity\"`"
  )
  expect_error(
    geom_bipnet_box(
      layout = abundance_layout,
      type = "row",
      tip_positions_row = data.frame(label = "row_a", y = 1)
    ),
    "Tip positions are already included"
  )
  expect_error(
    geom_bipnet_interaction(
      layout = binary_layout,
      interaction_type = "abundance"
    ),
    "must match `layout\\$params\\$interaction`"
  )
  expect_error(
    geom_bipnet_interaction(
      layout = abundance_layout,
      type = "row"
    ),
    "`type` must be `\"interaction\"`"
  )
})
