test_that("row and column metadata can use independent fill scales", {
  skip_if_not_installed("ggnewscale")

  interaction_data <- fixture_bipartite_long()
  row_metadata <- tibble::tibble(
    row_id = c("row_a", "row_b", "row_c"),
    row_group = c("Unknown", "Host A", "Unknown")
  )
  column_metadata <- tibble::tibble(
    column_id = c("column_a", "column_b"),
    column_group = c("Unknown", "Guild B")
  )
  row_palette <- c("Unknown" = "#4E79A7", "Host A" = "#F28E2B")
  column_palette <- c("Unknown" = "#59A14F", "Guild B" = "#B07AA1")

  plot <- ggplot2::ggplot(
    interaction_data,
    ggplot2::aes(row = row, column = column, count = count)
  ) +
    geom_bipnet_interaction(
      ggplot2::aes(fill = ggplot2::after_stat(column_group)),
      column_nm = "column_id",
      metadata_column = column_metadata,
      show.legend = FALSE
    ) +
    geom_bipnet_box(
      ggplot2::aes(fill = ggplot2::after_stat(column_group)),
      type = "column",
      column_nm = "column_id",
      metadata_column = column_metadata
    ) +
    ggplot2::scale_fill_manual(
      name = "Symbiont guild",
      values = column_palette
    ) +
    ggnewscale::new_scale_fill() +
    geom_bipnet_box(
      ggplot2::aes(fill = ggplot2::after_stat(row_group)),
      type = "row",
      row_nm = "row_id",
      metadata_row = row_metadata
    ) +
    ggplot2::scale_fill_manual(
      name = "Host family",
      values = row_palette
    )

  built <- ggplot2::ggplot_build(plot)
  scale_aesthetics <- vapply(
    built$plot$scales$scales,
    function(scale) paste(scale$aesthetics, collapse = ","),
    character(1)
  )

  expect_true(any(grepl("^fill_ggnewscale", scale_aesthetics)))
  expect_true("fill" %in% scale_aesthetics)

  interaction_layer <- built$data[[1L]]
  column_layer <- built$data[[2L]]
  row_layer <- built$data[[3L]]
  column_fill <- grep("^fill_ggnewscale", names(column_layer), value = TRUE)
  interaction_fill <- grep(
    "^fill_ggnewscale",
    names(interaction_layer),
    value = TRUE
  )

  expect_length(interaction_fill, 1L)
  expect_length(column_fill, 1L)
  expect_true("fill" %in% names(row_layer))
  expect_setequal(
    unique(column_layer[[column_fill]]),
    unname(column_palette)
  )
  expect_setequal(unique(row_layer$fill), unname(row_palette))
  expect_false(row_palette[["Unknown"]] %in% column_layer[[column_fill]])
  expect_false(column_palette[["Unknown"]] %in% row_layer$fill)
})
