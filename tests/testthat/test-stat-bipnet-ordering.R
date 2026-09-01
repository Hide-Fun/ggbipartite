make_ordered_interactions <- function() {
  interaction_matrix <- matrix(
    c(2, 0, 7, 3, 5, 1),
    nrow = 3,
    byrow = TRUE,
    dimnames = list(c("row_c", "row_a", "row_b"), c("otu_2", "otu_1"))
  )
  interaction_data <- to_longer(interaction_matrix, .rowname = "row") |>
    dplyr::rename(count = interaction)

  list(matrix = interaction_matrix, data = interaction_data)
}

test_that("raw geoms retain explicit factor order for boxes and labels", {
  inputs <- make_ordered_interactions()
  expected <- construct_bn_coordination(
    .mat = inputs$matrix,
    .row = "row",
    .column = "column",
    .gap = 0.75
  )
  plot <- ggplot2::ggplot(
    inputs$data,
    ggplot2::aes(row = row, column = column, count = count)
  ) +
    geom_bipnet_box(type = "row", gap = 0.75) +
    geom_bipnet_box(type = "column", gap = 0.75) +
    geom_bipnet_interaction(gap = 0.75)
  built <- ggplot2::ggplot_build(plot)$data

  for (i in seq_along(c("row", "column"))) {
    side <- c("row", "column")[[i]]
    boxes <- expected[[paste0(side, "_box")]]
    actual <- built[[i]][match(boxes[[side]], built[[i]][[side]]), ]
    coordinates <- c("xmin", "xmax", "ymin", "ymax")
    labels <- compute_box_label_coords(boxes, .by = side, .side = side)
    label_y <- labels$y[match(boxes[[side]], labels[[side]])]

    expect_equal(actual[coordinates], as.data.frame(boxes[coordinates]))
    expect_equal(label_y, (actual$ymin + actual$ymax) / 2)
  }

  expected_edges <- expected$interaction_coords |>
    dplyr::arrange(row, column, x, y) |>
    dplyr::select(row, column, x, y)
  actual_edges <- built[[3]] |>
    dplyr::arrange(row, column, x, y) |>
    dplyr::select(row, column, x, y)
  expect_equal(actual_edges, as.data.frame(expected_edges))
})

test_that("factor levels govern layout independently of observation order", {
  inputs <- make_ordered_interactions()
  shuffled <- inputs$data[rev(seq_len(nrow(inputs$data))), ]
  shuffled$row <- factor(
    shuffled$row,
    levels = c("unused_row", rownames(inputs$matrix)),
    ordered = TRUE
  )
  shuffled$column <- factor(
    shuffled$column,
    levels = c(colnames(inputs$matrix), "unused_column")
  )

  for (side in c("row", "column", "interaction")) {
    expected <- ggbipartite:::StatBipnet$compute_panel(
      inputs$data,
      scales = NULL,
      type = side,
      gap = 0.75
    )
    actual <- ggbipartite:::StatBipnet$compute_panel(
      shuffled,
      scales = NULL,
      type = side,
      gap = 0.75
    )
    expect_equal(actual, expected)
  }
})

test_that("character IDs retain the legacy sorted matrix order", {
  inputs <- make_ordered_interactions()
  character_data <- inputs$data |>
    dplyr::mutate(
      row = as.character(row),
      column = as.character(column)
    )
  sorted_matrix <- inputs$matrix[
    sort(rownames(inputs$matrix)),
    sort(colnames(inputs$matrix)),
    drop = FALSE
  ]
  expected <- construct_bn_coordination(
    .mat = sorted_matrix,
    .row = "row",
    .column = "column",
    .gap = 0.75
  )

  for (side in c("row", "column")) {
    actual <- ggbipartite:::StatBipnet$compute_panel(
      character_data,
      scales = NULL,
      type = side,
      gap = 0.75
    )
    boxes <- expected[[paste0(side, "_box")]]
    expect_equal(actual[names(boxes)], boxes)
  }
})

test_that("each partition can specify factor order independently", {
  inputs <- make_ordered_interactions()

  for (factor_side in c("row", "column")) {
    data <- inputs$data
    character_side <- setdiff(c("row", "column"), factor_side)
    data[[character_side]] <- as.character(data[[character_side]])
    row_order <- rownames(inputs$matrix)
    column_order <- colnames(inputs$matrix)
    if (character_side == "row") {
      row_order <- sort(row_order)
    } else {
      column_order <- sort(column_order)
    }
    expected <- construct_bn_coordination(
      .mat = inputs$matrix[row_order, column_order, drop = FALSE],
      .row = "row",
      .column = "column",
      .gap = 0.75
    )

    for (side in c("row", "column")) {
      actual <- ggbipartite:::StatBipnet$compute_panel(
        data,
        scales = NULL,
        type = side,
        gap = 0.75
      )
      boxes <- expected[[paste0(side, "_box")]]
      expect_equal(actual[names(boxes)], boxes)
    }
  }
})

test_that("unused factor levels do not add singleton nodes", {
  data <- data.frame(
    row = factor("row_a", levels = c("unused_row", "row_a")),
    column = factor("otu_1", levels = c("otu_1", "unused_column")),
    count = 3
  )

  for (side in c("row", "column")) {
    actual <- ggbipartite:::StatBipnet$compute_panel(
      data,
      scales = NULL,
      type = side,
      gap = 0.75
    )
    expect_equal(nrow(actual), 1L)
    expect_equal(actual$ymin, 0)
    expect_equal(actual$ymax, 3)
  }
})

test_that("tree-link endpoints use the same centres as factor-ordered geoms", {
  skip_if_not_installed("ggtree")
  inputs <- make_ordered_interactions()
  coordinates <- construct_bn_coordination(
    .mat = inputs$matrix,
    .row = "row",
    .column = "column",
    .gap = 0.75
  )
  trees <- list(
    row = ape::read.tree(text = "(row_c:1,(row_a:1,row_b:1):1);"),
    column = ape::read.tree(text = "(otu_2:1,otu_1:1);")
  )

  for (side in c("row", "column")) {
    boxes <- coordinates[[paste0(side, "_box")]]
    tree <- adjust_tree(
      .phylo = trees[[side]],
      .box = boxes,
      .adjust = 1,
      .tree_position = if (side == "row") "left" else "right"
    )
    links <- create_link(boxes, tree, side = side)
    plot <- ggplot2::ggplot(
      inputs$data,
      ggplot2::aes(row = row, column = column, count = count)
    ) +
      geom_bipnet_box(type = side, gap = 0.75)
    actual <- ggplot2::ggplot_build(plot)$data[[1]]
    actual <- actual[match(links[[side]], actual[[side]]), ]
    tips <- as.data.frame(tree$data)
    tips <- tips[tips$isTip, ]

    expect_equal(links$y1, (actual$ymin + actual$ymax) / 2)
    expect_equal(links$y2, tips$y[match(links[[side]], tips$label)])
  }
})
