test_that("composer reuses a layout and needs no patchwork for one panel", {
  layout <- layout_bipartite(fixture_bipartite_matrix(), gap = 0.25)
  composed <- plot_bipartite(layout)

  expect_s3_class(composed, "ggbipartite_plot")
  expect_identical(composed$layout, layout)
  expect_named(
    composed$components,
    c(
      "row_tree",
      "row_link",
      "network",
      "column_link",
      "column_tree"
    )
  )
  expect_null(composed$components$row_tree)
  expect_null(composed$components$row_link)
  expect_s3_class(composed$components$network, "ggplot")
  expect_null(composed$components$column_link)
  expect_null(composed$components$column_tree)
  expect_equal(composed$components$network$coordinates$ratio, 1)
  network_data <- ggplot2::ggplot_build(composed$components$network)$data
  expect_length(network_data, 3L)
  expect_true(all(vapply(network_data, nrow, integer(1)) > 0L))

  local_mocked_bindings(
    is_patchwork_installed = function() FALSE,
    .package = "ggbipartite"
  )
  expect_s3_class(as_patchwork(composed), "ggplot")
})

test_that("composer normalizes raw long data exactly once", {
  interaction_long <- fixture_bipartite_long()
  original_layout <- layout_bipartite
  call_count <- 0L
  local_mocked_bindings(
    layout_bipartite = function(...) {
      call_count <<- call_count + 1L
      original_layout(...)
    },
    .package = "ggbipartite"
  )

  composed <- plot_bipartite(
    interaction_long,
    row = row,
    column = column,
    weight = count,
    gap = 0.25
  )

  expect_equal(call_count, 1L)
  expect_s3_class(composed$layout, "bipartite_layout")
  expect_equal(composed$layout$params$interaction, "abundance")
})

test_that("abundance composer represents zero, one, and two trees", {
  skip_if_not_installed("ape")
  row_tree <- ape::read.tree(
    text = "(row_a:1,(row_b:1,row_c:1):0.5);"
  )
  column_tree <- ape::read.tree(
    text = "(column_a:1,column_b:1);"
  )

  one_tree <- plot_bipartite(
    fixture_bipartite_matrix(),
    row_tree = row_tree,
    gap = 0.25
  )
  expect_s3_class(one_tree$components$row_tree, "ggplot")
  expect_s3_class(one_tree$components$row_link, "ggplot")
  expect_null(one_tree$components$column_link)
  expect_null(one_tree$components$column_tree)
  expect_setequal(
    one_tree$layout$tree_links$id,
    row_tree$tip.label
  )

  two_trees <- plot_bipartite(
    fixture_bipartite_matrix(),
    row_tree = row_tree,
    column_tree = column_tree,
    gap = 0.25
  )
  expect_true(
    all(vapply(two_trees$components, inherits, logical(1), "ggplot"))
  )
  expect_setequal(
    unique(two_trees$layout$tree_links$side),
    c("row", "column")
  )
})

test_that("binary composer uses direct tree alignment without link panels", {
  skip_if_not_installed("ape")
  row_tree <- ape::read.tree(
    text = "(row_a:1,(row_b:1,row_c:1):0.5);"
  )
  column_tree <- ape::read.tree(
    text = "(column_a:1,column_b:1);"
  )

  composed <- plot_bipartite(
    fixture_bipartite_matrix(),
    interaction = "binary",
    row_tree = row_tree,
    column_tree = column_tree,
    gap = 0.25
  )

  expect_s3_class(composed$components$row_tree, "ggplot")
  expect_null(composed$components$row_link)
  expect_s3_class(composed$components$network, "ggplot")
  expect_null(composed$components$column_link)
  expect_s3_class(composed$components$column_tree, "ggplot")
  expect_equal(nrow(composed$layout$tree_links), 0L)
  network_data <- ggplot2::ggplot_build(composed$components$network)$data
  expect_equal(nrow(network_data[[1L]]), 5L)

  row_tree_data <- as.data.frame(
    composed$layout$trees$row$geometry$data
  )
  row_tips <- row_tree_data |>
    dplyr::filter(.data$isTip) |>
    dplyr::transmute(id = as.character(.data$label), tree_y = .data$y)
  row_nodes <- composed$layout$nodes |>
    dplyr::filter(.data$side == "row") |>
    dplyr::transmute(id = .data$id, node_y = .data$y)
  aligned <- dplyr::left_join(row_nodes, row_tips, by = "id")
  expect_equal(aligned$node_y, aligned$tree_y)
})

test_that("tree compositions share vertical ranges without changing layouts", {
  skip_if_not_installed("ape")
  trees <- list(
    row = ape::read.tree(text = "(row_a:1,(row_b:1,row_c:1):0.5);"),
    column = ape::read.tree(text = "(column_a:1,column_b:1);")
  )

  for (mode in c("abundance", "binary")) {
    for (sides in list("row", "column", c("row", "column"))) {
      selected_trees <- trees[sides]
      layout <- layout_bipartite(
        fixture_bipartite_matrix(),
        interaction = mode,
        row_tree = selected_trees$row,
        column_tree = selected_trees$column,
        y0 = 7,
        gap = 0.25
      )
      original_tree_data <- lapply(sides, function(side) {
        as.data.frame(layout$trees[[side]]$geometry$data)
      })
      composed <- plot_bipartite(layout)
      active <- Filter(Negate(is.null), composed$components)
      panel_ranges <- lapply(active, function(component) {
        ggplot2::ggplot_build(component)$layout$panel_params[[1L]]$y.range
      })

      expect_identical(composed$layout, layout)
      for (panel in names(active)) {
        expect_equal(
          panel_ranges[[panel]],
          panel_ranges$network,
          info = paste(mode, paste(sides, collapse = "+"), panel)
        )
        expect_null(active[[panel]]$coordinates$ratio)
      }
      expect_lte(panel_ranges$network[1L], min(layout$nodes$ymin))
      expect_gte(panel_ranges$network[2L], max(layout$nodes$ymax))
      for (index in seq_along(sides)) {
        panel <- paste0(sides[index], "_tree")
        expect_identical(
          as.data.frame(composed$components[[panel]]$data),
          original_tree_data[[index]]
        )
        expect_identical(
          as.data.frame(layout$trees[[sides[index]]]$geometry$data),
          original_tree_data[[index]]
        )
      }
    }
  }
})

test_that("pruned singleton trees share the network vertical range", {
  skip_if_not_installed("ape")
  interaction_matrix <- matrix(
    c(1, 2),
    nrow = 1,
    dimnames = list("row_a", c("column_a", "column_b"))
  )
  row_tree <- ape::read.tree(text = "(row_a:1,tree_only:1);")

  for (mode in c("abundance", "binary")) {
    layout <- layout_bipartite(
      interaction_matrix,
      interaction = mode,
      row_tree = row_tree,
      unmatched_tree = "prune",
      gap = 0.25
    )
    composed <- plot_bipartite(layout)
    active <- Filter(Negate(is.null), composed$components)
    panel_ranges <- lapply(active, function(component) {
      ggplot2::ggplot_build(component)$layout$panel_params[[1L]]$y.range
    })

    expect_identical(composed$layout, layout)
    expect_equal(composed$layout$trees$row$validated$tip.label, "row_a")
    for (panel in names(active)) {
      expect_true(all(is.finite(panel_ranges[[panel]])))
      expect_equal(panel_ranges[[panel]], panel_ranges$network)
    }
  }
})

measure_composer_panels <- function(composed, width, height) {
  # Open the device before constructing grobs to avoid an implicit Rplots.pdf.
  grDevices::pdf(NULL, width = width, height = height)
  on.exit(grDevices::dev.off(), add = TRUE)
  grid::grid.newpage()
  grid::grid.draw(patchwork::patchworkGrob(as_patchwork(composed)))
  grid::grid.force()

  viewports <- grid::grid.ls(
    viewports = TRUE,
    grobs = FALSE,
    print = FALSE
  )
  # Fixed-aspect panels can be nested; include their inner panel viewport.
  panels <- viewports$name[grepl("^panel(-[0-9]+)?[.]", viewports$name)]
  bounds <- vapply(panels, function(panel) {
    grid::seekViewport(panel)
    lower <- grid::deviceLoc(
      grid::unit(0, "npc"),
      grid::unit(0, "npc"),
      valueOnly = TRUE
    )
    upper <- grid::deviceLoc(
      grid::unit(1, "npc"),
      grid::unit(1, "npc"),
      valueOnly = TRUE
    )
    grid::upViewport(0)
    c(left = lower$x, right = upper$x, bottom = lower$y, top = upper$y)
  }, numeric(4))
  bounds <- t(bounds)
  bounds[order(bounds[, "left"]), , drop = FALSE]
}

test_that("tree endpoints stay aligned at wide and tall device sizes", {
  skip_if_not_installed("ape")
  skip_if_not_installed("patchwork")
  row_tree <- ape::read.tree(text = "(row_a:1,(row_b:1,row_c:1):0.5);")
  column_tree <- ape::read.tree(text = "(column_a:1,column_b:1);")

  for (mode in c("abundance", "binary")) {
    composed <- plot_bipartite(
      fixture_bipartite_matrix(),
      interaction = mode,
      row_tree = row_tree,
      column_tree = column_tree,
      gap = 0.25
    )
    active <- Filter(Negate(is.null), composed$components)
    panel_ranges <- lapply(active, function(component) {
      ggplot2::ggplot_build(component)$layout$panel_params[[1L]]$y.range
    })
    network_x_range <- ggplot2::ggplot_build(active$network)$layout$
      panel_params[[1L]]$x.range

    for (size in list(c(12, 3), c(6, 9))) {
      bounds <- measure_composer_panels(composed, size[1L], size[2L])
      expect_equal(nrow(bounds), length(active))
      rownames(bounds) <- names(active)
      expect_equal(
        unname(bounds[, "top"] - bounds[, "bottom"]),
        rep(
          bounds["network", "top"] - bounds["network", "bottom"],
          length(active)
        )
      )
      if (mode == "abundance") {
        expect_equal(bounds["row_link", "right"], bounds["network", "left"])
        expect_equal(bounds["column_link", "left"], bounds["network", "right"])
        box_x_range <- range(c(
          composed$layout$nodes$xmin,
          composed$layout$nodes$xmax
        ))
        expect_equal(
          (box_x_range - network_x_range[1L]) / diff(network_x_range),
          c(0, 1)
        )
      }
      transform_y <- function(y, panel) {
        panel_range <- panel_ranges[[panel]]
        bounds[panel, "bottom"] +
          (y - panel_range[1L]) / diff(panel_range) *
            (bounds[panel, "top"] - bounds[panel, "bottom"])
      }

      for (side in c("row", "column")) {
        layout <- composed$layout
        nodes <- layout$nodes[layout$nodes$side == side, ]
        tree_data <- as.data.frame(layout$trees[[side]]$geometry$data)
        tips <- tree_data[tree_data$isTip, ]
        tip_y <- tips$y[match(nodes$id, tips$label)]
        tree_panel <- paste0(side, "_tree")

        if (mode == "binary") {
          expect_equal(
            transform_y(tip_y, tree_panel),
            transform_y(nodes$y, "network")
          )
        } else {
          links <- layout$tree_links[layout$tree_links$side == side, ]
          links <- links[match(nodes$id, links$id), ]
          if (side == "row") {
            tree_y <- links$y
            node_y <- links$yend
          } else {
            tree_y <- links$yend
            node_y <- links$y
          }
          link_panel <- paste0(side, "_link")
          expect_equal(
            transform_y(tip_y, tree_panel),
            transform_y(tree_y, link_panel)
          )
          expect_equal(
            transform_y(nodes$y, "network"),
            transform_y(node_y, link_panel)
          )
        }
      }
    }
  }
})

test_that("components remain editable and can be reassembled", {
  skip_if_not_installed("ape")
  skip_if_not_installed("patchwork")
  row_tree <- ape::read.tree(
    text = "(row_a:1,(row_b:1,row_c:1):0.5);"
  )
  composed <- plot_bipartite(
    fixture_bipartite_matrix(),
    row_tree = row_tree,
    gap = 0.25
  )

  original_layout <- composed$layout
  composed$components$network <- composed$components$network +
    ggplot2::theme(panel.background = ggplot2::element_rect(fill = "ivory"))
  assembled <- as_patchwork(
    composed,
    widths = c(row_tree = 2, row_link = 0.75, network = 5)
  )

  expect_s3_class(assembled, "patchwork")
  expect_identical(composed$layout, original_layout)
})

test_that("multiple panels report an actionable patchwork error", {
  skip_if_not_installed("ape")
  row_tree <- ape::read.tree(
    text = "(row_a:1,(row_b:1,row_c:1):0.5);"
  )
  composed <- plot_bipartite(
    fixture_bipartite_matrix(),
    row_tree = row_tree,
    gap = 0.25
  )
  local_mocked_bindings(
    is_patchwork_installed = function() FALSE,
    .package = "ggbipartite"
  )

  expect_error(
    as_patchwork(composed),
    "install.packages.*patchwork"
  )
})

test_that("precomputed layouts reject layout-changing arguments", {
  layout <- layout_bipartite(fixture_bipartite_matrix(), gap = 0.25)

  expect_error(
    plot_bipartite(layout, gap = 1),
    "must not be supplied"
  )
  expect_error(
    plot_bipartite(layout, row = row),
    "must not be supplied"
  )
  expect_error(
    plot_bipartite(layout, widths = c(network = 0)),
    "positive finite"
  )
  expect_error(
    plot_bipartite(layout, widths = c(1, 2)),
    "one value per active component"
  )
})
