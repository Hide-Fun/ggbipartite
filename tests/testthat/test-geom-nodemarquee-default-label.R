test_that("geom_nodemarquee maps and builds internal labels by default", {
  skip_if_not_installed("ape")
  skip_if_not_installed("marquee")

  tree <- ape::read.tree(text = "((A:1,B:1):1,C:2);")
  tree$node.label <- c("90", "100")

  plot <- ggtree::ggtree(tree) + geom_nodemarquee()
  expect_no_error(built <- ggplot2::ggplot_build(plot))

  marquee_data <- built$data[[length(built$data)]]
  expect_equal(nrow(marquee_data), tree$Nnode)
  expect_setequal(marquee_data$label, tree$node.label)
})

test_that("marquee tree label guide segments accept explicit colours", {
  skip_if_not_installed("ape")
  skip_if_not_installed("marquee")

  tree <- ape::read.tree(text = "((A:1,B:1):1,C:2);")
  tree$node.label <- c("90", "100")

  tip_plot <- ggtree::ggtree(tree) +
    geom_tipmarquee(align = TRUE, segment_colour = "grey45")
  node_plot <- ggtree::ggtree(tree) +
    geom_nodemarquee(segment = TRUE, segment_colour = "grey45")

  tip_built <- ggplot2::ggplot_build(tip_plot)
  node_built <- ggplot2::ggplot_build(node_plot)

  tip_segment_data <- tip_built$data[[length(tip_built$data) - 1L]]
  node_segment_data <- node_built$data[[length(node_built$data) - 1L]]

  expect_true(all(tip_segment_data$colour == "grey45"))
  expect_true(all(node_segment_data$colour == "grey45"))
})
