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
