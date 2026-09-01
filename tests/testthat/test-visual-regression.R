expect_fixed_linux_snapshot <- function(title, figure, filename) {
  testthat::announce_snapshot_file(filename)
  testthat::skip_if_not_installed("vdiffr")

  old_not_cran <- Sys.getenv("NOT_CRAN", unset = NA_character_)
  on.exit(
    {
      if (is.na(old_not_cran)) {
        Sys.unsetenv("NOT_CRAN")
      } else {
        Sys.setenv(NOT_CRAN = old_not_cran)
      }
    },
    add = TRUE
  )

  if (Sys.getenv("RUN_VDIFFR") != "true") {
    Sys.setenv(NOT_CRAN = "false")
  }

  vdiffr::expect_doppelganger(title, figure, cran = FALSE)
}

test_that("abundance grouping remains distinct for adversarial IDs", {
  interaction_matrix <- matrix(
    c(2, 1, 1, 3),
    nrow = 2,
    byrow = TRUE,
    dimnames = list(c("a", "ab"), c("bc", "c"))
  )
  row_metadata <- data.frame(
    row_id = c("a", "ab"),
    guild = c("first", "second")
  )
  layout <- layout_bipartite(
    interaction_matrix,
    metadata_row = row_metadata,
    metadata_row_key = "row_id",
    gap = 0.5
  )
  plot <- ggplot2::ggplot() +
    geom_bipnet_interaction(layout = layout, alpha = 0.45) +
    geom_bipnet_box(
      layout = layout,
      type = "row",
      mapping = ggplot2::aes(fill = row_guild)
    ) +
    geom_bipnet_box(
      layout = layout,
      type = "column",
      fill = "grey80"
    ) +
    ggplot2::coord_fixed() +
    ggplot2::theme_void()

  expect_fixed_linux_snapshot(
    "adversarial abundance IDs",
    plot,
    "adversarial-abundance-ids.svg"
  )
})

test_that("binary nodes align directly with tree tips", {
  skip_if_not_installed("ape")
  skip_if_not_installed("patchwork")

  interaction_matrix <- matrix(
    c(
      1,
      0,
      1,
      0,
      1,
      1,
      1,
      1,
      0
    ),
    nrow = 3,
    byrow = TRUE,
    dimnames = list(
      c("host_a", "host_b", "host_c"),
      c("otu_a", "otu_b", "otu_c")
    )
  )
  row_tree <- ape::read.tree(
    text = "((host_a:1,host_b:1):1,host_c:2);"
  )
  column_tree <- ape::read.tree(
    text = "(otu_a:1,(otu_b:1,otu_c:1):1);"
  )
  layout <- layout_bipartite(
    interaction_matrix,
    interaction = "binary",
    row_tree = row_tree,
    column_tree = column_tree,
    gap = 0.5
  )

  expect_fixed_linux_snapshot(
    "binary tree tip alignment",
    as_patchwork(plot_bipartite(layout)),
    "binary-tree-tip-alignment.svg"
  )
})
