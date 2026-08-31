fixture_bipartite_matrix <- function() {
  matrix(
    c(
      2,
      0,
      1,
      3,
      2,
      1
    ),
    nrow = 3,
    byrow = TRUE,
    dimnames = list(
      c("row_a", "row_b", "row_c"),
      c("column_a", "column_b")
    )
  )
}

fixture_bipartite_long <- function() {
  fixture_bipartite_matrix() |>
    ggbipartite::to_longer(.rowname = "row") |>
    dplyr::rename(count = interaction)
}

fixture_bipartite_metadata <- function() {
  list(
    row = tibble::tibble(
      row_id = c("row_a", "row_b", "row_c"),
      row_group = c("alpha", "beta", "beta")
    ),
    column = tibble::tibble(
      column_id = c("column_a", "column_b"),
      column_group = c("gamma", "delta")
    )
  )
}

fixture_bipartite_coords <- function(
  x0 = 0,
  y0 = 0,
  gap = 0.25,
  with_metadata = FALSE
) {
  metadata <- fixture_bipartite_metadata()

  ggbipartite::construct_bn_coordination(
    .mat = fixture_bipartite_matrix(),
    .row = "row_id",
    .column = "column_id",
    .metadata_row = if (with_metadata) metadata$row else NULL,
    .metadata_column = if (with_metadata) metadata$column else NULL,
    .x0 = x0,
    .y0 = y0,
    .gap = gap,
    .adjust_box_height = FALSE
  )
}

fixture_row_tree_plot <- function() {
  tree_data <- tibble::tibble(
    label = c("row_a", "row_b", "row_c", NA_character_),
    x = c(2, 2, 2, 1),
    y = c(1, 2, 3, 2),
    isTip = c(TRUE, TRUE, TRUE, FALSE)
  )

  ggplot2::ggplot(tree_data, ggplot2::aes(x = x, y = y))
}
