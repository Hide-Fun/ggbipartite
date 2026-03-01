library(tidyverse)
library(ggtree)
library(marquee)
library(patchwork)

.is_ggplot_obj <- function(x) {
  if ("is_ggplot" %in% getNamespaceExports("ggplot2")) {
    return(ggplot2::is_ggplot(x))
  }

  ggplot2::is.ggplot(x)
}

.extract_tree_data <- function(x) {
  if (.is_ggplot_obj(x)) {
    tree_data <- x$data
  } else if (isS4(x) && "data" %in% methods::slotNames(x)) {
    tree_data <- x@data
  } else {
    tree_data <- ggtree::ggtree(x)$data
  }

  if (!is.data.frame(tree_data)) {
    stop("Tree data must be a data frame.")
  }

  tree_data
}

get_tip_order <- function(x) {
  tree_data <- .extract_tree_data(x)
  required_cols <- c("isTip", "label", "y")
  missing_cols <- setdiff(required_cols, names(tree_data))

  if (length(missing_cols) > 0) {
    stop(
      "Tree data is missing required columns: ",
      paste(missing_cols, collapse = ", "),
      "."
    )
  }

  tip_labels <- tree_data |>
    dplyr::filter(!is.na(.data$isTip) & .data$isTip) |>
    dplyr::filter(!is.na(.data$label), !is.na(.data$y)) |>
    dplyr::arrange(.data$y, .data$label) |>
    dplyr::distinct(.data$label) |>
    dplyr::pull(.data$label)

  if (length(tip_labels) == 0) {
    stop("No tips were found in tree data.")
  }

  tip_labels
}

create_link <- function(box, ggtree, direction, x = 0, xend = 1) {
  direction <- match.arg(direction, choices = c("left", "right"))

  if (!is.data.frame(box)) {
    stop("`box` must be a data frame.")
  }
  if (!is.numeric(x) || length(x) != 1 || !is.finite(x)) {
    stop("`x` must be a finite numeric scalar.")
  }
  if (!is.numeric(xend) || length(xend) != 1 || !is.finite(xend)) {
    stop("`xend` must be a finite numeric scalar.")
  }

  required_box_cols <- c("xmin", "xmax", "ymin", "ymax")
  missing_box_cols <- setdiff(required_box_cols, names(box))
  if (length(missing_box_cols) > 0) {
    stop(
      "`box` is missing required columns: ",
      paste(missing_box_cols, collapse = ", "),
      "."
    )
  }

  id_col <- if (identical(direction, "left")) "row" else "column"
  if (!(id_col %in% names(box))) {
    stop(
      "`box` must contain `",
      id_col,
      "` when `direction = \"",
      direction,
      "\"`."
    )
  }

  box_summary <- box |>
    tidyr::pivot_longer(
      cols = c("xmin", "xmax", "ymin", "ymax"),
      names_to = c("axis", "end"),
      names_pattern = "([xy])(min|max)"
    ) |>
    tidyr::pivot_wider(
      names_from = "axis",
      values_from = "value"
    ) |>
    dplyr::group_by(.data[[id_col]]) |>
    dplyr::summarise(
      y1 = mean(.data$y, na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      x = x,
      xend = xend
    )

  tree_data <- .extract_tree_data(ggtree)
  required_tree_cols <- c("isTip", "label", "y")
  missing_tree_cols <- setdiff(required_tree_cols, names(tree_data))
  if (length(missing_tree_cols) > 0) {
    stop(
      "Tree data is missing required columns: ",
      paste(missing_tree_cols, collapse = ", "),
      "."
    )
  }

  tip_summary <- tree_data |>
    dplyr::filter(!is.na(.data$isTip) & .data$isTip) |>
    dplyr::group_by(.data$label) |>
    dplyr::summarise(
      y2 = mean(.data$y, na.rm = TRUE),
      .groups = "drop"
    )

  join_by <- stats::setNames("label", id_col)
  coords <- dplyr::left_join(box_summary, tip_summary, by = join_by)

  if (any(is.na(coords$y2))) {
    warning(
      "Some entries in `box` could not be matched to tree tip labels.",
      call. = FALSE
    )
  }

  coords
}

.tiplab_align_x <- function(df, offset = 0) {
  if (!is.data.frame(df) || !("x" %in% names(df))) {
    stop("`df` must be a data frame containing column `x`.")
  }
  if (!is.numeric(offset) || length(offset) != 1 || !is.finite(offset)) {
    stop("`offset` must be a finite numeric scalar.")
  }

  x_range <- range(df$x, na.rm = TRUE)
  if (!all(is.finite(x_range))) {
    stop("`df$x` must contain at least one finite value.")
  }

  max(x_range) + diff(x_range) / 200 + offset
}

.tiplab_segment_data <- function(df, offset = 0) {
  required_cols <- c("isTip", "y")
  missing_cols <- setdiff(required_cols, names(df))
  if (length(missing_cols) > 0) {
    stop(
      "`df` is missing required columns: ",
      paste(missing_cols, collapse = ", "),
      "."
    )
  }

  tip_data <- df |>
    dplyr::filter(!is.na(.data$isTip) & .data$isTip) |>
    dplyr::mutate(
      xend = .tiplab_align_x(df = df, offset = offset),
      yend = .data$y
    )

  tip_data
}

.tiplab_marquee_data <- function(df, offset = 0) {
  required_cols <- c("isTip", "y")
  missing_cols <- setdiff(required_cols, names(df))
  if (length(missing_cols) > 0) {
    stop(
      "`df` is missing required columns: ",
      paste(missing_cols, collapse = ", "),
      "."
    )
  }

  tip_data <- df |>
    dplyr::filter(!is.na(.data$isTip) & .data$isTip) |>
    dplyr::mutate(
      x_lab = .tiplab_align_x(df = df, offset = offset)
    )

  tip_data
}

.tiplab_marquee_data_unaligned <- function(df, offset = 0) {
  required_cols <- c("isTip", "x", "y")
  missing_cols <- setdiff(required_cols, names(df))
  if (length(missing_cols) > 0) {
    stop(
      "`df` is missing required columns: ",
      paste(missing_cols, collapse = ", "),
      "."
    )
  }
  if (!is.numeric(offset) || length(offset) != 1 || !is.finite(offset)) {
    stop("`offset` must be a finite numeric scalar.")
  }

  x_range <- range(df$x, na.rm = TRUE)
  if (!all(is.finite(x_range))) {
    stop("`df$x` must contain at least one finite value.")
  }
  eps <- diff(x_range) / 200

  tip_data <- df |>
    dplyr::filter(!is.na(.data$isTip) & .data$isTip) |>
    dplyr::mutate(
      x_lab = .data$x + eps + offset
    )

  tip_data
}

get_yrange <- function(p) {
  if (!.is_ggplot_obj(p)) {
    stop("`p` must be a ggplot object.")
  }

  built_plot <- ggplot2::ggplot_build(p)
  panel_params <- built_plot$layout$panel_params[[1]]

  # ggplot2 >= 3.4
  if (!is.null(panel_params$y.range)) {
    return(panel_params$y.range)
  }

  # ggplot2 compatibility fallback
  if (!is.null(panel_params$y$range)) {
    return(panel_params$y$range)
  }

  panel_scale <- built_plot$layout$panel_scales_y[[1]]
  if (!is.null(panel_scale$range$range)) {
    return(panel_scale$range$range)
  }

  if (!is.null(p$data$y)) {
    return(range(p$data$y, na.rm = TRUE))
  }

  stop("Could not determine the y-axis range from the plot.")
}


set.seed(123)
## host (plants) phylogeny.
host_phylo <- rtree(10)
host_phylo$tip.label <- LETTERS[1:10]
## symbiont (fungal) phylogeny.
symbiont_phylo <- rtree(4)
symbiont_phylo$tip.label <- sprintf("otu%d", 1:4)

host_order <- get_tip_order(host_phylo) |> rev()
symbiont_order <- get_tip_order(symbiont_phylo) |> rev()

interaction_matrix <- tibble(
  host = LETTERS[1:10],
  otu1 = c(1, 0, 100, 2, 1, 500, 40, 0, 1, 0),
  otu2 = c(10, 0, 1, 5, 1, 0, 10, 0, 4, 0),
  otu3 = c(350, 10, 0, 0, 0, 0, 0, 1, 3, 0),
  otu4 = c(0, 0, 0, 0, 1, 0, 1, 0, 0, 150)
) %>%
  arrange(factor(host, host_order)) |>
  relocate(host, all_of(symbiont_order)) |>
  column_to_rownames("host") %>%
  as.matrix()

interaction_df <- interaction_matrix %>%
  as_tibble(rownames = "host") %>%
  pivot_longer(
    cols = -host,
    names_to = "otu",
    values_to = "num_seq"
  ) %>%
  filter(num_seq > 0) %>%
  mutate(
    host = factor(host, host_order),
    otu = factor(otu, symbiont_order)
  )

metadata_row <- tibble(
  host = LETTERS[1:10],
  family = c("A", "A", "A", "B", "B", "B", "B", "B", "C", "C")
)

p <-
  ggplot(
    interaction_df,
    aes(row = host, column = otu, count = num_seq),
  ) +
  geom_bipnet_box(
    type = "box2",
    fill = "navy",
    linewidth = .1
  ) +
  geom_bipnet_interaction(
    type = "interaction",
    alpha = 0.6,
    show.legend = FALSE,
    linewidth = .1
  ) +
  geom_bipnet_box(
    aes(fill = after_stat(family)),
    type = "box1",
    row_nm = "host",
    metadata_row = metadata_row,
    show.legend = FALSE,
    linewidth = .1
  ) +
  theme_void()

# -------------------------------------------------------------------------

bn_coords <- construct_bn_coordination(
  .mat = interaction_matrix,
  .row = "host",
  .metadata_row = metadata_row,
  .gap = sum(interaction_matrix) / 10,
  .adjust_box_height = FALSE
)

# -------------------------------------------------------------------------

label_df_box1 <- bn_coords$box1 %>%
  pivot_longer(
    cols = c(xmin, xmax, ymin, ymax),
    names_to = c("axis", "end"),
    names_pattern = "([xy])(min|max)"
  ) %>%
  pivot_wider(
    names_from = axis,
    values_from = value
  ) %>%
  summarise(
    x = min(x),
    y = mean(y),
    .by = c(row, family)
  )

label_df_box2 <- bn_coords$box2 %>%
  pivot_longer(
    cols = c(xmin, xmax, ymin, ymax),
    names_to = c("axis", "end"),
    names_pattern = "([xy])(min|max)"
  ) %>%
  pivot_wider(
    names_from = axis,
    values_from = value
  ) %>%
  summarise(
    x = max(x),
    y = mean(y),
    .by = c(column)
  )

p +
  annotate(
    geom = "text",
    x = label_df_box1$x - 100,
    y = label_df_box1$y,
    label = label_df_box1$row
  ) +
  annotate(
    geom = "text",
    x = label_df_box2$x + 100,
    y = label_df_box2$y,
    label = label_df_box2$column
  ) +
  theme_void()

# -------------------------------------------------------------------------

t1 <- adjust_tree(
  .phylo = host_phylo,
  .box = bn_coords$box1,
  .tree_position = "left",
  .adjust = 1
) +
  geom_tiplab()

t2 <- adjust_tree(
  .phylo = symbiont_phylo,
  .box = bn_coords$box2,
  .tree_position = "right",
  .adjust = 1
) +
  geom_tiplab(hjust = 1)

df_link1 <- create_link(
  box = bn_coords$box1,
  ggtree = t1,
  direction = "left",
  x = 1e+05 / 8,
  xend = 0
)

df_link2 <- create_link(
  box = bn_coords$box2,
  ggtree = t2,
  direction = "right",
  x = 0,
  xend = 1e+05 / 8
)

p_link1 <- ggplot() +
  geom_segment(
    data = df_link1,
    mapping = aes(x = x, y = y1, xend = xend, yend = y2, group = row),
    linetype = "dotted",
    linewidth = .5
  ) +
  geom_point(
    data = df_link1,
    mapping = aes(x, y1),
    size = 0.5
  ) +
  geom_point(
    data = df_link1,
    mapping = aes(xend, y2),
    size = 0.5
  ) +
  theme_void()

p_link2 <- ggplot() +
  geom_segment(
    data = df_link2,
    mapping = aes(x = x, y = y1, xend = xend, yend = y2, group = column),
    linetype = "dotted",
    linewidth = .5
  ) +
  geom_point(
    data = df_link2,
    mapping = aes(x, y1),
    size = 0.5
  ) +
  geom_point(
    data = df_link2,
    mapping = aes(xend, y2),
    size = 0.5
  ) +
  theme_void()

yr_t1 <- get_yrange(t1)
yr_t2 <- get_yrange(t2)

# Take the larger y-range (span) and apply it to both plots
ylim_common <- if (diff(yr_t1) >= diff(yr_t2)) yr_t1 else yr_t2

scale_y_common <- scale_y_continuous(
  limits = ylim_common,
  expand = expansion(mult = 0)
)

t1 <- t1 + scale_y_common
t2 <- t2 + scale_y_common
p_link1 <- p_link1 + scale_y_common
p_link2 <- p_link2 + scale_y_common

t1 +
  p_link1 +
  p +
  p_link2 +
  t2 +
  plot_layout(nrow = 1, widths = c(1, 0.25, 1, 0.25, 1))

# `align = T` version.
t1 <- adjust_tree(
  .phylo = host_phylo,
  .box = bn_coords$box1,
  .tree_position = "left",
  .adjust = 1
)

t2 <- adjust_tree(
  .phylo = symbiont_phylo,
  .box = bn_coords$box2,
  .tree_position = "right",
  .adjust = 1
)

t1 <- t1 +
  geom_segment(
    data = function(df) .tiplab_segment_data(df, offset = 0),
    aes(x = x, xend = xend, y = y, yend = yend),
    linetype = "dotted",
    linewidth = 0.5,
    inherit.aes = FALSE
  ) +
  geom_marquee(
    data = function(df) .tiplab_marquee_data(df, offset = 0),
    aes(x = x_lab, y = y, label = label),
    hjust = 0,
    inherit.aes = FALSE
  ) +
  geom_nodelab(aes(label = label), hjust = 1, vjust = -1) +
  coord_cartesian(clip = "off")

t2 <- t2 +
  geom_segment(
    data = function(df) .tiplab_segment_data(df, offset = 0),
    aes(x = x, xend = xend, y = y, yend = yend),
    linetype = "dotted",
    linewidth = 0.5,
    inherit.aes = FALSE
  ) +
  geom_marquee(
    data = function(df) .tiplab_marquee_data(df, offset = 0),
    aes(x = x_lab, y = y, label = label),
    hjust = 1,
    inherit.aes = FALSE
  ) +
  geom_nodelab(aes(label = label), hjust = 1, vjust = -1) +
  coord_cartesian(clip = "off")

t1 <- t1 + scale_y_common
t2 <- t2 + scale_y_common
p_link1 <- p_link1 + scale_y_common
p_link2 <- p_link2 + scale_y_common

t1 +
  p_link1 +
  p +
  p_link2 +
  t2 +
  plot_layout(nrow = 1, widths = c(1, 0.25, 1, 0.25, 1))

# binary ------------------------------------------------------------------

bmat <- (interaction_matrix != 0) * 1L

interaction_df <- bmat %>%
  as_tibble(rownames = "host") %>%
  pivot_longer(
    cols = -host,
    names_to = "otu",
    values_to = "num_seq"
  ) %>%
  filter(num_seq > 0) |>
  mutate(
    host = factor(host, host_order),
    otu = factor(otu, symbiont_order)
  )

bn_coords <- construct_bn_coordination(
  .mat = bmat,
  .row = "host",
  .metadata_row = metadata_row,
  .gap = sum(bmat) / 10
)

t1 <- adjust_tree(
  .phylo = host_phylo,
  .box = bn_coords$box1,
  .tree_position = "left",
  .adjust = 1
) +
  geom_tiplab()

t2 <- adjust_tree(
  .phylo = symbiont_phylo,
  .box = bn_coords$box2,
  .tree_position = "right",
  .adjust = 1
) +
  geom_tiplab(hjust = 1)

df_link1 <- create_link(
  box = bn_coords$box1,
  ggtree = t1,
  direction = "left",
  x = 1e+05 / 8,
  xend = 0
)

df_link2 <- create_link(
  box = bn_coords$box2,
  ggtree = t2,
  direction = "right",
  x = 0,
  xend = 1e+05 / 8
)

p_link1 <- ggplot() +
  geom_segment(
    data = df_link1,
    mapping = aes(x = x, y = y1, xend = xend, yend = y2, group = row),
    linetype = "dotted",
    linewidth = .5
  ) +
  geom_point(
    data = df_link1,
    mapping = aes(x, y1),
    size = 0.5
  ) +
  geom_point(
    data = df_link1,
    mapping = aes(xend, y2),
    size = 0.5
  ) +
  theme_void()

p_link2 <- ggplot() +
  geom_segment(
    data = df_link2,
    mapping = aes(x = x, y = y1, xend = xend, yend = y2, group = column),
    linetype = "dotted",
    linewidth = .5
  ) +
  geom_point(
    data = df_link2,
    mapping = aes(x, y1),
    size = 0.5
  ) +
  geom_point(
    data = df_link2,
    mapping = aes(xend, y2),
    size = 0.5
  ) +
  scale_x_continuous(expand = expansion(mult = 0.5)) +
  theme_void()

yr_t1 <- get_yrange(t1)
yr_t2 <- get_yrange(t2)

# Take the larger y-range (span) and apply it to both plots
ylim_common <- if (diff(yr_t1) >= diff(yr_t2)) yr_t1 else yr_t2

scale_y_common <- scale_y_continuous(
  limits = ylim_common,
  expand = expansion(mult = 0)
)

t1 <- t1 + scale_y_common
t2 <- t2 + scale_y_common
p_link1 <- p_link1 + scale_y_common
p_link2 <- p_link2 + scale_y_common

p_binary <- ggplot(
  interaction_df,
  aes(row = host, column = otu, count = num_seq)
) +
  geom_bipnet_interaction(
    type = "interaction",
    interaction_type = "binary",
    linewidth = .5,
    show.legend = FALSE
  ) +
  geom_bipnet_point(
    type = "box1",
    fill = "steelblue"
  ) +
  geom_bipnet_point(
    type = "box2",
    fill = "navy"
  ) +
  theme_void()

t1 +
  p_link1 +
  p_binary +
  p_link2 +
  t2 +
  plot_layout(nrow = 1, widths = c(1, 0.25, 1, 0.25, 1))
