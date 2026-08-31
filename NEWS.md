# ggbipartite 0.10.0

## Shared layout API

- Added `layout_bipartite()`, which validates matrix or explicitly mapped long
  data once and returns an inspectable `bipartite_layout` with stable `nodes`,
  `interactions`, `tree_links`, `params`, and `trees` components.
- Added explicit abundance and binary modes. Binary mode treats every positive
  value as presence; the mode is never inferred from the data.
- Added strict tree/data set matching, tree-tip ordering, and explicit
  `unmatched_data = "drop"` and `unmatched_tree = "prune"` opt-ins.
- Added an explicit `duplicate` function hook for users who deliberately need
  to aggregate repeated long-format cells.

## Plotting

- Binary layouts now draw edges in a stable matrix order, independent of
  platform-specific rounding in intermediate polygon areas.
- Tree/network compositions now share a y scale and panel height, keeping
  abundance connectors and binary tree tips visually aligned across sizes.
- Raw-data geoms now preserve explicit factor-level order, keeping box
  labels and tree-link endpoints aligned with matrix-based coordinates.
- `geom_bipnet_box()`, `geom_bipnet_point()`, and
  `geom_bipnet_interaction()` now accept `layout`. This path draws precomputed
  coordinates with the identity stat, while valid raw-data calls remain as a
  compatibility path.
- Added `plot_bipartite()` and `ggbipartite_plot`. Named tree, connector, and
  network components can be edited before `as_patchwork()` assembles them.
- Tree-free plots do not require `patchwork`. Multi-panel assembly checks for
  the suggested package only when it is needed.
- Abundance tree plots use connector panels. Binary tree plots align node
  centres directly to tips without altering topology or branch lengths.

## Documentation and lifecycle

- Added an English quick start, layout contract, tree integration guide, and
  0.9.x migration guide. Generated HTML and authoring assets are no longer
  tracked as source.
- The shared layout is now the recommended stable core. Low-level coordinate
  helpers are superseded by `layout_bipartite()` but remain available through
  at least the 0.11.x series; removal will not occur before 0.12.0. Raw-data
  layers remain supported compatibility paths.

# ggbipartite 0.9.7

## Correctness fixes

- Interaction groups now use collision-free row-column tuple IDs.
- Duplicate metadata keys and duplicate long-format cells now fail before a
  join or reshape can multiply geometry.
- Numeric and factor metadata IDs are normalized to character before joins,
  and joins use explicit keys.
- `x0` and `y0` now translate both node partitions and all interaction
  coordinates consistently.
- Equal-height and singleton layouts now retain finite, deterministic gaps.
- Tree tip extraction filters `isTip` before combining labels, preventing an
  identically labelled internal node from changing a tip coordinate.
- `geom_nodemarquee()` now maps its default label aesthetic.
- `compute_interaction_coords()` now permits boxes without an
  `interaction_size` column and avoids joining on unrelated same-named fields.

## Stricter input behavior

- Matrix row and column names must be present, non-empty, and unique.
- Interaction values must be numeric, finite, and non-negative. Rows and
  columns with total weight zero are errors and are listed in the message.
- `.rowname = NULL` is no longer accepted by `to_longer()` because the strict
  coordinate contract requires row identifiers.
- Metadata keys must exist, be non-missing, non-empty, and unique. These
  correctness checks intentionally replace ambiguous or silently incorrect
  output with actionable errors.

## Quality infrastructure

- Added testthat edition 3 fixtures and regression coverage for the confirmed
  bugs, coordinate invariants, public layers, and tree helpers.
- Added a source-tarball GitHub Actions matrix spanning current, old-release,
  development, and declared-floor R/Bioconductor environments.

# ggbipartite 0.9.6

- Added `geom_tipmarquee()` for tip-level marquee annotations.
- Added `geom_nodemarquee()` for node-level marquee annotations.
