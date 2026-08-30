core_linters <- lintr::linters_with_defaults(
  # R CMD check performs package-wide symbol analysis without cross-file noise.
  object_usage_linter = NULL,
  # Descriptive internal validator names are intentionally longer than 30.
  object_length_linter = NULL
)

legacy_linters <- lintr::linters_with_defaults(
  object_usage_linter = NULL,
  object_length_linter = NULL,
  # ggplot2 extension interfaces retain names such as `na.rm` and ggproto types.
  object_name_linter = NULL,
  # Compatibility code retains the existing `%>%` convention.
  pipe_consistency_linter = NULL
)

lint_targets <- list(
  "R/bipartite-layout.R" = core_linters,
  "R/plot-bipartite.R" = core_linters,
  "R/geom-bipnet.R" = legacy_linters,
  "R/stat-bipnet.R" = legacy_linters,
  "R/bipartite_network.R" = legacy_linters,
  "R/compute_box_coords.R" = legacy_linters,
  "R/compute_interaction_coords.R" = legacy_linters,
  "R/global_layout.R" = legacy_linters,
  "R/globals.R" = core_linters,
  "tests/testthat/test-bipartite-layout.R" = core_linters,
  "tests/testthat/test-geom-layout.R" = core_linters,
  "tests/testthat/test-plot-bipartite.R" = core_linters,
  "tests/testthat/test-visual-regression.R" = core_linters,
  "tools/lint.R" = core_linters
)

lints <- unlist(
  Map(
    function(path, linters) {
      lintr::lint(path, linters = linters)
    },
    names(lint_targets),
    lint_targets
  ),
  recursive = FALSE
)

if (length(lints) > 0L) {
  print(lints)
  stop("Lint failures were found in the v0.10 layout path.", call. = FALSE)
}

message("Lint passed for the v0.10 changed-code set.")
