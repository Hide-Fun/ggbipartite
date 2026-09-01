#' Construct box and interaction coordinates for a bipartite network
#'
#' `r lifecycle::badge("superseded")`
#'
#' Given a bipartite interaction matrix, compute:
#' (1) global layout parameters, (2) per-side box coordinates for rows and
#' columns, and (3) per-cell interaction polygons connecting the two sides.
#' Optionally, row/column metadata can be joined onto the resulting frames.
#'
#' @param .mat A numeric matrix of interactions; rows and columns represent the
#'   two node sets.
#' @param .row A single string giving the key column name in `.metadata_row`
#'   used to join row metadata to row identifiers.
#' @param .column A single string giving the key column name in
#'   `.metadata_column` used to join column metadata to column identifiers.
#' @param .metadata_row Optional tibble/data frame of row-level metadata to join
#'   to the **row-side** box table (`row_box`). Must contain the column
#'   specified by `.row`.
#' @param .metadata_column Optional tibble/data frame of column-level metadata
#'   to join to the **column-side** box table (`column_box`) and to
#'   `interaction_coords`. Must contain the column specified by `.column`.
#' @param .x0,.y0 Numeric scalars; global origin for the left/bottom corner of
#'   the first box.
#' @param .gap Non-negative numeric scalar; baseline vertical gap between
#'   adjacent items on each side (subject to adjustment; see
#'   \code{.adjust_box_height}).
#' @param .box_ratio Positive numeric scalar; divisor to derive per-box width as
#'   \code{sum(.mat) / .box_ratio}.
#' @param .ratio Positive numeric scalar; divisor to derive overall width as
#'   \code{sum(.mat) / .ratio}.
#' @param .adjust_box_height Logical; if \code{TRUE}, the vertical gaps for each
#'   side are adjusted so that the total heights of the two sides match (uses
#'   \code{\link{calc_global_params}} and its helper).
#'
#' @details
#' Pipeline:
#' \enumerate{
#'   \item \code{\link{bipartite_network}()} produces row/column summaries and a
#'         long-form interaction table.
#'   \item \code{\link{calc_global_params}()} computes layout constants (box
#'         positions, widths, and side-specific gaps).
#'   \item \code{\link{compute_box_coords}()} builds the boxes for rows
#'         (\code{.var = "row"}) and columns (\code{.var = "column"}).
#'   \item \code{\link{compute_interaction_coords}()} constructs a 4-vertex
#'         polygon for each nonzero interaction cell connecting the two sides.
#' }
#'
#' If metadata is supplied:
#' \itemize{
#'   \item When both row and column metadata are provided, they are left-joined
#'         into \code{row_box}, \code{column_box}, and \code{interaction_coords}
#'         (first columns, then rows for the latter).
#'   \item When only one side is provided, it is joined to the corresponding box
#'         table; \code{interaction_coords} is left unchanged.
#' }
#'
#' @note
#' Row/column identifiers and metadata keys are coerced to character before
#' joining. Metadata keys must be non-missing, non-empty, and unique within
#' each side.
#'
#' The helper \code{compute_box_coords()} is expected to accept arguments
#' \code{(.df, .var, .size, .x0, .width, .gap)} and return a data frame with
#' at least \code{.var}, \code{x}, \code{xmin}, \code{xmax}, and vertical span
#' information used downstream.
#'
#' @return A named list with three components:
#' \describe{
#'   \item{\code{row_box}}{Tibble/data frame of row-side box coordinates.}
#'   \item{\code{column_box}}{Tibble/data frame of column-side box coordinates.}
#'   \item{\code{interaction_coords}}{Tibble/data frame of CCW-ordered polygon
#'         vertices (\code{x}, \code{y}) for each \code{row}–\code{column}
#'         interaction, including an \code{area} column.}
#'   \item{\code{box1}, \code{box2}}{Backward-compatible aliases for
#'         \code{row_box} and \code{column_box}.}
#' }
#'
#' @seealso \code{\link{bipartite_network}}, \code{\link{calc_global_params}},
#'   \code{\link{compute_box_coords}}, \code{\link{compute_interaction_coords}},
#'   and [layout_bipartite()] for the recommended shared-layout workflow.
#'
#' @examples
#' \dontrun{
#' m <- matrix(c(2, 1, 0,
#'               0, 3, 2), nrow = 2, byrow = TRUE)
#'
#' rownames(m) <- c(1, 2)
#' colnames(m) <- c(1, 2, 3)
#'
#' # Optional metadata (must contain `row` / `column` keys)
#' row_meta <- tibble::tibble(row = 1:2, group = c("A", "B"))
#' col_meta <- tibble::tibble(column = 1:3, type = c("x","y","z"))
#'
#' res <- construct_bn_coordination(
#'   .mat = m,
#'   .metadata_row = row_meta,
#'   .metadata_column = col_meta,
#'   .x0 = 0, .y0 = 0,
#'   .gap = 0.1,
#'   .box_ratio = 5,
#'   .ratio = 1 / 1.618,
#'   .adjust_box_height = TRUE
#' )
#' str(res$row_box); str(res$column_box); str(res$interaction_coords)
#' }
#'
#' @importFrom dplyr left_join
#' @export
construct_bn_coordination <- function(
  .mat,
  .row,
  .column,
  .metadata_row = NULL,
  .metadata_column = NULL,
  .x0 = 0,
  .y0 = 0,
  .gap = 0,
  .box_ratio = 5,
  .ratio = 1 / 1.618,
  .adjust_box_height = FALSE
) {
  if (!is.null(.metadata_row)) {
    .metadata_row <- normalize_metadata_key(
      metadata = .metadata_row,
      key = .row,
      metadata_arg = ".metadata_row",
      key_arg = ".row"
    )
  }

  if (!is.null(.metadata_column)) {
    .metadata_column <- normalize_metadata_key(
      metadata = .metadata_column,
      key = .column,
      metadata_arg = ".metadata_column",
      key_arg = ".column"
    )
  }

  dfs <- bipartite_network(.mat = .mat)

  params <- calc_global_params(
    .mat = .mat,
    .gap = .gap,
    .x0 = .x0,
    .y0 = .y0,
    .box_ratio = .box_ratio,
    .ratio = .ratio,
    .adjust_box_height = .adjust_box_height
  )

  row_box <- compute_box_coords(
    .df = dfs$rsf,
    .var = "row",
    .size = "interaction_size",
    .x0 = params$row_box[[1]],
    .y0 = params$row_box[[2]],
    .width = params$box_width,
    .gap = params$gap_row
  )

  column_box <- compute_box_coords(
    .df = dfs$csf,
    .var = "column",
    .size = "interaction_size",
    .x0 = params$column_box[[1]],
    .y0 = params$column_box[[2]],
    .width = params$box_width,
    .gap = params$gap_column
  )

  interaction_coords <- compute_interaction_coords(
    .row_box = row_box,
    .column_box = column_box,
    .interation_cell = dfs$ilf
  )

  row_box <- row_box %>%
    dplyr::mutate(row = as.character(.data$row))
  column_box <- column_box %>%
    dplyr::mutate(column = as.character(.data$column))
  interaction_coords <- interaction_coords %>%
    dplyr::mutate(
      row = as.character(.data$row),
      column = as.character(.data$column)
    )

  if (!is.null(.metadata_column)) {
    column_box <- column_box %>%
      dplyr::left_join(.metadata_column, by = c("column" = .column))
    interaction_coords <- interaction_coords %>%
      dplyr::left_join(.metadata_column, by = c("column" = .column))
  }

  if (!is.null(.metadata_row)) {
    row_box <- row_box %>%
      dplyr::left_join(.metadata_row, by = c("row" = .row))
    interaction_coords <- interaction_coords %>%
      dplyr::left_join(.metadata_row, by = c("row" = .row))
  }

  list(
    row_box = row_box,
    column_box = column_box,
    interaction_coords = interaction_coords,
    box1 = row_box,
    box2 = column_box
  )
}

normalize_metadata_key <- function(metadata, key, metadata_arg, key_arg) {
  if (!is.data.frame(metadata)) {
    stop("`", metadata_arg, "` must be a data frame.", call. = FALSE)
  }
  duplicated_names <- unique(names(metadata)[duplicated(names(metadata))])
  if (length(duplicated_names) > 0L) {
    stop(
      "`",
      metadata_arg,
      "` must have unique column names; duplicated names: ",
      paste(duplicated_names, collapse = ", "),
      ".",
      call. = FALSE
    )
  }
  if (
    !is.character(key) ||
      length(key) != 1L ||
      is.na(key) ||
      key == ""
  ) {
    stop(
      "`",
      key_arg,
      "` must be a single non-empty column name.",
      call. = FALSE
    )
  }
  if (!key %in% names(metadata)) {
    stop(
      "`",
      metadata_arg,
      "` must contain the key column `",
      key,
      "`.",
      call. = FALSE
    )
  }

  key_values <- as.character(metadata[[key]])
  if (anyNA(key_values) || any(key_values == "")) {
    stop(
      "`",
      metadata_arg,
      "$",
      key,
      "` must not contain missing or empty IDs.",
      call. = FALSE
    )
  }

  duplicated_ids <- unique(key_values[duplicated(key_values)])
  if (length(duplicated_ids) > 0L) {
    stop(
      "`",
      metadata_arg,
      "$",
      key,
      "` must be unique; duplicated IDs: ",
      paste(duplicated_ids, collapse = ", "),
      ".",
      call. = FALSE
    )
  }

  metadata[[key]] <- key_values
  metadata
}

#' Summarize a bipartite interaction matrix
#'
#' Compute row- and column-wise interaction totals and a long-form version of
#' the bipartite interaction matrix.
#'
#' @param .mat A numeric matrix representing a bipartite interaction network.
#'   Rows and columns correspond to the two disjoint node sets; entries are
#'   interaction magnitudes (e.g., counts, weights).
#'
#' @return A list with three tibbles:
#' \itemize{
#'   \item `rsf`: row-sum features with columns `row` (row index) and
#'     `interaction_size` (sum over the row).
#'   \item `csf`: column-sum features with columns `column` (column index) and
#'     `interaction_size` (sum over the column).
#'   \item `ilf`: long-form interactions as returned by [to_longer()], typically
#'     containing row/column identifiers and a value column.
#' }
#'
#' @details
#' This function is a light-weight helper that:
#' \enumerate{
#'   \item Computes row totals via [base::rowSums()] and wraps them with
#'     [tibble::enframe()].
#'   \item Computes column totals via [base::colSums()] and wraps them with
#'     [tibble::enframe()].
#'   \item Delegates reshaping to the package's [to_longer()] helper, which
#'     applies the same strict matrix contract and returns a long tibble.
#' }
#'
#' @section Input validation:
#' `.mat` must be a numeric matrix with finite, non-negative values and
#' non-empty, unique row and column names. Every row and column must have a
#' positive total interaction weight. Invalid IDs and zero-total nodes are
#' reported before coordinates are computed.
#'
#' @examples
#' m <- matrix(c(1, 0, 2,
#'               3, 1, 1), nrow = 2, byrow = TRUE)
#' dimnames(m) <- list(c("row_1", "row_2"), c("col_1", "col_2", "col_3"))
#'
#' bip <- bipartite_network(m)
#' bip$rsf
#' bip$csf
#' bip$ilf
#'
#' @importFrom tibble enframe
#' @export
bipartite_network <- function(.mat) {
  row_ids <- validate_bipartite_dimnames(
    ids = rownames(.mat),
    size = nrow(.mat),
    side = "row"
  )
  column_ids <- validate_bipartite_dimnames(
    ids = colnames(.mat),
    size = ncol(.mat),
    side = "column"
  )
  .mat <- validate_bipartite_matrix_values(
    .mat,
    row_ids = row_ids,
    column_ids = column_ids
  )

  # Calculate row/column sums.
  rsf <- rowSums(.mat) %>%
    tibble::enframe(name = "row", value = "interaction_size")

  csf <- colSums(.mat) %>%
    tibble::enframe(name = "column", value = "interaction_size")

  # Use the package validator again so the public long form has one contract.
  ilf <- to_longer(.mat = .mat)

  list(
    rsf = rsf,
    csf = csf,
    ilf = ilf
  )
}
