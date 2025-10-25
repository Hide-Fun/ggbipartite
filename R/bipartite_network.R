#' Construct box and interaction coordinates for a bipartite network
#'
#' Given a bipartite interaction matrix, compute:
#' (1) global layout parameters, (2) per-side box coordinates for rows and
#' columns, and (3) per-cell interaction polygons connecting the two sides.
#' Optionally, row/column metadata can be joined onto the resulting frames.
#'
#' @param .mat A numeric matrix of interactions; rows and columns represent the
#'   two node sets.
#' @param .metadata_row Optional tibble/data frame of row-level metadata to join
#'   to the **row-side** box table (`box1`). Must contain a `row` key that
#'   matches the identifiers used in `.mat`/`bipartite_network()`.
#' @param .metadata_column Optional tibble/data frame of column-level metadata to
#'   join to the **column-side** box table (`box2`) and to `interaction_coords`.
#'   Must contain a `column` key that matches the identifiers used in
#'   `.mat`/`bipartite_network()`.
#' @param .x0,.y0 Numeric scalars; global origin for the left/bottom corner of
#'   the first box.
#' @param .gap Non-negative numeric scalar; baseline vertical gap between
#'   adjacent items on each side (subject to adjustment; see \code{.adjust_box_height}).
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
#'         into \code{box1}, \code{box2}, and \code{interaction_coords}
#'         (first columns, then rows for the latter).
#'   \item When only one side is provided, it is joined to the corresponding box
#'         table; \code{interaction_coords} is left unchanged.
#' }
#'
#' @note
#' The function body refers to \code{.metadata_row} and \code{.metadata_column}
#' (with correct spelling) in the joins, while the arguments are named
#' \code{.metadata_row} and \code{.metadata_column}. Align these names in your
#' implementation to avoid runtime errors.
#'
#' The helper \code{compute_box_coords()} is expected to accept arguments
#' \code{(.df, .var, .size, .x0, .width, .gap)} and return a data frame with
#' at least \code{.var}, \code{x}, \code{xmin}, \code{xmax}, and vertical span
#' information used downstream.
#'
#' @return A named list with three components:
#' \describe{
#'   \item{\code{box1}}{Tibble/data frame of row-side box coordinates.}
#'   \item{\code{box2}}{Tibble/data frame of column-side box coordinates.}
#'   \item{\code{interaction_coords}}{Tibble/data frame of CCW-ordered polygon
#'         vertices (\code{x}, \code{y}) for each \code{row}–\code{column}
#'         interaction, including an \code{area} column.}
#' }
#'
#' @seealso \code{\link{bipartite_network}}, \code{\link{calc_global_params}},
#'   \code{\link{compute_box_coords}}, \code{\link{compute_interaction_coords}}
#'
#' @examples
#' \dontrun{
#' m <- matrix(c(2, 1, 0,
#'               0, 3, 2), nrow = 2, byrow = TRUE)
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
#' str(res$box1); str(res$box2); str(res$interaction_coords)
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
  .adjust_box_height = TRUE
) {
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

  box1 <- compute_box_coords(
    .df = dfs$rsf,
    .var = "row",
    .size = "interaction_size",
    .x0 = params$box1[[1]],
    .width = params$box_width,
    .gap = params$gap1
  )

  box2 <- compute_box_coords(
    .df = dfs$csf,
    .var = "column",
    .size = "interaction_size",
    .x0 = params$box2[[1]],
    .width = params$box_width,
    .gap = params$gap2
  )

  interaction_coords <- compute_interaction_coords(
    .box1 = box1,
    .box2 = box2,
    .interation_cell = dfs$ilf
  )

  if (!is.null(.metadata_row) && !is.null(.metadata_column)) {
    box1 <- box1 %>%
      left_join(.metadata_row, by = c("row" = .row))

    box2 <- box2 %>%
      left_join(.metadata_column, by = c("column" = .column))

    interaction_coords <- interaction_coords %>%
      left_join(.metadata_column, by = c("column" = .column)) %>%
      left_join(.metadata_row, by = c("row" = .row))
  } else if (!is.null(.metadata_column)) {
    box2 <- box2 %>%
      left_join(.metadata_columnby = c("column" = .column))

    interaction_coords <- interaction_coords %>%
      left_join(.metadata_column, by = c("column" = .column))
  } else if (!is.null(.metadata_row)) {
    box1 <- box1 %>%
      left_join(.metadata_row, by = c("row" = .row))

    interaction_coords <- interaction_coords %>%
      left_join(.metadata_row, by = c("row" = .row))
  }

  return(list(
    box1 = box1,
    box2 = box2,
    interaction_coords = interaction_coords
  ))
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
#'   \item Delegates reshaping to a user- or package-provided [to_longer()]
#'     helper that converts the matrix to a long tibble.
#' }
#'
#' The function assumes that [to_longer()] is available in scope; if not,
#' provide your own implementation that accepts `.mat` and returns a tibble.
#'
#' @section Input validation:
#' Minimal checks are performed. For robust workflows, validate that `.mat` is a
#' numeric matrix (or coercible) and contains non-missing values as needed.
#'
#' @examples
#' m <- matrix(c(1, 0, 2,
#'               3, 1, 1), nrow = 2, byrow = TRUE)
#'
#' # Example stub for `to_longer()` if not available:
#' to_longer <- function(.mat) {
#'   tibble::as_tibble(as.data.frame(as.table(.mat))) |>
#'     dplyr::rename(row = Var1, column = Var2, interaction_size = Freq)
#' }
#'
#' bip <- bipartite_network(m)
#' bip$rsf
#' bip$csf
#' bip$ilf
#'
#' @importFrom tibble enframe
#' @export
bipartite_network <- function(.mat) {
  # Calculate row/column sums.
  rsf <- rowSums(.mat) |>
    tibble::enframe(name = "row", value = "interaction_size")

  csf <- colSums(.mat) |>
    tibble::enframe(name = "column", value = "interaction_size")

  # Convert to longer tibble (expects a `to_longer()` helper to exist).
  ilf <- to_longer(.mat = .mat)

  return(
    list(
      rsf = rsf,
      csf = csf,
      ilf = ilf
    )
  )
}
