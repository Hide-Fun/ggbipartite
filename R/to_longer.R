#' Convert a matrix-like object to a long tibble
#'
#' Transforms a numeric matrix or data frame into a long-format tibble with
#' one row per nonzero cell. The input must have non-empty, unique row and
#' column names.
#'
#' @param .mat A matrix or data-frame-like object to be reshaped.
#' @param .rowname A single non-empty string giving the column name to store
#'   row identifiers. `NULL` is not supported because row identifiers are
#'   required. Defaults to `"row"`.
#'
#' @return A tibble with columns:
#' \itemize{
#'   \item The column named by \code{.rowname}: original row identifiers
#'   \item \code{column}: original column names
#'   \item \code{interaction}: cell values (filtered to nonzero)
#' }
#'
#' @details
#' The input is first validated for complete and unique dimension names. It is
#' then converted to a tibble, pivoted to long format, and filtered to keep only
#' rows where \code{interaction != 0}.
#'
#' @examples
#' m <- matrix(c(1, 0, 2, 3), nrow = 2,
#'             dimnames = list(c("r1", "r2"), c("c1", "c2")))
#' to_longer(m, .rowname = "row")
#'
#' @seealso [matrix_to_tibble()]
#' @importFrom tidyr pivot_longer
#' @importFrom dplyr filter all_of
#' @export
to_longer <- function(.mat, .rowname = "row") {
  if (is.null(.rowname)) {
    stop(
      paste0(
        "`.rowname = NULL` is not supported because row identifiers ",
        "are required."
      ),
      call. = FALSE
    )
  }
  if (
    !is.character(.rowname) ||
      length(.rowname) != 1L ||
      is.na(.rowname) ||
      .rowname == ""
  ) {
    stop("`.rowname` must be a single non-empty string.", call. = FALSE)
  }

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
  validated_matrix <- validate_bipartite_matrix_values(
    .mat,
    row_ids = row_ids,
    column_ids = column_ids
  )

  if (.rowname %in% column_ids) {
    stop(
      "`.rowname` must not duplicate a column name in `.mat`.",
      call. = FALSE
    )
  }

  mdf <- matrix_to_tibble(
    .mat = validated_matrix,
    .rowname = .rowname
  )

  df_longer <- mdf %>%
    tidyr::pivot_longer(
      cols = -tidyselect::all_of(.rowname),
      names_to = "column",
      values_to = "interaction"
    ) %>%
    dplyr::filter(.data$interaction != 0) %>%
    dplyr::mutate(
      "{.rowname}" := factor(.data[[.rowname]], levels = row_ids),
      column = factor(.data$column, levels = column_ids)
    )

  df_longer
}

#' Validate matrix dimension names used as bipartite identifiers
#'
#' @param ids Dimension names to validate.
#' @param size Expected number of identifiers.
#' @param side Dimension name used in error messages.
#'
#' @return `ids` as a character vector.
#' @keywords internal
#' @noRd
validate_bipartite_dimnames <- function(ids, size, side) {
  if (is.null(ids) || length(ids) != size) {
    stop(
      paste0(
        "`.mat` must have non-empty, unique ",
        side,
        " names; none were supplied."
      ),
      call. = FALSE
    )
  }

  ids <- as.character(ids)
  invalid_positions <- which(is.na(ids) | ids == "")
  if (length(invalid_positions) > 0) {
    stop(
      paste0(
        "`.mat` has empty ",
        side,
        " names at positions: ",
        paste(invalid_positions, collapse = ", "),
        "."
      ),
      call. = FALSE
    )
  }

  duplicate_ids <- unique(ids[duplicated(ids)])
  if (length(duplicate_ids) > 0) {
    stop(
      paste0(
        "`.mat` has duplicate ",
        side,
        " names: ",
        paste(duplicate_ids, collapse = ", "),
        "."
      ),
      call. = FALSE
    )
  }

  ids
}

#' Validate values and node totals in a bipartite matrix
#'
#' @param .mat Matrix-like interaction input.
#' @param row_ids,column_ids Validated identifiers for each side.
#'
#' @return A numeric matrix with the supplied dimnames.
#' @keywords internal
#' @noRd
validate_bipartite_matrix_values <- function(.mat, row_ids, column_ids) {
  validated_matrix <- as.matrix(.mat)
  if (!typeof(validated_matrix) %in% c("integer", "double")) {
    stop("`.mat` must contain numeric interaction values.", call. = FALSE)
  }
  if (any(!is.finite(validated_matrix))) {
    stop("`.mat` must contain only finite interaction values.", call. = FALSE)
  }
  if (any(validated_matrix < 0)) {
    stop("`.mat` must not contain negative interaction values.", call. = FALSE)
  }

  dimnames(validated_matrix) <- list(row_ids, column_ids)
  zero_rows <- row_ids[rowSums(validated_matrix) == 0]
  zero_columns <- column_ids[colSums(validated_matrix) == 0]
  problems <- character()
  if (length(zero_rows) > 0L) {
    problems <- c(
      problems,
      paste0("zero-sum row IDs: ", paste(zero_rows, collapse = ", "))
    )
  }
  if (length(zero_columns) > 0L) {
    problems <- c(
      problems,
      paste0("zero-sum column IDs: ", paste(zero_columns, collapse = ", "))
    )
  }
  if (length(problems) > 0L) {
    stop(
      "Every node must have positive total interaction; ",
      paste(problems, collapse = "; "),
      ".",
      call. = FALSE
    )
  }
  validated_matrix
}

#' Convert a matrix or data frame to a tibble (optionally adding row names)
#'
#' Wraps coercion to tibble with an option to preserve row names as a column.
#'
#' @param .mat A matrix or data-frame-like object.
#' @param .rowname A single string for the row-name column. If `NULL`,
#'   row names are not added.
#'
#' @return A tibble. If \code{.rowname} is non-NULL, the tibble includes a
#'   leading column with that name containing row names.
#'
#' @examples
#' m <- matrix(1:4, nrow = 2, dimnames = list(c("r1","r2"), c("c1","c2")))
#' matrix_to_tibble(m, .rowname = "row")
#'
#' @importFrom tibble rownames_to_column as_tibble
#' @export
matrix_to_tibble <- function(.mat, .rowname = NULL) {
  # Coerce to data.frame first to work well with rownames_to_column()
  if (is.null(.rowname)) {
    df <- .mat %>%
      as.data.frame()
  } else {
    df <- .mat %>%
      as.data.frame() %>%
      tibble::rownames_to_column(var = .rowname)
  }

  tbl <- df %>% tibble::as_tibble()
  return(tbl)
}
