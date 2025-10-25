#' Convert a matrix-like object to a long tibble
#'
#' Transforms a numeric matrix or data frame into a long-format tibble with
#' one row per nonzero cell. Optionally preserves original row names in a
#' dedicated column.
#'
#' @param .mat A matrix or data-frame-like object to be reshaped.
#' @param .rowname A single string giving the column name to store row names.
#'   If `NULL`, row names are not added. Defaults to `"row"`.
#'
#' @return A tibble with columns:
#' \itemize{
#'   \item \code{.rowname} (if provided): original row identifiers
#'   \item \code{column}: original column names
#'   \item \code{interaction}: cell values (filtered to nonzero)
#' }
#'
#' @details
#' The input is first converted to a tibble (optionally adding a row-name
#' column), then pivoted to long format and filtered to keep only rows where
#' \code{interaction != 0}.
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
  # Always create a tibble first; optionally include row names as a column
  mdf <- matrix_to_tibble(.mat = .mat, .rowname = .rowname)

  # Long reshape and drop zeros
  df_longer <- mdf %>%
    tidyr::pivot_longer(
      cols = -tidyselect::all_of(.rowname),
      names_to = "column",
      values_to = "interaction"
    ) %>%
    dplyr::filter(interaction != 0) %>%
    mutate(
      row = factor(row, rownames(.mat)),
      column = factor(column, colnames(.mat))
    )

  return(df_longer)
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
