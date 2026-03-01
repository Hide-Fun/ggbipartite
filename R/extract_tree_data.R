#' Extract Tree Data from Supported Inputs
#'
#' Resolves tree-associated data from a ggplot/ggtree object, an S4 object with
#' a `data` slot, or a raw tree object that can be passed to
#' [ggtree::ggtree()].
#'
#' @param x A supported tree input object.
#'
#' @return A data frame containing tree data.
#' @keywords internal
#' @noRd
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
