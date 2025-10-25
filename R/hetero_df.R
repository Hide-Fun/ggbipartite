#' Prefix all column names with "<prefix><sep>"
#'
#' @param df A data frame / tibble.
#' @param prefix Scalar character to prepend.
#' @param sep Separator between prefix and original name (default: "__").
#' @return A tibble with renamed columns.
#' @importFrom tibble as_tibble
prefix_cols <- function(df, prefix, sep = "__") {
  # Coerce to tibble
  df <- tibble::as_tibble(df)
  # Build new names
  names(df) <- paste0(prefix, sep, names(df))
  df
}

#' Pad a data frame to n rows with typed NAs
#'
#' @param df A data frame / tibble.
#' @param n Integer target #rows. If nrow(df) >= n, returns df unchanged.
#' @return A tibble with max(nrow(df), n) rows.
#' @importFrom tibble as_tibble
#' @importFrom vctrs vec_init
#' @importFrom purrr imap
#' @importFrom dplyr bind_rows
pad_rows_typed <- function(df, n) {
  # Ensure tibble
  df <- tibble::as_tibble(df)
  # Nothing to do
  if (nrow(df) >= n) {
    return(df)
  }

  n_pad <- n - nrow(df)

  # Build a tibble of typed NA vectors for each column
  padded_cols <- purrr::imap(df, function(col, nm) vctrs::vec_init(col, n_pad))
  pad_tbl <- tibble::as_tibble(padded_cols)

  dplyr::bind_rows(df, pad_tbl)
}

#' Escape a string for safe use in regular expressions
#'
#' @param x Character vector to escape.
#' @return Character vector with metacharacters escaped.
#' @importFrom stringr str_replace_all
escape_regex <- function(x) {
  # Escape regex metacharacters by backreferencing the capture group
  stringr::str_replace_all(x, "([\\^$.|?*+(){}\\[\\]\\\\])", "\\\\\\1")
}

#' Column-bind heterogeneous data frames with typed NA padding
#'
#' @description
#' Column-binds a list of data frames / tibbles (possibly different row counts)
#' after prefixing each element's columns. Returns a single tibble.
#'
#' @param x A list of data frames / tibbles.
#' @param prefixes Optional character vector of prefixes to use:
#'   * If `NULL` (default): use `names(x)`; if missing/empty, use "df1", "df2", ...
#'   * If unnamed: must have length `length(x)` and is matched by position.
#'   * If named: matched by names of `x`; order is ignored.
#' @param separator String separator between prefix and column name (default `"__"`).
#'
#' @return A tibble formed by prefixing and padding, then column-binding.
#'
#' @examples
#' # Named input -> names used as prefixes
#' x <- list(a = tibble::tibble(id = 1:2), b = tibble::tibble(z = 1:3))
#' combine_hetero_df(x)
#'
#' # Unnamed input -> provide prefixes by position
#' x2 <- list(tibble::tibble(id = 1:2), tibble::tibble(z = 1:3))
#' combine_hetero_df(x2, prefixes = c("left", "right"))
#'
#' # Custom separator
#' combine_hetero_df(x, separator = "::")
#'
#' @export
#' @importFrom tibble as_tibble tibble
#' @importFrom purrr map imap imap_int map2
#' @importFrom dplyr bind_cols
combine_hetero_df <- function(x, prefixes = NULL, separator = "__") {
  # Validate input list
  if (!is.list(x)) {
    stop("`x` must be a list of data frames / tibbles.")
  }

  n <- length(x)
  # Coerce elements to tibble
  x_tbl <- purrr::imap(x, ~ tibble::as_tibble(.x))

  # Resolve prefixes
  if (is.null(prefixes)) {
    pfx <- names(x_tbl)
    if (is.null(pfx) || any(is.na(pfx) | pfx == "")) {
      pfx <- paste0("df", seq_len(n))
    }
  } else {
    if (length(prefixes) == 0L) {
      stop("`prefixes` must not be empty if provided.")
    }
    if (!is.null(names(prefixes)) && any(names(prefixes) != "")) {
      # Named mapping: match by element names
      if (is.null(names(x_tbl)) || any(names(x_tbl) == "")) {
        stop("Named `prefixes` requires `x` to have names.")
      }
      if (!all(names(x_tbl) %in% names(prefixes))) {
        missing_keys <- setdiff(names(x_tbl), names(prefixes))
        stop("Missing prefixes for: ", paste(missing_keys, collapse = ", "))
      }
      pfx <- unname(prefixes[names(x_tbl)])
    } else {
      # Positional mapping
      if (length(prefixes) != n) {
        stop("Length of positional `prefixes` must equal length(x).")
      }
      pfx <- prefixes
    }
  }

  # Prefix columns per element
  x_pref <- purrr::map2(x_tbl, pfx, ~ prefix_cols(.x, .y, sep = separator))

  # Compute max rows
  max_rows <- if (n == 0L) 0L else max(purrr::imap_int(x_tbl, ~ nrow(.x)))

  # Pad rows to max using typed NA
  x_padded <- purrr::map(x_pref, ~ pad_rows_typed(.x, max_rows))

  # Check for duplicate names after prefixing
  all_names <- unlist(lapply(x_padded, names), use.names = FALSE)
  if (any(duplicated(all_names))) {
    dups <- unique(all_names[duplicated(all_names)])
    stop(
      "Duplicated column names after prefixing: ",
      paste(dups, collapse = ", ")
    )
  }

  # Bind by columns; return tibble (data frame)
  if (length(x_padded) == 0L) {
    return(tibble::tibble())
  }
  dplyr::bind_cols(x_padded)
}


x <- list(
  a = tibble::tibble(id = 1:2, v = c(10, 20)),
  b = tibble::tibble(z = c("x", "y", "z"))
)
combine_hetero_df(x)
#> a__id a__v b__z
#> ...

# 2) 無名 + prefixes を位置で指定
x2 <- list(tibble::tibble(id = 1:2), tibble::tibble(z = 1:3))
combine_hetero_df(x2, prefixes = c("left", "right"))

# 3) 区切り文字を変更
combine_hetero_df(x, separator = "::")

#' Reconstruct a named list of tibbles from a combined tibble
#'
#' @description
#' Split a combined tibble created by `combine_hetero_df()` back into a
#' named list of tibbles by parsing column name prefixes. Row counts are
#' inferred by trimming trailing all-NA rows per prefix group, or can be
#' explicitly provided via `rows`.
#'
#' @param combined A tibble returned by `combine_hetero_df()`.
#' @param prefixes Optional character vector of prefixes to reconstruct.
#'   - If `NULL` (default): auto-detect from column names.
#'   - If supplied: only these prefixes are reconstructed (order preserved).
#' @param separator String separator between prefix and original name
#'   (default `"__"`).
#' @param rows Optional integer vector of original row counts per prefix.
#'   - If named: matched by prefix name.
#'   - If unnamed: must have length equal to `length(prefixes)` and is
#'     matched by position.
#'   When provided, `rows` overrides inference.
#' @param drop_empty Logical; if `TRUE`, drop groups inferred to have 0 rows.
#'   Default `FALSE` (return 0-row tibbles).
#'
#' @return A named list of tibbles keyed by prefix.
#'
#' @examples
#' x <- list(
#'   a = tibble::tibble(id = 1:2, v = c(10, 20)),
#'   b = tibble::tibble(z = c("x", "y", "z"))
#' )
#' comb <- combine_hetero_df(x, prefixes = c("left", "right"))
#' parts <- reconstruct_hetero_df(comb, prefixes = c("left", "right"))
#' names(parts)        # "left" "right"
#' parts$left          # restores columns id, v
#' parts$right         # restores column z, trimmed to 3 rows
#'
#' # If your original data had trailing all-NA rows, pass `rows` explicitly:
#' # parts <- reconstruct_hetero_df(comb, prefixes = c("left", "right"),
#' #                                rows = c(left = 5, right = 3))
#'
#' @export
#' @importFrom tibble as_tibble
#' @importFrom dplyr select slice all_of
#' @importFrom purrr map set_names
#' @importFrom stringr fixed str_remove str_starts
reconstruct_hetero_df <- function(
  combined,
  prefixes = NULL,
  separator = "__",
  rows = NULL,
  drop_empty = FALSE
) {
  # Ensure tibble
  combined <- tibble::as_tibble(combined)

  # --- detect or validate prefixes -----------------------------------------
  # Auto-detect prefixes by scanning names "<prefix><separator><col>"
  detect_prefixes <- function(nms, sep) {
    # Keep only names that contain the separator at least once
    has_sep <- grepl(sep, nms, fixed = TRUE)
    nms <- nms[has_sep]
    if (length(nms) == 0L) {
      return(character())
    }
    # Extract text before the first separator occurrence
    # (stable order by first appearance)
    pref <- sub(
      paste0(
        "(.*?)",
        gsub("([\\^$.|?*+(){}\\[\\]\\\\])", "\\\\\\1", sep),
        ".*"
      ),
      "\\1",
      nms,
      perl = TRUE
    )
    unique(pref)
  }

  if (is.null(prefixes)) {
    prefixes <- detect_prefixes(names(combined), separator)
  } else {
    # Validate user-supplied prefixes exist in the combined tibble
    missing <- prefixes[
      !vapply(
        prefixes,
        function(pfx) {
          any(stringr::str_starts(
            names(combined),
            stringr::fixed(paste0(pfx, separator))
          ))
        },
        logical(1)
      )
    ]
    if (length(missing)) {
      stop(
        "Unknown `prefixes` not found in `combined`: ",
        paste(missing, collapse = ", ")
      )
    }
  }

  # Nothing to reconstruct
  if (length(prefixes) == 0L) {
    return(list())
  }

  # --- normalize `rows` -----------------------------------------------------
  rows_vec <- NULL
  if (!is.null(rows)) {
    if (is.null(names(rows)) || all(names(rows) == "")) {
      if (length(rows) != length(prefixes)) {
        stop("Unnamed `rows` must have the same length as `prefixes`.")
      }
      rows_vec <- stats::setNames(as.integer(rows), prefixes)
    } else {
      # Named rows: ensure all prefixes are present
      if (!all(prefixes %in% names(rows))) {
        miss <- setdiff(prefixes, names(rows))
        stop("Missing `rows` entries for: ", paste(miss, collapse = ", "))
      }
      rows_vec <- as.integer(rows[prefixes])
      names(rows_vec) <- prefixes
    }
  }

  # --- reconstruct per prefix ----------------------------------------------
  out <- purrr::map(prefixes, function(pfx) {
    pfx_tag <- paste0(pfx, separator)

    # Select prefixed columns and strip the prefix from their names
    sel <- stringr::str_starts(names(combined), stringr::fixed(pfx_tag))
    df_sub <- dplyr::select(combined, which(sel))
    names(df_sub) <- stringr::str_remove(names(df_sub), stringr::fixed(pfx_tag))

    # If no columns for this prefix, return 0-col tibble with 0 rows
    if (ncol(df_sub) == 0L) {
      return(df_sub[0, , drop = FALSE])
    }

    # Determine number of rows to keep
    if (!is.null(rows_vec)) {
      n_keep <- rows_vec[[pfx]]
      if (is.na(n_keep)) n_keep <- 0L
    } else {
      # Infer by trimming trailing all-NA rows across this group
      # (works if padding appended only at the bottom)
      any_non_na <- rowSums(!is.na(df_sub)) > 0
      idx <- which(any_non_na)
      n_keep <- if (length(idx)) max(idx) else 0L
    }

    # Slice to requested/inferred row count (pad defensively if needed)
    if (n_keep <= 0L) {
      df_sub <- df_sub[0, , drop = FALSE]
    } else if (nrow(df_sub) >= n_keep) {
      df_sub <- dplyr::slice(df_sub, seq_len(n_keep))
    } else {
      # Should not happen if `combined` came from `combine_hetero_df()`,
      # but pad with typed NAs to be robust.
      df_sub <- pad_rows_typed(df_sub, n_keep)
    }

    df_sub
  })

  # Assign names and optionally drop empty groups
  names(out) <- prefixes
  if (isTRUE(drop_empty)) {
    keep <- vapply(out, nrow, integer(1)) > 0L
    out <- out[keep]
  }
  out
}
