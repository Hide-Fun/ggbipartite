# Numeric-like labels include examples such as "100", "100/98",
# "100 - 98", and "10.5/9.8".
.is_numeric_like_label <- function(s) {
  if (is.na(s)) {
    return(FALSE)
  }

  s_trim <- trimws(s)
  if (s_trim == "") {
    return(FALSE)
  }

  grepl(
    "^\\d+(?:\\.\\d+)?(?:\\s*[/-]\\s*\\d+(?:\\.\\d+)?)*$",
    s_trim
  )
}

#' Style scientific names into Markdown
#'
#' Scientific-name parts are converted to Markdown italics and normalized for
#' common qualifiers (`cf.`, `aff.`, `nr.`), infraspecific rank markers
#' (`subsp.`, `var.`, `f.`), and hybrid notation (`×`).
#'
#' Numeric-like labels (for example, node support values) are returned
#' unchanged.
#'
#' @param x Character vector.
#'
#' @return Character vector of Markdown-styled labels.
#'
#' @examples
#' x <- c(
#'   "88",
#'   "Cremastra aphylla",
#'   "Cremastra appendiculata var. variabilis",
#'   "Cremastra cf. appendiculata",
#'   "×Cremastra appendiculata var. variabilis f. alba",
#'   "Cremastra sp.",
#'   "Odontochilus yakushimensis x Odontochilus nakaianus",
#'   "Odontochilus yakushimensis x O. nakaianus",
#'   "Odontochilus yakushimensis × nakaianus"
#' )
#'
#' style_sciname(x)
#' @export
style_sciname <- function(x) {
  x <- as.character(x)

  rank_tokens <- c(
    "subsp.",
    "ssp.",
    "var.",
    "v.",
    "forma",
    "f.",
    "subvar.",
    "subf."
  )
  id_qualifiers <- c(
    "cf.",
    "aff.",
    "nr.",
    "indet.",
    "ined.",
    "agg.",
    "s.l."
  )
  sp_tokens <- c("sp.", "spp.")
  hybrid_tokens <- c("×", "x", "X")

  italic_md <- function(s) {
    paste0("*", s, "*")
  }

  normalize_token <- function(tok) {
    tok_lower <- tolower(tok)
    tok_no_dot <- gsub("\\.+$", "", tok_lower)

    if (tok_no_dot == "cf") {
      return("cf.")
    }
    if (tok_no_dot == "aff") {
      return("aff.")
    }
    if (tok_no_dot == "nr") {
      return("nr.")
    }
    if (tok_no_dot == "indet") {
      return("indet.")
    }
    if (tok_no_dot == "ined") {
      return("ined.")
    }
    if (tok_no_dot == "agg") {
      return("agg.")
    }
    if (tok_no_dot %in% c("s.l", "sl")) {
      return("s.l.")
    }
    if (tok_no_dot == "sp") {
      return("sp.")
    }
    if (tok_no_dot == "spp") {
      return("spp.")
    }
    if (tok_no_dot == "subsp") {
      return("subsp.")
    }
    if (tok_no_dot == "ssp") {
      return("ssp.")
    }
    if (tok_no_dot == "var") {
      return("var.")
    }
    if (tok_no_dot == "v") {
      return("v.")
    }
    if (tok_no_dot == "forma") {
      return("forma")
    }
    if (tok_no_dot == "f") {
      return("f.")
    }
    if (tok_no_dot == "subvar") {
      return("subvar.")
    }
    if (tok_no_dot == "subf") {
      return("subf.")
    }

    tok
  }

  is_sp_number_token <- function(tok) {
    grepl("^spp?\\.\\d+$", tolower(tok))
  }

  style_one_simple <- function(name) {
    s_raw <- as.character(name)

    if (is.na(s_raw) || s_raw == "") {
      return("")
    }
    if (.is_numeric_like_label(s_raw)) {
      return(s_raw)
    }

    s_norm <- gsub("_+", " ", s_raw)
    s_norm <- stringr::str_squish(s_norm)
    if (s_norm == "") {
      return("")
    }

    toks <- stringr::str_split(s_norm, "\\s+")[[1]]
    if (length(toks) == 0) {
      return("")
    }

    hybrid_marker <- NULL
    marker_attached <- FALSE

    if (toks[1] %in% hybrid_tokens) {
      hybrid_marker <- "×"
      marker_attached <- FALSE
      toks <- toks[-1]
    } else if (stringr::str_detect(toks[1], "^×")) {
      hybrid_marker <- "×"
      marker_attached <- TRUE
      toks[1] <- stringr::str_remove(toks[1], "^×")
    } else if (stringr::str_detect(toks[1], "^[xX][A-Z]")) {
      hybrid_marker <- "×"
      marker_attached <- TRUE
      toks[1] <- stringr::str_remove(toks[1], "^[xX]")
    }

    toks <- toks[toks != ""]
    if (length(toks) == 0) {
      if (is.null(hybrid_marker)) {
        return("")
      }

      return(hybrid_marker)
    }

    genus <- toks[1]
    if (length(toks) >= 2) {
      rest <- toks[-1]
    } else {
      rest <- character(0)
    }

    rest <- vapply(
      rest,
      normalize_token,
      FUN.VALUE = character(1),
      USE.NAMES = FALSE
    )

    parts <- character(0)
    genus_md <- italic_md(genus)

    if (!is.null(hybrid_marker) && marker_attached) {
      parts <- c(parts, paste0(hybrid_marker, genus_md))
    } else {
      if (!is.null(hybrid_marker)) {
        parts <- c(parts, hybrid_marker)
      }
      parts <- c(parts, genus_md)
    }

    while (length(rest) >= 1 && rest[1] %in% id_qualifiers) {
      parts <- c(parts, rest[1])
      rest <- rest[-1]
    }

    if (
      length(rest) >= 1 &&
      (rest[1] %in% sp_tokens || is_sp_number_token(rest[1]))
    ) {
      parts <- c(parts, rest[1])
      if (length(rest) >= 2) {
        parts <- c(parts, rest[-1])
      }
      return(paste(parts, collapse = " "))
    }

    if (length(rest) >= 1) {
      parts <- c(parts, italic_md(rest[1]))
      rest <- rest[-1]
    } else {
      return(paste(parts, collapse = " "))
    }

    i <- 1
    while (i <= length(rest)) {
      tok <- rest[i]
      if (tok %in% rank_tokens) {
        parts <- c(parts, tok)
        if (i < length(rest)) {
          parts <- c(parts, italic_md(rest[i + 1]))
          i <- i + 2
        } else {
          i <- i + 1
        }
        next
      }

      parts <- c(parts, tok)
      i <- i + 1
    }

    paste(parts, collapse = " ")
  }

  style_one <- function(name) {
    s_raw <- as.character(name)

    if (is.na(s_raw) || s_raw == "") {
      return("")
    }
    if (.is_numeric_like_label(s_raw)) {
      return(s_raw)
    }

    s_norm <- stringr::str_squish(gsub("_+", " ", s_raw))
    if (s_norm == "") {
      return("")
    }

    sep_re <- "\\s+(×|x|X)\\s+"
    matched <- stringr::str_match(s_norm, paste0("^(.*)", sep_re, "(.*)$"))
    if (!all(is.na(matched))) {
      left_raw <- stringr::str_squish(matched[1, 2])
      right_raw <- stringr::str_squish(matched[1, 4])

      left_md <- style_one_simple(left_raw)
      right_md <- style_one_simple(right_raw)
      return(paste(left_md, "×", right_md))
    }

    style_one_simple(s_norm)
  }

  vapply(x, style_one, FUN.VALUE = character(1), USE.NAMES = FALSE)
}

#' Style tree labels into Markdown
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' Scientific-name parts are styled by [style_sciname()].
#' Empty strings and numeric-like strings (for example, `"100"` and `"100/98"`)
#' are returned unchanged.
#' If a trailing token should be treated as a taxon name, its rank can be
#' given with `last_taxon_rank`.
#'
#' @param x Character vector of labels.
#' @param last_taxon_rank Character scalar indicating the rank of the final
#'   token (for example, `"genus"`, `"species"`, `"family"`). Use
#'   `NA_character_` or `""` to disable this behavior.
#'
#' @return Character vector of Markdown-styled labels.
#'
#' @examples
#' x <- c(
#'   "Coprinellus-aureogranulatus_GQ249274.1",
#'   "mycobiont_of_Cremastra-aphylla_(OrM)_LC026052",
#'   "mycobiont_of_Cremastra-aphylla_(OrM)_OTU1",
#'   "mycobiont_of_Cremastra-aphylla_(OrM)_OTU1_Psathyrella",
#'   "mycobiont_of_Cremastra-aphylla_(OrM)_OTU1_Psathyrellaceae",
#'   "",
#'   "100",
#'   "100/98"
#' )
#'
#' style_tree_label(x, last_taxon_rank = "genus")
#' style_tree_label(x, last_taxon_rank = "family")
#' @export
style_tree_label <- function(x, last_taxon_rank = NA_character_) {
  if (length(last_taxon_rank) != 1) {
    stop("`last_taxon_rank` must be a character scalar or `NA_character_`.")
  }

  x <- as.character(x)
  last_taxon_rank <- as.character(last_taxon_rank)

  to_space_sciname <- function(s) {
    gsub("-", " ", s, fixed = TRUE)
  }

  is_accession <- function(tok) {
    grepl("^[A-Z]{1,3}\\d{5,}(?:\\.\\d+)?$", tok)
  }

  is_otu <- function(tok) {
    grepl("^OTU\\d+$", tok, ignore.case = TRUE)
  }

  should_handle_last_taxon <- function(rank) {
    !is.na(rank) && nzchar(trimws(rank))
  }

  style_last_taxon <- function(tok, rank) {
    if (rank %in% c("genus", "species")) {
      return(style_sciname(to_space_sciname(tok)))
    }

    tok
  }

  style_one <- function(s_raw) {
    if (is.na(s_raw) || s_raw == "") {
      return("")
    }
    if (.is_numeric_like_label(s_raw)) {
      return(s_raw)
    }

    s_norm <- stringr::str_squish(s_raw)
    if (s_norm == "") {
      return("")
    }

    toks <- stringr::str_split(s_norm, "_")[[1]]
    if (length(toks) == 0) {
      return("")
    }

    if (length(toks) >= 3 && toks[1] == "mycobiont" && toks[2] == "of") {
      host_raw <- toks[3]
      host_md <- style_sciname(to_space_sciname(host_raw))
      if (length(toks) >= 4) {
        remaining <- toks[4:length(toks)]
      } else {
        remaining <- character(0)
      }

      last_tok <- NULL
      if (should_handle_last_taxon(last_taxon_rank) && length(remaining) >= 1) {
        cand <- remaining[length(remaining)]
        if (!is_accession(cand) &&
            !is_otu(cand) &&
            !grepl("^\\(.*\\)$", cand)) {
          last_tok <- cand
          remaining <- remaining[-length(remaining)]
        }
      }

      parts <- c("mycobiont of", host_md)
      if (length(remaining) > 0) {
        parts <- c(parts, remaining)
      }
      if (!is.null(last_tok)) {
        parts <- c(parts, style_last_taxon(last_tok, last_taxon_rank))
      }

      return(paste(parts, collapse = " "))
    }

    sciname_raw <- toks[1]
    if (length(toks) >= 2) {
      rest <- toks[-1]
    } else {
      rest <- character(0)
    }

    sciname_md <- style_sciname(to_space_sciname(sciname_raw))
    if (length(rest) == 0) {
      return(sciname_md)
    }

    last_tok <- NULL
    if (should_handle_last_taxon(last_taxon_rank) && length(rest) >= 2) {
      cand <- rest[length(rest)]
      if (!is_accession(cand) &&
          !is_otu(cand) &&
          !grepl("^\\(.*\\)$", cand)) {
        last_tok <- cand
        rest <- rest[-length(rest)]
      }
    }

    out_parts <- c(sciname_md, rest)
    if (!is.null(last_tok)) {
      out_parts <- c(out_parts, style_last_taxon(last_tok, last_taxon_rank))
    }

    paste(out_parts, collapse = " ")
  }

  vapply(x, style_one, FUN.VALUE = character(1), USE.NAMES = FALSE)
}

#' Format node support labels for plotting
#'
#' Parses node-support labels and applies cutoffs to suppress weak values.
#'
#' For two-value labels (`"SH/UFBoot"`), each side is compared against its
#' cutoff and weak values are replaced by `missing_mark`. If both sides are
#' weak, an empty string is returned.
#'
#' For one-value labels, the value can be interpreted as either UFBoot
#' (`single_value = "ufboot"`) or SH-aLRT (`single_value = "sh_alrt"`).
#'
#' @param x Character vector of support labels.
#' @param sh_alrt_cutoff Numeric scalar cutoff for SH-aLRT.
#' @param boot_cutoff Numeric scalar cutoff for UFBoot.
#' @param sep Separator between SH-aLRT and UFBoot values.
#' @param missing_mark Replacement marker for suppressed values.
#' @param single_value Interpretation of single-value labels.
#' @param sig_digits Number of significant digits used for output formatting.
#' @param keep_na Logical scalar. If `TRUE`, keep `NA` as `NA`.
#'
#' @return Character vector of formatted support labels.
#'
#' @examples
#' x <- c("66.6/59", "90/94", "70/96", "85/97", "55", "90", NA, "")
#' format_node_support(x)
#'
#' format_node_support("95", boot_cutoff = 90)
#' @export
format_node_support <- function(
  x,
  sh_alrt_cutoff = 80,
  boot_cutoff = 95,
  sep = "/",
  missing_mark = "—",
  single_value = c("ufboot", "sh_alrt"),
  sig_digits = 3,
  keep_na = TRUE
) {
  single_value <- match.arg(single_value)

  if (!is.numeric(sh_alrt_cutoff) ||
      length(sh_alrt_cutoff) != 1 ||
      !is.finite(sh_alrt_cutoff)) {
    stop("`sh_alrt_cutoff` must be a finite numeric scalar.")
  }
  if (!is.numeric(boot_cutoff) ||
      length(boot_cutoff) != 1 ||
      !is.finite(boot_cutoff)) {
    stop("`boot_cutoff` must be a finite numeric scalar.")
  }
  if (!is.character(sep) || length(sep) != 1 || is.na(sep)) {
    stop("`sep` must be a non-missing character scalar.")
  }
  if (!is.character(missing_mark) ||
      length(missing_mark) != 1 ||
      is.na(missing_mark)) {
    stop("`missing_mark` must be a non-missing character scalar.")
  }
  if (!is.numeric(sig_digits) ||
      length(sig_digits) != 1 ||
      is.na(sig_digits) ||
      sig_digits < 1) {
    stop("`sig_digits` must be a positive numeric scalar.")
  }
  if (!is.logical(keep_na) || length(keep_na) != 1 || is.na(keep_na)) {
    stop("`keep_na` must be a single TRUE/FALSE value.")
  }

  x_chr <- as.character(x)
  out <- rep("", length(x_chr))
  if (keep_na) {
    out[is.na(x_chr)] <- NA_character_
  }

  idx <- which(!is.na(x_chr) & nzchar(x_chr))
  if (length(idx) == 0) {
    return(out)
  }

  x_work <- x_chr[idx]
  has_sep <- grepl(sep, x_work, fixed = TRUE)

  parts <- strsplit(x_work, split = sep, fixed = TRUE)
  left <- vapply(parts, function(z) z[1], character(1))
  right <- vapply(
    parts,
    function(z) {
      if (length(z) >= 2) {
        return(z[2])
      }

      NA_character_
    },
    character(1)
  )

  sh <- suppressWarnings(as.numeric(left))
  uf <- suppressWarnings(as.numeric(right))

  if (single_value == "ufboot") {
    uf[!has_sep] <- sh[!has_sep]
    sh[!has_sep] <- NA_real_
  }

  fmt_num <- function(z) {
    s <- rep(NA_character_, length(z))
    ok <- !is.na(z)
    if (!any(ok)) {
      return(s)
    }

    s[ok] <- format(
      signif(z[ok], sig_digits),
      digits = sig_digits,
      scientific = FALSE,
      trim = TRUE
    )

    has_dot <- grepl("\\.", s[ok])
    s_ok <- s[ok]
    s_ok[has_dot] <- sub("(\\.[0-9]*?)0+$", "\\1", s_ok[has_dot])
    s_ok[has_dot] <- sub("\\.$", "", s_ok[has_dot])
    s[ok] <- s_ok

    s
  }

  sh_s <- fmt_num(sh)
  uf_s <- fmt_num(uf)

  both_present <- !is.na(sh) & !is.na(uf)
  only_sh <- !is.na(sh) & is.na(uf)
  only_uf <- is.na(sh) & !is.na(uf)

  single_only_sh <- only_sh & !has_sep
  single_only_uf <- only_uf & !has_sep

  res <- rep("", length(x_work))

  idx_both_low <- both_present & sh < sh_alrt_cutoff & uf < boot_cutoff
  res[idx_both_low] <- ""

  idx_sh_low_uf_high <- both_present & sh < sh_alrt_cutoff & uf >= boot_cutoff
  res[idx_sh_low_uf_high] <- paste0(missing_mark, sep, uf_s[idx_sh_low_uf_high])

  idx_sh_high_uf_low <- both_present & sh >= sh_alrt_cutoff & uf < boot_cutoff
  res[idx_sh_high_uf_low] <- paste0(sh_s[idx_sh_high_uf_low], sep, missing_mark)

  idx_both_high <- both_present & sh >= sh_alrt_cutoff & uf >= boot_cutoff
  res[idx_both_high] <- paste0(sh_s[idx_both_high], sep, uf_s[idx_both_high])

  idx_single_uf_low <- single_only_uf & uf < boot_cutoff
  res[idx_single_uf_low] <- ""

  idx_single_uf_high <- single_only_uf & uf >= boot_cutoff
  res[idx_single_uf_high] <- uf_s[idx_single_uf_high]

  idx_single_sh_low <- single_only_sh & sh < sh_alrt_cutoff
  res[idx_single_sh_low] <- ""

  idx_single_sh_high <- single_only_sh & sh >= sh_alrt_cutoff
  res[idx_single_sh_high] <- sh_s[idx_single_sh_high]

  idx_only_uf_sep <- only_uf & has_sep & uf >= boot_cutoff
  res[idx_only_uf_sep] <- paste0(missing_mark, sep, uf_s[idx_only_uf_sep])

  idx_only_sh_sep <- only_sh & has_sep & sh >= sh_alrt_cutoff
  res[idx_only_sh_sep] <- paste0(sh_s[idx_only_sh_sep], sep, missing_mark)

  out[idx] <- res
  out
}
