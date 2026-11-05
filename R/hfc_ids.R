# =============================================================================
#  hfc_ids : duplicates in the survey id
# =============================================================================

#' Stata `x[a] != x[b]` element-wise for two equally long slices of a column.
#' Numeric: missing values compare equal only when they carry the same tag
#' (. == ., .d == .d, . != .d). Strings: exact comparison (NA treated as "").
.hfc_ids_neq <- function(a, b) {
  if (is.character(a) || is.character(b)) {
    a <- as.character(a); b <- as.character(b); a[is.na(a)] <- ""; b[is.na(b)] <- ""
    return(a != b)
  }
  a <- as.vector(unclass(a)); b <- as.vector(unclass(b)); attributes(a) <- NULL; attributes(b) <- NULL
  a <- as.double(a); b <- as.double(b)
  na_a <- is.na(a); na_b <- is.na(b)
  ta <- ifelse(na_a, haven::na_tag(a), NA_character_); tb <- ifelse(na_b, haven::na_tag(b), NA_character_)
  ta[na_a & is.na(ta)] <- "."; tb[na_b & is.na(tb)] <- "."
  out <- logical(length(a))
  both <- !na_a & !na_b
  out[both] <- a[both] != b[both]
  out[na_a != na_b] <- TRUE
  bm <- na_a & na_b
  out[bm] <- ta[bm] != tb[bm]
  out
}

#' Row subset keeping all column attributes (variable/value labels, formats)
.hfc_ids_rows <- function(df, rows) {
  if (is.logical(rows)) rows <- which(rows)
  cols <- lapply(df, function(x) { att <- attributes(x); att$names <- NULL; y <- unclass(x)[rows]; attributes(y) <- att; y })
  structure(cols, names = names(df), class = "data.frame", row.names = seq_along(rows))
}

#' Stable grouping key for a column (Stata sort semantics: missing last)
.hfc_ids_key <- function(x) {
  if (is.character(x)) { x[is.na(x)] <- ""; return(x) }
  hfc_num(x)
}

#' Check for duplicates in the survey id
#'
#' Port of Stata `hfc_ids`. Lists every observation whose id is duplicated,
#' with a serial number (within id, ordered by date), the number of variables
#' compared and the number/percent of variables whose values differ from the
#' first submission of that id (admin vars id, date, key excluded). A thin
#' bottom border separates id groups.
#'
#' Optional: `dupfile` saves the duplicate observations (date already converted
#' to a daily date, as in Stata) and `save` (requires `force = TRUE`) saves a
#' de-duplicated copy of the full data, keeping the first occurrence of each id
#' (Stata `duplicates drop id, force`).
#'
#' @param data        data.frame
#' @param varname     survey id variable
#' @param enumerator  enumerator variable
#' @param date        date/datetime variable
#' @param key         unique key variable
#' @param outfile     output .xlsx
#' @param outsheet    sheet (default "id duplicates")
#' @param dupfile     optional .dta/.rds path for duplicate observations
#' @param save        optional .dta/.rds path for de-duplicated dataset
#' @param keep        additional variables to show
#' @param sheetmodify,sheetreplace  export options (sheet always replaced in R)
#' @param nolabel     export values instead of value labels
#' @param replace     accepted for compatibility (files are always overwritten)
#' @param force       required with `save`
#' @return the data invisibly: de-duplicated when `save` is given (that is what
#'   the Stata pipeline reloads), unchanged otherwise; attr(, "r") =
#'   list(N_dups, N_pairs).
#' @examples
#' df <- hfc_ids(df, "hhid", enumerator = "a_enum_id", date = "starttime", key = "key",
#'                   outfile = "hfc_output.xlsx", dupfile = "survey_duplicates.dta",
#'                   save = "household_survey_checked.dta", force = TRUE)
hfc_ids <- function(data, varname, enumerator, date, key, outfile, outsheet = NULL,
                        dupfile = NULL, save = NULL, keep = NULL, sheetmodify = FALSE,
                        sheetreplace = TRUE, nolabel = FALSE, replace = FALSE, force = FALSE) {
  data <- as.data.frame(data, stringsAsFactors = FALSE)
  id <- hfc_vars(data, varname); enumerator <- hfc_vars(data, enumerator)
  date <- hfc_vars(data, date); key <- hfc_vars(data, key)
  keepv <- if (!is.null(keep) && length(hfc_tokens(keep))) hfc_vars(data, keep) else character(0)

  result <- data
  # save a de-duplicated dataset
  if (!is.null(save) && nzchar(save)) {
    if (!force) hfc_stop("force option required with save() option")
    k <- .hfc_ids_key(data[[id]])
    keep_first <- !duplicated(k)
    # duplicates drop keeps the first obs in current sort order and leaves the
    # data sorted by the varlist
    dd <- .hfc_ids_rows(data, keep_first)
    dd <- .hfc_ids_rows(dd, order(.hfc_ids_key(dd[[id]]), method = "radix"))
    hfc_save(dd, save)
    result <- dd
  }

  d <- data
  d[[date]] <- hfc_to_date(d[[date]])
  outsheet <- outsheet %||% "id duplicates"

  k <- .hfc_ids_key(d[[id]])
  isdup <- k %in% k[duplicated(k)]
  d <- .hfc_ids_rows(d, isdup)

  n_dups <- nrow(d); n_pairs <- 0L
  if (n_dups == 0) {
    message(sprintf("There are no duplicates of %s in the data.", id))
  } else {
    if (!is.null(dupfile) && nzchar(dupfile)) hfc_save(d, dupfile)

    # bysort id (date): serial
    kk <- .hfc_ids_key(d[[id]]); dn <- as.numeric(d[[date]])
    o <- order(kk, dn, seq_len(nrow(d)), method = "radix", na.last = TRUE)
    d <- .hfc_ids_rows(d, o)
    kk <- kk[o]
    serial <- stats::ave(seq_along(kk), kk, FUN = seq_along)

    compvars <- setdiff(names(d), c(id, date, key))
    compared <- rep(length(compvars), nrow(d))

    first <- match(kk, kk)                         # index of first row of each id
    later <- which(seq_len(nrow(d)) != first)
    diffs <- numeric(nrow(d))
    for (v in compvars) {
      x <- d[[v]]
      diffs[later] <- diffs[later] + .hfc_ids_neq(x[first[later]], x[later])
    }
    perc <- diffs / compared
    # recode diffs perc (0 = .) ; compared = . if missing(diffs)
    perc[diffs == 0] <- NA
    diffs[diffs == 0] <- NA
    compared[is.na(diffs)] <- NA

    # keep/order: enumerator keep serial date key id compared diffs perc
    out <- d[, unique(c(enumerator, keepv)), drop = FALSE]
    out$serial <- serial
    for (v in unique(c(date, key, id))) if (!(v %in% names(out))) out[[v]] <- d[[v]]
    out <- out[, unique(c(enumerator, keepv, "serial", date, key, id)), drop = FALSE]
    out$.compared <- compared; out$.diffs <- diffs; out$.perc <- perc
    # all variable labels removed -> headers are variable names
    for (v in names(out)) attr(out[[v]], "label") <- NULL
    attr(out$.compared, "label") <- "# compared"
    attr(out$.diffs, "label") <- "# different"
    attr(out$.perc, "label") <- "% different"
    attr(out$serial, "label") <- "serial"

    if (length(keepv)) out <- hfc_labels(out, keepv, nolabel = nolabel)
    out <- hfc_labels(out, unique(c(id, key, enumerator)), nolabel = nolabel)
    # export excel exports remaining value labels as text unless nolabel
    if (!nolabel) out <- hfc_labels(out, names(out))

    wb <- hfc_write_sheet(outfile, outsheet, out, header = "label",
                          formats = list(percent_d2 = ".perc", number_sep = c(".diffs", ".compared"),
                                         date_d_mon_yy = date), save = FALSE)
    # separator line under the last row of each id group
    cnt <- stats::ave(seq_along(kk), kk, FUN = length)
    rows <- which(serial == cnt) + 1
    if (length(rows)) openxlsx::addStyle(wb, outsheet, openxlsx::createStyle(border = "bottom", borderStyle = "thin"),
                                         rows = rows, cols = seq_len(ncol(out)), gridExpand = TRUE, stack = TRUE)
    openxlsx::saveWorkbook(wb, outfile, overwrite = TRUE)

    n_pairs <- length(unique(kk))
    message(sprintf("Found %d duplicates observations in %d duplicate pairs.", n_dups, n_pairs))
  }

  attr(result, "r") <- list(N_dups = n_dups, N_pairs = n_pairs)
  invisible(result)
}
