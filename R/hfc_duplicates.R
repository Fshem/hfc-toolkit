# =============================================================================
#  hfc_duplicates : duplicates in non-id variables
# =============================================================================

#' Stata string() of a numeric vector (%9.0g display; "." for missing)
.hfc_duplicates_string <- function(x) {
  if (hfc_is_date(x) || hfc_is_datetime(x)) x <- hfc_num(x)
  xv <- as.vector(unclass(x)); attributes(xv) <- NULL; xv <- as.double(xv)
  out <- rep(".", length(xv))
  tag <- haven::na_tag(xv)
  out[is.na(xv) & !is.na(tag)] <- paste0(".", tag[is.na(xv) & !is.na(tag)])
  ok <- !is.na(xv)
  if (any(ok)) {
    s <- sprintf("%.7g", xv[ok])
    e <- grepl("e", s); s[e] <- sprintf("%.2e", xv[ok][e])
    out[ok] <- sub("^(-?)0\\.", "\\1.", s)
  }
  out
}

#' Check variables for duplicate values
#'
#' Port of Stata `hfc_duplicates`. For each variable in `varlist` (e.g. phone
#' numbers, GPS), finds non-missing values shared by more than one observation
#' and lists them in long format (one row per observation x variable), sorted
#' by variable, value and id, with a serial number within each duplicate group
#' and a thin bottom border after each group.
#'
#' @param data        data.frame
#' @param varlist     variables to check (Stata varlist string)
#' @param id          id variable (must be unique)
#' @param enumerator  enumerator variable
#' @param date        date/datetime variable
#' @param outfile     output .xlsx
#' @param outsheet    sheet (default "duplicates")
#' @param keep        additional variables to show
#' @param sheetmodify,sheetreplace  export options (sheet always replaced in R)
#' @param nolabel     export values instead of value labels
#' @return invisible list(N_obs, N_vars)
#' @examples
#' hfc_duplicates(df, "gpslatitude gpslongitude", id = "hhid", enumerator = "a_enum_id",
#'              date = "starttime", outfile = "hfc_output.xlsx", sheetreplace = TRUE)
hfc_duplicates <- function(data, varlist, id, enumerator, date, outfile, outsheet = NULL,
                         keep = NULL, sheetmodify = FALSE, sheetreplace = TRUE, nolabel = FALSE) {
  data <- as.data.frame(data, stringsAsFactors = FALSE)
  vars <- hfc_vars(data, varlist)
  id <- hfc_vars(data, id); enumerator <- hfc_vars(data, enumerator); date <- hfc_vars(data, date)
  keepv <- if (!is.null(keep) && length(hfc_tokens(keep))) hfc_vars(data, keep) else character(0)

  if (anyDuplicated(data[[id]])) hfc_stop("id() variable %s does not uniquely identify the observations", id)
  outsheet <- outsheet %||% "duplicates"

  d <- data
  d[[date]] <- hfc_to_date(d[[date]])

  varwithdups <- character(0)
  check <- rep(FALSE, nrow(d))
  for (v in vars) {
    x <- d[[v]]
    nm <- !hfc_missing(x)
    if (!any(nm)) next
    k <- if (is.character(x)) x else hfc_num(x)
    flag <- nm & k %in% k[nm][duplicated(k[nm])]
    check <- check | flag
    if (any(flag)) varwithdups <- c(varwithdups, v)
  }

  if (any(check)) {
    sub <- which(check)
    rows <- list()
    for (v in varwithdups) {
      x <- d[[v]]
      val <- if (hfc_is_num(x) || hfc_is_date(x) || hfc_is_datetime(x)) .hfc_duplicates_string(x[sub]) else { z <- x[sub]; z[is.na(z)] <- ""; z }
      # NOTE(stata-bug): for numeric vars, string() gives "." for missing which is not
      # dropped by `drop if missing(_v)`; when 2+ retained obs have the var missing,
      # value "." is then reported as a duplicate group. Kept faithfully.
      # NOTE(stata-bug): the numeric-id branch uses `forval i = 1/1(`vl_cnt')`obs_cnt'`,
      # an invalid range; R implements the intended reshape for both id types.
      keepr <- val != ""
      if (!any(keepr)) next
      r <- data.frame(.row = sub[keepr], stringsAsFactors = FALSE)
      r$.variable <- v
      r$.value <- val[keepr]
      r$.label <- attr(x, "label", exact = TRUE) %||% ""
      rows[[length(rows) + 1]] <- r
    }
    long <- do.call(rbind, rows)
    idk <- if (is.character(d[[id]])) d[[id]][long$.row] else hfc_num(d[[id]])[long$.row]
    long <- long[order(long$.variable, long$.value, idk, method = "radix"), , drop = FALSE]
    idk <- if (is.character(d[[id]])) d[[id]][long$.row] else hfc_num(d[[id]])[long$.row]
    grp <- paste(long$.variable, long$.value, sep = "\r")
    # drop if not duplicated within variable/value
    dupd <- grp %in% grp[duplicated(grp)]
    long <- long[dupd, , drop = FALSE]; grp <- grp[dupd]
    long$serial <- stats::ave(seq_along(grp), grp, FUN = seq_along)
    maxs <- stats::ave(seq_along(grp), grp, FUN = length)
    sep_rows <- which(long$serial == maxs) + 1

    take <- function(v) { x <- d[[v]]; att <- attributes(x); att$names <- NULL; y <- unclass(x)[long$.row]; attributes(y) <- att; attr(y, "label") <- NULL; y }
    out <- data.frame(serial = long$serial)
    for (v in unique(c(date, id, enumerator))) out[[v]] <- take(v)
    out$variable <- long$.variable
    out$label <- long$.label
    out$value <- long$.value
    for (v in keepv) if (!(v %in% names(out))) out[[v]] <- take(v)
    out <- out[, unique(c("serial", date, id, enumerator, "variable", "label", "value", keepv)), drop = FALSE]
    # tmp var labels; all others blank -> header = name
    attr(out$serial, "label") <- "serial"
    attr(out$variable, "label") <- "variable"
    attr(out$label, "label") <- "label"
    attr(out$value, "label") <- "value"

    if (length(keepv)) out <- hfc_labels(out, keepv, nolabel = nolabel)
    out <- hfc_labels(out, unique(c(id, enumerator)), nolabel = nolabel)
    if (!nolabel) out <- hfc_labels(out, names(out))

    if (nrow(out) > 0) {
      wb <- hfc_write_sheet(outfile, outsheet, out, header = "label", save = FALSE)
      openxlsx::addStyle(wb, outsheet, openxlsx::createStyle(border = "bottom", borderStyle = "thin"),
                         rows = sep_rows, cols = seq_len(ncol(out)), gridExpand = TRUE, stack = TRUE)
      openxlsx::saveWorkbook(wb, outfile, overwrite = TRUE)
    }
    var_cnt <- length(unique(out$variable)); obs_cnt <- nrow(out)
    message(sprintf("Found %d duplicates in %d variable(s).", obs_cnt, var_cnt))
  } else {
    var_cnt <- 0L; obs_cnt <- 0L
    message("Found 0 duplicates.")
  }
  invisible(list(N_obs = obs_cnt, N_vars = var_cnt))
}
