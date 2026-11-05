# =============================================================================
#  hfc_versions : submissions per form version & outdated submissions
# =============================================================================

#' Check form versions
#'
#' Port of Stata `hfc_versions`. Writes a table with, for each form version,
#' the number of submissions, the number of submissions made on/after the first
#' day of the latest version ("outdated"), and first/last submission dates,
#' plus a Total row. Versions whose last date equals the last date of the latest
#' version are highlighted light pink (still in use). If any outdated
#' submissions exist they are listed in `outsheet2`.
#'
#' @param data        data.frame
#' @param varname     numeric form-version variable (no missing allowed)
#' @param enumerator  enumerator variable
#' @param date        date or datetime variable
#' @param outfile     output .xlsx
#' @param outsheet1   summary sheet (default "form versions")
#' @param outsheet2   outdated list sheet (default "outdated")
#' @param keep        additional variables for outsheet2
#' @param sheetmodify,sheetreplace  export options (sheet always replaced in R)
#' @param nolabel     export values instead of value labels
#' @return invisible list(N_versions, N_outdated)
#' @examples
#' hfc_versions(df, "formdef_version", enumerator = "a_enum_id", date = "starttime",
#'                  outfile = "hfc_output.xlsx", sheetreplace = TRUE)
hfc_versions <- function(data, varname, enumerator, date, outfile, outsheet1 = NULL,
                             outsheet2 = NULL, keep = NULL, sheetmodify = FALSE,
                             sheetreplace = TRUE, nolabel = FALSE) {
  data <- as.data.frame(data, stringsAsFactors = FALSE)
  varname <- hfc_vars(data, varname); enumerator <- hfc_vars(data, enumerator); date <- hfc_vars(data, date)
  keepv <- if (!is.null(keep) && length(hfc_tokens(keep))) hfc_vars(data, keep) else character(0)

  ver <- hfc_num(data[[varname]])
  if (any(is.na(ver))) hfc_stop("variable %s has %d missing values. Version variable should not contain missing values",
                                varname, sum(is.na(ver)))
  outsheet1 <- outsheet1 %||% "form versions"
  outsheet2 <- outsheet2 %||% "outdated"

  data[[date]] <- hfc_to_date(data[[date]])
  dnum <- as.numeric(data[[date]])

  curr_ver <- max(ver)
  curr_ver_fdate <- hfc_min(dnum[ver == curr_ver])
  vers <- sort(unique(ver))

  ge <- function(a, b) is.na(a) | (!is.na(b) & a >= b)   # Stata: missing date >= anything
  out <- data.frame(v = character(0), submitted = numeric(0), outdated = numeric(0),
                    first_date = numeric(0), last_date = numeric(0), stringsAsFactors = FALSE)
  for (v in vers) {
    sel <- ver == v
    out[nrow(out) + 1, ] <- list(substr(format(v, scientific = FALSE, trim = TRUE), 1, 10), sum(sel),
                                 sum(sel & ge(dnum, curr_ver_fdate)),
                                 hfc_min(dnum[sel]), hfc_max(dnum[sel]))
  }
  outdated <- sum(ge(dnum, curr_ver_fdate) & ver != curr_ver)
  out[nrow(out) + 1, ] <- list("Total", nrow(data), outdated, NA, NA)

  curr_lab <- format(curr_ver, scientific = FALSE, trim = TRUE)
  out$outdated[out$v == curr_lab] <- NA
  names(out)[1] <- varname
  out$first_date <- as.Date(out$first_date, origin = "1970-01-01")
  out$last_date  <- as.Date(out$last_date, origin = "1970-01-01")
  attr(out[[varname]], "label") <- varname
  attr(out$submitted, "label")  <- "# submitted"
  attr(out$outdated, "label")   <- "# outdated"
  attr(out$first_date, "label") <- "first date"
  attr(out$last_date, "label")  <- "last date"

  wb <- hfc_write_sheet(outfile, outsheet1, out, header = "label",
                        formats = list(date_d_mon_yy = c("first_date", "last_date"),
                                       number_sep = c("submitted", "outdated")),
                        total_row = TRUE, save = FALSE)
  # highlight versions still in use: rows whose last_date == last_date of the latest version
  n <- nrow(out)
  lastdate <- out$last_date[n - 1]
  rows <- which(!is.na(out$last_date) & out$last_date == lastdate & seq_len(n) != n - 1)
  if (length(rows)) hfc_cell_style(wb, outsheet1, rows = rows + 1, cols = match("last_date", names(out)),
                                   fill = "#FFB6C1")
  openxlsx::saveWorkbook(wb, outfile, overwrite = TRUE)

  if (outdated > 0) {
    sel <- ver != curr_ver & ge(dnum, curr_ver_fdate)
    od <- data[sel, intersect(names(data), unique(c(date, enumerator, keepv, varname))), drop = FALSE]
    for (v in names(od)) attr(od[[v]], "label") <- v
    if (length(keepv)) od <- hfc_labels(od, keepv, nolabel = nolabel)
    od <- hfc_labels(od, enumerator, nolabel = nolabel)
    hfc_write_sheet(outfile, outsheet2, od, header = "label",
                    formats = list(date_d_mon_yy = date))
  }

  message(sprintf("Found %d submissions with outdated forms.", outdated))
  invisible(list(N_versions = length(vers), N_outdated = outdated))
}
