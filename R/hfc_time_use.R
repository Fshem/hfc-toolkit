# =============================================================================
#  hfc_time_use : active survey hours heatmap from SurveyCTO text audits
#  routine format_timeuse()
# =============================================================================
# Requires the shared front-end helpers in R/hfc_text_audit.R
# (.hfc_text_audit_merge, .hfc_text_audit_split, .hfc_text_audit_var).

#' Stata xl() rgb "r g b" -> "#RRGGBB"
.hfc_time_use_rgb <- function(r, g, b) sprintf("#%02X%02X%02X", as.integer(r), as.integer(g), as.integer(b))

#' Port of Mata format_timeuse(): writes the heatmap formatting on `sheet`.
#'  first : vector exported in column B from row 4 (dates or enumerators)
#'  hh    : numeric matrix nobs x 24 (hour counts, 0 recoded to NA)
.hfc_time_use_format <- function(wb, sheet, first, hh, title, fmtdate) {
  nobs <- nrow(hh)
  # colwidth = max(strlen(st_sdata(., 1))) : numeric first column -> 0
  colwidth <- if (is.character(first)) max(c(0, nchar(first[!is.na(first)]))) else 0
  if (colwidth == 0) {
    colwidth <- 10
    if (fmtdate) openxlsx::addStyle(wb, sheet, openxlsx::createStyle(numFmt = "d-mmm-yy"),
                                    rows = 4:(nobs + 4), cols = 2, stack = TRUE)
  }
  openxlsx::showGridLines(wb, sheet, showGridLines = FALSE)
  openxlsx::setColWidths(wb, sheet, cols = 2, widths = colwidth + 3)
  openxlsx::setColWidths(wb, sheet, cols = 3:26, widths = 3)

  # hour labels below the data, rotated 90 degrees, top aligned
  labs <- vapply(0:23, function(i) {
    if (i == 0) "Midnight" else if (i <= 11) paste(i, "AM") else if (i == 12) "12 Noon" else paste(i - 12, "PM")
  }, character(1))
  openxlsx::writeData(wb, sheet, t(labs), startRow = nobs + 5, startCol = 3, colNames = FALSE)
  openxlsx::addStyle(wb, sheet, openxlsx::createStyle(textRotation = 90, valign = "top"),
                     rows = nobs + 5, cols = 3:26, stack = TRUE)

  vmin <- suppressWarnings(min(hh, na.rm = TRUE))
  vmax <- suppressWarnings(max(hh, na.rm = TRUE))
  if (!is.finite(vmin)) vmin <- NA
  if (!is.finite(vmax)) vmax <- NA
  max_r <- 209; max_g <- 200; max_b <- 162
  min_r <- 11;  min_g <- 59;  min_b <- 79
  colormax <- if (!is.na(vmax) && vmax > 20) 20 else vmax

  # heatmap cell fills (one style per distinct colour)
  if (!is.na(vmax)) {
    cols_rgb <- matrix(NA_character_, nobs, 24)
    for (i in 1:24) for (j in seq_len(nobs)) {
      value <- hh[j, i]
      if (!is.na(value)) {
        cols_rgb[j, i] <- .hfc_time_use_rgb(max_r - floor(value / vmax * (max_r - min_r)),
                                               max_g - floor(value / vmax * (max_g - min_g)),
                                               max_b - floor(value / vmax * (max_b - min_b)))
      }
    }
    for (cl in unique(stats::na.omit(as.vector(cols_rgb)))) {
      idx <- which(cols_rgb == cl, arr.ind = TRUE)
      openxlsx::addStyle(wb, sheet, openxlsx::createStyle(fgFill = cl),
                         rows = idx[, 1] + 3, cols = idx[, 2] + 2, gridExpand = FALSE, stack = TRUE)
    }
  }

  # scale legend
  openxlsx::writeData(wb, sheet, "Scale", startRow = 4, startCol = 28, colNames = FALSE)
  openxlsx::writeData(wb, sheet, vmin, startRow = 6, startCol = 29, colNames = FALSE)
  if (!is.na(colormax) && colormax >= 1) for (i in 1:colormax) {
    cl <- .hfc_time_use_rgb(max_r - floor(i / colormax * (max_r - min_r)),
                               max_g - floor(i / colormax * (max_g - min_g)),
                               max_b - floor(i / colormax * (max_b - min_b)))
    openxlsx::addStyle(wb, sheet, openxlsx::createStyle(fgFill = cl), rows = 5 + i, cols = 28, stack = TRUE)
  }
  if (!is.na(colormax)) openxlsx::writeData(wb, sheet, vmax, startRow = 5 + colormax, startCol = 29, colNames = FALSE)
  openxlsx::setColWidths(wb, sheet, cols = 28:29, widths = 5)

  # title, merged C2:AA2, centred bold italic
  openxlsx::writeData(wb, sheet, title, startRow = 2, startCol = 3, colNames = FALSE)
  openxlsx::mergeCells(wb, sheet, cols = 3:27, rows = 2)
  openxlsx::addStyle(wb, sheet, openxlsx::createStyle(halign = "center", textDecoration = c("bold", "italic")),
                     rows = 2, cols = 3:27, stack = TRUE)
  invisible(wb)
}

#' collapse (sum) hh = 1, by(key hour); reshape wide; fill hours 0-23;
#' recode 0 = .  Returns list(keys, hh matrix) with keys sorted.
.hfc_time_use_table <- function(key, hour) {
  keys <- sort(unique(key), method = "radix", na.last = TRUE)
  hh <- matrix(NA_real_, length(keys), 24)
  ki <- match(key, keys)
  for (r in seq_along(key)) hh[ki[r], hour[r] + 1] <- (if (is.na(hh[ki[r], hour[r] + 1])) 0 else hh[ki[r], hour[r] + 1]) + 1
  hh[!is.na(hh) & hh == 0] <- NA
  list(keys = keys, hh = hh)
}

#' Write a heatmap sheet: `first` at B4 (no header), then format_timeuse().
.hfc_time_use_sheet <- function(outfile, sheet, first, hh, title, fmtdate) {
  # export excel <var> using outfile, sheet() cell(B4) sheetreplace : the
  # sheet holds no header row, so hfc_write_sheet (header + column A) does not
  # apply; the sheet is replaced in place keeping other sheets.
  dir.create(dirname(outfile), showWarnings = FALSE, recursive = TRUE)
  wb <- hfc_wb(outfile)
  if (sheet %in% names(wb)) openxlsx::removeWorksheet(wb, sheet)
  openxlsx::addWorksheet(wb, sheet)
  x <- first; attributes(x) <- attributes(x)[intersect(names(attributes(x)), c("class", "tzone"))]
  if (inherits(x, "haven_labelled")) x <- hfc_num(x)
  if (length(x)) openxlsx::writeData(wb, sheet, data.frame(x = x), startRow = 4, startCol = 2,
                                     colNames = FALSE, keepNA = FALSE)
  if (inherits(x, "Date") && length(x))
    openxlsx::addStyle(wb, sheet, openxlsx::createStyle(numFmt = "d-mmm-yy"),
                       rows = 3 + seq_along(x), cols = 2, stack = TRUE)
  .hfc_time_use_format(wb, sheet, x, hh, title, fmtdate)
  openxlsx::saveWorkbook(wb, outfile, overwrite = TRUE)
}

#' Active survey hours heatmap from text audit data
#'
#' Port of Stata `hfc_time_use`. Uses the "first appeared" timestamp of
#' each field in collated SurveyCTO text audits plus the submission start
#' time to compute the clock hours during which each survey was active, and
#' writes two heatmap sheets ("timeuse by date", "timeuse by enumerator")
#' with cells coloured by the number of surveys active in each hour.
#'
#' @param data data.frame (survey data in memory)
#' @param varname text audit media variable (e.g. "text_audit")
#' @param enumerator enumerator variable
#' @param starttime datetime (POSIXct, Stata %tc) start time variable
#' @param textauditdata path to collated text audit data (.dta/.rds) or data.frame
#' @param outfile output .xlsx
#' @param sheetmodify,sheetreplace accepted for compatibility (sheets replaced)
#' @param nolabel export enumerator values instead of value labels
#' @return invisible list: N_obs (matched submissions), N_dates, N_enums,
#'   by_date and by_enum (data.frames of hour counts).
#' @examples
#' hfc_time_use(df, "text_audit", enumerator = "a_enum_id",
#'   starttime = "starttime", textauditdata = "textaudit_data.dta",
#'   outfile = "timeuse.xlsx")
hfc_time_use <- function(data, varname, enumerator, starttime, textauditdata, outfile,
                            sheetmodify = FALSE, sheetreplace = TRUE, nolabel = FALSE) {
  varname <- hfc_vars(data, varname); enumerator <- hfc_vars(data, enumerator)
  starttime <- hfc_vars(data, starttime)
  if (!hfc_is_datetime(data[[starttime]])) hfc_stop("%s is not a datetime variable", starttime)
  if (missing(textauditdata) || is.null(textauditdata) || identical(textauditdata, ""))
    hfc_stop("must specify either media() or textauditdata() option")

  res <- list(N_obs = 0L, N_dates = 0L, N_enums = 0L)
  m <- .hfc_text_audit_merge(data, varname, c(enumerator, starttime), textauditdata)
  if (is.null(m)) return(invisible(res))
  nms <- names(m)
  if ("devicetime" %in% nms) {
    fv <- if ("formtimems" %in% nms) "formtimems" else .hfc_text_audit_var(nms, "formtime")
    first_ms <- suppressWarnings(as.numeric(as.character(m[[fv]])))
  } else {
    fv <- .hfc_text_audit_var(nms, "firstappeared")
    if (is.na(fv)) hfc_stop("variable firstappeared not found in text audit data")
    first_ms <- suppressWarnings(as.numeric(as.character(m[[fv]]))) * 1000
  }
  st <- m[[starttime]]
  # time = starttime + firstappeared (ms); hour = hh(time); date = dofc(starttime)
  tm <- as.POSIXct(as.numeric(st) + first_ms / 1000, origin = "1970-01-01", tz = "UTC")
  hour <- as.POSIXlt(tm, tz = "UTC")$hour
  date <- as.Date(as.POSIXct(st), tz = attr(st, "tzone") %||% "UTC")

  # duplicates drop varname hour, force
  keep <- !duplicated(data.frame(m[[varname]], hour))
  # NOTE(stata-bug): a missing firstappeared gives hour == . which makes
  # `reshape wide ..., j(hour)` fail in Stata; such rows are dropped here.
  keep <- keep & !is.na(hour)
  hour <- hour[keep]; date <- date[keep]
  enum <- m[[enumerator]][keep]
  res$N_obs <- length(unique(m[[varname]]))

  # *** timeuse by date ***
  td <- .hfc_time_use_table(as.numeric(date), hour)
  dkeys <- as.Date(td$keys, origin = "1970-01-01")
  .hfc_time_use_sheet(outfile, "timeuse by date", dkeys, td$hh,
                         paste("Active survey hours by", starttime), fmtdate = 1)

  # *** timeuse by enumerator ***
  ekey <- if (is.character(enum)) enum else hfc_num(enum)
  te <- .hfc_time_use_table(ekey, hour)
  ev <- enum[match(te$keys, ekey)]
  edf <- data.frame(row = seq_along(ev)); edf[[enumerator]] <- ev
  edf <- hfc_labels(edf, enumerator, nolabel = nolabel)
  .hfc_time_use_sheet(outfile, "timeuse by enumerator", edf[[enumerator]], te$hh,
                         "Active survey hours by enumerator", fmtdate = 0)

  hn <- paste0("hh", 0:23)
  res$by_date <- setNames(data.frame(dkeys, td$hh), c("date", hn))
  res$by_enum <- setNames(data.frame(edf[[enumerator]], te$hh, stringsAsFactors = FALSE), c(enumerator, hn))
  res$N_dates <- length(dkeys); res$N_enums <- nrow(te$hh)
  invisible(res)
}
