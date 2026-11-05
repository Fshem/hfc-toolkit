# =============================================================================
#  hfc_survey_dashboard : survey dashboard
#
#  This file also holds the private helpers shared with hfc_enum_dashboard.R
#  (prefix .hfc_survey_dashboard_). Both files must be loaded (they are, as part
#  of the package / when sourced together).
# =============================================================================

# -----------------------------------------------------------------------------
#  Private helpers
# -----------------------------------------------------------------------------

#' Stata missing() for any column type ("" or NA for strings, NA/tagged NA else)
.hfc_survey_dashboard_miss <- function(x) {
  if (is.character(x)) is.na(x) | x == "" else is.na(x)
}

#' Number of distinct non-missing values (Stata `tab var`, r(r))
.hfc_survey_dashboard_ndistinct <- function(x) {
  keep <- !.hfc_survey_dashboard_miss(x)
  x <- if (is.character(x)) x[keep] else hfc_num(x)[keep]
  length(unique(x))
}

#' Stata week(): week of year 1-52 based on day-of-year (days 358+ are week 52).
#' NB: not ISO weeks and not Sunday-Saturday weeks (despite the help file).
.hfc_survey_dashboard_week <- function(d) {
  doy <- as.integer(format(d, "%j"))
  pmin(52L, (doy - 1L) %/% 7L + 1L)
}

#' Stata %td display format, e.g. "18sep2017"
.hfc_survey_dashboard_td <- function(d) {
  ifelse(is.na(d), ".", paste0(format(d, "%d"), tolower(format(d, "%b")), format(d, "%Y")))
}

#' Stata internal date number (days since 01jan1960)
.hfc_survey_dashboard_stdate <- function(d) as.numeric(d) + 3653

#' Expand a Stata numlist ("1 2 3", "1/4 12", "1,2")
.hfc_survey_dashboard_numlist <- function(x) {
  toks <- hfc_tokens(gsub(",", " ", paste(x, collapse = " ")))
  out <- numeric(0)
  for (t in toks) {
    if (grepl("^-?[0-9.]+/-?[0-9.]+$", t)) {
      ab <- as.numeric(strsplit(t, "/", fixed = TRUE)[[1]])
      out <- c(out, seq(ab[1], ab[2]))
    } else {
      v <- suppressWarnings(as.numeric(t))
      if (is.na(v)) hfc_stop("invalid numlist '%s'", t)
      out <- c(out, v)
    }
  }
  out
}

#' Sortable key with Stata sort order (numeric missing last, missing string first)
.hfc_survey_dashboard_key <- function(x) {
  if (is.character(x)) { x[is.na(x)] <- ""; x } else hfc_num(x)
}

#' Group ids in Stata (sorted) order for the by-variables of `df`.
#' Returns list(id = integer per row, rows = list of row indices per group,
#'              first = first row (data order) of each group)
.hfc_survey_dashboard_groups <- function(df, by) {
  keys <- lapply(by, function(v) .hfc_survey_dashboard_key(df[[v]]))
  sig <- do.call(paste, c(lapply(keys, function(k) ifelse(is.na(k), "\001NA", as.character(k))), sep = "\r"))
  o <- do.call(order, c(unname(keys), list(na.last = TRUE, method = "radix")))
  id <- match(sig, unique(sig[o]))
  rows <- split(seq_len(nrow(df)), factor(id, levels = seq_len(max(c(0L, id)))))
  list(id = id, rows = unname(rows), first = vapply(rows, function(r) r[1], integer(1)))
}

#' Subset by-columns of df at rows `r` (NA rows allowed), keeping variable labels
.hfc_survey_dashboard_bycols <- function(df, by, r) {
  out <- df[r, by, drop = FALSE]
  for (v in by) attr(out[[v]], "label") <- attr(df[[v]], "label", exact = TRUE)
  rownames(out) <- NULL
  out
}

#' Validate the numeric part of dontknow()/refuse()
.hfc_survey_dashboard_check_num <- function(num, opt) {
  if (!is.null(num) && length(num) && !is.na(num[1]) && nzchar(trimws(num[1]))) {
    v <- trimws(paste(num, collapse = " "))
    if (!grepl("^-?[0-9]+$", v) && !grepl("^[.][a-z]$", v))
      hfc_stop("%s found where integer is expected in option %s()", v, opt)
    return(v)
  }
  NULL
}

#' Common preparation for both dashboards (tempvars of the Stata code).
#' Returns list(data, t (data.frame of tmv_* per row), flags, counts, period)
.hfc_survey_dashboard_prep <- function(data, date, enumerator, formversion, period,
                                   consent, cons_vals, dontknow_num, dontknow_str,
                                   refuse_num, refuse_str, otherspecify, duration,
                                   team = NULL) {
  data <- as.data.frame(data, stringsAsFactors = FALSE, check.names = FALSE)
  for (v in c(date, enumerator, formversion, team, duration))
    if (!(v %in% names(data))) hfc_stop("variable %s not found", v)

  obs_count  <- nrow(data)
  vars_count <- ncol(data)
  allvars    <- names(data)

  flags <- list(
    cons  = !is.null(consent) && length(consent) && !is.na(consent) && nzchar(consent),
    dk    = length(c(dontknow_num, dontknow_str)) > 0 && any(nzchar(c(dontknow_num, dontknow_str), keepNA = FALSE)),
    ref   = length(c(refuse_num, refuse_str)) > 0 && any(nzchar(c(refuse_num, refuse_str), keepNA = FALSE)),
    other = length(otherspecify) > 0 && any(nzchar(otherspecify, keepNA = FALSE)),
    dur   = !is.null(duration) && length(duration) && !is.na(duration) && nzchar(duration),
    team  = !is.null(team) && length(team) && !is.na(team) && nzchar(team)
  )

  # egen rowmiss(_all)
  miss <- integer(obs_count)
  for (v in allvars) miss <- miss + .hfc_survey_dashboard_miss(data[[v]])
  t <- data.frame(miss = miss)

  # dontknow / refuse : ipaanycount _all
  if (flags$dk) {
    num <- .hfc_survey_dashboard_check_num(dontknow_num, "dontknow")
    t$dk <- hfc_anycount(data, allvars, numval = num, strval = dontknow_str)
  } else t$dk <- 0
  if (flags$ref) {
    num <- .hfc_survey_dashboard_check_num(refuse_num, "refuse")
    t$ref <- hfc_anycount(data, allvars, numval = num, strval = refuse_str)
  } else t$ref <- 0

  # other specify: egen rownonmiss(), strok
  if (flags$other) {
    othervars <- hfc_vars(data, otherspecify, unique = FALSE)
    other_tokens <- length(hfc_tokens(otherspecify))
    other <- integer(obs_count)
    for (v in othervars) other <- other + !.hfc_survey_dashboard_miss(data[[v]])
    t$other <- other
  } else {
    othervars <- character(0); other_tokens <- 0L; t$other <- 0
  }

  # team
  t$team <- if (flags$team) data[[team]] else rep("", obs_count)
  if (flags$team) attributes(t$team) <- NULL   # gen copies values, not labels

  # ipagettd
  data[[date]] <- hfc_to_date(data[[date]])

  # period
  period <- period %||% ""
  if (period != "" && !(period %in% c("auto", "daily", "weekly", "monthly")))
    hfc_stop("option period incorrectly specified. Expecting auto, daily, weekly or monthly.")
  if (period == "") period <- "auto"
  if (period == "auto") {
    rng <- range(data[[date]], na.rm = TRUE)
    days <- as.numeric(rng[2] - rng[1])
    period <- if (days <= 40) "daily" else if (days <= 280) "weekly" else "monthly"
  }

  # duration
  if (flags$dur) {
    if (!hfc_is_num(data[[duration]]))
      hfc_stop("variable %s found at option duration() where numeric variable is expected", duration)
    t$dur <- hfc_num(data[[duration]])
  } else t$dur <- 0

  # consent
  if (flags$cons) {
    cv <- hfc_vars(data, consent, strict = FALSE)
    if (!length(hfc_tokens(consent))) hfc_stop("no variables specified for consent() option")
    if (!length(cv)) hfc_stop("variable %s specifed in consent() option not found", consent)
    cv <- cv[1]
    if (!hfc_is_num(data[[cv]])) hfc_stop("variable %s specifed in consent() option is not numeric", cv)
    if (is.null(cons_vals) || !length(hfc_tokens(gsub(",", " ", cons_vals))))
      hfc_stop("no values specified with consent() option.expected format is consent(varname, varlist).")
    vals <- .hfc_survey_dashboard_numlist(cons_vals)
    x <- hfc_num(data[[cv]])
    t$cons <- as.numeric(!is.na(x) & x %in% vals)
  } else t$cons <- 0

  list(data = data, t = t, flags = flags, period = period,
       obs_count = obs_count, vars_count = vars_count, allvars = allvars,
       othervars = othervars, other_tokens = other_tokens)
}

#' collapse of the tempvars by group (summary sheets)
.hfc_survey_dashboard_collapse <- function(data, t, by, date, enumerator, formversion,
                                       vars_count, other_count, team_first = FALSE,
                                       count_enum = FALSE) {
  g <- .hfc_survey_dashboard_groups(data, by)
  out <- .hfc_survey_dashboard_bycols(data, by, g$first)
  rows <- g$rows
  # rows of groups with missing by value get 0 for the "tab" counts
  miss_grp <- vapply(g$first, function(r) any(vapply(by, function(v) .hfc_survey_dashboard_miss(data[[v]][r]), logical(1))), logical(1))
  ndist <- function(var) ifelse(miss_grp, 0, vapply(rows, function(r) .hfc_survey_dashboard_ndistinct(data[[var]][r]), numeric(1)))
  n <- lengths(rows)
  s <- function(x) vapply(rows, function(r) sum(x[r]), numeric(1))
  f <- function(fun, x) vapply(rows, function(r) fun(x[r]), numeric(1))
  dnum <- as.numeric(data[[date]])

  if (team_first) {
    # collapse (first): first non-missing value in the group
    tm <- t$team
    out$team <- tm[vapply(rows, function(r) { k <- r[!.hfc_survey_dashboard_miss(tm[r])]; if (length(k)) k[1] else r[1] }, integer(1))]
  }
  if (count_enum) out$enumerators <- ndist(enumerator)
  out$submissions     <- n
  out$consent_rate    <- f(mean, t$cons)
  out$missing_rate    <- s(t$miss) / (n * vars_count)
  out$dontknow_rate   <- s(t$dk)   / (n * vars_count)
  out$refuse_rate     <- s(t$ref)  / (n * vars_count)
  out$other_rate      <- s(t$other) / (n * other_count)
  out$other_rate[!is.finite(out$other_rate)] <- NA
  out$duration_min    <- f(hfc_min, t$dur)
  out$duration_mean   <- f(hfc_mean, t$dur)
  out$duration_median <- f(function(z) hfc_pctile(z, 50), t$dur)
  out$duration_max    <- f(hfc_max, t$dur)
  if (!count_enum && !team_first) out$enumerators <- ndist(enumerator)
  out$formversion     <- ndist(formversion)
  out$firstdate       <- as.Date(f(hfc_min, dnum), origin = "1970-01-01")
  out$lastdate        <- as.Date(f(hfc_max, dnum), origin = "1970-01-01")
  out$days            <- ndist(date)

  labs <- c(team = "team", enumerators = "# of enums", submissions = "# of submissions",
            consent_rate = "% of consent", missing_rate = "% missing", dontknow_rate = "% dont know",
            refuse_rate = "% refuse", other_rate = "% other", duration_min = "min duration",
            duration_mean = "mean duration", duration_median = "median duration",
            duration_max = "max duration", formversion = "# of form versions",
            firstdate = "first date", lastdate = "last date", days = "# of days")
  for (v in intersect(names(labs), names(out))) attr(out[[v]], "label") <- labs[[v]]
  out
}

#' Drop optional columns as the Stata code does
.hfc_survey_dashboard_dropcols <- function(out, flags, team = FALSE) {
  drop <- c(if (team && !flags$team) "team",
            if (!flags$cons) "consent_rate", if (!flags$dk) "dontknow_rate",
            if (!flags$ref) "refuse_rate", if (!flags$other) "other_rate",
            if (!flags$dur) c("duration_min", "duration_mean", "duration_median", "duration_max"))
  out[, setdiff(names(out), drop), drop = FALSE]
}

#' Number formats of the summary sheets (ipacolformat calls)
#' NOTE(stata-bug): missing_rate is formatted as percent only when consent() is specified.
.hfc_survey_dashboard_sumformats <- function(flags, count_cols) {
  pct <- c(if (flags$cons) c("consent_rate", "missing_rate"), if (flags$dk) "dontknow_rate",
           if (flags$ref) "refuse_rate", if (flags$other) "other_rate")
  num <- c(if (flags$dur) c("duration_min", "duration_mean", "duration_median", "duration_max"), count_cols)
  list(percent_d2 = pct, number_sep = num, date_d_mon_yy = c("firstdate", "lastdate"))
}

#' ipagetcal + merge 1:m date using data: calendar rows with weight
.hfc_survey_dashboard_calendar <- function(data, date, by) {
  dts <- data[[date]]
  rng <- range(dts, na.rm = TRUE)
  cal <- seq(rng[1], rng[2], by = "day")
  has <- !is.na(dts)
  unmatched <- cal[!(cal %in% dts)]
  # matched (weight 1), master-only calendar days (weight 0), using-only rows with missing date (weight 0)
  r_by <- c(which(has), rep(NA_integer_, length(unmatched)), which(!has))
  d <- c(dts[has], unmatched, dts[!has])
  out <- if (length(by)) .hfc_survey_dashboard_bycols(data, by, r_by) else data.frame(row.names = seq_along(d))
  out[[date]] <- d
  out$weight <- c(rep(1, sum(has)), rep(0, length(unmatched)), rep(0, sum(!has)))
  out$year  <- as.integer(format(d, "%Y"))
  out$week  <- .hfc_survey_dashboard_week(d)
  out$month <- as.integer(format(d, "%m"))
  out
}

#' Ungrouped productivity table (surveydb)
.hfc_survey_dashboard_prod <- function(cal, date, period) {
  if (period == "daily") {
    g <- .hfc_survey_dashboard_groups(cal, date)
    out <- data.frame(day = seq_along(g$rows))
    out[[date]] <- cal[[date]][g$first]
    out$submissions <- vapply(g$rows, function(r) sum(cal$weight[r]), numeric(1))
  } else {
    unit <- if (period == "weekly") "week" else "month"
    g <- .hfc_survey_dashboard_groups(cal, c("year", unit))
    dn <- as.numeric(cal[[date]])
    out <- data.frame(x = seq_along(g$rows))
    names(out) <- unit
    out$startdate <- as.Date(vapply(g$rows, function(r) hfc_min(dn[r]), numeric(1)), origin = "1970-01-01")
    out$enddate   <- as.Date(vapply(g$rows, function(r) hfc_max(dn[r]), numeric(1)), origin = "1970-01-01")
    out$submissions <- vapply(g$rows, function(r) sum(cal$weight[r]), numeric(1))
  }
  out
}

#' Grouped (wide) productivity table with total row
.hfc_survey_dashboard_prodwide <- function(cal, by, date, period) {
  if (period == "daily") {
    jv <- .hfc_survey_dashboard_stdate(cal[[date]])
  } else {
    unit <- if (period == "weekly") "week" else "month"
    ok <- !is.na(cal$year) & !is.na(cal[[unit]])
    yw <- cal$year * 100 + cal[[unit]]
    jv <- rep(NA_real_, nrow(cal))
    jv[ok] <- match(yw[ok], sort(unique(yw[ok])))   # egen group(year unit)
  }
  # reshape wide cannot have missing j: drop (Stata would error)
  keep <- !is.na(jv)
  cal <- cal[keep, , drop = FALSE]; jv <- jv[keep]
  js <- sort(unique(jv))
  g <- .hfc_survey_dashboard_groups(cal, by)
  m <- matrix(0, nrow = length(g$rows), ncol = length(js))
  for (k in seq_along(g$rows)) {
    r <- g$rows[[k]]
    agg <- tapply(cal$weight[r], factor(jv[r], levels = js), sum)
    agg[is.na(agg)] <- 0
    m[k, ] <- agg
  }
  subs <- rowSums(m)
  keepg <- subs != 0
  m <- m[keepg, , drop = FALSE]
  out <- .hfc_survey_dashboard_bycols(cal, by, c(g$first[keepg], NA_integer_))
  out$submissions <- c(subs[keepg], sum(m))
  for (j in seq_along(js)) {
    nm <- paste0("vv_", js[j])
    out[[nm]] <- c(m[, j], sum(m[, j]))
    attr(out[[nm]], "label") <- if (period == "daily") .hfc_survey_dashboard_td(as.Date(js[j] - 3653, origin = "1970-01-01"))
                                else if (period == "weekly") paste("week", js[j]) else paste("month", js[j])
  }
  out
}

# -----------------------------------------------------------------------------
#  hfc_survey_dashboard
# -----------------------------------------------------------------------------

#' Survey dashboard
#'
#' Port of Stata `hfc_survey_dashboard`. Creates an Excel workbook with general
#' survey statistics: a formatted "summary" sheet (submissions, consent rate,
#' missing / don't know / refuse / other-specify rates, variable stats,
#' duration, enumerators, form versions, dates), an optional
#' "summary (grouped)" sheet when `by` is given, a "<period> productivity"
#' sheet and, with `by`, a "<period> productivity (grouped)" sheet.
#'
#' As in Stata (`export excel ..., replace`), the output workbook is
#' overwritten entirely.
#'
#' @param data data.frame (survey data, dk/ref already recoded to .d/.r).
#' @param by up to 2 grouping variables (varlist string).
#' @param date date or datetime variable (datetime converted to date).
#' @param period "auto" (default), "daily", "weekly" or "monthly". auto =
#'   daily if (max date - min date) <= 40 days, weekly if <= 280, else monthly.
#'   Weeks are Stata `week()` weeks (day-of-year based, 1-52) within year.
#' @param enumerator enumerator variable.
#' @param consent consent variable (numeric); `cons_vals` numlist of consent values.
#' @param dontknow_num,dontknow_str numeric (integer or ".d") and string values
#'   for don't know (Stata `dontknow(.d, -999)`).
#' @param refuse_num,refuse_str same for refusals (`refuse(.r, -888)`).
#' @param otherspecify other-specify child variables (varlist / character vector).
#' @param duration numeric duration variable.
#' @param formversion form version variable.
#' @param outfile output .xlsx file.
#' @param sheetreplace,sheetmodify accepted for compatibility (file is replaced).
#' @param nolabel export values instead of value labels for `by`.
#' @return invisibly a list with summary statistics and the period used.
#' @examples
#' \dontrun{
#' hfc_survey_dashboard(df, date = "starttime", enumerator = "a_enum_id",
#'   consent = "c_consent", cons_vals = "1", dontknow_num = ".d", dontknow_str = "-999",
#'   refuse_num = ".r", refuse_str = "-888", otherspecify = childvars,
#'   duration = "duration", formversion = "formdef_version", outfile = "surveydb.xlsx")
#' }
hfc_survey_dashboard <- function(data, by = NULL, date, period = "auto", enumerator,
                             consent = NULL, cons_vals = NULL,
                             dontknow_num = NULL, dontknow_str = NULL,
                             refuse_num = NULL, refuse_str = NULL,
                             otherspecify = NULL, duration = NULL, formversion,
                             outfile, sheetreplace = TRUE, sheetmodify = FALSE,
                             nolabel = FALSE) {
  by <- if (length(by) && any(nzchar(by, keepNA = FALSE))) hfc_vars(data, by) else character(0)
  if (length(by) > 2) hfc_stop("too many variables specified in by()")
  # NOTE(stata-bug): with 2 by() variables the Stata code fails at `levelsof `by''
  # (levelsof takes one variable); here groups are combinations of both variables.

  p <- .hfc_survey_dashboard_prep(data, date, enumerator, formversion, period, consent, cons_vals,
                              dontknow_num, dontknow_str, refuse_num, refuse_str,
                              otherspecify, duration)
  data <- p$data; t <- p$t; fl <- p$flags; period <- p$period
  N <- p$obs_count; K <- p$vars_count
  # NOTE(stata-bug): other_count = wordcount of the otherspecify() *string* (not the
  # unab'd list), so wildcards / ranges understate the number of variables.
  other_count <- p$other_tokens

  # ---- summary statistics ----
  dts <- data[[date]]
  today <- Sys.Date()
  n_today <- sum(!is.na(dts) & dts == today)
  n_week  <- sum(!is.na(dts) & .hfc_survey_dashboard_week(dts) == .hfc_survey_dashboard_week(today) &
                 format(dts, "%Y") == format(today, "%Y"))
  n_month <- sum(!is.na(dts) & format(dts, "%m%Y") == format(today, "%m%Y"))
  consent_rate  <- hfc_mean(t$cons)
  miss_rate     <- sum(t$miss) / (N * K)
  dontknow_rate <- sum(t$dk) / (N * K)
  refuse_rate   <- sum(t$ref) / (N * K)
  other_rate    <- sum(t$other) / (N * other_count)
  if (!is.finite(other_rate)) other_rate <- NA

  uniq_count <- 0; nomiss_count <- 0; allmiss_count <- 0
  for (v in p$allvars) {
    m <- .hfc_survey_dashboard_miss(data[[v]])
    if (all(m)) allmiss_count <- allmiss_count + 1
    else if (!any(m)) {
      nomiss_count <- nomiss_count + 1
      x <- data[[v]]; x <- if (is.character(x)) x else as.vector(unclass(x))
      if (!anyDuplicated(x)) uniq_count <- uniq_count + 1
    }
  }
  if (fl$dur) {
    dv <- t$dur
    dur <- c(hfc_min(dv), hfc_mean(dv), hfc_pctile(dv, 50), hfc_max(dv))
  } else dur <- rep(NA_real_, 4)
  enum_count <- .hfc_survey_dashboard_ndistinct(data[[enumerator]])
  formversion_count <- .hfc_survey_dashboard_ndistinct(data[[formversion]])
  firstdate <- .hfc_survey_dashboard_td(min(dts, na.rm = TRUE))
  lastdate  <- .hfc_survey_dashboard_td(max(dts, na.rm = TRUE))
  days_count <- .hfc_survey_dashboard_ndistinct(dts)

  desc <- c("Survey Dashboard", trimws(format(Sys.Date(), "%e %b %Y")), "submissions", "today",
            "this week", "this_month", "all", "consent", "consent rate", "missingness",
            "% of missing values", "% of don't know value", "% of refuse to answer value",
            "other specify", "# of other specify variables", "% of other specify values",
            "variables", "# with only unique values", "# with no missing values",
            "# with all missing values", "total number", "duration", "minimum", "mean",
            "median", "maximum", "other details", "# of enumerators", "# of form versions",
            "first date of survey", "last date of survey", "# of days")
  vals <- c(NA, NA, NA, n_today, n_week, n_month, N, NA, consent_rate, NA, miss_rate,
            dontknow_rate, refuse_rate, NA, other_count, other_rate, NA, uniq_count,
            nomiss_count, allmiss_count, K, NA, dur, NA, enum_count, formversion_count,
            NA, NA, days_count)

  # ---- summary sheet (export excel ..., replace + mata format_sdb_summary) ----
  # Written with openxlsx directly: the sheet has no header row and a custom layout.
  hfc_rm(outfile)
  dir.create(dirname(outfile), showWarnings = FALSE, recursive = TRUE)
  wb <- openxlsx::createWorkbook()
  sh <- "summary"
  openxlsx::addWorksheet(wb, sh)
  openxlsx::writeData(wb, sh, desc, startCol = 2, startRow = 1, colNames = FALSE)
  for (i in seq_along(vals)) if (!is.na(vals[i]))
    openxlsx::writeData(wb, sh, vals[i], startCol = 3, startRow = i, colNames = FALSE)
  dash <- c(if (!fl$cons) 9, if (!fl$dk) 12, if (!fl$ref) 13, if (!fl$other) c(15, 16),
            if (!fl$dur) 23:26)
  for (r in dash) openxlsx::writeData(wb, sh, "-", startCol = 3, startRow = r, colNames = FALSE)
  openxlsx::writeData(wb, sh, firstdate, startCol = 3, startRow = 30, colNames = FALSE)
  openxlsx::writeData(wb, sh, lastdate,  startCol = 3, startRow = 31, colNames = FALSE)

  nr <- length(desc)
  openxlsx::setColWidths(wb, sh, cols = 1:3, widths = c(2, 42, 16))
  st <- function(...) openxlsx::createStyle(...)
  add <- function(style, rows, cols) openxlsx::addStyle(wb, sh, style, rows = rows, cols = cols, gridExpand = TRUE, stack = TRUE)
  add(st(border = c("top", "bottom", "left", "right"), borderStyle = "thin"), 1:nr, 2:3)
  add(st(border = c("top", "bottom", "left", "right"), borderStyle = c("thin", "medium", "thin", "thin")), 1, 2:3)
  add(st(halign = "center"), 1:nr, 3)
  add(st(textDecoration = "bold"), setdiff(1:nr, 2), 2)
  add(st(textDecoration = "italic"), 2, 2)
  add(st(textDecoration = "italic"), 1:nr, 3)
  for (r in c(1, 2, 3, 8, 10, 14, 17, 22, 27)) {
    openxlsx::mergeCells(wb, sh, cols = 2:3, rows = r)
    add(st(halign = "center"), r, 2:3)
  }
  for (r in c(3, 8, 10, 14, 17, 22, 27)) add(st(fgFill = "#FFC000"), r, 2:3)
  add(st(numFmt = "#,##0"), c(4:7, 15, 18:21, 23:26, 28:29, 32), 3)
  add(st(numFmt = "0.00%"), c(9, 11:13, 16), 3)

  # ---- summary (grouped) ----
  if (length(by)) {
    out <- .hfc_survey_dashboard_collapse(data, t, by, date, enumerator, formversion, K, other_count)
    out <- .hfc_survey_dashboard_dropcols(out, fl)
    out <- hfc_labels(out, by, nolabel = nolabel)
    hfc_write_sheet(outfile, "summary (grouped)", out, header = "label",
                    formats = .hfc_survey_dashboard_sumformats(fl, c("enumerators", "formversion", "days")),
                    wb = wb, save = FALSE, replace_sheet = !sheetmodify)
  }

  # ---- productivity ----
  cal <- .hfc_survey_dashboard_calendar(data, date, by)
  prod <- .hfc_survey_dashboard_prod(cal, date, period)
  psheet <- paste(period, "productivity")
  pfmt <- if (period == "daily") list(number_sep = c("day", "submissions"), date_d_mon_yy = date)
          else list(number_sep = c(if (period == "weekly") "week" else "month", "submissions"),
                    date_d_mon_yy = c("startdate", "enddate"))
  hfc_write_sheet(outfile, psheet, prod, header = "name", formats = pfmt,
                  wb = wb, save = FALSE, replace_sheet = !sheetmodify)

  # ---- productivity (grouped) ----
  if (length(by)) {
    pw <- .hfc_survey_dashboard_prodwide(cal, by, date, period)
    pw <- hfc_labels(pw, by, nolabel = nolabel)
    numcols <- names(pw)[vapply(pw, hfc_is_num, logical(1))]
    hfc_write_sheet(outfile, paste(period, "productivity (grouped)"), pw, header = "label",
                    formats = list(number_sep = numcols), total_row = TRUE,
                    wb = wb, save = FALSE, replace_sheet = !sheetmodify)
  }

  openxlsx::saveWorkbook(wb, outfile, overwrite = TRUE)

  invisible(list(N = N, N_vars = K, period = period, consent_rate = consent_rate,
                 missing_rate = miss_rate, dontknow_rate = dontknow_rate,
                 refuse_rate = refuse_rate, other_count = other_count, other_rate = other_rate,
                 N_unique_vars = uniq_count, N_nomiss_vars = nomiss_count,
                 N_allmiss_vars = allmiss_count, N_enums = enum_count,
                 N_formversions = formversion_count, N_days = days_count))
}
