# =============================================================================
#  hfc_backcheck : compare survey and back-check data
#  Requires R/00_utils.R to be sourced first.
# =============================================================================

# -----------------------------------------------------------------------------
#  Private helpers (prefix .hfc_backcheck_)
# -----------------------------------------------------------------------------

#' Round a double to single precision (Stata float()/float storage)
.hfc_backcheck_float <- function(x) {
  x <- as.numeric(x)
  ok <- !is.na(x) & is.finite(x)
  if (any(ok)) x[ok] <- readBin(writeBin(x[ok], raw(), size = 4), "double", size = 4, n = sum(ok))
  x
}

#' Stata %w.0g display (string(x) is %9.0g): as many significant digits as fit
#' in w, trailing zeros dropped, no leading zero (".5"), exponential otherwise;
#' missing -> "." / ".d"
.hfc_backcheck_fmt_g <- function(x, width = 9) {
  tags <- if (is.double(x)) haven::na_tag(x) else rep(NA_character_, length(x))
  x <- as.numeric(x)
  vapply(seq_along(x), function(i) {
    v <- x[i]
    if (is.na(v)) return(if (!is.na(tags[i])) paste0(".", tags[i]) else ".")
    if (v == 0) return("0")
    # values stored as Stata float (exactly single-representable) show at most
    # 7 significant digits (e.g. float 121.2 -> "121.2", not "121.19999")
    isfl <- v == .hfc_backcheck_float(v) && v != round(v)
    for (d in min(width, if (isfl) 7 else width):1) {
      s <- trimws(formatC(v, digits = d, format = "g"))
      s <- sub("^(-?)0\\.", "\\1.", s)
      s <- sub("e([+-])0?(\\d\\d)$", "e\\1\\2", s)
      # %9.0g shows up to 8 characters (plus sign): 12345678 / .3333333
      if (nchar(s) <= width - 1 + (v < 0)) return(s)
    }
    s
  }, character(1))
}

#' tostring var, usedisplayformat force : use the Stata display format
.hfc_backcheck_tostring <- function(x, fmt = NULL) {
  fmt <- fmt %||% attr(x, "format.stata", exact = TRUE) %||% "%9.0g"
  v <- hfc_num(x)
  tags <- if (is.double(unclass(x))) haven::na_tag(as.vector(unclass(x))) else rep(NA_character_, length(v))
  m <- regmatches(fmt, regexec("^%-?(\\d+)\\.(\\d+)(g|f|fc|gc)$", fmt))[[1]]
  out <- if (length(m) && m[4] %in% c("f", "fc")) {
    s <- formatC(v, format = "f", digits = as.integer(m[3]), big.mark = if (m[4] == "fc") "," else "")
    s <- sub("^(-?)0\\.", "\\1.", s); s
  } else {
    w <- if (length(m)) as.integer(m[2]) else 9
    .hfc_backcheck_fmt_g(v, w)
  }
  out[is.na(v)] <- ifelse(is.na(tags[is.na(v)]), ".", paste0(".", tags[is.na(v)]))
  out
}

#' decode : value label text ("" when unlabelled value / missing)
.hfc_backcheck_decode <- function(x) {
  labs <- attr(x, "labels", exact = TRUE)
  v <- hfc_num(x)
  y <- names(labs)[match(v, unname(labs))]
  y[is.na(y)] <- ""
  y
}
.hfc_backcheck_has_vallab <- function(x) inherits(x, "haven_labelled") && length(attr(x, "labels", exact = TRUE)) > 0

#' Column ready for export excel: labelled -> label text unless nolabel
.hfc_backcheck_export_col <- function(x, nolabel) {
  lab <- attr(x, "label", exact = TRUE)
  if (.hfc_backcheck_has_vallab(x)) x <- if (nolabel) hfc_num(x) else .hfc_backcheck_decode(x)
  attr(x, "label") <- lab
  x
}

#' Stata `var ~= _bcvar` with missing semantics (. == ., .d ~= .r)
.hfc_backcheck_neq <- function(x, y) {
  if (is.character(x) || is.character(y)) {
    x <- as.character(x); y <- as.character(y)
    x[is.na(x)] <- ""; y[is.na(y)] <- ""
    return(x != y)
  }
  tx <- if (is.double(unclass(x))) haven::na_tag(as.vector(unclass(x))) else rep(NA_character_, length(x))
  ty <- if (is.double(unclass(y))) haven::na_tag(as.vector(unclass(y))) else rep(NA_character_, length(y))
  a <- hfc_num(x); b <- hfc_num(y)
  out <- rep(TRUE, length(a))
  both <- !is.na(a) & !is.na(b)
  out[both] <- a[both] != b[both]
  bm <- is.na(a) & is.na(b)
  tx[is.na(tx)] <- ""; ty[is.na(ty)] <- ""
  out[bm] <- tx[bm] != ty[bm]
  out
}

#' Parse a Stata `string asis` option ("a" "b c") or a character vector
.hfc_backcheck_strtokens <- function(x) {
  if (is.null(x) || !length(x)) return(character(0))
  if (length(x) > 1) return(as.character(x))
  if (is.na(x) || trimws(x) == "") return(character(0))
  scan(text = x, what = "", quiet = TRUE, quote = "\"")
}

#' numlist option -> numeric vector
.hfc_backcheck_numlist <- function(x) {
  if (is.null(x) || !length(x)) return(numeric(0))
  if (is.numeric(x)) return(as.numeric(x))
  v <- suppressWarnings(as.numeric(hfc_tokens(gsub(",", " ", x))))
  if (any(is.na(v))) hfc_stop("invalid numlist: %s", paste(x, collapse = " "))
  v
}

#' change_str : nosymbol / lower / upper / trim on string variables
.hfc_backcheck_change_str <- function(data, vars, nosymbol, lower, upper, trim) {
  for (v in vars) {
    x <- data[[v]]
    if (!is.character(x)) next
    lab <- attr(x, "label", exact = TRUE)
    if (nosymbol) {
      for (i in c(33:47, 58:64, 91:96, 123:126)) x <- gsub(intToUtf8(i), " ", x, fixed = TRUE)
    }
    if (lower || upper) {
      if (lower && upper) hfc_stop("options lower and upper are mutually exclusive")
      x <- if (lower) tolower(x) else toupper(x)
    }
    if (trim) x <- gsub("^ +| +$", "", gsub(" +", " ", sub("^\\s+", "", x, perl = TRUE)))
    attr(x, "label") <- lab
    data[[v]] <- x
  }
  data
}

#' Row key for (possibly multiple) id variables
.hfc_backcheck_key <- function(data, id) {
  do.call(paste, c(lapply(id, function(v) {
    x <- data[[v]]
    if (is.character(x)) x else formatC(hfc_num(x), digits = 17, format = "g")
  }), sep = "\r"))
}

#' Convert a survey/backcheck date variable to Date (dofc when %tc)
.hfc_backcheck_as_date <- function(x, opt) {
  lab <- attr(x, "label", exact = TRUE)
  if (hfc_is_date(x)) { y <- x }
  else if (hfc_is_datetime(x)) y <- as.Date(x, tz = attr(x, "tzone") %||% "UTC")
  else if (hfc_is_num(x)) {
    fmt <- attr(x, "format.stata", exact = TRUE) %||% "%td"
    v <- hfc_num(x)
    y <- if (startsWith(fmt, "%tc") || startsWith(fmt, "%tC")) as.Date(floor(v / 86400000), origin = "1960-01-01")
         else as.Date(v, origin = "1960-01-01")
  } else hfc_stop("Variable %s in option %s() must be a date formatted variable", deparse(substitute(x)), opt)
  attr(y, "label") <- lab
  y
}

#' Stata `"`=x'"` macro text, then `piece 1 4` (first 4 characters)
.hfc_backcheck_piece4 <- function(x) {
  if (is.na(x)) return(".")
  substr(.hfc_backcheck_fmt_g(x, 17), 1, 4)
}

# ---- Stata statistical tests ------------------------------------------------

#' ttest x == y (paired): returns r(p) (two-sided)
.hfc_backcheck_ttest <- function(x, y) {
  ok <- !is.na(x) & !is.na(y)
  d <- x[ok] - y[ok]; n <- length(d)
  if (n < 2) return(NA_real_)
  se <- stats::sd(d) / sqrt(n)
  if (!is.finite(se) || se == 0) return(NA_real_)
  t <- mean(d) / se
  2 * stats::pt(-abs(t), df = n - 1)
}

#' prtest x == y (two-sample test of proportions using variables): r(p)
.hfc_backcheck_prtest <- function(x, y, var) {
  x <- x[!is.na(x)]; y <- y[!is.na(y)]
  if (!all(x %in% c(0, 1)) || !all(y %in% c(0, 1)))
    hfc_stop("variable %s in option prtest: values must be 0/1 (prtest requires 0/1 variables)", var)
  n1 <- length(x); n2 <- length(y)
  if (n1 == 0 || n2 == 0) return(NA_real_)
  p1 <- mean(x); p2 <- mean(y)
  p <- (p1 * n1 + p2 * n2) / (n1 + n2)
  se <- sqrt(p * (1 - p) * (1 / n1 + 1 / n2))
  if (!is.finite(se) || se == 0) return(NA_real_)
  z <- (p1 - p2) / se
  2 * stats::pnorm(-abs(z))
}

#' signrank x = y (Wilcoxon matched-pairs, Stata normal approximation):
#' zeros are ranked with the non-zero differences (and contribute nothing to
#' the signed-rank sum); adjusted variance = sum(signed rank^2)/4. Returns r(p_2)
.hfc_backcheck_signrank <- function(x, y) {
  ok <- !is.na(x) & !is.na(y)
  d <- x[ok] - y[ok]
  if (!length(d)) return(NA_real_)
  r <- rank(abs(d), ties.method = "average")
  sr <- sign(d) * r
  sum_pos <- sum(r[d > 0]); sum_neg <- sum(r[d < 0])
  expected <- (sum_pos + sum_neg) / 2
  v <- sum(sr^2) / 4
  if (v <= 0) return(NA_real_)
  z <- (sum_pos - expected) / sqrt(v)
  2 * stats::pnorm(-abs(z))
}

# ---- Excel formatting helpers ----------------------------------------------

.hfc_backcheck_border <- function(wb, sheet, rows, cols, side, style = "medium") {
  if (!length(rows) || !length(cols) || any(rows < 1) || any(cols < 1)) return(invisible())
  st <- openxlsx::createStyle(border = side, borderStyle = style)
  openxlsx::addStyle(wb, sheet, st, rows = rows, cols = cols, gridExpand = TRUE, stack = TRUE)
}
.hfc_backcheck_style <- function(wb, sheet, rows, cols, ...) {
  if (!length(rows) || !length(cols)) return(invisible())
  openxlsx::addStyle(wb, sheet, openxlsx::createStyle(...), rows = rows, cols = cols, gridExpand = TRUE, stack = TRUE)
}

#' export excel ..., cell(<col><row>) : write df with `startcol - 1` blank
#' leading columns via hfc_write_sheet (header cells of padding are cleared)
.hfc_backcheck_export <- function(wb, file, sheet, df, start_row, start_col = 2, header = "label", formats = list()) {
  pad <- start_col - 1
  if (pad > 0) {
    blank <- as.data.frame(setNames(replicate(pad, rep(NA_character_, nrow(df)), simplify = FALSE),
                                    paste0("_pad", seq_len(pad))), stringsAsFactors = FALSE)
    for (b in names(blank)) attr(blank[[b]], "label") <- " "
    df <- cbind(blank, df)
  }
  hfc_write_sheet(file, sheet, df, header = header, formats = formats, wb = wb, save = FALSE,
                  start_row = start_row, colwidth = FALSE, header_format = FALSE)
  # hfc_write_sheet de-duplicates headers (make.unique); Stata does not
  # (e.g. surveydate and bcdate both labelled "starttime") -> rewrite header row
  hdr <- vapply(names(df), function(v) if (header == "label") hfc_varlabel(df, v) else v, character(1))
  openxlsx::writeData(wb, sheet, matrix(unname(hdr), nrow = 1), startRow = start_row, startCol = 1, colNames = FALSE)
  if (pad > 0) openxlsx::deleteData(wb, sheet, cols = seq_len(pad), rows = start_row, gridExpand = TRUE)
  invisible(wb)
}

#' Column width as Stata's mata adjust_column_width (strings only count)
.hfc_backcheck_strwidth <- function(x) {
  if (!is.character(x)) return(0)
  x <- x[!is.na(x)]
  if (!length(x)) 0 else max(nchar(x, type = "chars"))
}

# -----------------------------------------------------------------------------
#  Main function
# -----------------------------------------------------------------------------

#' hfc_backcheck: compare survey and back check data
#'
#' Compares back check data with survey data and produces an Excel workbook of
#' comparisons and error rates (sheets "summary", "backcheck only", "IDs",
#' "comparison", "enumerator stats", "enumerator team stats",
#' "backchecker stats", "backchecker team stats", "variable stats").
#' Type 1/2 variables feed enumerator checks; type 2/3 variables feed stability
#' checks (paired ttest, prtest, Wilcoxon signrank) and reliability (simple
#' response variance, SRV, and reliability ratio = 1 - SRV / var(backcheck)).
#'
#' @param surveydata,bcdata  survey / back check data: a file path (.dta, .rds,
#'   ...) or a data.frame.
#' @param id unique ID variable(s) (Stata varlist string).
#' @param enumerator numeric enumerator variable in the survey data.
#' @param backchecker numeric back checker variable in the back check data.
#' @param enumteam,bcteam optional numeric team variables (survey / back check).
#' @param t1vars,t2vars,t3vars type 1 / 2 / 3 variables (at least one required).
#' @param ttest,prtest,signrank,reliability type 2/3 numeric variables for the
#'   stability / reliability checks.
#' @param filename output .xlsx (".xls" or no extension is adjusted).
#' @param surveydate,bcdate date or datetime variables in survey / bc data.
#' @param showid "30%" (error-rate threshold) or "5" (number of differences).
#' @param level confidence level (10-99.99) for ttest / prtest.
#' @param keepsurvey,keepbc extra variables shown in the comparison sheet.
#' @param okrange e.g. "e_hhh_howlong [-1, 1], m_mon_exp [-5%, 5%]".
#' @param nodiffnum,nodiffstr back check values never counted as differences.
#' @param excludenum,excludestr back check values not compared.
#' @param excludemissing do not compare when survey or back check is missing.
#' @param lower,upper,nosymbol,trim string normalisation before comparing.
#' @param full include all comparisons (not just differences) in "comparison".
#' @param nolabel export values instead of value labels.
#' @param replace overwrite an existing output file.
#' @return invisibly, a list mimicking Stata r(): scalars showid, bc_only,
#'   total_rate, avd, survey, bc; matrices rates, rates_time, rates_<unit>,
#'   enum, enum1-3, enum_bc, enumteam*, backchecker*, backchecker_avd,
#'   bcteam*, var, var1-3, ttest*, prtest*, signrank*.
#' @examples
#' \dontrun{
#' hfc_backcheck(surveydata = "survey.dta", bcdata = "bc.dta", id = "hhid",
#'            enumerator = "a_enum_id", backchecker = "a_bcer_id",
#'            t1vars = "e_hhh_relig", t2vars = "f_hhm_cnt h_rooms",
#'            t3vars = "m_mon_exp", ttest = "m_mon_exp", signrank = "f_hhm_cnt",
#'            okrange = "m_mon_exp [-5%, 5%]", filename = "bc_output.xlsx",
#'            surveydate = "starttime", bcdate = "starttime")
#' }
hfc_backcheck <- function(surveydata, bcdata, id, enumerator, backchecker, enumteam = NULL, bcteam = NULL,
                       t1vars = NULL, t2vars = NULL, t3vars = NULL,
                       ttest = NULL, prtest = NULL, signrank = NULL, reliability = NULL,
                       filename, surveydate, bcdate, showid = "30%", level = 95,
                       keepsurvey = NULL, keepbc = NULL, okrange = NULL,
                       nodiffnum = NULL, nodiffstr = NULL, excludenum = NULL, excludestr = NULL,
                       excludemissing = FALSE, lower = FALSE, upper = FALSE, nosymbol = FALSE,
                       trim = FALSE, full = FALSE, nolabel = FALSE, replace = TRUE) {

  e0 <- function(x) is.null(x) || !length(x) || all(is.na(x)) || all(trimws(x) == "")
  if (e0(enumteam)) enumteam <- NULL
  if (e0(bcteam)) bcteam <- NULL
  if (!is.null(bcteam) && !is.null(enumteam) && identical(bcteam, enumteam))
    hfc_stop("variable %s not allowed in both enumteam() and bcteam(): the merged data keeps only the survey copy", bcteam)

  # ---------------------------------------------------------------- syntax --
  if (e0(t1vars) && e0(t2vars) && e0(t3vars))
    hfc_stop("must specify, at minimum, one of options t1vars(), t2vars() or t3vars()")
  # Stata: level(-1) default means "not specified"; here level = 95 is default
  if (!isTRUE(all.equal(level, 95)) && e0(ttest) && e0(prtest))
    hfc_stop("option level must be specified with option ttest or prtest")
  if (level < 10 || level > 99.99)
    hfc_stop("level %s not allowed; level() must be between 10 and 99.99 inclusive", level)

  # output extension
  dot <- regexpr("\\.[^.]*$", basename(filename))
  ext <- if (dot > 0) substring(basename(filename), dot) else ""
  if (ext == ".xls") {
    message("File must be exported in .xlsx format. Adjusting file extension to .xlsx.")
    filename <- sub("\\.xls$", ".xlsx", filename)
  } else if (ext == "") {
    filename <- paste0(filename, ".xlsx")
  } else if (ext != ".xlsx") hfc_stop("file type %s not allowed. File must be in .xlsx format", ext)

  # showid
  showid <- if (e0(showid)) "30%" else as.character(showid)
  percent <- grepl("%", showid)
  if (percent) {
    showid_v <- as.numeric(gsub("%", "", showid)) / 100
    if (showid_v > 1) hfc_stop("opt showid (%s%%) cannot be higher than 100%%. Use a lower percentage or use an absolute number.", showid_v * 100)
  } else showid_v <- as.numeric(showid)

  if (!replace && file.exists(filename))
    hfc_stop("file %s already exists. Specify new filename or use -replace- option.", filename)

  nodiffnum  <- .hfc_backcheck_numlist(nodiffnum)
  excludenum <- .hfc_backcheck_numlist(excludenum)
  if (length(nodiffnum) && length(excludenum)) {
    both <- intersect(nodiffnum, excludenum)
    if (length(both)) hfc_stop("value(s) \"%s\" not allowed in both nodiffnum() and excludenum()", paste(both, collapse = " "))
  }
  nodiffstr  <- .hfc_backcheck_strtokens(nodiffstr)
  excludestr <- .hfc_backcheck_strtokens(excludestr)
  # NOTE(stata-bug): the nodiffstr value list (nodiffstr_list) is only built
  # inside `if nodiffstr ~= "" & excludestr ~= ""`, so nodiffstr() has no effect
  # unless excludestr() is also specified. Reproduced faithfully.
  nodiffstr_list <- character(0)
  if (length(nodiffstr) && length(excludestr)) {
    for (k in seq_along(nodiffstr)) {
      if (nodiffstr[k] %in% excludestr)
        hfc_stop("value(s) \"%s\" not allowed in both nodiffstr() and excludestr()", nodiffstr[k])
      if (k + 1 > 6) hfc_stop("expression too long. You can only specify a maximum of 6 strings with option nodiffstr()")
      nodiffstr_list <- c(nodiffstr_list, nodiffstr[k])
    }
  }

  # ----------------------------------------------------------- survey data --
  sname <- if (is.character(surveydata)) surveydata else deparse(substitute(surveydata))
  bname <- if (is.character(bcdata)) bcdata else deparse(substitute(bcdata))
  sraw <- if (is.character(surveydata)) hfc_load(surveydata) else as.data.frame(surveydata)
  braw <- if (is.character(bcdata)) hfc_load(bcdata) else as.data.frame(bcdata)

  id <- hfc_vars(sraw, id)
  t1vars <- hfc_vars(sraw, t1vars); t2vars <- hfc_vars(sraw, t2vars); t3vars <- hfc_vars(sraw, t3vars)
  tvars <- unique(c(t1vars, t2vars, t3vars))
  ttest <- hfc_vars(sraw, ttest); prtest <- hfc_vars(sraw, prtest)
  signrank <- hfc_vars(sraw, signrank); reliability <- hfc_vars(sraw, reliability)
  keepsurvey <- hfc_vars(sraw, keepsurvey)
  for (v in c(enumerator, enumteam, surveydate)) if (!(v %in% names(sraw))) hfc_stop("variable %s not found in survey data", v)

  scols <- unique(c(id, tvars, enumerator, enumteam, ttest, signrank, prtest, reliability, keepsurvey, surveydate))
  sd <- sraw[, scols, drop = FALSE]

  if (!hfc_isid(sd, id))
    hfc_stop("variable(s) \"%s\" does not uniquely identify the observations in survey data", paste(id, collapse = " "))
  for (v in id) if (!is.character(sd[[v]])) {
    mx <- hfc_max(abs(hfc_num(sd[[v]])))
    nd <- abs(floor(log10(mx))) + 1
    if (is.finite(nd) && nd > 20) {
      message("Warning: cannot reversibly convert ", v, " to string without loss of precision. Consider using a different ID or convert yourself.")
      message("Columns widths may not automatically adjust for this variable.")
    } else if (is.finite(nd) && nd > 8) {
      message("Warning: using large numeric IDs may result in loss of precision. Consider converting to string!")
      message("Columns widths may not automatically adjust for this variable.")
    }
  }
  if (!hfc_is_num(sd[[enumerator]]))
    hfc_stop("Enumerator variable %s in enumerator() must be a numeric variable.", enumerator)
  if (!is.null(enumteam) && !hfc_is_num(sd[[enumteam]])) {
    message("Enumerator team variable ", enumteam, " in enumteam() must be a numeric variable.")
    message("enumteam option will not be included in output.")
    enumteam <- NULL
  }
  bad <- grep("^_bc", names(sd), value = TRUE)
  if (length(bad)) hfc_stop("variable %s has illegal prefix _bc in survey data", bad[1])
  # NOTE(stata-bug): Stata loops `foreach opt in surveydate bcdate { confirm numeric var `opt' }`
  # i.e. it checks variables literally named "surveydate"/"bcdate"; the check is a no-op.
  if (!(hfc_is_date(sd[[surveydate]]) || hfc_is_datetime(sd[[surveydate]]) || hfc_is_num(sd[[surveydate]])))
    hfc_stop("Variable %s in option surveydate() must be a date formatted variable", surveydate)

  sd <- .hfc_backcheck_change_str(sd, tvars, nosymbol, lower, upper, trim)

  for (v in keepsurvey) {
    if (v %in% id) hfc_stop("variable %s not allowed in both id() and keepsurvey() options", v)
    if (v %in% enumerator) hfc_stop("variable %s not allowed in both enumerator() and keepsurvey() options", v)
    if (!is.null(enumteam) && v %in% enumteam) hfc_stop("variable %s not allowed in both enumteam() and keepsurvey() options", v)
  }

  # okrange parsing
  okrvars <- character(0); okrmins <- character(0); okrmaxs <- character(0)
  showokrange <- !e0(okrange)
  if (showokrange) {
    okr <- gsub(" ", "", paste(okrange, collapse = ","))
    while (nchar(okr) > 0) {
      pos <- regexpr("]", okr, fixed = TRUE)
      comb <- if (pos > 0) substr(okr, 1, pos) else okr
      lb <- regexpr("[", comb, fixed = TRUE)
      okrvar <- if (lb > 0) substr(comb, 1, lb - 1) else comb
      okrvar <- sub("^,", "", okrvar)
      okrcomb <- if (lb > 0) substring(comb, lb) else ""
      p_l <- regexpr("[", okrcomb, fixed = TRUE); p_c <- regexpr(",", okrcomb, fixed = TRUE); p_r <- regexpr("]", okrcomb, fixed = TRUE)
      if (!(p_l > 0 && p_c > p_l && p_r > p_c))
        hfc_stop("option okrange() incorrectly specified: range \"%s\" not allowed", okrcomb)
      if (grepl(",", okrvar, fixed = TRUE))
        hfc_stop("option okrange() incorrectly specified; variable list \"%s\" not allowed", okrvar)
      mn <- substr(okrcomb, p_l + 1, p_c - 1); mx <- substr(okrcomb, p_c + 1, p_r - 1)
      if (mn == "" && mx == "") hfc_stop("option okrange() incorrectly specified: range \"%s\" not allowed", okrcomb)
      if ((mn != "" && !grepl("-", mn, fixed = TRUE)) || (mx != "" && grepl("-", mx, fixed = TRUE)))
        hfc_stop("option okrange() incorrectly specified: range \"%s\" not allowed", okrcomb)
      ov <- tryCatch(hfc_vars(sd, okrvar),
                     error = function(e) hfc_stop("variable %s specified in okrange() not found in survey data", okrvar))
      okrvars <- c(okrvars, ov)
      okrmins <- c(okrmins, rep(if (mn == "") "0" else mn, length(ov)))
      okrmaxs <- c(okrmaxs, rep(if (mx == "") "0" else mx, length(ov)))
      okr <- substring(okr, nchar(comb) + 1)
      okr <- sub("^,", "", okr)
    }
  }
  # NOTE(stata-bug): Stata checks `foreach var of loc rangevars` (an undefined
  # local) so okrange variables are never verified to be t1/t2/t3 variables.

  # stability / reliability test variables
  if (length(c(ttest, prtest, signrank, reliability))) {
    if (length(c(t2vars, t3vars))) {
      t2t3 <- c(t2vars, t3vars)
      for (opt in c("ttest", "prtest", "signrank", "reliability")) {
        for (v in get(opt)) {
          if (!(v %in% t2t3)) hfc_stop("variable %s in option %s must be specified as t2vars() or t2vars() variables", v, opt)
          if (!hfc_is_num(sd[[v]])) hfc_stop("variable %s in option %s must be a numeric variable", v, opt)
        }
      }
    } else {
      hfc_stop("option ttest must be specified with option t2vars() or t3vars() variables")
    }
    tp <- intersect(ttest, prtest); ts <- intersect(ttest, signrank); ps <- intersect(prtest, signrank)
    if (length(tp)) hfc_stop("variable(s) \"%s\" cannot be specified in both ttest and prtest", paste(tp, collapse = " "))
    if (length(ts)) hfc_stop("variable(s) \"%s\" cannot be specified in both ttest and signrank", paste(ts, collapse = " "))
    if (length(ps)) hfc_stop("variable(s) \"%s\" cannot be specified in both prtest and signrank", paste(ps, collapse = " "))
  }

  if (!percent && showid_v > length(c(t1vars, t2vars, t3vars))) {
    hfc_stop("option showid (%s) is higher than the highest possible number of comparisons (%d).\nUse a lower number or use a percentage (add '%%').",
             showid, length(c(t1vars, t2vars, t3vars)))
  }


  # ------------------------------------------------------------- bc data --
  bcols_need <- unique(c(id, tvars, backchecker, bcteam, ttest, signrank, prtest, reliability, hfc_vars(braw, keepbc), bcdate))
  miss <- setdiff(bcols_need, names(braw))
  if (length(miss)) hfc_stop("variable %s not found in backcheck data", miss[1])
  keepbc <- hfc_vars(braw, keepbc)
  bd <- braw[, bcols_need, drop = FALSE]
  if (!hfc_isid(bd, id))
    hfc_stop("variable(s) \"%s\" does not uniquely identify the observations in backcheck data", paste(id, collapse = " "))
  if (!hfc_is_num(bd[[backchecker]]))
    hfc_stop("Backchecker variable %s in backchecker() must be a numeric variable.", backchecker)
  if (!is.null(bcteam) && !hfc_is_num(bd[[bcteam]])) {
    message("Backchecker team variable ", bcteam, " in bcteam() must be a numeric variable.")
    message("enumteam option will not be included in output.")
    bcteam <- NULL
  }
  bd <- .hfc_backcheck_change_str(bd, tvars, nosymbol, lower, upper, trim)
  for (v in okrvars) if (!(v %in% names(bd))) hfc_stop("variable %s specified in okrange() not found in backcheck data", v)
  for (v in keepbc) {
    if (v %in% id) hfc_stop("variable %s not allowed in both id() and keepbc() options", v)
    if (v %in% backchecker) hfc_stop("variable %s not allowed in both backchecker() and keepbc() options", v)
    if (!is.null(bcteam) && v %in% bcteam) hfc_stop("variable %s not allowed in both bcteam() and keepbc() options", v)
  }
  pref <- unique(c(tvars, ttest, signrank, prtest, reliability, keepbc, bcdate))
  for (v in pref) {
    if (grepl("^_bc", v)) hfc_stop("variable %s has illegal prefix _bc in backcheck data", v)
    if (nchar(v) > 29) hfc_stop("variable %s is too long. Rename variable", v)
  }
  names(bd)[match(pref, names(bd))] <- paste0("_bc", pref)
  bc_keepbc <- if (length(keepbc)) paste0("_bc", keepbc) else character(0)
  bcdate_v <- paste0("_bc", bcdate)

  # ------------------------------------------------------------ merge ----
  skey <- .hfc_backcheck_key(sd, id); bkey <- .hfc_backcheck_key(bd, id)
  mi <- match(skey, bkey)
  bonly <- which(!(bkey %in% skey))
  ns <- nrow(sd); nb <- length(bonly)
  sidx <- c(seq_len(ns), rep(NA_integer_, nb))
  bidx <- c(mi, bonly)
  m <- data.frame(row.names = seq_len(ns + nb))
  for (v in names(sd)) {
    x <- sd[[v]][sidx]
    if (v %in% names(bd) && nb > 0) {   # common variable: master kept, using fills bc-only rows
      y <- bd[[v]][bonly]
      x <- tryCatch({ x[(ns + 1):(ns + nb)] <- y; x }, error = function(e) {
        z <- if (is.character(x)) as.character(x) else hfc_num(x)
        z[(ns + 1):(ns + nb)] <- if (is.character(z)) as.character(y) else hfc_num(y); z })
    }
    m[[v]] <- x
  }
  for (v in setdiff(names(bd), names(sd))) m[[v]] <- bd[[v]][bidx]
  m$`_mergebc` <- ifelse(is.na(sidx), 2L, ifelse(is.na(bidx), 1L, 3L))
  ord <- do.call(order, c(lapply(id, function(v) hfc_num_or_chr(m[[v]])), list(method = "radix")))
  m <- m[ord, , drop = FALSE]; rownames(m) <- NULL

  survey_only <- sum(m$`_mergebc` == 1); bc_only <- sum(m$`_mergebc` == 2)
  # NOTE(stata-bug): _surveyed is _N after the merge, i.e. it includes the
  # back-check-only observations.
  surveyed <- nrow(m)
  m <- m[m$`_mergebc` %in% c(2, 3), , drop = FALSE]
  backchecked <- nrow(m)
  pct_bc <- .hfc_backcheck_piece4(backchecked / surveyed * 100)
  bconly_data <- m[m$`_mergebc` == 2, , drop = FALSE]
  mm <- m[m$`_mergebc` == 3, , drop = FALSE]; rownames(mm) <- NULL
  mm[[surveydate]] <- .hfc_backcheck_as_date(mm[[surveydate]], "surveydate")
  mm[[bcdate_v]] <- .hfc_backcheck_as_date(mm[[bcdate_v]], "bcdate")

  admin <- c(id, enumerator, enumteam, backchecker, bcteam, surveydate, bcdate_v)
  admin <- unique(admin)

  # surveys / backchecks per group
  grp_counts <- function(xall, xbc) {
    ka <- hfc_num_or_chr(xall); kb <- hfc_num_or_chr(xbc)
    keys <- sort(unique(kb), na.last = TRUE)
    data.frame(key = keys,
               surveys = if (is.null(xall)) NA else vapply(keys, function(k) sum(ka %in% k), numeric(1)),
               backchecks = vapply(keys, function(k) sum(kb %in% k), numeric(1)))
  }

  # ------------------------------------------------- comparisons (long) --
  diffs <- list()
  for (i in seq_along(tvars)) {
    var <- tvars[i]; bvar <- paste0("_bc", var)
    x <- mm[[var]]; y <- mm[[bvar]]; n <- nrow(mm)
    vtype <- if (var %in% t1vars) "type 1" else if (var %in% t2vars) "type 2" else "type 3"
    compared <- if (excludemissing) !hfc_missing(x) & !hfc_missing(y) else rep(TRUE, n)
    isnum <- hfc_is_num(x) && !is.character(x)
    if (isnum) {
      for (ev in excludenum) compared[!is.na(hfc_num(y)) & hfc_num(y) == ev] <- FALSE
    } else {
      yy <- as.character(y); yy[is.na(yy)] <- ""
      for (es in excludestr) compared[yy == es] <- FALSE
    }
    vdiff <- ifelse(compared, as.numeric(.hfc_backcheck_neq(x, y)), NA_real_)
    okr_txt <- NULL
    if (isnum) {
      j <- match(var, okrvars)
      if (!is.na(j)) {
        mn <- okrmins[j]; mx <- okrmaxs[j]; xv <- hfc_num(x); yv <- hfc_num(y)
        if (grepl("%$", mn)) { perc <- abs(.hfc_backcheck_float(as.numeric(sub("%", "", mn)) / 100))
          okmin <- if (perc == 0) xv else xv - perc * xv
        } else okmin <- if (as.numeric(mn) == 0) xv else xv + as.numeric(mn)
        if (grepl("%$", mx)) { perc <- abs(.hfc_backcheck_float(as.numeric(sub("%", "", mx)) / 100))
          okmax <- if (perc == 0) xv else xv + perc * xv
        } else okmax <- if (as.numeric(mx) == 0) xv else xv + as.numeric(mx)
        okmin <- .hfc_backcheck_float(okmin); okmax <- .hfc_backcheck_float(okmax)   # gen (float)
        # Stata missing (.) is +Inf: y >= . false, y <= . true
        inr <- !is.na(yv) & !is.na(okmin) & yv >= okmin & (is.na(okmax) | yv <= okmax)
        # NOTE(stata-bug): okrange (and nodiffnum below) also set _vdiff = 0 on
        # rows that were "not compared" (excluded), turning them into "not different".
        vdiff[inr] <- 0
        okr_txt <- ifelse(compared, paste0("okrange of [", mn, ", ", mx, "] is [", .hfc_backcheck_fmt_g(okmin), ", ",
                                           .hfc_backcheck_fmt_g(okmax), "]"), "")
      }
      if (length(nodiffnum)) vdiff[!is.na(hfc_num(y)) & hfc_num(y) %in% nodiffnum] <- 0
    } else {
      yy <- as.character(y); yy[is.na(yy)] <- ""
      for (ns_ in nodiffstr_list) vdiff[yy == ns_] <- 0
    }
    if (!isnum) {
      s_str <- as.character(x); s_str[is.na(s_str)] <- ""
      b_str <- as.character(y); b_str[is.na(b_str)] <- ""
      s_lab <- rep("", n); b_lab <- rep("", n)
    } else {
      s_str <- .hfc_backcheck_tostring(x, attr(sraw[[var]], "format.stata", exact = TRUE))
      b_str <- .hfc_backcheck_tostring(y, attr(braw[[var]], "format.stata", exact = TRUE))
      s_lab <- if (.hfc_backcheck_has_vallab(x)) .hfc_backcheck_decode(x) else rep("", n)
      if (.hfc_backcheck_has_vallab(y)) b_lab <- .hfc_backcheck_decode(y) else { b_lab <- rep(""); s_lab <- rep("", n) }
      b_lab <- rep_len(b_lab, n)
    }
    diffs[[i]] <- data.frame(.row = seq_len(n), `_seq` = i, `_vtype` = rep(vtype, n), `_compared` = as.numeric(compared),
                             `_vdiff` = vdiff, `_vvar` = rep(var, n), `_vvlab` = rep(hfc_varlabel(sd, var, fallback = FALSE), n),
                             `_survey` = s_str, `_surveylab` = s_lab, `_backcheck` = b_str, `_backchecklab` = b_lab,
                             `_okrange` = if (is.null(okr_txt)) rep("", n) else okr_txt,
                             check.names = FALSE, stringsAsFactors = FALSE)
  }
  dl <- do.call(rbind, diffs)
  # attach admin / keep columns (keeps labelled classes)
  ac <- unique(c(admin, keepsurvey, bc_keepbc))
  adm <- mm[dl$.row, ac, drop = FALSE]
  dl <- cbind(adm, dl); rownames(dl) <- NULL
  # NOTE(deviation): Stata sorts by id only (`bysort id`), leaving the within-id
  # order unspecified; here rows are sorted by id then by variable order.
  ord <- do.call(order, c(lapply(id, function(v) hfc_num_or_chr(dl[[v]])), list(dl$`_seq`, method = "radix")))
  dl <- dl[ord, , drop = FALSE]; rownames(dl) <- NULL

  dl$days <- as.numeric(dl[[bcdate_v]] - dl[[surveydate]])
  dl$`_surveyday` <- dl[[surveydate]]
  first <- !duplicated(.hfc_backcheck_key(dl, id))
  days_diff <- .hfc_backcheck_piece4(hfc_mean(dl$days[first]))
  avd <- suppressWarnings(as.numeric(days_diff))
  dl$result <- ifelse(is.na(dl$`_vdiff`), "not compared", ifelse(dl$`_vdiff` == 0, "not different", "different"))

  # -------------------------------------------------------- workbook ------
  if (replace) hfc_rm(filename)
  dir.create(dirname(filename), showWarnings = FALSE, recursive = TRUE)
  wb <- openxlsx::createWorkbook()
  out <- list()

  # ---------------- summary
  types_present <- sort(unique(dl$`_vtype`))          # encode type, gen(vartype)
  code <- match(dl$`_vtype`, types_present)
  k <- length(types_present)
  sdays <- sort(unique(dl$`_surveyday`))
  rt <- data.frame(`_surveyday` = sdays, check.names = FALSE)
  for (cc in seq_len(k)) {
    sel <- code == cc
    vc <- tapply(!is.na(dl$`_vdiff`[sel]), dl$`_surveyday`[sel], sum)
    vd <- tapply(dl$`_vdiff`[sel], dl$`_surveyday`[sel], function(z) sum(z, na.rm = TRUE))
    rt[[paste0("_vdiff", cc)]] <- as.numeric(vd[as.character(sdays)])
    rt[[paste0("valcount", cc)]] <- as.numeric(vc[as.character(sdays)])
  }
  out$rates_time <- as.matrix(cbind(`_surveyday` = as.numeric(sdays - as.Date("1960-01-01")),
                                    rt[, grep("^_vdiff", names(rt)), drop = FALSE],
                                    rt[, grep("^valcount", names(rt)), drop = FALSE]))
  mindate <- min(sdays); maxdate <- max(sdays); count <- as.numeric(maxdate - mindate)
  if (count <= 30) {
    unit <- "days"; titleunit <- "Daily"
    agg <- rt; agg$days <- seq_len(nrow(rt))
  } else {
    if (count > 210) { unit <- "months"; nod <- 30; titleunit <- "Monthly" }
    else { unit <- "weeks"; nod <- 7; titleunit <- "Weekly" }
    units <- ceiling(as.numeric(maxdate - mindate) / nod)
    u <- rep(NA_real_, nrow(rt)); md <- mindate
    for (ii in seq_len(units)) { u[rt$`_surveyday` >= md] <- ii; md <- md + nod }
    rt[[unit]] <- u
    cols <- setdiff(names(rt), c("_surveyday", unit))
    agg <- aggregate(rt[, cols, drop = FALSE], by = list(u = rt[[unit]]), FUN = function(z) sum(z, na.rm = TRUE))
    names(agg)[1] <- unit
  }
  ers <- list()
  for (cc in seq_len(k)) ers[[paste0("error_rate", cc)]] <- round(agg[[paste0("_vdiff", cc)]] / agg[[paste0("valcount", cc)]] * 100, 2)
  vtot <- rowSums(as.matrix(agg[, paste0("valcount", seq_len(k)), drop = FALSE]), na.rm = TRUE)
  dtot <- rowSums(as.matrix(agg[, paste0("_vdiff", seq_len(k)), drop = FALSE]), na.rm = TRUE)
  ers$error_rate_total <- round(dtot / vtot * 100, 2)
  out[[paste0("rates_", unit)]] <- cbind(setNames(data.frame(agg[[unit]]), unit), as.data.frame(ers))
  out[[paste0("rates_", unit)]] <- as.matrix(out[[paste0("rates_", unit)]])

  # graph (errorrates.png)
  png_file <- file.path(tempdir(), "errorrates.png")
  grDevices::png(png_file, width = 460, height = 335)
  ermat <- as.matrix(as.data.frame(ers))
  cols_g <- c("#1a476f", "#90353b", "#55752f", "#e37e00")[seq_len(ncol(ermat))]
  graphics::matplot(agg[[unit]], ermat, type = "o", pch = 16, cex = 0.7,
                    lty = c(rep(2, k), 1), lwd = c(rep(1, k), 2.5), col = cols_g,
                    main = sprintf("Error Rates (%s)", titleunit), xlab = unit, ylab = "%")
  graphics::legend("bottom", inset = c(0, -0.32), xpd = TRUE, horiz = TRUE, bty = "n", cex = 0.8,
                   legend = c(paste("Type", seq_len(k)), "Total"), col = cols_g, lty = c(rep(2, k), 1), pch = 16)
  grDevices::dev.off()

  # summary table
  srows <- data.frame(vartype = c(types_present, rep("", max(0, 3 - k)), "All"), stringsAsFactors = FALSE)
  nr_s <- max(k, 3) + 1  # set obs 4
  srows <- srows[seq_len(nr_s), , drop = FALSE]
  if (k == 4) srows$vartype[4] <- "All"
  valcount <- c(vapply(seq_len(k), function(cc) sum(!is.na(dl$`_vdiff`[code == cc])), numeric(1)), rep(NA, nr_s - k))
  vdiffs <- c(vapply(seq_len(k), function(cc) sum(dl$`_vdiff`[code == cc], na.rm = TRUE), numeric(1)), rep(NA, nr_s - k))
  valcount[4] <- sum(valcount[1:k]); vdiffs[4] <- sum(vdiffs[1:k])
  # NOTE(stata-bug): varcount row i is filled with the count of t`i'vars by
  # position; when a type is absent the rows (codes from encode) are mislabelled.
  varcount <- c(length(t1vars), length(t2vars), length(t3vars), length(c(t1vars, t2vars, t3vars)))
  summ <- data.frame(vartype = srows$vartype, empty = "", varcount = varcount, valcount = valcount,
                     `_vdiff` = vdiffs, error_rate = vdiffs / valcount, check.names = FALSE, stringsAsFactors = FALSE)
  for (v in names(summ)) attr(summ[[v]], "label") <- c(vartype = "Type", empty = "empty", varcount = "# variables",
                                                         valcount = "# values", `_vdiff` = "differences", error_rate = "Error rate (%)")[[v]]
  rates <- as.matrix(summ[, c("varcount", "valcount", "_vdiff", "error_rate")])
  dimnames(rates) <- list(c("type 1", "type 2", "type 3", "total"), c("variables", "values", "differences", "error rate"))
  out$rates <- rates
  total_rate <- suppressWarnings(as.numeric(.hfc_backcheck_piece4(summ$error_rate[4])))

  sh <- "summary"
  .hfc_backcheck_export(wb, filename, sh, summ, start_row = 11, start_col = 3)
  openxlsx::showGridLines(wb, sh, showGridLines = FALSE)
  openxlsx::writeData(wb, sh, "Back Check Analysis", startCol = 3, startRow = 2)
  openxlsx::writeData(wb, sh, paste0("Average Days between Survey and Backcheck: ", days_diff), startCol = 3, startRow = 4)
  openxlsx::writeData(wb, sh, "Date: ", startCol = 3, startRow = 6)
  openxlsx::writeData(wb, sh, trimws(format(Sys.Date(), "%e %b %Y")), startCol = 5, startRow = 6)
  openxlsx::writeData(wb, sh, "Backcheck Rate: ", startCol = 3, startRow = 7)
  openxlsx::writeData(wb, sh, sprintf("%d / %d (%s%%)", backchecked, surveyed, pct_bc), startCol = 5, startRow = 7)
  openxlsx::writeData(wb, sh, "Survey Data:", startCol = 3, startRow = 8)
  openxlsx::writeData(wb, sh, sname, startCol = 5, startRow = 8)
  openxlsx::writeData(wb, sh, "Backcheck Data:", startCol = 3, startRow = 9)
  openxlsx::writeData(wb, sh, bname, startCol = 5, startRow = 9)
  .hfc_backcheck_style(wb, sh, 2, 3, fontName = "Calibri", fontSize = 18)
  .hfc_backcheck_style(wb, sh, 2:9, 3, textDecoration = "bold")
  .hfc_backcheck_style(wb, sh, 6:9, 4, textDecoration = "italic")
  openxlsx::setColWidths(wb, sh, cols = c(1, 2, 9, 4, 5:8), widths = c(2, 2, 2, 5, rep(12, 4)))
  .hfc_backcheck_style(wb, sh, 11:15, 3:8, halign = "center")
  .hfc_backcheck_style(wb, sh, 12:15, 8, numFmt = "0.00%")
  openxlsx::mergeCells(wb, sh, cols = 3:8, rows = 2)
  openxlsx::mergeCells(wb, sh, cols = 3:8, rows = 4)
  for (r in 6:15) openxlsx::mergeCells(wb, sh, cols = 3:4, rows = r)
  .hfc_backcheck_style(wb, sh, 6:9, 3, halign = "left")
  .hfc_backcheck_style(wb, sh, 11:15, 3, textDecoration = "bold")
  .hfc_backcheck_style(wb, sh, 11, 5:8, textDecoration = "bold")
  .hfc_backcheck_style(wb, sh, 2:4, 3, halign = "center")
  border <- 16
  if (count > 1) {
    openxlsx::insertImage(wb, sh, png_file, startRow = 18, startCol = 3, width = 460, height = 335, units = "px")
    border <- 38
  }
  .hfc_backcheck_border(wb, sh, border, 2:9, "bottom", "thin")
  .hfc_backcheck_border(wb, sh, 2:border, 2, "left", "thin")
  .hfc_backcheck_border(wb, sh, 2:border, 9, "right", "thin")
  .hfc_backcheck_border(wb, sh, 2, 2:9, "top", "thin")

  # ---------------- backcheck only
  if (bc_only > 0) {
    bo <- bconly_data
    bco <- bo[, c(id, backchecker, bcteam), drop = FALSE]
    bco[[bcdate]] <- bo[[bcdate_v]]
    bco <- bco[do.call(order, c(lapply(names(bco), function(v) hfc_num_or_chr(bco[[v]])), list(method = "radix"))), , drop = FALSE]
    for (v in names(bco)) {
      x <- bco[[v]]
      if (.hfc_backcheck_has_vallab(x)) x <- if (nolabel) hfc_num(x) else .hfc_backcheck_decode(x)
      bco[[v]] <- x
    }
    sh <- "backcheck only"
    .hfc_backcheck_export(wb, filename, sh, bco, start_row = 3, start_col = 2, header = "name")
    nr <- nrow(bco) + 3; nv <- ncol(bco)
    .hfc_backcheck_border(wb, sh, 3:nr, 1, "right"); .hfc_backcheck_border(wb, sh, 3:nr, nv + 1, "right")
    .hfc_backcheck_border(wb, sh, 3:4, 2:(nv + 1), "top"); .hfc_backcheck_border(wb, sh, nr, 2:(nv + 1), "bottom")
    .hfc_backcheck_style(wb, sh, 3:nr, 2:(nv + 1), halign = "center")
    w <- vapply(names(bco), function(v) max(.hfc_backcheck_strwidth(bco[[v]]),
                                               if (v %in% c(surveydate, bcdate, "starttime", "endtime", "submissiondate")) 16 else nchar(v)), numeric(1))
    openxlsx::setColWidths(wb, sh, cols = c(1, seq_len(nv) + 1), widths = c(1, w))
    openxlsx::setRowHeights(wb, sh, rows = 1, heights = 10)
  }

  # ---------------- IDs (showid)
  idk <- .hfc_backcheck_key(dl, id)
  iddiff <- tapply(dl$`_vdiff`, idk, function(z) sum(z, na.rm = TRUE))[idk]
  idcnt <- as.numeric(table(idk)[idk])
  iderr <- as.numeric(iddiff) / idcnt
  idmin <- min(idcnt); idmax <- max(idcnt)
  firstid <- !duplicated(idk)
  sel <- firstid & (if (percent) iderr > showid_v else as.numeric(iddiff) > showid_v)
  showidcount <- sum(sel)
  if (showidcount > 0) {
    ids <- dl[sel, c(id, enumerator, enumteam, backchecker, bcteam), drop = FALSE]
    ids$`_iddifferences` <- as.numeric(iddiff[sel]); ids$`_idcount` <- idcnt[sel]; ids$`_iderror_rate` <- iderr[sel]
    ids <- ids[order(-ids$`_iderror_rate`, method = "radix"), , drop = FALSE]
    for (v in c(id, enumerator, enumteam, backchecker, bcteam)) {
      x <- ids[[v]]
      if (.hfc_backcheck_has_vallab(x)) x <- if (nolabel) hfc_num(x) else .hfc_backcheck_decode(x)
      attr(x, "label") <- v
      ids[[v]] <- x
    }
    attr(ids$`_iddifferences`, "label") <- "differences"
    attr(ids$`_idcount`, "label") <- "# compared"
    attr(ids$`_iderror_rate`, "label") <- "% different"
    sh <- "IDs"
    .hfc_backcheck_export(wb, filename, sh, ids, start_row = 3, start_col = 2)
    # format_showids: nvars = # id/enum/bcer vars + 4 = last sheet column
    nr <- nrow(ids) + 3; nv <- ncol(ids) + 1
    .hfc_backcheck_border(wb, sh, 3:nr, 1, "right"); .hfc_backcheck_border(wb, sh, 3:nr, nv - 3, "right")
    .hfc_backcheck_border(wb, sh, 3:nr, nv, "right")
    .hfc_backcheck_border(wb, sh, 3:4, 2:nv, "top"); .hfc_backcheck_border(wb, sh, nr, 2:nv, "bottom")
    .hfc_backcheck_style(wb, sh, 4:nr, nv, numFmt = "0.00%")
    .hfc_backcheck_style(wb, sh, 3:nr, 5:nv, halign = "center")
    w <- vapply(names(ids)[seq_len(nv - 4)], function(v) max(.hfc_backcheck_strwidth(ids[[v]]), nchar(v)), numeric(1))
    openxlsx::setColWidths(wb, sh, cols = c(1, seq_len(nv - 4) + 1, (nv - 2):nv), widths = c(1, w, 10, 10, 10))
    openxlsx::setRowHeights(wb, sh, rows = 1, heights = 10)
  }
  # NOTE(stata-bug): showid (a fraction when given in %) is compared to counts
  if (showid_v > idmin && showid_v < idmax) {
    message(sprintf("opt showid (%s) is higher than the number of comparisons for at least one observation (%d).", .hfc_backcheck_fmt_g(showid_v), idmin))
    message("Use a lower number to include all observations or use a percentage (add '%'). ")
  }

  # ---------------- comparison
  cd <- if (full) dl else dl[!is.na(dl$`_vdiff`) & dl$`_vdiff` == 1, , drop = FALSE]
  comp <- data.frame(row.names = seq_len(nrow(cd)))
  for (v in c(id, enumerator, enumteam, backchecker, bcteam)) { comp[[v]] <- .hfc_backcheck_export_col(cd[[v]], nolabel); attr(comp[[v]], "label") <- v }
  comp$variable <- cd$`_vvar`; comp$label <- cd$`_vvlab`; comp$type <- cd$`_vtype`
  comp$survey <- cd$`_survey`; comp$surveylabel <- cd$`_surveylab`
  comp$backcheck <- cd$`_backcheck`; comp$backchecklabel <- cd$`_backchecklab`
  comp$result <- cd$result
  if (showokrange) comp$okrange <- cd$`_okrange`
  comp[[surveydate]] <- cd[[surveydate]]; attr(comp[[surveydate]], "label") <- surveydate
  comp[[bcdate_v]] <- cd[[bcdate_v]]; attr(comp[[bcdate_v]], "label") <- bcdate
  comp$days <- cd$days
  for (v in keepsurvey) { comp[[v]] <- .hfc_backcheck_export_col(cd[[v]], nolabel); attr(comp[[v]], "label") <- v }
  # keepbc: exported separately with first(var) and without nolabel
  for (v in bc_keepbc) { comp[[v]] <- .hfc_backcheck_export_col(cd[[v]], FALSE); attr(comp[[v]], "label") <- sub("^_bc", "", v) }
  for (v in c("variable", "label", "type", "survey", "surveylabel", "backcheck", "backchecklabel", "result", "okrange", "days"))
    if (v %in% names(comp)) attr(comp[[v]], "label") <- v
  sh <- "comparison"
  .hfc_backcheck_export(wb, filename, sh, comp, start_row = 4, start_col = 2)
  # format_comparison
  nrows <- nrow(comp) + 4; ncols <- ncol(comp) + 1
  idcount <- length(id); enumcount <- length(c(enumerator, enumteam)); bcer <- length(c(backchecker, bcteam))
  keeps <- length(keepsurvey); keepb <- length(keepbc); okrn <- as.integer(showokrange)
  idpos <- 1 + idcount; enumpos <- idpos + enumcount; bcerpos <- enumpos + bcer
  varpos <- bcerpos + 3; spos <- varpos + 2; bcpos <- spos + 2; respos <- bcpos + 1
  okrpos <- respos + okrn; datepos <- okrpos + 3; keepspos <- datepos + keeps; keepbcpos <- keepspos + keepb
  positions <- c(idpos, enumpos, bcerpos, varpos, spos, bcpos, respos, okrpos, datepos, keepspos, keepbcpos)
  if (nrows < 3000) {
    .hfc_backcheck_border(wb, sh, 4:nrows, 1, "right")
    for (p in positions) .hfc_backcheck_border(wb, sh, 4:nrows, p, "right")
  }
  openxlsx::mergeCells(wb, sh, cols = (okrpos + 1):(okrpos + 3), rows = 2)
  if (keeps > 0) {
    openxlsx::mergeCells(wb, sh, cols = (datepos + 1):keepspos, rows = 3)
    openxlsx::writeData(wb, sh, "Keep in Survey", startCol = datepos + 1, startRow = 3)
  }
  if (keepb > 0) {
    openxlsx::mergeCells(wb, sh, cols = (keepspos + 1):keepbcpos, rows = 3)
    openxlsx::writeData(wb, sh, "Keep in Backcheck", startCol = keepspos + 1, startRow = 3)
  }
  if (nrows < 3000) {
    .hfc_backcheck_border(wb, sh, 4:5, 2:keepbcpos, "top")
    .hfc_backcheck_border(wb, sh, nrows, 2:keepbcpos, "bottom")
    lastcol <- if (keeps > 0) keepbcpos else datepos
    .hfc_backcheck_border(wb, sh, 3, (okrpos + 1):lastcol, "TopBottomLeftRight")
  } else .hfc_backcheck_border(wb, sh, 4, 2:ncols, "bottom")
  .hfc_backcheck_style(wb, sh, 2:3, (okrpos + 1):keepbcpos, halign = "center")
  .hfc_backcheck_style(wb, sh, 2:4, 2:keepbcpos, textDecoration = "bold")
  openxlsx::writeData(wb, sh, "Survey", startCol = okrpos + 1, startRow = 3)
  openxlsx::writeData(wb, sh, "Backcheck", startCol = okrpos + 2, startRow = 3)
  openxlsx::writeData(wb, sh, "Difference", startCol = okrpos + 3, startRow = 3)
  openxlsx::writeData(wb, sh, "Dates", startCol = okrpos + 1, startRow = 2)
  openxlsx::setRowHeights(wb, sh, rows = 1, heights = 10)
  # adjust_column_width (decoded in-memory data; numeric columns count 0)
  wcols <- vapply(names(comp), function(v) {
    nl <- if (v == "days") 12 else if (v %in% c(surveydate, bcdate_v, "starttime", "endtime", "submissiondate",
                                                 "_bcstarttime", "_bcendtime", "_bcsubmissiondate")) 16 else nchar(v)
    x <- comp[[v]]
    if (v %in% names(cd) && .hfc_backcheck_has_vallab(cd[[v]])) x <- .hfc_backcheck_decode(cd[[v]])
    min(max(nl, .hfc_backcheck_strwidth(x)) + 3, 101)
  }, numeric(1))
  openxlsx::setColWidths(wb, sh, cols = seq_len(ncols), widths = c(1, wcols))
  wrap <- which(wcols >= 101) + 1
  if (length(wrap)) .hfc_backcheck_style(wb, sh, 1:nrows, wrap, wrapText = TRUE)

  # ---------------- enumerator / team / backchecker statistics
  create_stats <- function(grp, counts, bc, enumlabel) {
    g <- dl[[grp]]; gk <- hfc_num_or_chr(g)
    keys <- sort(unique(gk), na.last = TRUE)
    rep_i <- match(keys, gk)
    res <- data.frame(row.names = seq_along(keys))
    res[[grp]] <- g[rep_i]
    ck <- hfc_num_or_chr(counts$key)
    if (!bc) {
      res$surveys <- counts$surveys[match(keys, ck)]
      res$backchecks <- counts$backchecks[match(keys, ck)]
      res$backcheck_percent <- res$backchecks / res$surveys
      attr(res$backcheck_percent, "label") <- "% backchecked"
    } else res$backchecks <- counts$backchecks[match(keys, ck)]
    tnum <- sub("type ", "", dl$`_vtype`)
    tpres <- sort(unique(tnum))
    for (t in tpres) {
      s <- tnum == t
      cmp <- tapply(dl$`_compared`[s], factor(gk[s], levels = keys), function(z) sum(z, na.rm = TRUE))
      dff <- tapply(dl$`_vdiff`[s], factor(gk[s], levels = keys), function(z) sum(z, na.rm = TRUE))
      res[[paste0("compared", t)]] <- as.numeric(cmp); attr(res[[paste0("compared", t)]], "label") <- "# compared"
      res[[paste0("differences", t)]] <- as.numeric(dff); attr(res[[paste0("differences", t)]], "label") <- "# different"
      res[[paste0("error_rate", t)]] <- as.numeric(dff) / as.numeric(cmp); attr(res[[paste0("error_rate", t)]], "label") <- "% different"
    }
    cc <- grep("^compared\\d$", names(res)); dd <- grep("^differences\\d$", names(res))
    res$compared <- rowSums(as.matrix(res[, cc, drop = FALSE]), na.rm = TRUE); attr(res$compared, "label") <- "# compared"
    res$differences <- rowSums(as.matrix(res[, dd, drop = FALSE]), na.rm = TRUE); attr(res$differences, "label") <- "# different"
    res$error_rate <- res$differences / res$compared; attr(res$error_rate, "label") <- "% different"
    # recode missing/no comparisons to 0
    for (v in setdiff(names(res), grp)) { x <- res[[v]]; l <- attr(x, "label"); x[is.na(x)] <- 0; attr(x, "label") <- l; res[[v]] <- x }
    # NOTE(stata-bug): create_stats decodes the group variable when nolabel IS
    # specified (inverted test); combined with export's nolabel this means
    # value labels are always exported on the stats sheets.
    x <- res[[grp]]
    if (.hfc_backcheck_has_vallab(x)) x <- .hfc_backcheck_decode(x) else x <- hfc_num_or_chr(x)
    attr(x, "label") <- enumlabel
    res[[grp]] <- x
    list(res = res, types = as.integer(tpres))
  }
  sort_stats <- function(res, overall_first) {
    keys <- c(if (overall_first) "error_rate", grep("^error_rate\\d$", names(res), value = TRUE))
    o <- do.call(order, c(lapply(keys, function(k) -res[[k]]), list(method = "radix")))
    res <- res[o, , drop = FALSE]; rownames(res) <- NULL; res
  }
  format_enumstats <- function(sh, res, grp, bc) {
    ncols <- ncol(res) + 1; nrows <- nrow(res) + 2
    .hfc_backcheck_border(wb, sh, 3, 2:ncols, "top"); .hfc_backcheck_border(wb, sh, 3, 2:ncols, "bottom")
    .hfc_backcheck_border(wb, sh, nrows + 1, 2:ncols, "bottom")
    .hfc_backcheck_border(wb, sh, 3:(nrows + 1), 2, "left"); .hfc_backcheck_border(wb, sh, 3:(nrows + 1), 2, "right")
    .hfc_backcheck_border(wb, sh, 3:(nrows + 1), ncols, "right")
    openxlsx::setRowHeights(wb, sh, rows = 1, heights = 10)
    openxlsx::setColWidths(wb, sh, cols = 1:ncols, widths = c(1, max(12, .hfc_backcheck_strwidth(res[[grp]])), rep(14, ncols - 2)))
    .hfc_backcheck_style(wb, sh, 3:(nrows + 1), 3:ncols, halign = "center")
    if (bc) { cur <- 4; .hfc_backcheck_style(wb, sh, 4:(nrows + 1), 4, numFmt = "0.00"); .hfc_backcheck_border(wb, sh, 2, 5:ncols, "TopBottomLeftRight") }
    else { cur <- 5; .hfc_backcheck_style(wb, sh, 4:(nrows + 1), 5, numFmt = "0.00%"); .hfc_backcheck_border(wb, sh, 2, 6:ncols, "TopBottomLeftRight") }
    .hfc_backcheck_border(wb, sh, 3:(nrows + 1), cur, "right")
    for (i in 1:3) if (get(paste0("t", i, "flag"))) {
      cur <- cur + 3
      .hfc_backcheck_border(wb, sh, 3:(nrows + 1), cur, "right")
      .hfc_backcheck_style(wb, sh, 4:(nrows + 1), cur, numFmt = "0.00%")
      openxlsx::mergeCells(wb, sh, cols = (cur - 2):cur, rows = 2)
      openxlsx::writeData(wb, sh, paste("type", i), startCol = cur - 2, startRow = 2)
    }
    .hfc_backcheck_border(wb, sh, 3:(nrows + 1), ncols, "right")
    .hfc_backcheck_style(wb, sh, 4:(nrows + 1), ncols, numFmt = "0.00%")
    openxlsx::mergeCells(wb, sh, cols = (ncols - 2):ncols, rows = 2)
    openxlsx::writeData(wb, sh, "all", startCol = ncols - 2, startRow = 2)
    .hfc_backcheck_style(wb, sh, 2, 5:ncols, halign = "center")
    .hfc_backcheck_style(wb, sh, 2:3, 2:ncols, textDecoration = "bold")
  }
  t1flag <- length(t1vars) > 0; t2flag <- length(t2vars) > 0; t3flag <- length(t3vars) > 0
  rate_mats <- function(res, grp, prefix) {
    o <- list()
    for (i in 3:1) if (paste0("error_rate", i) %in% names(res))
      o[[paste0(prefix, i)]] <- setNames(data.frame(res[[grp]], res[[paste0("error_rate", i)]]), c(grp, paste0("error_rate", i)))
    o[[prefix]] <- setNames(data.frame(res[[grp]], res$error_rate), c(grp, "error_rate"))
    o
  }
  write_stats <- function(sh, res) .hfc_backcheck_export(wb, filename, sh, res, start_row = 3, start_col = 2)

  ecounts <- grp_counts(sd[[enumerator]], mm[[enumerator]])
  st <- create_stats(enumerator, ecounts, FALSE, "enumerator")
  res <- sort_stats(st$res, FALSE)
  write_stats("enumerator stats", res)
  out <- c(out, rate_mats(res, enumerator, "enum"))
  out$enum_bc <- setNames(data.frame(res[[enumerator]], round(res$backcheck_percent * 100, 2)), c(enumerator, "bc_pct"))
  format_enumstats("enumerator stats", res, enumerator, FALSE)

  if (!is.null(enumteam)) {
    tcounts <- grp_counts(sd[[enumteam]], mm[[enumteam]])
    st <- create_stats(enumteam, tcounts, FALSE, "enum team")
    res <- sort_stats(st$res, TRUE)
    write_stats("enumerator team stats", res)
    out <- c(out, rate_mats(res, enumteam, "enumteam"))
    out$enumteam_bc <- setNames(data.frame(res[[enumteam]], round(res$backcheck_percent * 100, 2)), c(enumteam, "backcheck_percent"))
    format_enumstats("enumerator team stats", res, enumteam, FALSE)
  }

  bc_stats <- function(grp, label, sheet, prefix) {
    bcounts <- grp_counts(NULL, bd[[grp]])
    st <- create_stats(grp, bcounts, TRUE, label)
    res <- st$res
    gk <- hfc_num_or_chr(mm[[grp]])
    avgd <- tapply(as.numeric(mm[[bcdate_v]] - mm[[surveydate]]), gk, mean)
    keys_raw <- sort(unique(hfc_num_or_chr(dl[[grp]])), na.last = TRUE)
    days <- as.numeric(avgd[as.character(keys_raw)])
    attr(days, "label") <- "average days"
    res <- cbind(res[, 1:2, drop = FALSE], days = days, res[, -(1:2), drop = FALSE])
    attr(res$days, "label") <- "average days"
    res <- sort_stats(res, TRUE)
    write_stats(sheet, res)
    o <- rate_mats(res, grp, prefix)
    o[[paste0(prefix, "_avd")]] <- setNames(data.frame(res[[grp]], res$days), c(grp, "average_days"))
    format_enumstats(sheet, res, grp, TRUE)
    o
  }
  out <- c(out, bc_stats(backchecker, "backchecker", "backchecker stats", "backchecker"))
  if (!is.null(bcteam)) out <- c(out, bc_stats(bcteam, "bc team", "backchecker team stats", "bcteam"))

  # ---------------- variable stats
  testvars <- c(ttest, prtest, signrank, reliability)
  vs <- list()
  for (var in tvars) {
    s <- dl$`_vvar` == var
    type <- if (var %in% t1vars) "type 1" else if (var %in% t2vars) "type 2" else "type 3"
    diff <- sum(dl$`_vdiff`[s] %in% 1)
    total <- sum(dl$`_compared`[s] == 1)
    label <- hfc_varlabel(sd, var, fallback = FALSE)
    row <- list(variable = var, label = label, type = type, diffs = NA_real_, total = 0, error_rate = NA_real_,
                surveymean = NA_real_, bcmean = NA_real_, differences = NA_real_, test = "", pvalue = NA_real_,
                srv = NA_real_, ratio = NA_real_)
    if (total > 0) {
      cs <- s & dl$`_compared` == 1
      # destring _survey _backcheck, force
      sv <- suppressWarnings(as.numeric(dl$`_survey`[cs])); bv <- suppressWarnings(as.numeric(dl$`_backcheck`[cs]))
      sv[grepl("^\\.", dl$`_survey`[cs]) & !grepl("^\\.\\d", dl$`_survey`[cs])] <- NA
      bv[grepl("^\\.", dl$`_backcheck`[cs]) & !grepl("^\\.\\d", dl$`_backcheck`[cs])] <- NA
      sm <- bm <- dfm <- NA_real_
      if (var %in% testvars) {
        sm <- round(hfc_mean(sv), 2); bm <- round(hfc_mean(bv), 2); dfm <- round(sm - bm, 2)
      }
      srv <- ratio <- NA_real_
      if (var %in% reliability) {
        srv <- hfc_sd(sv - bv)^2 / 2
        ratio <- 1 - srv / hfc_sd(bv)^2
      }
      test <- ""; pv <- NA_real_
      if (var %in% ttest) { test <- "ttest"; pv <- .hfc_backcheck_ttest(sv, bv) }
      else if (var %in% prtest) { test <- "prtest"; pv <- .hfc_backcheck_prtest(sv, bv, var) }
      else if (var %in% signrank) { test <- "signrank"; pv <- .hfc_backcheck_signrank(sv, bv) }
      else { sm <- bm <- dfm <- NA_real_ }   # Stata blanks means for reliability-only vars
      row[c("diffs", "total", "error_rate", "surveymean", "bcmean", "differences", "test", "pvalue", "srv", "ratio")] <-
        list(diff, total, round(diff / total, 2), sm, bm, dfm, test, pv, srv, ratio)
    }
    vs[[length(vs) + 1]] <- as.data.frame(row, stringsAsFactors = FALSE)
  }
  vstats <- do.call(rbind, vs)
  vstats <- vstats[, c("variable", "label", "type", "diffs", "total", "error_rate", "test", "surveymean", "bcmean",
                       "differences", "pvalue", "srv", "ratio")]
  mt <- length(c(ttest, prtest, signrank)) > 0
  rlb <- length(reliability) > 0
  if (!mt) vstats <- vstats[, setdiff(names(vstats), c("surveymean", "bcmean", "differences", "pvalue", "test"))]
  if (!rlb) vstats <- vstats[, setdiff(names(vstats), c("srv", "ratio"))]
  vlabs <- c(surveymean = "survey mean", bcmean = "backcheck mean", test = "test type")
  for (v in names(vstats)) attr(vstats[[v]], "label") <- if (v %in% names(vlabs)) vlabs[[v]] else v
  sh <- "variable stats"
  .hfc_backcheck_export(wb, filename, sh, vstats, start_row = 3, start_col = 2)
  nrows <- nrow(vstats) + 3; ncols <- ncol(vstats) + 1
  if (mt) {
    testpos <- 12
    openxlsx::setColWidths(wb, sh, cols = 8:12, widths = c(10, 11, 14, 10, 10))
    openxlsx::mergeCells(wb, sh, cols = 8:12, rows = 2)
    .hfc_backcheck_style(wb, sh, 2, 8:12, halign = "center")
    openxlsx::writeData(wb, sh, "mean comparison test", startCol = 8, startRow = 2)
    .hfc_backcheck_style(wb, sh, 3:nrows, 8:12, numFmt = "0.00")
  } else testpos <- 7
  if (rlb) {
    openxlsx::setColWidths(wb, sh, cols = testpos:(testpos + 2), widths = 10)
    openxlsx::mergeCells(wb, sh, cols = (testpos + 1):ncols, rows = 2)
    .hfc_backcheck_style(wb, sh, 2, (testpos + 1):ncols, halign = "center")
    openxlsx::writeData(wb, sh, "reliability test", startCol = testpos + 1, startRow = 2)
    .hfc_backcheck_style(wb, sh, 3:nrows, (testpos + 1):ncols, numFmt = "0.00")
  }
  for (p in unique(c(1, 4, 7, testpos, ncols))) .hfc_backcheck_border(wb, sh, 3:nrows, p, "right")
  .hfc_backcheck_border(wb, sh, 3, 2:ncols, "top"); .hfc_backcheck_border(wb, sh, 3, 2:ncols, "bottom")
  .hfc_backcheck_border(wb, sh, nrows, 2:ncols, "bottom")
  .hfc_backcheck_style(wb, sh, 2:3, 2:ncols, textDecoration = "bold")
  openxlsx::setRowHeights(wb, sh, rows = 1, heights = 10)
  openxlsx::setColWidths(wb, sh, cols = 1:3, widths = c(1, max(nchar("variable"), .hfc_backcheck_strwidth(vstats$variable)),
                                                       max(nchar("label"), .hfc_backcheck_strwidth(vstats$label))))
  openxlsx::setColWidths(wb, sh, cols = 4:7, widths = 10)
  .hfc_backcheck_style(wb, sh, 3:nrows, 7, numFmt = "0.00%")
  .hfc_backcheck_style(wb, sh, 4:nrows, 5:ncols, halign = "center")

  openxlsx::saveWorkbook(wb, filename, overwrite = TRUE)

  # ---------------- r() results
  for (i in c(3, 2, 1, 0)) {
    vv <- if (i == 0) vstats else vstats[vstats$type == paste("type", i), , drop = FALSE]
    vv <- vv[!duplicated(vv$variable), , drop = FALSE]
    if (nrow(vv)) {
      mtx <- matrix(vv$error_rate, nrow = 1, dimnames = list(NULL, vv$variable))
      out[[if (i == 0) "var" else paste0("var", i)]] <- mtx
    }
  }
  if (length(testvars)) for (tst in c("ttest", "prtest", "signrank", "reliability")) for (i in c(3, 2, 0)) {
    if (!("test" %in% names(vstats))) next
    vv <- vstats[vstats$test == tst & (i == 0 | vstats$type == paste("type", i)), , drop = FALSE]
    vv <- vv[!duplicated(vv$variable), , drop = FALSE]
    if (nrow(vv)) out[[if (i == 0) tst else paste0(tst, i)]] <- matrix(vv$pvalue, nrow = 1, dimnames = list(NULL, vv$variable))
  }
  out <- c(list(showid = showidcount, bc_only = bc_only, total_rate = total_rate, avd = avd,
                survey = surveyed, bc = backchecked, survey_only = survey_only,
                filename = filename, comparisons = dl, varstats = vstats), out)
  invisible(out)
}
