# =============================================================================
#  hfc-toolkit : shared utilities
#  Shared utilities.
#
#  Conventions used across all check functions
#  -------------------------------------------
#  * Data are data.frames read with haven::read_dta() (or any data.frame).
#    Value labels are haven "labelled" vectors; variable labels are the
#    "label" attribute.
#  * Stata extended missing values (.d, .r, ...) are haven tagged NAs:
#      haven::tagged_na("d"), haven::na_tag(x), haven::is_tagged_na(x, "d")
#    read_dta() / write_dta() preserve them.
#  * Variable lists may be given as character vectors using Stata varlist
#    syntax ("hh_*", "q1-q10", "f_hr_age_r?"); expand with hfc_vars().
#  * Expressions in input sheets (logic asserts, if-conditions) are Stata
#    expressions; evaluate with hfc_eval() which mimics Stata semantics
#    (numeric missing sorts above every number, missing strings are "").
# =============================================================================

suppressPackageStartupMessages({
  library(haven)
  library(openxlsx)
  library(readxl)
})

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0 || (length(a) == 1 && (is.na(a) || identical(a, "")))) b else a

hfc_msg  <- function(...) message(sprintf(...))
hfc_stop <- function(...) stop(sprintf(...), call. = FALSE)

# -----------------------------------------------------------------------------
#  Stata-like varlist expansion
# -----------------------------------------------------------------------------

#' Split a Stata-style varlist string into tokens. Accepts character vectors.
hfc_tokens <- function(x) {
  if (is.null(x)) return(character(0))
  x <- unlist(strsplit(paste(x[!is.na(x)], collapse = " "), "\\s+"))
  # allow "a - b" with spaces around the dash
  x <- x[x != ""]
  out <- character(0); i <- 1
  isname <- function(z) grepl("^[A-Za-z_][A-Za-z0-9_*?~]*$", z)
  while (i <= length(x)) {
    # "a - b" (spaced variable range)
    if (i + 2 <= length(x) && x[i + 1] == "-" && isname(x[i]) && isname(x[i + 2])) {
      out <- c(out, paste0(x[i], "-", x[i + 2])); i <- i + 3
    } else if (i + 1 <= length(x) && grepl("-$", x[i]) && isname(sub("-$", "", x[i])) && isname(x[i + 1])) {
      out <- c(out, paste0(x[i], x[i + 1])); i <- i + 2
    } else if (i + 1 <= length(x) && isname(x[i]) && grepl("^-[A-Za-z_]", x[i + 1])) {
      out <- c(out, paste0(x[i], x[i + 1])); i <- i + 2
    } else {
      out <- c(out, x[i]); i <- i + 1
    }
  }
  out
}

#' Expand a Stata varlist against the names of `data` (or a character vector
#' of names). Supports *, ?, ~ wildcards, var1-var2 ranges and _all.
#' Unmatched tokens raise an error unless `strict = FALSE`.
hfc_vars <- function(data, spec, strict = TRUE, unique = TRUE) {
  nms <- if (is.character(data)) data else names(data)
  toks <- hfc_tokens(spec)
  out <- character(0)
  for (t in toks) {
    if (t %in% c("_all", "*")) { out <- c(out, nms); next }
    if (grepl("^[A-Za-z_][A-Za-z0-9_]*-[A-Za-z_][A-Za-z0-9_]*$", t) && !(t %in% nms)) {
      ends <- strsplit(t, "-", fixed = TRUE)[[1]]
      a <- match(ends[1], nms); b <- match(ends[2], nms)
      if (is.na(a) || is.na(b)) {
        if (strict) hfc_stop("variable range '%s' not found", t) else next
      }
      out <- c(out, nms[a:b]); next
    }
    if (grepl("[*?~]", t)) {
      rx <- paste0("^", gsub("~", ".*", gsub("\\?", ".", gsub("\\*", ".*", t))), "$")
      m <- nms[grepl(rx, nms)]
      if (!length(m) && strict) hfc_stop("variable %s not found", t)
      out <- c(out, m); next
    }
    if (t %in% nms) { out <- c(out, t); next }
    # Stata allows unique abbreviations
    ab <- nms[startsWith(nms, t)]
    if (length(ab) == 1) { out <- c(out, ab); next }
    if (strict) hfc_stop("variable %s not found", t)
  }
  if (unique) out <- unique(out)
  out
}

hfc_has_vars <- function(data, spec) length(hfc_vars(data, spec, strict = FALSE)) > 0

# -----------------------------------------------------------------------------
#  Variable types, labels and missing values
# -----------------------------------------------------------------------------

hfc_is_num <- function(x) is.numeric(x) || inherits(x, "haven_labelled") && is.numeric(unclass(x))
hfc_is_str <- function(x) is.character(x)
hfc_is_datetime <- function(x) inherits(x, "POSIXt")
hfc_is_date <- function(x) inherits(x, "Date")

#' Variable label (Stata "variable label"); falls back to the name.
hfc_varlabel <- function(data, var, fallback = TRUE) {
  l <- attr(data[[var]], "label", exact = TRUE)
  if (is.null(l) || is.na(l) || l == "") if (fallback) var else "" else l
}

#' Set a variable label
hfc_set_varlabel <- function(x, label) { attr(x, "label") <- label; x }

#' Plain numeric values (drops labels; tagged NAs become NA)
hfc_num <- function(x) {
  if (hfc_is_date(x) || hfc_is_datetime(x)) return(as.numeric(x))
  x <- unclass(haven::zap_labels(x)); attributes(x) <- NULL
  suppressWarnings(as.numeric(x))
}

#' Stata missing() : NA (any tag) for numeric, "" or NA for strings
hfc_missing <- function(x) {
  if (is.character(x)) is.na(x) | trimws(x) == "" else is.na(x)
}

#' Is the value Stata extended missing .d / .r / generic
hfc_is_tag <- function(x, tag) if (is.double(unclass(x))) haven::is_tagged_na(as.vector(unclass(x)), tag) else rep(FALSE, length(x))
hfc_is_dk  <- function(x) hfc_is_tag(x, "d")
hfc_is_ref <- function(x) hfc_is_tag(x, "r")

#' ipalabels: replace value-labelled numeric vars by their label text
#' (default) or strip labels to underlying values (nolabel = TRUE).
hfc_labels <- function(data, vars, nolabel = FALSE) {
  vars <- intersect(hfc_vars(data, vars, strict = FALSE), names(data))
  for (v in vars) {
    x <- data[[v]]
    if (inherits(x, "haven_labelled")) {
      lab <- attr(x, "label", exact = TRUE)
      if (nolabel) {
        y <- haven::zap_labels(x)
      } else {
        labs <- attr(x, "labels", exact = TRUE)
        vals <- hfc_num(x)
        y <- as.character(labs[match(vals, unname(labs))] |> names())
        # unlabelled values keep their numeric representation (Stata decode gives "")
        y[is.na(y) & !is.na(vals)] <- ""
      }
      attr(y, "label") <- lab
      data[[v]] <- y
    }
  }
  data
}

#' Label text for a single value of a labelled vector
hfc_value_label <- function(x, value) {
  labs <- attr(x, "labels", exact = TRUE)
  if (is.null(labs)) return(NA_character_)
  n <- names(labs)[match(value, unname(labs))]
  if (length(n) == 0) NA_character_ else n
}

#' ipagettd: convert datetimes to dates (keeps label)
hfc_to_date <- function(x) {
  lab <- attr(x, "label", exact = TRUE)
  if (hfc_is_datetime(x)) x <- as.Date(x, tz = attr(x, "tzone") %||% "UTC")
  else if (!hfc_is_date(x)) hfc_stop("variable is not a date or datetime variable")
  attr(x, "label") <- lab
  x
}

hfc_dates <- function(data, vars) {
  for (v in intersect(vars, names(data))) data[[v]] <- hfc_to_date(data[[v]])
  data
}

#' Recode numeric values in all numeric vars to a tagged NA (e.g. dk -> .d)
hfc_recode_to_tag <- function(data, values, tag) {
  values <- suppressWarnings(as.numeric(hfc_tokens(gsub(",", " ", values))))
  values <- values[!is.na(values)]
  if (!length(values)) return(data)
  for (v in names(data)) {
    x <- data[[v]]
    if (!(is.numeric(x) || inherits(x, "haven_labelled")) || hfc_is_date(x) || hfc_is_datetime(x)) next
    if (!is.double(unclass(x))) { att <- attributes(x); x <- unclass(x); storage.mode(x) <- "double"; attributes(x) <- att }
    hit <- !is.na(unclass(x)) & abs(hfc_num(x) - 0) >= 0 & hfc_num(x) %in% values
    # floating point tolerance for values like .999
    nx <- hfc_num(x)
    hit <- !is.na(nx) & vapply(nx, function(z) any(abs(z - values) < 1e-9), logical(1))
    if (any(hit)) {
      att <- attributes(x)
      y <- unclass(x); y[hit] <- haven::tagged_na(tag)
      attributes(y) <- att
      data[[v]] <- y
    }
  }
  data
}

#' ipaanycount: number of vars in `vars` equal to any of numval / strval
hfc_anycount <- function(data, vars, numval = NULL, strval = NULL) {
  vars <- hfc_vars(data, vars, strict = FALSE)
  out <- integer(nrow(data))
  strval <- hfc_tokens(strval)
  for (v in vars) {
    x <- data[[v]]
    if (is.character(x)) {
      if (!length(strval)) next
      xs <- trimws(gsub("\\s+", " ", x)); xs[is.na(xs)] <- ""
      hit <- rep(FALSE, length(xs))
      for (s in strval) {
        s_rx <- gsub("([][{}()+*^$|\\\\.?])", "\\\\\\1", s)
        hit <- hit | xs == s | grepl(paste0("^", s_rx, " | ", s_rx, " | ", s_rx, "$"), xs)
      }
      out <- out + hit
    } else if (!is.null(numval)) {
      hit <- rep(FALSE, length(x))
      for (nv in numval) {
        if (is.character(nv) && grepl("^\\.[a-z]$", nv)) hit <- hit | hfc_is_tag(x, substring(nv, 2))
        else hit <- hit | (!is.na(hfc_num(x)) & hfc_num(x) == as.numeric(nv))
      }
      out <- out + hit
    }
  }
  out
}

# -----------------------------------------------------------------------------
#  Statistics with Stata definitions
# -----------------------------------------------------------------------------

#' Stata egen pctile() / _pctile percentile definition (= R quantile type 2)
hfc_pctile <- function(x, p) {
  x <- x[!is.na(x)]
  if (!length(x)) return(NA_real_)
  unname(stats::quantile(x, p / 100, type = 2))
}
hfc_iqr <- function(x) hfc_pctile(x, 75) - hfc_pctile(x, 25)
hfc_sd <- function(x) { x <- x[!is.na(x)]; if (length(x) < 2) NA_real_ else stats::sd(x) }
hfc_mean <- function(x) { x <- x[!is.na(x)]; if (!length(x)) NA_real_ else mean(x) }
hfc_min <- function(x) { x <- x[!is.na(x)]; if (!length(x)) NA_real_ else min(x) }
hfc_max <- function(x) { x <- x[!is.na(x)]; if (!length(x)) NA_real_ else max(x) }

# -----------------------------------------------------------------------------
#  Stata expression evaluation (for logic / constraints input sheets)
# -----------------------------------------------------------------------------

# Stata numeric missing values are larger than any number and ordered
# . < .a < .b < ... < .z ; represented here by distinct huge values.
.hfc_miss_base <- 1e300
.hfc_miss_code <- function(tag) .hfc_miss_base * (1 + ifelse(tag == "", 0, match(tag, letters)) / 100)

.hfc_stata_fns <- list(
  missing  = function(...) { a <- list(...); Reduce(`|`, lapply(a, function(z) if (is.character(z)) z == "" else z >= .hfc_miss_base)) },
  mi       = function(...) .hfc_stata_fns$missing(...),
  inlist   = function(x, ...) { v <- list(...); Reduce(`|`, lapply(v, function(z) x == z)) },
  inrange  = function(x, a, b) x >= a & x <= b,
  regexm   = function(s, p) grepl(p, s, perl = TRUE),
  strpos   = function(s, p) { r <- regexpr(p, s, fixed = TRUE); ifelse(r < 0, 0L, r) },
  lower    = tolower, upper = toupper, strlower = tolower, strupper = toupper,
  trim     = trimws, strtrim = trimws, itrim = function(s) gsub("\\s+", " ", s),
  length   = nchar, strlen = nchar, ustrlen = nchar,
  real     = function(s) { r <- suppressWarnings(as.numeric(s)); r[is.na(r)] <- .hfc_miss_base; r },
  string   = function(x) ifelse(x >= .hfc_miss_base, ".", as.character(x)),
  abs      = abs, round = function(x, u = 1) round(x / u) * u, floor = floor, ceil = ceiling,
  int      = trunc, max = function(...) pmax(...), min = function(...) pmin(...),
  cond     = function(c, a, b) ifelse(c, a, b),
  substr   = function(s, a, n) substr(s, a, a + n - 1),
  wordcount = function(s) lengths(strsplit(trimws(s), "\\s+")) * (trimws(s) != "")
)

#' Translate a Stata expression to an R expression string
hfc_stata_to_r <- function(expr) {
  e <- expr
  # protect string literals
  lits <- regmatches(e, gregexpr('"[^"]*"', e))[[1]]
  for (i in seq_along(lits)) e <- sub(lits[i], sprintf("__LIT%d__", i), e, fixed = TRUE)
  e <- gsub("~=", "!=", e, fixed = TRUE)
  e <- gsub("~", "!", e, fixed = TRUE)
  # Stata missing literals (., .a-.z) -> ordered large codes (. < .a < ... < .z)
  m <- gregexpr("(?<![A-Za-z0-9_.])\\.[a-z]?(?![0-9A-Za-z_])", e, perl = TRUE)
  regmatches(e, m) <- lapply(regmatches(e, m), function(z) sprintf(" .hfc_miss_code('%s') ", substring(z, 2)))
  e <- gsub("&", " & ", e, fixed = TRUE)
  e <- gsub("|", " | ", e, fixed = TRUE)
  for (i in seq_along(lits)) e <- sub(sprintf("__LIT%d__", i), lits[i], e, fixed = TRUE)
  e
}

#' Evaluate a Stata expression on a data.frame with Stata semantics.
#' Returns a logical vector (NA treated as FALSE).
hfc_eval <- function(data, expr) {
  if (is.null(expr) || is.na(expr) || trimws(expr) == "") return(rep(TRUE, nrow(data)))
  rx <- hfc_stata_to_r(expr)
  parsed <- tryCatch(parse(text = rx), error = function(e) hfc_stop("cannot parse expression '%s': %s", expr, conditionMessage(e)))
  used <- intersect(all.vars(parsed), names(data))
  env <- list2env(c(.hfc_stata_fns, list(.hfc_miss_code = .hfc_miss_code, .hfc_miss_base = .hfc_miss_base)), parent = baseenv())
  for (v in used) {
    x <- data[[v]]
    if (is.character(x)) { x[is.na(x)] <- ""; assign(v, x, envir = env) }
    else {
      y <- hfc_num(x)
      if (anyNA(y)) {
        tags <- if (is.double(unclass(x))) haven::na_tag(as.vector(unclass(x))) else rep(NA_character_, length(y))
        tags[is.na(tags)] <- ""
        y[is.na(y)] <- .hfc_miss_code(tags[is.na(y)])
      }
      assign(v, y, envir = env)
    }
  }
  res <- tryCatch(eval(parsed[[1]], env), error = function(e) hfc_stop("error evaluating '%s': %s", expr, conditionMessage(e)))
  if (is.numeric(res)) res <- res != 0
  res <- rep_len(as.logical(res), nrow(data))
  res[is.na(res)] <- FALSE
  res
}

# -----------------------------------------------------------------------------
#  Reading input sheets
# -----------------------------------------------------------------------------

#' import excel ..., first case(l) allstr : all text, lower-case names.
#' Works for xlsx/xlsm/xls, csv and dta.
hfc_read_inputs <- function(file, sheet = NULL, required = NULL) {
  if (!file.exists(file)) hfc_stop("file %s not found", file)
  ext <- tolower(tools::file_ext(file))
  df <- if (ext %in% c("xlsx", "xlsm", "xls")) {
    sheets <- readxl::excel_sheets(file)
    if (!is.null(sheet) && !(sheet %in% sheets)) hfc_stop("worksheet '%s' not found in %s", sheet, file)
    suppressMessages(readxl::read_excel(file, sheet = sheet, col_types = "text", .name_repair = "minimal"))
  } else if (ext == "csv") {
    utils::read.csv(file, colClasses = "character", check.names = FALSE)
  } else if (ext == "dta") {
    d <- haven::read_dta(file); d[] <- lapply(d, function(x) as.character(haven::as_factor(x, levels = "values"))); d
  } else hfc_stop("unsupported input file type: %s", ext)
  df <- as.data.frame(df, stringsAsFactors = FALSE)
  names(df) <- tolower(trimws(names(df)))
  df <- df[, names(df) != "" & !is.na(names(df)), drop = FALSE]
  names(df) <- gsub("[^a-z0-9_]", "_", names(df))
  df[] <- lapply(df, function(x) { x <- as.character(x); x[!is.na(x)] <- trimws(x[!is.na(x)]); x[x %in% ""] <- NA; x })
  # drop fully empty rows
  if (nrow(df)) df <- df[rowSums(!is.na(df)) > 0, , drop = FALSE]
  for (r in required) if (!(r %in% names(df))) hfc_stop("column '%s' required in sheet %s", r, sheet %||% file)
  rownames(df) <- NULL
  df
}

#' Add any missing columns (as NA character) to an inputs table
hfc_ensure_cols <- function(df, cols) {
  for (c in cols) if (!(c %in% names(df))) df[[c]] <- rep(NA_character_, nrow(df))
  df
}

# -----------------------------------------------------------------------------
#  _hfcokay : observations marked "okay" by hfc_corrections
# -----------------------------------------------------------------------------

#' Returns a data.frame(id, vars) of okayed id/variable pairs, or NULL.
hfc_okay_table <- function(data, id) {
  if (!all(c("_hfcokay", "_hfcokayvar") %in% names(data))) return(NULL)
  k <- !is.na(hfc_num(data[["_hfcokay"]])) & hfc_num(data[["_hfcokay"]]) == 1
  if (!any(k)) return(NULL)
  data.frame(id = as.character(hfc_num_or_chr(data[[id]][k])), vars = as.character(data[["_hfcokayvar"]][k]), stringsAsFactors = FALSE)
}

hfc_num_or_chr <- function(x) if (is.character(x)) x else hfc_num(x)

#' Drop rows (id, variable) that were marked okay.  Mirrors
#' `drop if id == okid & regexm("okvars", variable)`
hfc_drop_okay <- function(out, okay, id, varcol = "variable") {
  if (is.null(okay) || !nrow(out)) return(out)
  ids <- as.character(hfc_num_or_chr(out[[id]]))
  drop <- rep(FALSE, nrow(out))
  for (i in seq_len(nrow(okay))) {
    hit <- ids == okay$id[i] & vapply(out[[varcol]], function(v) grepl(v, okay$vars[i], fixed = TRUE), logical(1))
    drop <- drop | hit
  }
  out[!drop, , drop = FALSE]
}

# -----------------------------------------------------------------------------
#  Excel output (export excel + ipacolwidth/ipacolformat/iparowformat)
# -----------------------------------------------------------------------------

.hfc_formats <- c(
  number_sep    = "#,##0",
  number_sep_d2 = "#,##0.00",
  number_d2     = "0.00",
  percent       = "0%",
  percent_d2    = "0.00%",
  date_d_mon_yy = "d-mmm-yy",
  date          = "d-mmm-yy",
  datetime      = "d-mmm-yy hh:mm:ss",
  text          = "@"
)

#' Open (or create) a workbook for writing
hfc_wb <- function(file) {
  if (file.exists(file)) openxlsx::loadWorkbook(file) else openxlsx::createWorkbook()
}

#' Write a data.frame to a sheet, replacing the sheet if it already exists
#' (sheetreplace).  Column headers are variable labels when
#' `header = "label"` (Stata first(varl)) or names when "name" (first(var)).
#'  formats: named list, e.g. list(number_sep = c("count"), date_d_mon_yy = "date")
#'  Returns the workbook invisibly (also saved to `file` when save = TRUE).
hfc_write_sheet <- function(file, sheet, df, header = c("label", "name"), formats = list(),
                            total_row = FALSE, wb = NULL, save = TRUE, start_row = 1,
                            replace_sheet = TRUE, colwidth = TRUE, header_format = TRUE) {
  header <- match.arg(header)
  dir.create(dirname(file), showWarnings = FALSE, recursive = TRUE)
  if (is.null(wb)) wb <- hfc_wb(file)
  sheet <- substr(sheet, 1, 31)
  if (sheet %in% names(wb) && replace_sheet && start_row == 1) openxlsx::removeWorksheet(wb, sheet)
  if (!(sheet %in% names(wb))) openxlsx::addWorksheet(wb, sheet)

  out <- as.data.frame(df, stringsAsFactors = FALSE, check.names = FALSE)
  hdr <- vapply(names(out), function(v) if (header == "label") hfc_varlabel(out, v) else v, character(1))
  # plain columns for writing: labelled -> values, tagged NA -> blank
  for (v in names(out)) {
    x <- out[[v]]
    if (inherits(x, "haven_labelled")) x <- hfc_num(x)
    if (hfc_is_datetime(x)) { x <- as.POSIXct(x); attr(x, "tzone") <- "UTC" }
    attr(x, "label") <- NULL
    out[[v]] <- x
  }
  names(out) <- make.unique(unname(hdr))
  openxlsx::writeData(wb, sheet, out, startRow = start_row, colNames = TRUE, keepNA = FALSE)

  nr <- nrow(out); nc <- ncol(out)
  data_rows <- if (nr > 0) (start_row + 1):(start_row + nr) else integer(0)
  # number formats
  for (f in names(formats)) {
    cols <- match(formats[[f]], names(df)); cols <- cols[!is.na(cols)]
    if (length(cols) && length(data_rows)) {
      st <- openxlsx::createStyle(numFmt = .hfc_formats[[f]] %||% f)
      openxlsx::addStyle(wb, sheet, st, rows = data_rows, cols = cols, gridExpand = TRUE, stack = TRUE)
    }
  }
  # date columns default formats
  fmt_cols <- unlist(formats)
  if (length(data_rows)) for (j in seq_len(nc)) {
    if (names(df)[j] %in% fmt_cols) next   # explicit format wins
    if (hfc_is_date(df[[j]]))
      openxlsx::addStyle(wb, sheet, openxlsx::createStyle(numFmt = .hfc_formats[["date"]]), rows = data_rows, cols = j, stack = TRUE)
    else if (hfc_is_datetime(df[[j]]))
      openxlsx::addStyle(wb, sheet, openxlsx::createStyle(numFmt = .hfc_formats[["datetime"]]), rows = data_rows, cols = j, stack = TRUE)
  }
  if (header_format) hfc_format_header(wb, sheet, nc, row = start_row)
  if (total_row && nr > 0) hfc_format_total(wb, sheet, nc, row = start_row + nr)
  if (colwidth) hfc_colwidth(wb, sheet, rbind(hdr, as.matrix(format_for_width(out))))
  if (save) openxlsx::saveWorkbook(wb, file, overwrite = TRUE)
  invisible(wb)
}

format_for_width <- function(df) {
  if (!ncol(df)) return(matrix(character(0), nrow = 0, ncol = 0))
  m <- vapply(df, function(x) { y <- as.character(x); y[is.na(y)] <- ""; y }, character(nrow(df)))
  if (is.null(dim(m))) m <- matrix(m, nrow = nrow(df))
  m
}

#' ipacolwidth: width = max strlen + 2, capped at 81 + 2
hfc_colwidth <- function(wb, sheet, mat) {
  if (!length(mat)) return(invisible())
  w <- apply(mat, 2, function(z) max(nchar(z, type = "width"), na.rm = TRUE))
  w <- pmin(w, 81) + 2
  openxlsx::setColWidths(wb, sheet, cols = seq_along(w), widths = w)
}

#' iparowformat type(header): bold italic, medium bottom border
hfc_format_header <- function(wb, sheet, ncol, row = 1) {
  if (ncol < 1) return(invisible())
  st <- openxlsx::createStyle(textDecoration = c("bold", "italic"), border = "bottom", borderStyle = "medium")
  openxlsx::addStyle(wb, sheet, st, rows = row, cols = seq_len(ncol), gridExpand = TRUE, stack = TRUE)
}

#' iparowformat type(total): bold italic, medium top border, number_sep
hfc_format_total <- function(wb, sheet, ncol, row) {
  st <- openxlsx::createStyle(textDecoration = c("bold", "italic"), border = "top", borderStyle = "medium")
  openxlsx::addStyle(wb, sheet, st, rows = row, cols = seq_len(ncol), gridExpand = TRUE, stack = TRUE)
  if (ncol >= 2) openxlsx::addStyle(wb, sheet, openxlsx::createStyle(numFmt = "#,##0"), rows = row, cols = 2:ncol, gridExpand = TRUE, stack = TRUE)
}

#' ipacellcolor / ipacellfont equivalents
hfc_cell_style <- function(wb, sheet, rows, cols, fill = NULL, font_color = NULL, bold = FALSE, italic = FALSE) {
  deco <- c(if (bold) "bold", if (italic) "italic")
  args <- list()
  if (!is.null(fill)) args$fgFill <- fill
  if (!is.null(font_color)) args$fontColour <- font_color
  if (length(deco)) args$textDecoration <- deco
  st <- do.call(openxlsx::createStyle, args)
  openxlsx::addStyle(wb, sheet, st, rows = rows, cols = cols, gridExpand = TRUE, stack = TRUE)
}

#' Delete a file if it exists (used at start of check scripts)
hfc_rm <- function(...) for (f in c(...)) if (!is.null(f) && nzchar(f) && file.exists(f)) file.remove(f)

#' Save an R data.frame keeping labels/tagged NA: .dta via haven, else .rds
hfc_save <- function(data, file) {
  dir.create(dirname(file), showWarnings = FALSE, recursive = TRUE)
  if (tolower(tools::file_ext(file)) == "dta") {
    names(data) <- substr(names(data), 1, 32)
    haven::write_dta(data, file)
  } else saveRDS(data, file)
  invisible(file)
}

hfc_load <- function(file) {
  if (tolower(tools::file_ext(file)) == "dta") as.data.frame(haven::read_dta(file))
  else if (tolower(tools::file_ext(file)) %in% c("xlsx", "xls", "xlsm")) as.data.frame(readxl::read_excel(file))
  else if (tolower(tools::file_ext(file)) == "csv") utils::read.csv(file, stringsAsFactors = FALSE)
  else readRDS(file)
}

#' Stata `isid`
hfc_isid <- function(data, vars) {
  !anyDuplicated(data[, vars, drop = FALSE]) && !any(vapply(vars, function(v) any(hfc_missing(data[[v]])), logical(1)))
}

#' Period grouping for dashboards: daily / weekly / monthly / auto
hfc_period <- function(date, period = "auto") {
  date <- as.Date(date)
  if (period == "auto") {
    span <- as.numeric(diff(range(date, na.rm = TRUE)))
    period <- if (is.na(span) || span <= 40) "daily" else if (span <= 280) "weekly" else "monthly"
  }
  period
}

#' Stop if `var` has missing values or does not uniquely identify observations
#' (key / skey checks in the prep scripts)
hfc_check_unique <- function(data, var) {
  nmiss <- sum(hfc_missing(data[[var]]))
  if (nmiss > 0) stop(sprintf("Variable %s should never be missing. It has %d missing values", var, nmiss), call. = FALSE)
  dup <- duplicated(data[[var]]) | duplicated(data[[var]], fromLast = TRUE)
  if (any(dup)) {
    d <- data.frame(row = which(dup), value = data[[var]][dup])
    print(d[order(d$value, d$row), ], row.names = FALSE)
    stop(sprintf("variable %s does not uniquely identify the observations", var), call. = FALSE)
  }
  invisible(TRUE)
}

#' Row subset that keeps variable labels (base `[` drops attributes of plain vectors)
hfc_rows <- function(data, rows) {
  out <- data[rows, , drop = FALSE]
  for (v in names(data)) {
    a <- attributes(data[[v]]); keep <- setdiff(names(a), c("names", "dim", "dimnames"))
    for (k in keep) if (is.null(attr(out[[v]], k))) attr(out[[v]], k) <- a[[k]]
  }
  rownames(out) <- NULL
  out
}
