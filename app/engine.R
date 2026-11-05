# ==============================================================================
#  engine.R — runs hfc-toolkit checks on an uploaded dataset for the web app.
#  Wraps the package functions in R/ without changing them: each check writes
#  its usual Excel sheets, and the engine reads them back for display.
# ==============================================================================

hfc_app_load_toolkit <- function(dir) {
  for (f in sort(list.files(dir, pattern = "\\.R$", full.names = TRUE))) source(f, local = globalenv())
  invisible(TRUE)
}

# Read an uploaded file by its original name (Shiny gives a temp path without extension)
hfc_app_read <- function(path, name = path) {
  ext <- tolower(tools::file_ext(name))
  d <- switch(ext,
    dta  = haven::read_dta(path),
    csv  = utils::read.csv(path, stringsAsFactors = FALSE, check.names = FALSE, na.strings = c("", "NA")),
    xlsx = , xls = , xlsm = readxl::read_excel(path, guess_max = 5000),
    rds  = readRDS(path),
    stop("Unsupported file type: .", ext, ". Upload a .dta, .csv, .xlsx or .rds file.", call. = FALSE)
  )
  d <- as.data.frame(d, stringsAsFactors = FALSE)
  names(d) <- make.names(names(d), unique = TRUE)
  d
}

# Parse a date/datetime column into POSIXct, trying the formats survey platforms export
hfc_app_datetime <- function(x) {
  if (inherits(x, "POSIXt")) return(x)
  if (inherits(x, "Date")) return(as.POSIXct(x))
  if (is.numeric(x)) {                       # Excel serial dates
    return(as.POSIXct(as.Date(x, origin = "1899-12-30")))
  }
  x <- trimws(as.character(x))
  fmts <- c("%Y-%m-%d %H:%M:%S", "%Y-%m-%dT%H:%M:%S", "%Y-%m-%d %H:%M", "%Y-%m-%d",
            "%b %d, %Y %I:%M:%S %p", "%d/%m/%Y %H:%M:%S", "%d/%m/%Y %H:%M", "%d/%m/%Y",
            "%m/%d/%Y %H:%M:%S", "%m/%d/%Y %I:%M:%S %p", "%m/%d/%Y")
  best <- NULL; best_ok <- -1
  for (f in fmts) {
    p <- as.POSIXct(substr(sub("\\.\\d+.*$|Z$|[+-]\\d{2}:?\\d{2}$", "", x), 1, 40), format = f, tz = "UTC")
    ok <- sum(!is.na(p))
    if (ok > best_ok) { best <- p; best_ok <- ok }
    if (ok == sum(!is.na(x) & nzchar(x))) break
  }
  best
}

# Guess which column plays each admin role, from common SurveyCTO / ODK / Kobo names
hfc_app_guess <- function(d) {
  n <- names(d); low <- tolower(n)
  g <- function(patterns) {
    for (p in patterns) { i <- which(grepl(p, low)); if (length(i)) return(n[i[1]]) }
    ""
  }
  list(
    key = g(c("^key$", "^instanceid$", "^x_uuid$", "^_uuid$", "^uuid$", "meta.instanceid")),
    id = g(c("^hhid$", "^id$", "^respondent_?id$", "^resp_?id$", "^hh_?id$", "^household_?id$", "_id$")),
    enum = g(c("^a?_?enum_?id$", "^enumerator", "^a?_?enum", "enum_?id", "^interviewer", "^surveyor", "^username$")),
    team = g(c("team", "supervisor")),
    date = g(c("^starttime$", "^start$", "^x_start$", "^submissiondate$", "date")),
    duration = g(c("^duration$", "duration")),
    consent = g(c("consent$", "^c_consent", "consent")),
    formversion = g(c("formdef_version", "^x_version$", "version")),
    gps = paste(n[grepl("latitude$|longitude$|phone_?number$|^phone$|^mobile_?(no|number)$", low) & !grepl("^device", low)], collapse = " ")
  )
}

# Run the checks. Returns list(sheets = named list of data frames, counts, errors, files)
hfc_app_run <- function(d, map, inputs = NULL, opts = list(), outdir = tempfile("hfc_")) {
  dir.create(outdir, showWarnings = FALSE, recursive = TRUE)
  f_hfc <- file.path(outdir, "hfc_output.xlsx")
  f_sdb <- file.path(outdir, "surveydb.xlsx")
  f_edb <- file.path(outdir, "enumdb.xlsx")
  errors <- list(); done <- character(0)
  nz <- function(x) !is.null(x) && length(x) && nzchar(x[1])
  attempt <- function(name, expr) {
    r <- tryCatch({ force(expr); done <<- c(done, name); TRUE },
                  error = function(e) { errors[[name]] <<- conditionMessage(e); FALSE })
    r
  }

  # ---- prepare ----------------------------------------------------------------
  if (!nz(map$key)) { d$row_key <- sprintf("row%06d", seq_len(nrow(d))); map$key <- "row_key" }
  if (anyDuplicated(d[[map$key]]) || any(is.na(d[[map$key]])))
    stop("The submission key column '", map$key, "' must be unique and complete. Choose another column or leave it blank.", call. = FALSE)
  if (!nz(map$id) || !nz(map$enum) || !nz(map$date))
    stop("Choose the respondent ID, enumerator and date columns before running the checks.", call. = FALSE)
  d[[map$date]] <- hfc_app_datetime(d[[map$date]])
  if (all(is.na(d[[map$date]]))) stop("Could not read any dates in '", map$date, "'.", call. = FALSE)
  if (nz(map$duration) && !is.numeric(d[[map$duration]])) d[[map$duration]] <- suppressWarnings(as.numeric(d[[map$duration]]))
  if (nz(opts$dk_num))  d <- hfc_recode_to_tag(d, opts$dk_num, "d")
  if (nz(opts$ref_num)) d <- hfc_recode_to_tag(d, opts$ref_num, "r")
  n_in <- nrow(d)

  # ---- checks -------------------------------------------------------------------
  attempt("Duplicate IDs", {
    hfc_ids(d, varname = map$id, enumerator = map$enum, date = map$date, key = map$key,
            outfile = f_hfc, outsheet = "id duplicates", force = TRUE, sheetreplace = TRUE)
  })
  # later checks need a unique ID: keep the first submission of each ID (Stata: duplicates drop, force)
  n_dup_dropped <- sum(duplicated(d[[map$id]]))
  if (n_dup_dropped) d <- hfc_rows(d, !duplicated(d[[map$id]]))
  if (nz(map$formversion)) attempt("Form versions", {
    hfc_versions(d, varname = map$formversion, enumerator = map$enum, date = map$date,
                 outfile = f_hfc, outsheet1 = "form versions", outsheet2 = "outdated", sheetreplace = TRUE)
  })
  if (nz(map$dupvars)) attempt("Duplicate values", {
    hfc_duplicates(d, varlist = map$dupvars, id = map$id, enumerator = map$enum, date = map$date,
                   outfile = f_hfc, outsheet = "duplicates", sheetreplace = TRUE)
  })
  attempt("Missingness", {
    hfc_missingness(d, varlist = "_all", priority = paste(map$id, map$key, map$date, map$enum),
                    show = "0", outfile = f_hfc, outsheet = "missing", sheetreplace = TRUE)
  })
  childvars <- character(0)
  if (nz(inputs)) {
    sheets_in <- readxl::excel_sheets(inputs)
    if ("outliers" %in% sheets_in) attempt("Outliers", {
      hfc_outliers(d, using = inputs, id = map$id, enumerator = map$enum, date = map$date,
                   outfile = f_hfc, outsheet = "outliers", sheetreplace = TRUE,
                   use_multiplier = !isTRUE(opts$classic_outliers))
    })
    if ("constraints" %in% sheets_in) attempt("Constraints", {
      hfc_constraints(d, using = inputs, id = map$id, enumerator = map$enum, date = map$date,
                      outfile = f_hfc, outsheet = "constraints", sheetreplace = TRUE)
    })
    if ("logic" %in% sheets_in) attempt("Logic", {
      hfc_logic(d, using = inputs, id = map$id, enumerator = map$enum, date = map$date,
                outfile = f_hfc, outsheet = "logic", sheetreplace = TRUE)
    })
    if ("other specify" %in% sheets_in) attempt("Other specify", {
      r <- hfc_other_specify(d, using = inputs, id = map$id, enumerator = map$enum, date = map$date,
                             outfile = f_hfc, sheetreplace = TRUE)
      childvars <- r$childvarlist
    })
  }
  common <- list(enumerator = map$enum, date = map$date,
                 consent = if (nz(map$consent)) map$consent else NULL,
                 cons_vals = if (nz(map$consent)) opts$cons_vals else NULL,
                 dontknow_num = ".d", dontknow_str = opts$dk_str %||% "",
                 refuse_num = ".r", refuse_str = opts$ref_str %||% "",
                 otherspecify = childvars,
                 duration = if (nz(map$duration)) map$duration else NULL,
                 formversion = if (nz(map$formversion)) map$formversion else NULL)
  attempt("Survey dashboard", do.call(hfc_survey_dashboard, c(list(data = d), common,
          list(outfile = f_sdb, sheetreplace = TRUE))))
  attempt("Enumerator dashboard", do.call(hfc_enum_dashboard, c(list(data = d,
          using = if (nz(inputs) && "enumstats" %in% readxl::excel_sheets(inputs)) inputs else NULL,
          sheetname = "enumstats", team = if (nz(map$team)) map$team else NULL), common,
          list(outfile = f_edb, sheetreplace = TRUE))))

  # ---- read results back --------------------------------------------------------
  read_all <- function(f) {
    if (!file.exists(f)) return(list())
    s <- openxlsx::getSheetNames(f)
    stats::setNames(lapply(s, function(x) {
      df <- tryCatch(openxlsx::read.xlsx(f, sheet = x, check.names = FALSE, sep.names = " "), error = function(e) NULL)
      if (is.null(df)) return(data.frame())
      hfc_app_tidy_header(df)
    }), s)
  }
  list(sheets = read_all(f_hfc), surveydb = read_all(f_sdb), enumdb = read_all(f_edb),
       errors = errors, done = done, n = n_in, n_after = nrow(d), n_dup_dropped = n_dup_dropped,
       files = c(hfc = f_hfc, surveydb = f_sdb, enumdb = f_edb)[file.exists(c(f_hfc, f_sdb, f_edb))],
       data = d, map = map)
}

# Dashboard sheets use a two-row header (variable name over statistic). Flatten it.
hfc_app_tidy_header <- function(df) {
  nm <- names(df)
  if (!nrow(df) || !any(grepl("^X\\d+$", nm))) return(df)
  first <- vapply(df[1, ], function(x) as.character(x), "")
  cur <- ""; out <- character(length(nm))
  for (i in seq_along(nm)) {
    if (!grepl("^X\\d+$", nm[i])) cur <- nm[i]
    out[i] <- if (is.na(first[i]) || !nzchar(first[i])) cur else if (i == 1 || identical(cur, first[i])) first[i] else paste(cur, first[i], sep = ": ")
  }
  df <- df[-1, , drop = FALSE]; names(df) <- make.unique(out, sep = " "); rownames(df) <- NULL
  df
}
