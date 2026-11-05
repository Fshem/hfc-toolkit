# =============================================================================
#  hfc_corrections : make corrections to data
# =============================================================================

# -- helpers (file-private) ---------------------------------------------------

#' Excel column letters (A, B, ..., AA) - Stata names blank headers this way
.hfc_corrections_colletter <- function(i) {
  out <- character(length(i))
  for (k in seq_along(i)) {
    n <- i[k]; s <- ""
    while (n > 0) { r <- (n - 1) %% 26; s <- paste0(LETTERS[r + 1], s); n <- (n - 1) %/% 26 }
    out[k] <- s
  }
  out
}

#' Mimic `import excel ..., firstrow allstr` / `import delim, stringcols(_all)` / `use`.
#' Column names become Stata-valid names (case preserved for Excel, lower for csv),
#' the original header text is kept as the variable label (as Stata does).
.hfc_corrections_import <- function(file, sheet = NULL) {
  ext <- tolower(tools::file_ext(file))
  if (!file.exists(file)) hfc_stop("file %s not found", file)
  if (ext %in% c("xlsx", "xls", "xlsm")) {
    if (is.null(sheet) || !nzchar(sheet)) hfc_stop("sheet option required with .xlsx, .xls or .xlsm files")
    if (!(sheet %in% readxl::excel_sheets(file))) hfc_stop("worksheet %s not found", sheet)
    df <- suppressMessages(readxl::read_excel(file, sheet = sheet, col_types = "text",
                                              col_names = TRUE, .name_repair = "minimal"))
    hdr <- names(df)
    df <- as.data.frame(df, stringsAsFactors = FALSE)
    # date-formatted cells: Stata's allstr import yields the %td display text (e.g. 28oct2017)
    guess <- tryCatch(suppressWarnings(suppressMessages(as.data.frame(readxl::read_excel(
      file, sheet = sheet, col_names = TRUE, .name_repair = "minimal")))), error = function(e) NULL)
    if (!is.null(guess) && ncol(guess) == ncol(df) && nrow(guess) == nrow(df)) {
      for (j in seq_along(guess)) if (inherits(guess[[j]], "POSIXt")) {
        dt <- guess[[j]]
        df[[j]] <- ifelse(is.na(dt), df[[j]], tolower(format(dt, "%d%b%Y")))
      }
    }
    blank <- is.na(hdr) | trimws(hdr) == ""
    nm <- hdr
    nm[blank] <- .hfc_corrections_colletter(which(blank))
    nm <- gsub("[^A-Za-z0-9_]", "_", trimws(nm)); nm <- substr(nm, 1, 32)
    nm[grepl("^[0-9]", nm)] <- paste0("_", nm[grepl("^[0-9]", nm)])
    nm <- make.unique(nm, sep = "")
    names(df) <- nm
    for (j in seq_along(df)) {
      x <- as.character(df[[j]]); x[is.na(x)] <- ""
      attr(x, "label") <- if (blank[j]) NULL else hdr[j]
      df[[j]] <- x
    }
    df
  } else if (ext == "csv") {
    df <- utils::read.csv(file, colClasses = "character", check.names = FALSE, na.strings = character(0))
    names(df) <- tolower(gsub("[^A-Za-z0-9_]", "_", names(df)))
    df
  } else {
    as.data.frame(haven::read_dta(file))
  }
}

#' Row subset that keeps every column attribute (labels, value labels, classes),
#' like Stata keep/drop which never touch variable metadata.
.hfc_corrections_subset <- function(df, rows) {
  if (is.logical(rows)) rows <- which(rows)
  cols <- lapply(df, function(x) {
    att <- attributes(x); att$names <- NULL
    y <- unclass(x)[rows]; attributes(y) <- att; y
  })
  out <- structure(cols, names = names(df), class = "data.frame", row.names = seq_along(rows))
  out
}

#' Parse a Stata numeric literal: number, "." (sysmiss) or ".a"-".z" (tagged NA).
#' Returns list(ok, value) ; value is a double (tagged NA where relevant).
.hfc_corrections_num <- function(s) {
  s <- trimws(s)
  if (is.na(s) || s == "") return(list(ok = FALSE, value = NA_real_))
  if (s == ".") return(list(ok = TRUE, value = NA_real_))
  if (grepl("^\\.[a-z]$", s)) return(list(ok = TRUE, value = haven::tagged_na(substring(s, 2))))
  v <- suppressWarnings(as.numeric(s))
  list(ok = !is.na(v), value = v)
}

#' Stata float() rounding (single precision)
.hfc_corrections_float <- function(v) readBin(writeBin(as.double(v), raw(), size = 4), "double", n = length(v), size = 4)

#' Stata `==` between a numeric vector (may contain tagged NA) and a scalar
.hfc_corrections_numeq <- function(x, v) {
  xv <- as.vector(unclass(x)); attributes(xv) <- NULL
  if (is.na(v)) {
    tag <- haven::na_tag(v)
    if (is.na(tag)) return(is.na(xv) & is.na(haven::na_tag(xv)))
    return(haven::is_tagged_na(xv, tag))
  }
  !is.na(xv) & (xv == v | xv == .hfc_corrections_float(v))
}

#' Apply corrections to a dataset
#'
#' Port of Stata `hfc_corrections`. Reads a corrections sheet (columns: <id>,
#' variable, value, newvalue, action [+ any others]) and, row by row, checks that
#' `variable` currently equals `value` for the observation with that id, then
#' applies `action`:
#'   * "replace": set variable to newvalue
#'   * "drop"   : drop the observation
#'   * "okay"   : set `_hfcokay` = 1 and append "variable/" to `_hfcokayvar`
#'     (used by other checks to exclude okayed id/variable pairs).
#' Rows whose current value does not match are logged as "failed".
#'
#' @param data     data.frame (dataset in memory)
#' @param using    corrections file (.xlsx/.xls/.xlsm, .csv or .dta)
#' @param sheet    sheet name (required for Excel files)
#' @param id       name of the id variable (must uniquely identify obs)
#' @param logfile  optional .xlsx log file
#' @param logsheet sheet for the log (required with logfile)
#' @param nolabel  export values instead of value labels for id in the log
#' @param ignore   do not stop when some corrections fail
#' @return the corrected data.frame, with attr(, "r") =
#'   list(N_obs, N_succesful, N_failed) (Stata r() names, incl. "succesful" typo).
#' @examples
#' df <- hfc_corrections(df, "corrections.xlsm", sheet = "other issues", id = "hhid",
#'                           logfile = "correction_log.xlsx", logsheet = "other issues", ignore = TRUE)
hfc_corrections <- function(data, using, sheet = NULL, id, logfile = NULL, logsheet = NULL,
                                nolabel = FALSE, ignore = FALSE) {
  data <- as.data.frame(data, stringsAsFactors = FALSE)
  id <- hfc_vars(data, id)
  if (length(id) != 1) hfc_stop("id() must be a single variable")
  if (!hfc_isid(data, id)) hfc_stop("variable %s does not uniquely identify the observations", id)
  str_id <- is.character(data[[id]])

  rep <- .hfc_corrections_import(using, sheet)

  if (!is.null(logfile) && nzchar(logfile) && (is.null(logsheet) || !nzchar(logsheet)))
    hfc_stop("option logsheet required with option logfile")

  for (v in c("variable", "value", "newvalue", "action"))
    if (!(v %in% names(rep))) hfc_stop("variable %s not found in %s", v, using)
  if (!(id %in% names(rep))) hfc_stop("variable %s not found in %s", id, using)

  # keep if !missing(id)
  a <- attr(rep[[id]], "label", exact = TRUE)
  rep[[id]] <- as.character(rep[[id]]); rep[[id]][is.na(rep[[id]])] <- ""
  attr(rep[[id]], "label") <- a
  rep <- .hfc_corrections_subset(rep, trimws(rep[[id]]) != "")
  rep_cnt <- nrow(rep)
  failed_cnt <- 0L

  if (rep_cnt > 0) {
    for (v in names(rep)) if (is.character(rep[[v]])) { a <- attr(rep[[v]], "label"); x <- rep[[v]]; x[is.na(x)] <- ""; attr(x, "label") <- a; rep[[v]] <- x }
    a <- attr(rep$action, "label")
    rep$action <- gsub("\\s+", " ", trimws(tolower(as.character(rep$action)))); attr(rep$action, "label") <- a

    for (v in c(id, "variable", "action")) {
      miss <- hfc_missing(rep[[v]])
      if (any(miss)) {
        message(sprintf("The following row(s) in replacement sheet have missing values for %s", v))
        print(data.frame(row = which(miss) + 1, value = rep[[v]][miss]))
        hfc_stop("The following row(s) in replacement sheet have missing values for %s", v)
      }
    }
    bad <- !(rep$action %in% c("okay", "replace", "drop"))
    if (any(bad)) {
      print(data.frame(action = rep$action[bad]))
      hfc_stop("Invalid values for column action. Expected Values are okay, replace, drop")
    }

    # id column format must match main dataset
    if (!str_id) {
      idnum <- suppressWarnings(as.numeric(rep[[id]]))
      if (any(is.na(idnum) & trimws(rep[[id]]) != "")) {
        message(sprintf("ID column (%s) in using file cannot be converted to numeric. Contains non-numeric chars at the following", id))
        print(rep[[id]][is.na(idnum)])
        # NOTE(stata-bug): Stata only lists the bad ids and does not exit; rows then
        # fail to match any id. We keep going in the same way.
      }
    }

    status <- rep("", rep_cnt)

    if (!("_hfcokay" %in% names(data)))    data[["_hfcokay"]] <- 0
    if (!("_hfcokayvar" %in% names(data))) data[["_hfcokayvar"]] <- "/"

    for (i in seq_len(rep_cnt)) {
      var <- rep$variable[i]
      val <- rep$value[i]
      idval <- rep[[id]][i]
      ids <- if (str_id) { z <- data[[id]]; z[is.na(z)] <- ""; z } else hfc_num(data[[id]])
      hit <- if (str_id) ids == idval else (!is.na(ids) & ids == suppressWarnings(as.numeric(idval)))
      hit[is.na(hit)] <- FALSE

      has_var <- var %in% names(data)
      str_var <- has_var && is.character(data[[var]])

      # cap assert `var' == `val' if `id' == idval  -> _rc 9 means "failed".
      # Any other error (unknown variable, empty/non-numeric value for a numeric
      # variable -> syntax error) is swallowed by -cap- and the row proceeds.
      failed <- FALSE
      if (has_var && any(hit)) {
        if (str_var) {
          cur <- data[[var]][hit]; cur[is.na(cur)] <- ""
          failed <- !all(cur == val)
        } else {
          p <- .hfc_corrections_num(val)
          # NOTE(stata-bug): an empty or non-numeric `value` for a numeric variable
          # makes the assert a syntax error which -cap- swallows, so the check is
          # skipped and the correction is applied regardless.
          if (p$ok) failed <- !all(.hfc_corrections_numeq(data[[var]][hit], p$value))
        }
      }

      if (failed) { status[i] <- "failed"; next }

      if (!any(hit)) hfc_stop("ID value %s not a valid id for id variable %s", idval, id)

      action <- rep$action[i]
      if (action == "okay") {
        data[["_hfcokay"]][hit] <- 1
        data[["_hfcokayvar"]][hit] <- paste0(data[["_hfcokayvar"]][hit], var, "/")
      } else if (action == "replace") {
        if (!has_var) hfc_stop("variable %s not found", var)
        newval <- rep$newvalue[i]
        if (str_var) {
          x <- data[[var]]; x[hit] <- newval; data[[var]] <- x
        } else {
          p <- .hfc_corrections_num(newval)
          if (!p$ok) hfc_stop("invalid newvalue '%s' for numeric variable %s (row %d)", newval, var, i + 1)
          x <- data[[var]]
          att <- attributes(x)
          y <- unclass(x)
          if (!is.double(y)) { storage.mode(y) <- "double" }
          y[hit] <- p$value
          attributes(y) <- att
          data[[var]] <- y
        }
      } else {
        data <- .hfc_corrections_subset(data, !hit)
      }
      status[i] <- "successful"
    }

    failed_cnt <- sum(status == "failed")
    if (failed_cnt > 0) {
      message(sprintf("List of %d failed correction ... ", failed_cnt))
      print(data.frame(rep[status == "failed", c(id, "variable", "value", "newvalue", "action")], row.names = NULL))
      if (!ignore) hfc_stop("Fix failed corrections and re-run. You can use the ignore option to bypass this error")
    }

    if (!is.null(logfile) && nzchar(logfile)) {
      log <- rep
      log$status <- status
      attr(log$status, "label") <- "status"
      log <- hfc_labels(log, id, nolabel = nolabel)
      hfc_write_sheet(logfile, logsheet, log, header = "label")
    }
  } else {
    message("No observations in replacement file. Skipping hfc_corrections")
  }

  attr(data, "r") <- list(N_obs = rep_cnt, N_succesful = rep_cnt - failed_cnt, N_failed = failed_cnt)
  data
}
