# =============================================================================
#  hfc_track_survey : survey progress tracking against master / tracking data
# =============================================================================

#' Track survey progress against master data or tracking (target) data
#'
#' Compares the survey data with either
#'  * `masterdata`  : one row per targeted respondent (unique by `id`, or by
#'                    `masterid`); the target per group is the number of rows
#'                    per value of `by`; or
#'  * `trackingdata`: one row per value of `by` with the number of expected
#'                    surveys in `target`.
#' and writes a progress report to `outfile`:
#'  * tracking mode: sheet "survey progress" (by, keeptracking, target,
#'    # submitted, % submitted, [# completed, % completed], first date, last date)
#'    with a total row; with `surveyok`, survey rows whose `by` value is not in
#'    the tracking data go to sheet "Unmatched IDs from Survey".
#'  * master mode: sheet "summary" (same layout, keepmaster instead of
#'    keeptracking) with a total row; with `surveyok`, survey ids not in the
#'    master data go to "Unmatched IDs from Survey"; unless `summaryonly`, one
#'    sheet per value of `by` (or one workbook "<outfile>_<value>.xlsx", sheet
#'    "status", with `workbooks`) listing every master respondent with status
#'    "submitted"/"not submitted" and the survey date. `save` writes a .dta of
#'    the status of every master/survey observation.
#' Like Stata's `export excel ..., replace`, an existing `outfile` is replaced.
#'
#' @param data         survey data.frame (data in memory); used when
#'                     `surveydata` is not given
#' @param surveydata   survey data: path (.dta/.xlsx/.csv) or data.frame
#' @param id           survey ID variable (required with masterdata)
#' @param date         date or datetime variable (datetimes are converted to dates)
#' @param by           grouping variable present in survey and master/tracking data
#' @param outcome      "var, vals" (Stata syntax, eg. "c_outcome, 1 2") or just
#'                     the variable name with values in `outcome_vals`
#' @param outcome_vals values of `outcome` that indicate a completed survey
#' @param target       numeric variable in trackingdata with the target count
#' @param masterdata   master data: path (.dta/.xlsx/.xls/.csv) or data.frame
#' @param masterid     id variable in master data if named differently from `id`
#' @param trackingdata tracking data: path (.dta/.xlsx/.xls/.csv) or data.frame
#' @param keepmaster,keeptracking,keepsurvey additional variables to export
#' @param outfile      output .xlsx workbook
#' @param save         optional .dta file for the status dataset (master mode)
#' @param nolabel      export values instead of value labels
#' @param summaryonly  master mode: export only the summary sheet
#' @param workbooks    master mode: one workbook per value of `by`
#' @param surveyok     allow observations found only in the survey data
#' @param replace      must be TRUE (Stata requires the replace option)
#' @return invisibly list(N_groups, N_target, N_submitted, N_completed,
#'         N_unmatched, summary = summary data.frame as exported)
#' @examples
#' # hfc_track_survey(survey, id = "hhid", date = "starttime", by = "a_district",
#' #                masterdata = "household_preloads.xlsx", outcome = "c_consent, 1",
#' #                outfile = "tracking.xlsx", surveyok = TRUE, summaryonly = TRUE)
#' # hfc_track_survey(survey, date = "starttime", by = "a_kg", target = "hh_count",
#' #                trackingdata = "respondent_targets.xlsx", keeptracking = "district",
#' #                outfile = "tracking.xlsx", surveyok = TRUE)
hfc_track_survey <- function(data = NULL, surveydata = NULL, id = NULL, date, by,
                           outcome = NULL, outcome_vals = NULL, target = NULL,
                           masterdata = NULL, masterid = NULL, trackingdata = NULL,
                           keepmaster = NULL, keeptracking = NULL, keepsurvey = NULL,
                           outfile, save = NULL, nolabel = FALSE, summaryonly = FALSE,
                           workbooks = FALSE, surveyok = FALSE, replace = TRUE) {
  nz <- .hfc_track_survey_nz
  nolabel <- isTRUE(nolabel); surveyok <- isTRUE(surveyok)
  summaryonly <- isTRUE(summaryonly); workbooks <- isTRUE(workbooks)
  if (!isTRUE(replace)) hfc_stop("option replace required")

  # ---- check syntax ---------------------------------------------------------
  tracking <- nz(trackingdata); master <- nz(masterdata)
  if (!tracking && !master) hfc_stop("must specify either trackingdata or masterdata option")
  if (tracking && master) hfc_stop("options trackingdata and masterdata are mutually exclusive")
  if (tracking && nz(keepsurvey) && !surveyok)
    hfc_stop("options trackingdata and keepsurvey are mutually exclusive if option surveyok is not used")

  # ---- survey data ----------------------------------------------------------
  if (!nz(surveydata)) surveydata <- data
  if (is.null(surveydata)) hfc_stop("no survey data: specify data or surveydata")
  sv <- .hfc_track_survey_read(surveydata)
  date <- hfc_vars(sv, date)[1]
  by   <- hfc_vars(sv, by)[1]
  id   <- if (nz(id)) hfc_vars(sv, id)[1] else character(0)

  # ipagettd
  if (hfc_is_datetime(sv[[date]])) {
    sv[[date]] <- hfc_to_date(sv[[date]])
  } else if (!hfc_is_date(sv[[date]])) {
    message(sprintf("variable %s is not a %%td, %%tc or %%tC date/datetime variable", date))
    print(utils::head(sv[date], 5), row.names = FALSE)
  }

  has_outcome <- nz(outcome)
  complete <- numeric(nrow(sv))
  if (has_outcome) {
    oc <- paste(outcome, collapse = " ")
    if (grepl(",", oc, fixed = TRUE)) {
      parts <- strsplit(oc, ",", fixed = TRUE)[[1]]
      ovar <- trimws(parts[1]); ovals <- hfc_tokens(paste(parts[-1], collapse = " "))
    } else {
      ovar <- trimws(oc); ovals <- hfc_tokens(outcome_vals)
    }
    ovar <- hfc_vars(sv, ovar)[1]
    ox <- sv[[ovar]]
    if (!hfc_is_num(ox) || hfc_is_date(ox) || hfc_is_datetime(ox))
      hfc_stop("%s found in option outcome where numeric variable expected", ovar)
    for (v in ovals) {
      hit <- if (grepl("^\\.[a-z]$", v)) hfc_is_tag(ox, substring(v, 2))
             else !is.na(hfc_num(ox)) & hfc_num(ox) == suppressWarnings(as.numeric(v))
      complete[hit %in% TRUE] <- 1
    }
  }

  if (any(hfc_missing(sv[[by]]))) hfc_stop("variable %s in option by() should never be missing", by)

  keepsurvey <- if (nz(keepsurvey)) hfc_vars(sv, keepsurvey) else character(0)
  main <- sv[, names(sv) %in% c(date, id, by, keepsurvey), drop = FALSE]
  main[["_tmv_complete"]] <- complete

  # ---- import using data ----------------------------------------------------
  inp <- .hfc_track_survey_read(if (tracking) trackingdata else masterdata, csv_lower = TRUE)
  res <- list(N_groups = 0, N_target = 0, N_submitted = 0, N_completed = NA, N_unmatched = 0, summary = NULL)

  tlab <- c("_tmv_target" = "target", "_tmv_survey" = "# submitted", "_tmv_survey_perc" = "% submitted",
            "_tmv_complete" = "# completed", "_tmv_complete_perc" = "% completed",
            "_tmv_fdate" = "first date", "_tmv_ldate" = "last date")

  # ===========================================================================
  #  tracking data
  # ===========================================================================
  if (tracking) {
    keeplist <- if (nz(keeptracking)) hfc_vars(inp, keeptracking) else character(0)
    if (!nz(target)) hfc_stop("option target() required with trackingdata")
    target <- hfc_vars(inp, target)[1]
    hfc_vars(inp, by)
    inp <- inp[, names(inp) %in% c(by, keeplist, target), drop = FALSE]
    if (!hfc_isid(inp, by)) hfc_stop("variable %s does not uniquely identify the observations in trackingdata", by)

    # destring target
    if (is.character(inp[[target]])) {
      tv <- trimws(inp[[target]]); num <- suppressWarnings(as.numeric(tv))
      if (!any(!is.na(tv) & tv != "" & is.na(num))) inp[[target]] <- .hfc_track_survey_keepattr(num, inp[[target]])
    }
    if (!hfc_is_num(inp[[target]])) hfc_stop("%s found where numeric variable expected at target", target)

    .hfc_track_survey_samekey(main, inp, by, "survey", "tracking")
    coll <- .hfc_track_survey_collapse(main, by, date)
    mg <- .hfc_track_survey_merge(inp, coll, by)
    tg <- hfc_num(mg[[target]])
    mg[["_tmv_survey_perc"]]   <- mg[["_tmv_survey"]] / tg
    mg[["_tmv_complete_perc"]] <- mg[["_tmv_complete"]] / tg
    mg <- .hfc_track_survey_setlabs(mg, c(stats::setNames("target", target), tlab[-1]))
    mg <- .hfc_track_survey_layout(mg, c(by, keeplist))
    if (!has_outcome) mg <- mg[, setdiff(names(mg), c("_tmv_complete", "_tmv_complete_perc")), drop = FALSE]

    if (!surveyok) {
      n2 <- sum(mg[["_merge"]] == 2)
      if (n2 > 0) {
        message(sprintf("The following %d values in %s are not in tracking data", n2, by))
        print(.hfc_track_survey_rows(mg, mg[["_merge"]] == 2)[by], row.names = FALSE)
        hfc_stop("The following %d values in %s are not in tracking data", n2, by)
      }
    }
    for (v in c(by, keeplist)) attr(mg[[v]], "label") <- v
    mg <- mg[, names(mg) != "_merge", drop = FALSE]

    srt <- if (has_outcome) c("_tmv_survey_perc", "_tmv_survey", "_tmv_complete_perc", "_tmv_complete", by)
           else c("_tmv_survey_perc", "_tmv_survey", by)
    mg <- .hfc_track_survey_rows(mg, .hfc_track_survey_order(mg, srt))
    mg <- .hfc_track_survey_totals(mg, target, has_outcome)

    # NOTE(stata-bug): tracking mode never calls ipalabels, so `nolabel` is ignored and
    # `export excel` writes value labels for any labelled variable.
    out <- .hfc_track_survey_decode(mg)
    numc <- c(target, "_tmv_survey", if (has_outcome) "_tmv_complete")
    pctc <- c("_tmv_survey_perc", if (has_outcome) "_tmv_complete_perc")
    hfc_rm(outfile)
    .hfc_track_survey_summary_sheet(outfile, "survey progress", out, numc, pctc, c("_tmv_fdate", "_tmv_ldate"))
    res$summary <- out
    res$N_groups <- nrow(out) - 1
    res$N_target <- hfc_num(out[[target]])[nrow(out)]
    res$N_submitted <- out[["_tmv_survey"]][nrow(out)]
    if (has_outcome) res$N_completed <- out[["_tmv_complete"]][nrow(out)]

    if (surveyok) {
      ki <- .hfc_track_survey_key(main[[by]]) %in% .hfc_track_survey_key(inp[[by]])
      so <- .hfc_track_survey_rows(main, !ki)
      if (nrow(so) > 0) {
        so <- .hfc_track_survey_rows(so, .hfc_track_survey_order(so, by))
        so <- so[, names(so) %in% c(id, date, keepsurvey), drop = FALSE]
        if (length(keepsurvey)) so <- hfc_labels(so, keepsurvey, nolabel)
        if (length(id)) so <- hfc_labels(so, id, nolabel)
        so <- .hfc_track_survey_decode(so)
        hfc_write_sheet(outfile, "Unmatched IDs from Survey", so, header = "label")
      }
      res$N_unmatched <- nrow(so)
    }
    # option save is silently ignored in tracking mode (as in Stata)
  }

  # ===========================================================================
  #  master data
  # ===========================================================================
  if (master) {
    if (!length(id)) hfc_stop("option id() required with masterdata")
    if (nz(masterid) && !identical(masterid, id)) {
      # NOTE(stata-bug): Stata tests `cap confirm var id` / `if _rc` (inverted: it errors when
      # id is NOT in master data) and then renames the undefined local `trackingid`, so
      # masterid() always fails in Stata. The intended behaviour is implemented here.
      if (!(masterid %in% names(inp))) hfc_stop("variable %s not found in master data", masterid)
      if (id %in% names(inp)) hfc_stop("variable %s not allowed in master dataset if masterid option is specified.", id)
      names(inp)[names(inp) == masterid] <- id
    }
    if (!(id %in% names(inp))) hfc_stop("variable %s not found in master data", id)
    if (!(by %in% names(inp))) hfc_stop("variable %s not found in master data", by)
    keepmaster <- if (nz(keepmaster)) hfc_vars(inp, keepmaster) else character(0)
    .hfc_track_survey_samekey(main, inp, by, "survey", "master")

    # target = rows per group; keep last obs per group  (bys by: keep if _n == _N)
    o <- .hfc_track_survey_order(inp, by)
    srt <- .hfc_track_survey_rows(inp, o)
    k <- .hfc_track_survey_key(srt[[by]])
    cnt <- as.numeric(table(k)[k])
    last <- !duplicated(k, fromLast = TRUE)
    ms <- .hfc_track_survey_rows(srt, last)
    ms <- ms[, names(ms) %in% c(id, by, keepmaster), drop = FALSE]
    ms[["_tmv_target"]] <- cnt[last]
    ms <- ms[, names(ms) != id, drop = FALSE]

    coll <- .hfc_track_survey_collapse(main, by, date)
    mg <- .hfc_track_survey_merge(ms, coll, by)
    mg[["_tmv_survey_perc"]]   <- mg[["_tmv_survey"]] / mg[["_tmv_target"]]
    mg[["_tmv_complete_perc"]] <- mg[["_tmv_complete"]] / mg[["_tmv_target"]]
    mg <- .hfc_track_survey_setlabs(mg, tlab)
    # NOTE(stata-bug): Stata orders by `keeplist' which is undefined in master mode, so only
    # `by` is moved first; keepmaster vars keep their master-data position.
    mg <- .hfc_track_survey_layout(mg, by)
    if (!has_outcome) mg <- mg[, setdiff(names(mg), c("_tmv_complete", "_tmv_complete_perc")), drop = FALSE]

    nomerge <- sum(mg[["_merge"]] == 2)
    if (!surveyok && nomerge) {
      message(sprintf("The following %d vaues in %s are not in the master data", nomerge, by))
      print(.hfc_track_survey_rows(mg, mg[["_merge"]] == 2)[by], row.names = FALSE)
      hfc_stop("The following %d vaues in %s are not in the master data", nomerge, by)
    }
    attr(mg[[by]], "label") <- by
    mg <- .hfc_track_survey_rows(mg, mg[["_merge"]] %in% c(1, 3))
    mg <- mg[, names(mg) != "_merge", drop = FALSE]

    srt <- if (has_outcome) c("_tmv_survey_perc", "_tmv_survey", "_tmv_complete_perc", "_tmv_complete", by)
           else c("_tmv_survey_perc", "_tmv_survey", by)
    mg <- .hfc_track_survey_rows(mg, .hfc_track_survey_order(mg, srt))
    mg <- .hfc_track_survey_totals(mg, "_tmv_target", has_outcome)

    out <- hfc_labels(mg, by, nolabel)
    out <- .hfc_track_survey_decode(out)
    numc <- c("_tmv_target", "_tmv_survey", if (has_outcome) "_tmv_complete")
    pctc <- c("_tmv_survey_perc", if (has_outcome) "_tmv_complete_perc")
    hfc_rm(outfile)
    .hfc_track_survey_summary_sheet(outfile, "summary", out, numc, pctc, c("_tmv_fdate", "_tmv_ldate"))
    res$summary <- out
    res$N_groups <- nrow(out) - 1
    res$N_target <- out[["_tmv_target"]][nrow(out)]
    res$N_submitted <- out[["_tmv_survey"]][nrow(out)]
    if (has_outcome) res$N_completed <- out[["_tmv_complete"]][nrow(out)]

    # merge 1:1 id using survey data, keepusing(date keepsurvey)
    .hfc_track_survey_samekey(main, inp, id, "survey", "master")
    if (anyDuplicated(.hfc_track_survey_key(inp[[id]])))
      hfc_stop("variable %s does not uniquely identify observations in the master data", id)
    if (anyDuplicated(.hfc_track_survey_key(main[[id]])))
      hfc_stop("variable %s does not uniquely identify observations in the using data", id)
    sm <- .hfc_track_survey_merge(inp, main[, names(main) %in% c(id, date, keepsurvey), drop = FALSE], id)

    if (nz(save)) {
      # NOTE(stata-bug): Stata keeps `tmv_status' (not yet created) instead of _survey_status,
      # so save() always fails in Stata; the intended dataset is written here.
      sd <- sm
      sd[["_survey_status"]] <- structure(ifelse(sd[["_merge"]] == 3, "submitted",
                                     ifelse(sd[["_merge"]] == 1, "master data only", "survey data only")),
                                     label = "status")
      sd <- sd[, unique(c(by, keepmaster, id, "_survey_status", date, keepsurvey)), drop = FALSE]
      sfile <- if (tools::file_ext(save) == "") paste0(save, ".dta") else save
      hfc_save(sd, sfile)
    }

    if (surveyok) {
      so <- .hfc_track_survey_rows(sm, sm[["_merge"]] == 2)
      if (nrow(so) > 0) {
        so <- so[, names(so) %in% c(id, date, keepsurvey), drop = FALSE]
        if (length(keepsurvey)) {
          so <- hfc_labels(so, keepsurvey, nolabel)
          for (v in intersect(keepsurvey, names(so))) attr(so[[v]], "label") <- v
        }
        so <- hfc_labels(so, id, nolabel)
        so <- .hfc_track_survey_decode(so)
        hfc_write_sheet(outfile, "Unmatched IDs from Survey", so, header = "label")
      }
      res$N_unmatched <- nrow(so)
    }

    st <- .hfc_track_survey_rows(sm, sm[["_merge"]] %in% c(1, 3))
    st[["_tmv_status"]] <- structure(ifelse(st[["_merge"]] == 3, "submitted", "not submitted"), label = "status")
    st <- .hfc_track_survey_rows(st, .hfc_track_survey_order(st, c(by, "_tmv_status", date)))
    st <- st[, unique(c(by, keepmaster, id, "_tmv_status", date, keepsurvey)), drop = FALSE]

    if (!summaryonly) {
      gk <- st[[by]][!hfc_missing(st[[by]])]
      isnum <- !is.character(st[[by]])
      groups <- if (isnum) sort(unique(hfc_num(gk))) else sort(unique(gk), method = "radix")
      group_cnt <- length(groups)
      if (group_cnt > 30 && !workbooks)
        hfc_stop("%s has more than 30 unique values. Please use the workbooks option to create seperate workbooks for each group.", by)
      if (workbooks) {
        message(sprintf("Creating %d workbooks from %s", group_cnt, by))
        ext <- tools::file_ext(outfile)
        filename <- if (nzchar(ext)) substr(outfile, 1, nchar(outfile) - nchar(ext) - 1) else outfile
      }
      for (g in groups) {
        sel <- if (isnum) hfc_num(st[[by]]) == g else st[[by]] == g
        gd <- .hfc_track_survey_rows(st, sel %in% TRUE)
        kv <- c(keepsurvey, keepmaster)
        if (length(kv)) {
          gd <- hfc_labels(gd, kv, nolabel)
          for (v in intersect(kv, names(gd))) attr(gd[[v]], "label") <- v
        }
        # numeric by: ipalabels by id ; string by: ipalabels by (Stata)
        gd <- hfc_labels(gd, if (isnum) c(by, id) else by, nolabel)
        gd <- .hfc_track_survey_decode(gd)
        gname <- if (isnum) format(g, scientific = FALSE, trim = TRUE) else g
        if (workbooks) {
          file <- paste0(filename, "_", gname, ".xlsx"); hfc_rm(file)
          hfc_write_sheet(file, "status", gd, header = "label")
        } else {
          hfc_write_sheet(outfile, gname, gd, header = "label")
        }
        if (workbooks) message(".", appendLF = FALSE)
      }
      if (workbooks && group_cnt) message("")
    }
  }
  invisible(res)
}

# -----------------------------------------------------------------------------
#  helpers (private)
# -----------------------------------------------------------------------------

.hfc_track_survey_nz <- function(x) {
  if (is.null(x)) return(FALSE)
  if (is.data.frame(x)) return(TRUE)
  if (is.logical(x)) return(isTRUE(any(x)))
  x <- x[!is.na(x)]
  length(x) > 0 && any(nzchar(trimws(as.character(x))))
}

#' read a dataset given as path (.dta/.xlsx/.xls/.csv/.rds) or data.frame
.hfc_track_survey_read <- function(x, csv_lower = TRUE) {
  if (is.data.frame(x)) return(as.data.frame(x, stringsAsFactors = FALSE))
  x <- as.character(x)[1]
  if (!file.exists(x)) hfc_stop("file %s not found", x)
  ext <- tolower(tools::file_ext(x))
  if (ext %in% c("xlsx", "xls", "xlsm")) {          # import excel, first
    as.data.frame(suppressMessages(readxl::read_excel(x, sheet = 1)), stringsAsFactors = FALSE)
  } else if (ext == "csv") {                         # import delimited, varnames(1) (case(lower))
    d <- utils::read.csv(x, stringsAsFactors = FALSE, check.names = FALSE)
    if (csv_lower) names(d) <- tolower(names(d))
    d
  } else if (ext == "rds") readRDS(x)
  else as.data.frame(haven::read_dta(x))
}

.hfc_track_survey_keepattr <- function(new, old) {
  attr(new, "label") <- attr(old, "label", exact = TRUE); new
}

#' row-subset a vector keeping all attributes (labels, class, tz)
.hfc_track_survey_take <- function(x, i) {
  att <- attributes(x)
  y <- unclass(x)[i]
  att <- att[setdiff(names(att), c("names", "dim", "dimnames"))]
  attributes(y) <- att
  y
}

.hfc_track_survey_df <- function(cols) {
  n <- if (length(cols)) length(cols[[1]]) else 0L
  structure(cols, class = "data.frame", row.names = .set_row_names(n))
}

.hfc_track_survey_rows <- function(df, i) {
  if (is.logical(i)) i <- which(i)
  .hfc_track_survey_df(lapply(as.list(df), .hfc_track_survey_take, i = i))
}

.hfc_track_survey_key <- function(x) if (is.character(x)) x else as.character(hfc_num(x))

#' Stata gsort (ascending): numeric missing last, strings in byte order
.hfc_track_survey_order <- function(df, cols) {
  keys <- lapply(cols, function(v) {
    x <- df[[v]]
    if (is.character(x)) { x[is.na(x)] <- ""; x } else hfc_num(x)
  })
  do.call(order, c(unname(keys), list(na.last = TRUE, method = "radix")))
}

.hfc_track_survey_samekey <- function(a, b, key, na, nb) {
  if (!(key %in% names(a))) hfc_stop("variable %s not found in %s data", key, na)
  if (!(key %in% names(b))) hfc_stop("variable %s not found in %s data", key, nb)
  ta <- is.character(a[[key]]); tb <- is.character(b[[key]])
  if (ta != tb) hfc_stop("key variable %s is %s in %s data but %s in %s data", key,
                         if (ta) "str" else "numeric", na, if (tb) "str" else "numeric", nb)
}

#' collapse (sum) survey complete (min) fdate=date (max) ldate=date, by(by)
.hfc_track_survey_collapse <- function(main, by, date) {
  k <- .hfc_track_survey_key(main[[by]])
  first <- !duplicated(k)
  g <- .hfc_track_survey_rows(main[by], first)
  gk <- k[first]
  idx <- match(k, gk)
  d <- hfc_num(main[[date]])
  surv <- as.numeric(tabulate(idx, nbins = length(gk)))
  comp <- as.numeric(vapply(split(main[["_tmv_complete"]], factor(idx, seq_along(gk))), sum, numeric(1)))
  mn <- vapply(split(d, factor(idx, seq_along(gk))), hfc_min, numeric(1))
  mx <- vapply(split(d, factor(idx, seq_along(gk))), hfc_max, numeric(1))
  isd <- hfc_is_date(main[[date]])
  fd <- if (isd) as.Date(unname(mn), origin = "1970-01-01") else unname(mn)
  ld <- if (isd) as.Date(unname(mx), origin = "1970-01-01") else unname(mx)
  out <- .hfc_track_survey_df(list(g[[by]], surv, comp, fd, ld))
  names(out) <- c(by, "_tmv_survey", "_tmv_complete", "_tmv_fdate", "_tmv_ldate")
  .hfc_track_survey_rows(out, .hfc_track_survey_order(out, by))
}

#' Stata merge (1:1 or 1:m, master unique on key): full join sorted by key with
#' _merge = 1 master only, 2 using only, 3 matched. Master values win for
#' common variables.
.hfc_track_survey_merge <- function(m, u, key) {
  km <- .hfc_track_survey_key(m[[key]]); ku <- .hfc_track_survey_key(u[[key]])
  mi_u <- match(ku, km)
  mi <- c(which(!(km %in% ku)), mi_u)
  ui <- c(rep(NA_integer_, sum(!(km %in% ku))), seq_along(ku))
  mrg <- ifelse(is.na(mi), 2, ifelse(is.na(ui), 1, 3))
  cols <- list()
  for (v in names(m)) {
    x <- .hfc_track_survey_take(m[[v]], mi)
    if (v == key || v %in% names(u)) {
      fill <- is.na(mi)
      if (any(fill) && v %in% names(u)) {
        y <- .hfc_track_survey_take(u[[v]], ui)
        xx <- unclass(x); xx[fill] <- unclass(y)[fill]
        attributes(xx) <- attributes(x); x <- xx
      }
    }
    cols[[v]] <- x
  }
  for (v in setdiff(names(u), names(m))) cols[[v]] <- .hfc_track_survey_take(u[[v]], ui)
  cols[["_merge"]] <- mrg
  out <- .hfc_track_survey_df(cols)
  .hfc_track_survey_rows(out, .hfc_track_survey_order(out, key))
}

.hfc_track_survey_setlabs <- function(df, labs) {
  for (v in intersect(names(labs), names(df))) attr(df[[v]], "label") <- unname(labs[[v]])
  df
}

#' order `first` first; survey_perc after survey; complete_perc after complete
.hfc_track_survey_layout <- function(df, first) {
  nm <- c(first, setdiff(names(df), first))
  move_after <- function(nm, v, after) {
    if (!(v %in% nm) || !(after %in% nm)) return(nm)
    nm <- nm[nm != v]; p <- match(after, nm)
    append(nm, v, after = p)
  }
  nm <- move_after(nm, "_tmv_survey_perc", "_tmv_survey")
  nm <- move_after(nm, "_tmv_complete_perc", "_tmv_complete")
  df[, nm, drop = FALSE]
}

#' add the total row (colsum, missing = 0) and recode missing counts/% to 0
.hfc_track_survey_totals <- function(df, target, has_outcome) {
  n <- nrow(df)
  df <- .hfc_track_survey_rows(df, c(seq_len(n), NA))
  for (v in names(df)) if (is.character(df[[v]])) df[[v]][n + 1] <- ""
  tgt <- hfc_num(df[[target]]); tt <- sum(tgt, na.rm = TRUE)
  st <- sum(df[["_tmv_survey"]], na.rm = TRUE)
  x <- unclass(df[[target]]); x[n + 1] <- tt; attributes(x) <- attributes(df[[target]]); df[[target]] <- x
  df[["_tmv_survey"]][n + 1] <- st
  df[["_tmv_survey_perc"]][n + 1] <- st / tt
  rc <- c("_tmv_survey", "_tmv_survey_perc")
  if (has_outcome) {
    ct <- sum(df[["_tmv_complete"]], na.rm = TRUE)
    df[["_tmv_complete"]][n + 1] <- ct
    df[["_tmv_complete_perc"]][n + 1] <- ct / tt
    rc <- c(rc, "_tmv_complete", "_tmv_complete_perc")
  }
  for (v in rc) df[[v]][is.na(df[[v]])] <- 0
  df
}

#' export excel (without nolabel) writes value labels of labelled vars
.hfc_track_survey_decode <- function(df) {
  lv <- names(df)[vapply(df, function(x) inherits(x, "haven_labelled"), logical(1))]
  for (v in lv) df <- hfc_labels(df, v, nolabel = FALSE)
  df
}

#' summary / survey progress sheet with header, total row and column formats
.hfc_track_survey_summary_sheet <- function(outfile, sheet, df, numc, pctc, datec) {
  wb <- hfc_write_sheet(outfile, sheet, df, header = "label", total_row = TRUE, save = FALSE)
  rows <- seq_len(nrow(df)) + 1
  fmt <- function(cols, f) {
    j <- match(cols, names(df)); j <- j[!is.na(j)]
    if (length(j) && length(rows))
      openxlsx::addStyle(wb, sheet, openxlsx::createStyle(numFmt = f), rows = rows, cols = j,
                         gridExpand = TRUE, stack = TRUE)
  }
  # Stata order: header, total, date_d_mon_yy, number_sep, percent_d2 (rows 2..N+1)
  fmt(datec, .hfc_formats[["date_d_mon_yy"]])
  fmt(numc,  .hfc_formats[["number_sep"]])
  fmt(pctc,  .hfc_formats[["percent_d2"]])
  openxlsx::saveWorkbook(wb, outfile, overwrite = TRUE)
  invisible(wb)
}
