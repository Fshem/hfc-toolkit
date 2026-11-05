# =============================================================================
#  hfc_enum_dashboard : enumerator dashboard
#  Uses the private helpers defined in R/hfc_survey_dashboard.R
#  (.hfc_survey_dashboard_*), which must be sourced as well.
# =============================================================================

#' Enumerator dashboard
#'
#' Port of Stata `hfc_enum_dashboard`. Writes to `outfile` the sheets
#' "summary" (statistics by enumerator), "summary (team)" (if `team`),
#' "<period> productivity" (submissions per enumerator per day/week/month, with
#' a total row), "<period> productivity (team)" (if `team`) and, when `using`
#' is given, "enumstats" (count/min/mean/median/sd/max of selected numeric
#' variables by enumerator, as configured in the `sheetname` sheet of the
#' inputs workbook: columns variable, min, mean, show_mean_as, median, sd, max,
#' combine). Other sheets of an existing `outfile` are kept.
#'
#' @param data data.frame (survey data, dk/ref recoded to .d/.r).
#' @param using inputs workbook (e.g. hfc_inputs.xlsm); NULL skips enumstats.
#' @param sheetname sheet of `using` with enumstats specs (default "enumstats").
#' @param date date or datetime variable.
#' @param period "auto" (default), "daily", "weekly", "monthly" (see hfc_survey_dashboard).
#' @param enumerator enumerator variable.
#' @param team team variable (optional).
#' @param consent,cons_vals consent variable and numlist of consent values.
#' @param dontknow_num,dontknow_str,refuse_num,refuse_str don't know / refuse values
#'   (numeric integer or ".d"/".r"; space-separated strings).
#' @param otherspecify other-specify child variables.
#' @param duration numeric duration variable.
#' @param formversion form version variable.
#' @param outfile output .xlsx file.
#' @param sheetreplace replace existing sheets (default TRUE); sheetmodify writes into them.
#' @param nolabel export values instead of value labels for enumerator / team.
#' @return invisibly list(N_enums, N_teams, period, N_statvars).
#' @examples
#' \dontrun{
#' hfc_enum_dashboard(df, using = "hfc_inputs.xlsm", sheetname = "enumstats",
#'   enumerator = "a_enum_id", team = "a_team_id", date = "starttime",
#'   consent = "c_consent", cons_vals = "1", dontknow_num = ".d", dontknow_str = "-999",
#'   refuse_num = ".r", refuse_str = "-888", otherspecify = childvars,
#'   duration = "duration", formversion = "formdef_version", outfile = "enumdb.xlsx")
#' }
hfc_enum_dashboard <- function(data, using = NULL, sheetname = NULL, date, period = "auto",
                           enumerator, team = NULL, consent = NULL, cons_vals = NULL,
                           dontknow_num = NULL, dontknow_str = NULL,
                           refuse_num = NULL, refuse_str = NULL,
                           otherspecify = NULL, duration = NULL, formversion,
                           outfile, sheetreplace = TRUE, sheetmodify = FALSE,
                           nolabel = FALSE) {
  if (!is.null(team) && (is.na(team) || !nzchar(team))) team <- NULL
  p <- .hfc_survey_dashboard_prep(data, date, enumerator, formversion, period, consent, cons_vals,
                              dontknow_num, dontknow_str, refuse_num, refuse_str,
                              otherspecify, duration, team = team)
  data <- p$data; t <- p$t; fl <- p$flags; period <- p$period
  K <- p$vars_count
  # NOTE(stata-bug): other_count = wordcount of the otherspecify() *string*, not of
  # the expanded varlist (here the brief passes expanded names so they coincide).
  other_count <- p$other_tokens
  replace_sheet <- !isTRUE(sheetmodify)
  wb <- hfc_wb(outfile)
  dir.create(dirname(outfile), showWarnings = FALSE, recursive = TRUE)

  noname <- function(df, v) { attr(df[[v]], "label") <- NULL; df }   # lab var v ""

  # ---- summary (by enumerator) ----
  out <- .hfc_survey_dashboard_collapse(data, t, enumerator, date, enumerator, formversion,
                                    K, other_count, team_first = TRUE)
  out <- .hfc_survey_dashboard_dropcols(out, fl, team = TRUE)
  out <- hfc_labels(out, enumerator, nolabel = nolabel)
  out <- noname(out, enumerator)
  hfc_write_sheet(outfile, "summary", out, header = "label",
                  formats = .hfc_survey_dashboard_sumformats(fl, c("formversion", "days")),
                  wb = wb, save = FALSE, replace_sheet = replace_sheet)

  # ---- summary (by team) ----
  if (fl$team) {
    tout <- .hfc_survey_dashboard_collapse(data, t, team, date, enumerator, formversion,
                                       K, other_count, count_enum = TRUE)
    tout <- .hfc_survey_dashboard_dropcols(tout, fl)
    tout <- hfc_labels(tout, team, nolabel = nolabel)
    tout <- noname(tout, team)
    hfc_write_sheet(outfile, "summary (team)", tout, header = "label",
                    formats = .hfc_survey_dashboard_sumformats(fl, c("enumerators", "formversion", "days")),
                    wb = wb, save = FALSE, replace_sheet = replace_sheet)
  }

  # ---- productivity ----
  cal <- .hfc_survey_dashboard_calendar(data, date, c(enumerator, team))
  pw <- .hfc_survey_dashboard_prodwide(cal, enumerator, date, period)
  pw <- hfc_labels(pw, enumerator, nolabel = nolabel)
  pw <- noname(pw, enumerator)
  vv <- grep("^vv_", names(pw), value = TRUE)
  hfc_write_sheet(outfile, paste(period, "productivity"), pw, header = "label",
                  formats = list(number_sep = vv), total_row = TRUE,
                  wb = wb, save = FALSE, replace_sheet = replace_sheet)

  if (fl$team) {
    pt <- .hfc_survey_dashboard_prodwide(cal, team, date, period)
    pt <- hfc_labels(pt, team, nolabel = nolabel)
    pt <- noname(pt, team)
    vv <- grep("^vv_", names(pt), value = TRUE)
    hfc_write_sheet(outfile, paste(period, "productivity (team)"), pt, header = "label",
                    formats = list(number_sep = vv), total_row = TRUE,
                    wb = wb, save = FALSE, replace_sheet = replace_sheet)
  }

  # ---- variable stats by enumerator ----
  n_stat <- 0L
  if (!is.null(using) && length(using) && nzchar(using)) {
    sheetname <- sheetname %||% "enumstats"
    inp <- hfc_read_inputs(using, sheetname)
    inp <- hfc_ensure_cols(inp, c("variable", "min", "mean", "show_mean_as", "median", "max", "sd", "combine"))
    inp <- inp[!is.na(inp$variable), c("variable", "min", "mean", "show_mean_as", "median", "max", "sd", "combine"), drop = FALSE]
    if (!nrow(inp)) hfc_stop("variable column is required in %s", sheetname)
    nmiss <- rowSums(is.na(inp[, c("min", "mean", "median", "sd", "max")]))
    if (any(nmiss == 5)) {
      bad <- inp[nmiss == 5, , drop = FALSE]; bad$row <- which(nmiss == 5) + 1
      print(bad)
      hfc_stop("missing stats specification for %d rows", sum(nmiss == 5))
    }
    inp$show_mean_as <- ifelse(!is.na(inp$show_mean_as) & tolower(inp$show_mean_as) == "percentage",
                               "percent_d2", "number_sep_d2")
    inp$show_mean_as[is.na(inp$mean)] <- ""
    inp$input_lab <- inp$variable
    n_stat <- nrow(inp)
    inp$vars <- lapply(inp$variable, function(s) unique(hfc_vars(data, s)))

    statvars <- unique(unlist(lapply(sort(unique(inp$variable), method = "radix"), function(s) hfc_vars(data, s))))
    for (v in statvars) if (!hfc_is_num(data[[v]])) hfc_stop("Variable %s must be a numeric variable", v)

    # reshape long, drop missing values
    enum_x <- data[[enumerator]]
    long <- do.call(rbind, lapply(statvars, function(v) {
      val <- hfc_num(data[[v]]); k <- !is.na(val)
      data.frame(row = which(k), value = val[k], variable = v, stringsAsFactors = FALSE)
    }))
    long$combine <- long$variable; long$combine_ind <- 0; long$input_lab <- ""
    for (s in c("count", "min", "mean", "median", "sd", "max")) long[[paste0("value_", s)]] <- NA_real_
    ekey <- .hfc_survey_dashboard_key(enum_x)[long$row]

    stats_by_enum <- function(idx) {
      grp <- split(idx, factor(ekey[idx], exclude = NULL))
      for (r in grp) {
        v <- long$value[r]
        long$value_count[r]  <<- length(v)
        long$value_min[r]    <<- min(v)
        long$value_mean[r]   <<- mean(v)
        long$value_median[r] <<- hfc_pctile(v, 50)
        # NOTE(stata-bug): Stata does `replace value_sd = vmedian`, so the "sd"
        # column shows the median. Kept faithfully.
        long$value_sd[r]     <<- hfc_pctile(v, 50)
        long$value_max[r]    <<- max(v)
      }
    }
    for (i in seq_len(n_stat)) {
      vs <- inp$vars[[i]]
      vstr <- paste(vs, collapse = " ")
      if (!is.na(inp$combine[i]) && tolower(inp$combine[i]) == "yes") {
        hit <- long$variable %in% vs
        long$combine[hit] <- vstr; long$combine_ind[hit] <- 1; long$input_lab[hit] <- inp$input_lab[i]
        stats_by_enum(which(long$combine == vstr))
      } else {
        for (v in vs) {
          hit <- long$variable == v
          long$input_lab[hit] <- inp$input_lab[i]
          stats_by_enum(which(hit))
        }
      }
    }

    # merge m:1 input_lab using inputs, keep(match)
    long <- long[long$input_lab %in% inp$input_lab, , drop = FALSE]
    m <- match(long$input_lab, inp$input_lab)
    for (s in c("min", "mean", "show_mean_as", "median", "max", "sd")) long[[paste0("in_", s)]] <- inp[[s]][m]
    long$input_lab[long$combine_ind == 0] <- long$variable[long$combine_ind == 0]
    long$enum <- .hfc_survey_dashboard_key(enum_x)[long$row]
    long <- long[!duplicated(long[, c("enum", "input_lab")]), , drop = FALSE]

    labs <- sort(unique(long$input_lab), method = "radix")       # encode: alphabetical
    long$index <- match(long$input_lab, labs)
    meta <- long[!duplicated(long$input_lab), , drop = FALSE]
    meta <- meta[order(meta$index), , drop = FALSE]
    percentcols <- meta$input_lab[meta$in_show_mean_as == "percent_d2"]

    # reshape wide i(enumerator) j(index)
    g <- .hfc_survey_dashboard_groups(data.frame(e = long$enum, stringsAsFactors = FALSE), "e")
    first_rows <- long$row[g$first]
    es <- .hfc_survey_dashboard_bycols(data, enumerator, first_rows)
    stats <- c("count", "min", "mean", "median", "sd", "max")
    for (j in seq_along(labs)) {
      for (s in stats) {
        nm <- paste0("value_", s, j)
        col <- rep(NA_real_, length(g$rows))
        for (k in seq_along(g$rows)) {
          r <- g$rows[[k]][long$index[g$rows[[k]]] == j]
          if (length(r)) col[k] <- long[[paste0("value_", s)]][r[1]]
        }
        if (s == "count") col[is.na(col)] <- 0
        keepstat <- if (s == "count") "yes" else meta[[paste0("in_", s)]][j]
        if (s != "count" && (is.na(keepstat) || tolower(keepstat) != "yes")) next
        es[[nm]] <- col
        attr(es[[nm]], "label") <- s
      }
    }
    es <- hfc_labels(es, enumerator, nolabel = nolabel)
    es <- noname(es, enumerator)

    sh <- "enumstats"
    if (sh %in% names(wb) && replace_sheet) openxlsx::removeWorksheet(wb, sh)
    hfc_write_sheet(outfile, sh, es, header = "label", wb = wb, save = FALSE,
                    start_row = 2, replace_sheet = replace_sheet, header_format = FALSE)
    # hfc_write_sheet() makes headers unique ("count.1"); Stata repeats "count" etc.
    hdr <- vapply(names(es), function(v) hfc_varlabel(es, v), character(1))
    openxlsx::writeData(wb, sh, as.data.frame(t(hdr)), startRow = 2, colNames = FALSE)
    # format_edb_stats
    nobs <- nrow(es); nv <- ncol(es)
    add <- function(style, rows, cols) openxlsx::addStyle(wb, sh, style, rows = rows, cols = cols, gridExpand = TRUE, stack = TRUE)
    add(openxlsx::createStyle(textDecoration = c("bold", "italic")), 1:2, seq_len(nv))
    if (nobs > 0 && nv >= 2) add(openxlsx::createStyle(numFmt = "0.00"), 3:(nobs + 2), 2:nv)
    add(openxlsx::createStyle(border = "bottom", borderStyle = "medium"), 2, seq_len(nv))
    vn <- names(es)
    n <- 2; meanvar <- NA_character_
    for (i in seq_along(labs)) {
      startcol <- n
      openxlsx::writeData(wb, sh, labs[i], startCol = n, startRow = 1, colNames = FALSE)
      for (j in 1:5) {
        if (n + 1 <= nv) {
          nextvar <- vn[n + 1]
          if (!grepl("_count", nextvar)) n <- n + 1
          if (grepl("_mean", nextvar)) meanvar <- nextvar
        }
      }
      if (n > startcol) openxlsx::mergeCells(wb, sh, cols = startcol:n, rows = 1)
      add(openxlsx::createStyle(halign = "center"), 1, startcol:n)
      add(openxlsx::createStyle(border = "left", borderStyle = "medium"), 1:(nobs + 2), startcol)
      n <- n + 1
      if (labs[i] %in% percentcols && !is.na(meanvar) && nobs > 0)
        add(openxlsx::createStyle(numFmt = "0.00%"), 3:(nobs + 2), match(meanvar, vn))
    }
  }

  openxlsx::saveWorkbook(wb, outfile, overwrite = TRUE)
  invisible(list(N_enums = nrow(out), N_teams = if (fl$team) nrow(tout) else 0L,
                 period = period, N_statvars = n_stat))
}
