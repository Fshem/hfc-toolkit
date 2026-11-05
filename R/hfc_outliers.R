# =============================================================================
#  hfc_outliers : flag outliers in numeric variables
# =============================================================================

#' Flag outliers in numeric variables
#'
#' Reads the "outliers" inputs sheet (columns variable, by, method, multiplier,
#' combine, keep), computes per-variable (optionally per `by` group, optionally
#' pooling `combine`d variables) statistics and flags values outside
#'   iqr : [p25 - 1.5*iqr, p75 + 1.5*iqr]
#'   sd  : [mean - multiplier*sd, mean + multiplier*sd]
#' Defaults: method "iqr"; multiplier 1.5 (iqr) / 3 (sd).
#' Results are written to `outsheet` of `outfile` with columns
#' enumerator, keep, date, id, variable, label, byvar, <by vars>, combine,
#' value, count, min, mean, max, range.
#'
#' @param data       survey data.frame
#' @param using      inputs workbook (hfc_inputs.xlsm)
#' @param sheet      inputs sheet (default "outliers")
#' @param outfile    output workbook
#' @param outsheet   output sheet (default "outliers")
#' @param id,enumerator,date variable names
#' @param sheetreplace replace the output sheet (default TRUE)
#' @param nolabel    export values instead of value labels for id/enumerator/keep
#' @param use_multiplier if TRUE, the iqr range uses the sheet multiplier instead
#'                   of Stata's hard-coded 1.5 (see NOTE(stata-bug) below)
#' @return invisibly list(N_outliers, N_vars, data = <flagged rows>)
#' @examples
#' # hfc_outliers(survey, "hfc_inputs.xlsm", outfile = "hfc_output.xlsx",
#' #                  id = "hhid", enumerator = "a_enum_id", date = "starttime")
hfc_outliers <- function(data, using, sheet = "outliers", outfile, outsheet = "outliers",
                             id, enumerator, date, sheetreplace = TRUE, nolabel = FALSE,
                             use_multiplier = FALSE) {
  data <- as.data.frame(data)
  sheet <- sheet %||% "outliers"; outsheet <- outsheet %||% "outliers"

  # ---- import input data ------------------------------------------------------
  inp <- hfc_read_inputs(using, sheet = sheet, required = "variable")
  inp <- inp[!is.na(inp$variable), , drop = FALSE]
  if (anyDuplicated(inp$variable)) {
    message("Duplicates found in inputs sheet:")
    print(inp[inp$variable %in% inp$variable[duplicated(inp$variable)],
              intersect(c("variable", "by", "method", "multiplier", "combine"), names(inp))])
    hfc_stop("variable does not uniquely identify the observations")
  }
  inp <- hfc_ensure_cols(inp, c("by", "method", "multiplier", "combine", "keep"))

  .lev <- function(x) { x <- unique(x[!is.na(x)]); sort(x, method = "radix") }   # levelsof, clean
  vars_cells <- .lev(inp$variable)
  byvars <- hfc_vars(data, .lev(inp$by))
  keep <- hfc_vars(data, .lev(inp$keep))

  inp$method[is.na(inp$method)] <- "iqr"
  inp$multiplier <- ifelse(inp$method == "iqr" & is.na(inp$multiplier), "1.5",
                    ifelse(inp$method == "sd" & is.na(inp$multiplier), "3", inp$multiplier))
  mult <- suppressWarnings(as.numeric(inp$multiplier))
  if (any(is.na(mult) & !is.na(inp$multiplier))) {
    message("Multiplier contains non-numeric variables")
    bad <- which(is.na(mult) & !is.na(inp$multiplier))
    print(data.frame(row = bad, inp[bad, c("variable", "method", "multiplier")]), row.names = FALSE)
    hfc_stop("invalid multiplier")
  }
  inp$multiplier <- mult

  okay <- hfc_okay_table(data, id)

  # expand vars in input sheet
  rowvars <- vector("list", nrow(inp))
  for (i in seq_len(nrow(inp))) {
    rowvars[[i]] <- hfc_vars(data, inp$variable[i], unique = FALSE)
    if (length(keep) && any(rowvars[[i]] %in% keep)) {
      hfc_stop("Variables in varlist and keep are mutually exclusive. %s is in both", paste(rowvars[[i]], collapse = " "))
    }
  }

  # ---- reshape outlier vars long ----------------------------------------------
  vars <- unique(hfc_vars(data, vars_cells, unique = FALSE))
  for (v in vars) if (!hfc_is_num(data[[v]]) || hfc_is_date(data[[v]]) || hfc_is_datetime(data[[v]]))
    hfc_stop("Variable %s must be a numeric variable", v)

  base <- data[, unique(c(id, enumerator, date, keep, byvars)), drop = FALSE]
  n <- nrow(data)
  # reshape long sorts by i(reshape_id) j(index): obs order, then variable order
  ord <- order(rep(seq_len(n), times = length(vars)), rep(seq_along(vars), each = n))
  long <- base[rep(seq_len(n), times = length(vars))[ord], , drop = FALSE]
  long$variable <- rep(vars, each = n)[ord]
  long$varlabel <- unname(vapply(vars, function(v) hfc_varlabel(data, v, fallback = FALSE), ""))[rep(seq_along(vars), each = n)[ord]]
  long$value <- unlist(lapply(vars, function(v) hfc_num(data[[v]])), use.names = FALSE)[ord]
  long <- long[!is.na(long$value), , drop = FALSE]
  rownames(long) <- NULL

  N <- nrow(long)
  st <- c("value_count", "value_min", "value_max", "value_mean", "value_median", "value_sd", "p25", "p75", "iqr")
  for (s in st) long[[s]] <- rep(NA_real_, N)
  long$byvar <- rep("", N); long$method <- rep("", N); long$multiplier <- rep(NA_real_, N)
  long$combine <- long$variable; combine_ind <- rep(FALSE, N)

  .stats <- function(sel, by) {
    grp <- if (nzchar(by %||% "")) {
      bv <- hfc_vars(data, by)
      interaction(lapply(bv, function(b) { z <- as.character(hfc_num_or_chr(long[[b]])); z[is.na(z)] <- "\001NA"; z }), drop = TRUE, lex.order = TRUE)
    } else factor(rep(1, N))
    for (g in unique(grp[sel])) {
      w <- sel & grp == g
      x <- long$value[w]
      vals <- c(length(x), hfc_min(x), hfc_max(x), hfc_mean(x), hfc_pctile(x, 50), hfc_sd(x),
                hfc_pctile(x, 25), hfc_pctile(x, 75), hfc_iqr(x))
      vals <- .hfc_outliers_float(vals)  # egen/gen create float variables
      for (k in seq_along(st)) long[[st[k]]][w] <<- vals[k]
    }
  }

  for (i in seq_len(nrow(inp))) {
    by_i <- inp$by[i]; m_i <- inp$method[i]; mu_i <- inp$multiplier[i]
    if (tolower(inp$combine[i] %||% "") == "yes") {
      cstr <- paste(rowvars[[i]], collapse = " ")
      hit <- long$variable %in% rowvars[[i]]
      long$combine[hit] <- cstr; combine_ind[hit] <- TRUE
      sel <- long$combine == cstr
      .stats(sel, by_i)
      long$byvar[sel] <- by_i %||% ""; long$method[sel] <- m_i; long$multiplier[sel] <- mu_i
    } else {
      for (v in rowvars[[i]]) {
        sel <- long$variable == v
        .stats(sel, by_i)
        long$byvar[sel] <- by_i %||% ""; long$method[sel] <- m_i; long$multiplier[sel] <- mu_i
      }
    }
  }
  long$combine[!combine_ind] <- ""

  # NOTE(stata-bug): Stata uses a hard-coded 1.5 * iqr here, ignoring the
  # multiplier column for method "iqr" (yet the "range" text prints the
  # multiplier). Kept faithful; set use_multiplier = TRUE to use the multiplier.
  iqr_mult <- if (use_multiplier) long$multiplier else 1.5
  sd_lo <- long$value_mean - long$multiplier * long$value_sd
  sd_hi <- long$value_mean + long$multiplier * long$value_sd
  long$range_min <- .hfc_outliers_float(ifelse(long$method == "iqr", long$p25 - iqr_mult * long$iqr, sd_lo))
  long$range_max <- .hfc_outliers_float(ifelse(long$method == "iqr", long$p75 + iqr_mult * long$iqr, sd_hi))

  long <- long[!.hfc_outliers_inrange(long$value, long$range_min, long$range_max), , drop = FALSE]

  # drop if already marked as ok
  long <- hfc_drop_okay(long, okay, id, varcol = "variable")

  N_out <- nrow(long); var_cnt <- 0L
  if (N_out > 0) {
    long[[date]] <- hfc_to_date(long[[date]])
    method_value <- ifelse(long$method == "iqr", long$iqr, long$value_sd)
    f2 <- function(x) ifelse(is.na(x), ".", sprintf("%.2f", x))
    long$range <- paste0("Range for ", f2(long$multiplier), " * ", long$method,
                         "(", f2(method_value), "): ", f2(long$range_min), " to ", f2(long$range_max))

    cols <- unique(c(enumerator, keep, date, id, "variable", "varlabel", "byvar", byvars, "combine",
                     "value", "value_count", "value_min", "value_mean", "value_max", "range"))
    out <- long[, cols, drop = FALSE]
    for (v in names(out)) attr(out[[v]], "label") <- v
    for (p in list(c("varlabel", "label"), c("value_count", "count"), c("value_mean", "mean"),
                   c("value_min", "min"), c("value_max", "max")))
      attr(out[[p[1]]], "label") <- p[2]

    if (length(keep)) out <- hfc_labels(out, keep, nolabel)
    out <- hfc_labels(out, c(id, enumerator), nolabel)
    rownames(out) <- NULL
    hfc_write_sheet(outfile, outsheet, out, header = "label", replace_sheet = sheetreplace,
                    formats = list(number_sep_d2 = c("value_min", "value_mean", "value_max"),
                                   number_sep = "value_count", date_d_mon_yy = date))
    var_cnt <- length(unique(out$variable))
    hfc_msg("Found %d outliers in %d variable(s).", N_out, var_cnt)
  } else {
    out <- long
    hfc_msg("Found 0 outliers.")
  }
  invisible(list(N_outliers = N_out, N_vars = var_cnt, data = out))
}

# round to Stata float (single) precision
.hfc_outliers_float <- function(x) {
  y <- x; ok <- is.finite(x)
  if (any(ok)) y[ok] <- readBin(writeBin(x[ok], raw(), size = 4), "numeric", size = 4, n = sum(ok))
  y
}

# Stata inrange(z, a, b) with missing semantics
.hfc_outliers_inrange <- function(z, a, b) {
  ifelse(is.na(z), FALSE,
  ifelse(is.na(a) & is.na(b), TRUE,
  ifelse(is.na(a), z <= b,
  ifelse(is.na(b), z >= a, z >= a & z <= b))))
}
