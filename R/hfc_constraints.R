# =============================================================================
#  hfc_constraints : flag hard/soft constraint violations
# =============================================================================

#' Flag numeric values that violate hard/soft min/max constraints
#'
#' Reads the "constraints" inputs sheet (variable, hard_min, soft_min,
#' soft_max, hard_max, keep). Constraint cells hold a number or a numeric
#' varlist (one var for all, or one per expanded variable). A value violates
#' a *_min if value < constraint, a *_max if value > constraint (missing
#' values never violate; a missing constraint variable is +Inf as in Stata).
#' For each id/enumerator/variable/value only one row is kept, hard
#' violations preferred over soft ones.
#'
#' @param data,using,sheet,outfile,outsheet,id,enumerator,date,sheetreplace,nolabel
#'   as in Stata (sheet/outsheet default "constraints")
#' @return invisibly list(N_constraints, N_vars, data = <violations>)
#' @examples
#' # hfc_constraints(survey, "hfc_inputs.xlsm", outfile = "hfc_output.xlsx",
#' #                     id = "hhid", enumerator = "a_enum_id", date = "starttime")
hfc_constraints <- function(data, using, sheet = "constraints", outfile, outsheet = "constraints",
                                id, enumerator, date, sheetreplace = TRUE, nolabel = FALSE) {
  data <- as.data.frame(data)
  sheet <- sheet %||% "constraints"; outsheet <- outsheet %||% "constraints"
  cols4 <- c("hard_min", "soft_min", "soft_max", "hard_max")

  inp <- hfc_read_inputs(using, sheet = sheet, required = "variable")
  inp <- hfc_ensure_cols(inp, c(cols4, "keep"))
  inp <- inp[!is.na(inp$variable), , drop = FALSE]
  if (anyDuplicated(inp$variable)) {
    message("Duplicates found in inputs sheet:")
    print(inp[inp$variable %in% inp$variable[duplicated(inp$variable)], c("variable", cols4)])
    hfc_stop("variable does not uniquely identify the observations")
  }
  nocons <- rowSums(!is.na(inp[, cols4, drop = FALSE])) == 0
  if (any(nocons)) {
    message("The following rows in the inputs sheet have no constraints conditions:")
    print(inp[nocons, c("variable", cols4)])
    hfc_stop("missing constraints conditions")
  }
  keep <- hfc_vars(data, sort(unique(inp$keep[!is.na(inp$keep)]), method = "radix"))

  .isnum <- function(s) !is.na(suppressWarnings(as.numeric(s)))   # confirm number
  .chk_num <- function(v) if (!hfc_is_num(data[[v]]) || hfc_is_date(data[[v]]) || hfc_is_datetime(data[[v]]))
    hfc_stop("variable %s found where numeric variable is expected. Only numeric variables are allowed in constraints check", v)

  rowvars <- vector("list", nrow(inp))
  for (i in seq_len(nrow(inp))) {
    vars <- hfc_vars(data, inp$variable[i])
    for (v in vars) .chk_num(v)
    rowvars[[i]] <- vars
    for (col in cols4) {
      val <- inp[[col]][i]
      if (!is.na(val) && !.isnum(val)) {
        valv <- hfc_vars(data, val)
        if ((length(vars) > 1 || length(valv) > 1) && length(vars) != length(valv) && length(valv) > 1) {
          message(sprintf("Number of variables in variable & %s columns don't match after expansion:", col))
          message(sprintf("variables column: %s", paste(vars, collapse = " ")))
          message(sprintf("%s column: %s", col, paste(valv, collapse = " ")))
          hfc_stop("variable/constraint mismatch")
        }
        for (v in valv) .chk_num(v)
        inp[[col]][i] <- paste(valv, collapse = " ")
      }
    }
  }

  okay <- hfc_okay_table(data, id)
  f <- .hfc_constraints_float

  viols <- NULL
  for (i in seq_len(nrow(inp))) {
    vars <- rowvars[[i]]
    for (j in seq_along(vars)) {
      var <- vars[j]; k <- 1
      for (col in cols4) {
        cval <- inp[[col]][i]
        if (!is.na(cval)) {
          isnum <- .isnum(cval)
          if (!isnum) { w <- strsplit(cval, " ")[[1]]; cval <- if (length(w) == 1) w else w[j] }
          x <- hfc_num(data[[var]])
          cnum <- if (isnum) rep(f(as.numeric(cval)), nrow(data)) else { z <- hfc_num(data[[cval]]); z[is.na(z)] <- Inf; z }
          viol <- !is.na(x) & (if (k <= 2) x < cnum else x > cnum)
          if (any(viol)) {
            colname <- gsub("_", " ", col)
            cons <- if (isnum) sprintf("%s is %s", colname, cval)
                    else paste0(colname, " [", cval, "] is ", .hfc_constraints_string(hfc_num(data[[cval]])[viol]))
            d <- data[viol, unique(c(id, enumerator, date, keep)), drop = FALSE]
            d$`__var` <- var
            d$`__lab` <- hfc_varlabel(data, var, fallback = FALSE)
            d$`__value` <- x[viol]
            d$`__constraint` <- cons
            viols <- if (is.null(viols)) d else rbind(d, viols)   # append using: new rows first
          }
        }
        k <- k + 1
      }
    }
  }

  N <- 0L; nv <- 0L; out <- NULL
  if (!is.null(viols) && nrow(viols) > 0) {
    viols <- .hfc_constraints_dropokay(viols, okay, id)
    if (nrow(viols) > 0) {
      key <- function(cols) do.call(paste, c(lapply(viols[cols], function(z) as.character(hfc_num_or_chr(z))), sep = "\r"))
      viols <- viols[!duplicated(key(c(id, enumerator, "__var", "__value", "__constraint"))), , drop = FALSE]
      hard <- grepl("^hard", viols$`__constraint`)
      # gsort id enumerator var value -hard ; duplicates drop id enumerator var value
      o <- order(.hfc_constraints_sortkey(viols[[id]]), .hfc_constraints_sortkey(viols[[enumerator]]),
                 viols$`__var`, viols$`__value`, -hard, method = "radix")
      viols <- viols[o, , drop = FALSE]
      viols <- viols[!duplicated(key(c(id, enumerator, "__var", "__value"))), , drop = FALSE]

      out <- viols[, unique(c(enumerator, keep, date, id, "__var", "__lab", "__value", "__constraint")), drop = FALSE]
      for (v in names(out)) attr(out[[v]], "label") <- v
      names(out)[match(c("__var", "__lab", "__value", "__constraint"), names(out))] <- c("variable", "label", "value", "constraint")
      attr(out$variable, "label") <- "variable"; attr(out$label, "label") <- "label"
      attr(out$value, "label") <- "value"; attr(out$constraint, "label") <- "constraint"
      if (length(keep)) out <- hfc_labels(out, keep, nolabel)
      out <- hfc_labels(out, c(id, enumerator), nolabel)
      out <- out[order(as.numeric(out[[date]]), method = "radix"), , drop = FALSE]
      rownames(out) <- NULL
      wb <- hfc_write_sheet(outfile, outsheet, out, header = "label", replace_sheet = sheetreplace,
                            formats = list(date_d_mon_yy = date), save = FALSE)
      # date stays a datetime (no ipagettd here); ipacolformat date_d_mon_yy must
      # override hfc_write_sheet's default datetime style
      .hfc_constraints_datefmt(wb, outsheet, out, date)
      openxlsx::saveWorkbook(wb, outfile, overwrite = TRUE)
      N <- nrow(out); nv <- length(unique(out$variable))
    }
  }
  hfc_msg("Found %d constraint violations in %d variable(s).", N, nv)
  invisible(list(N_constraints = N, N_vars = nv, data = out))
}

# drop rows marked okay: regexm(okvars, "^" + var + "/") | regexm(okvars, "/" + var + "/")
.hfc_constraints_dropokay <- function(d, okay, id, varcol = "__var") {
  if (is.null(okay) || !nrow(d)) return(d)
  ids <- as.character(hfc_num_or_chr(d[[id]]))
  drop <- rep(FALSE, nrow(d))
  for (i in seq_len(nrow(okay))) {
    ov <- okay$vars[i]
    hit <- ids == okay$id[i] & vapply(d[[varcol]], function(v)
      startsWith(ov, paste0(v, "/")) || grepl(paste0("/", v, "/"), ov, fixed = TRUE), logical(1))
    drop <- drop | hit
  }
  d[!drop, , drop = FALSE]
}

.hfc_constraints_float <- function(x) {
  y <- x; ok <- is.finite(x)
  if (any(ok)) y[ok] <- readBin(writeBin(x[ok], raw(), size = 4), "numeric", size = 4, n = sum(ok))
  y
}

# Stata string(x): %9.0g display format
.hfc_constraints_string <- function(x) vapply(x, function(z) {
  if (is.na(z)) return(".")
  for (d in 9:1) { s <- trimws(formatC(z, format = "fg", digits = d)); if (nchar(s) <= 9) return(sub("^(-?)0\\.", "\\1.", sub("\\.$", "", s))) }
  s <- formatC(z, format = "e", digits = 2); sub("e([+-])0?([0-9]{2})$", "e\\1\\2", s)
}, "")

.hfc_constraints_datefmt <- function(wb, sheet, out, date) {
  j <- match(date, names(out))
  if (!is.na(j) && nrow(out))
    openxlsx::addStyle(wb, sheet, openxlsx::createStyle(numFmt = "d-mmm-yy"), rows = 1 + seq_len(nrow(out)),
                       cols = j, stack = TRUE)
}

.hfc_constraints_sortkey <- function(x) { y <- hfc_num_or_chr(x); if (is.character(y)) y else { y[is.na(y)] <- Inf; y } }
