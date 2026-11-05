# =============================================================================
#  hfc_logic : flag logic violations
# =============================================================================

#' Flag observations that violate logical assertions
#'
#' Reads the "logic" inputs sheet (variable, assert, if_condition, keep).
#' For each row, observations where `!(assert)` holds (restricted to
#' `if_condition` when given) are flagged. Both columns hold Stata expressions
#' evaluated with hfc_eval(). Output columns: enumerator, keep, date, id,
#' variable, label, value (as string), logic.
#'
#' @param data,using,sheet,outfile,outsheet,id,enumerator,date,sheetreplace,nolabel
#'   as in Stata (sheet/outsheet default "logic")
#' @return invisibly list(N_logic, N_vars, data = <violations>)
#' @examples
#' # hfc_logic(survey, "hfc_inputs.xlsm", outfile = "hfc_output.xlsx",
#' #               id = "hhid", enumerator = "a_enum_id", date = "starttime")
hfc_logic <- function(data, using, sheet = "logic", outfile, outsheet = "logic",
                          id, enumerator, date, sheetreplace = TRUE, nolabel = FALSE) {
  data <- as.data.frame(data)
  sheet <- sheet %||% "logic"; outsheet <- outsheet %||% "logic"

  inp <- hfc_read_inputs(using, sheet = sheet, required = "variable")
  inp <- hfc_ensure_cols(inp, c("assert", "if_condition", "keep"))
  inp <- inp[!is.na(inp$variable), , drop = FALSE]
  if (any(is.na(inp$assert))) {
    message("Assert column is required for all rows")
    print(inp[is.na(inp$assert), c("variable", "assert", "if_condition")])
    hfc_stop("missing assert")
  }
  keep <- hfc_vars(data, sort(unique(inp$keep[!is.na(inp$keep)]), method = "radix"))

  for (i in seq_len(nrow(inp))) {
    vars <- hfc_vars(data, inp$variable[i])
    if (length(vars) > 1) {
      message("Only one variable is allowed in each cell of the variable column.")
      message(sprintf("Input sheet contains the following %d variables on row %d", length(vars), i))
      message(paste(vars, collapse = " "))
      hfc_stop("too many variables")
    }
    inp$variable[i] <- vars
  }

  okay <- hfc_okay_table(data, id)

  viols <- NULL
  for (i in seq_len(nrow(inp))) {
    var <- inp$variable[i]; assert <- inp$assert[i]; ifc <- inp$if_condition[i]
    # gen viol = !(assert) if (if_condition)
    viol <- !hfc_eval(data, assert)
    if (!is.na(ifc)) viol <- viol & hfc_eval(data, ifc)
    if (any(viol)) {
      d <- data[viol, unique(c(id, enumerator, date, keep)), drop = FALSE]
      x <- data[[var]][viol]
      d$`__value` <- if (hfc_is_num(x) && !is.character(x)) .hfc_logic_string(x) else as.character(x)
      d$`__var` <- var
      d$`__lab` <- hfc_varlabel(data, var, fallback = FALSE)
      # if_condition was rewritten to " if (cond)" -> "assert  if (cond)"
      d$`__logic` <- if (is.na(ifc)) assert else paste0(assert, "  if (", ifc, ")")
      viols <- if (is.null(viols)) d else rbind(d, viols)   # append using: new rows first
    }
  }

  N <- 0L; nv <- 0L; out <- NULL
  if (!is.null(viols) && nrow(viols) > 0) {
    viols <- .hfc_logic_dropokay(viols, okay, id)
    if (nrow(viols) > 0) {
      key <- do.call(paste, c(lapply(viols[c(id, enumerator, "__var", "__value")],
                                     function(z) as.character(hfc_num_or_chr(z))), sep = "\r"))
      viols <- viols[!duplicated(key), , drop = FALSE]

      out <- viols[, unique(c(enumerator, keep, date, id, "__var", "__lab", "__value", "__logic")), drop = FALSE]
      for (v in names(out)) attr(out[[v]], "label") <- v
      names(out)[match(c("__var", "__lab", "__value", "__logic"), names(out))] <- c("variable", "label", "value", "logic")
      for (v in c("variable", "label", "value", "logic")) attr(out[[v]], "label") <- v
      if (length(keep)) out <- hfc_labels(out, keep, nolabel)
      out <- hfc_labels(out, c(id, enumerator), nolabel)
      out <- out[order(as.numeric(out[[date]]), method = "radix"), , drop = FALSE]
      rownames(out) <- NULL
      wb <- hfc_write_sheet(outfile, outsheet, out, header = "label", replace_sheet = sheetreplace,
                            formats = list(date_d_mon_yy = date), save = FALSE)
      # ipacolformat date_d_mon_yy overrides the default datetime style
      j <- match(date, names(out))
      openxlsx::addStyle(wb, outsheet, openxlsx::createStyle(numFmt = "d-mmm-yy"),
                         rows = 1 + seq_len(nrow(out)), cols = j, stack = TRUE)
      openxlsx::saveWorkbook(wb, outfile, overwrite = TRUE)
      N <- nrow(out); nv <- length(unique(out$variable))
    }
  }
  hfc_msg("Found %d logic violations in %d variable(s).", N, nv)
  invisible(list(N_logic = N, N_vars = nv, data = out))
}

# Stata string(x) (%9.0g); extended missings -> ".d", ".r", "."
.hfc_logic_string <- function(x) {
  tag <- if (is.double(unclass(x))) haven::na_tag(as.vector(unclass(x))) else rep(NA_character_, length(x))
  v <- hfc_num(x)
  vapply(seq_along(v), function(i) {
    z <- v[i]
    if (is.na(z)) return(if (is.na(tag[i])) "." else paste0(".", tag[i]))
    for (d in 9:1) { s <- trimws(formatC(z, format = "fg", digits = d)); if (nchar(s) <= 9) return(sub("^(-?)0\\.", "\\1.", sub("\\.$", "", s))) }
    s <- formatC(z, format = "e", digits = 2); sub("e([+-])0?([0-9]{2})$", "e\\1\\2", s)
  }, "")
}

.hfc_logic_dropokay <- function(d, okay, id, varcol = "__var") {
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
