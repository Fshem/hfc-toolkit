# =============================================================================
#  hfc_other_specify : collate and list "other specify" values
# =============================================================================

#' Collate and list other-specify values
#'
#' Reads the "other specify" inputs sheet (parent, child, keep). Parent and
#' child cells are Stata varlists that must expand to the same number of
#' variables (paired by position). Writes two sheets:
#'   outsheet1 ("other specify"): every non-missing child value with its
#'     parent value (enumerator, date, keep, id, parent variable, parent label,
#'     parent value, child variable, child label, child value);
#'   outsheet2 ("other specify (choices)"): frequency of each parent choice
#'     (variable, variable type, choice list, value, label, frequency,
#'     percentage) with a thin bottom border after each variable.
#'
#' @param data,using,sheet,outfile,id,enumerator,date,sheetreplace,nolabel as in Stata
#' @param outsheet1,outsheet2 output sheet names
#' @return invisibly list(N_specify, N_vars, parentvarlist, childvarlist,
#'   data = <sheet1 data>, choices = <sheet2 data>). `parentvarlist` and
#'   `childvarlist` are space-separated strings like Stata's r() macros.
#' @examples
#' # r <- hfc_other_specify(survey, "hfc_inputs.xlsm", outfile = "hfc_output.xlsx",
#' #                      id = "hhid", enumerator = "a_enum_id", date = "starttime")
#' # r$childvarlist
hfc_other_specify <- function(data, using, sheet = "other specify", id, enumerator, date, outfile,
                            outsheet1 = "other specify", outsheet2 = "other specify (choices)",
                            sheetreplace = TRUE, nolabel = FALSE) {
  data <- as.data.frame(data)
  sheet <- sheet %||% "other specify"
  outsheet1 <- outsheet1 %||% "other specify"; outsheet2 <- outsheet2 %||% "other specify (choices)"

  inp <- hfc_read_inputs(using, sheet = sheet)
  inp <- hfc_ensure_cols(inp, c("child", "parent", "keep"))
  inp <- inp[!(is.na(inp$child) & is.na(inp$parent)), , drop = FALSE]
  keep <- sort(unique(inp$keep[!is.na(inp$keep)]), method = "radix")
  inp <- inp[, c("child", "parent"), drop = FALSE]
  bad <- is.na(inp$child) | is.na(inp$parent)
  if (any(bad)) {
    message("missing child or parent in input sheet")
    print(data.frame(inp[bad, ], row = which(bad) + 1))
    hfc_stop("missing child or parent")
  }
  keep <- if (length(keep)) hfc_vars(data, keep, unique = FALSE) else character(0)

  unab_child <- character(0); unab_parent <- character(0)
  for (i in seq_len(nrow(inp))) {
    child <- hfc_vars(data, inp$child[i], unique = FALSE)
    parent <- hfc_vars(data, inp$parent[i], unique = FALSE)
    if (length(child) != length(parent)) {
      hfc_stop("number of vars specified in child (%s) does not does not match the number of vars specified in parent (%s) on row %d",
               paste(child, collapse = " "), paste(parent, collapse = " "), i + 1)
    }
    unab_child <- c(unab_child, child); unab_parent <- c(unab_parent, parent)
  }

  choices <- list()
  meta <- list()
  long <- NULL
  base <- data[, unique(c(id, enumerator, date, keep)), drop = FALSE]
  varswithosp_cnt <- 0L

  for (i in seq_along(unab_child)) {
    p_var <- unab_parent[i]; c_var <- unab_child[i]
    p <- data[[p_var]]; cc <- data[[c_var]]
    if (!any(!hfc_missing(cc))) next
    varswithosp_cnt <- varswithosp_cnt + 1L

    p_is_str <- is.character(p)
    # levelsof p_var, clean ; list uniq (words)
    vals <- if (p_is_str) {
      lv <- sort(unique(p[!hfc_missing(p)]), method = "radix")
      unique(unlist(strsplit(paste(lv, collapse = " "), "\\s+")))
    } else {
      .hfc_other_specify_numstr(sort(unique(hfc_num(p)[!is.na(hfc_num(p))])))
    }
    vals <- vals[vals != ""]

    labs <- if (!p_is_str) attr(p, "labels", exact = TRUE) else NULL
    vallab <- if (!is.null(labs)) .hfc_other_specify_vallabname(p, p_var) else ""
    list_vals <- character(0)
    if (!is.null(labs)) {
      lv <- unclass(labs); lvn <- as.numeric(lv[!is.na(lv)])
      list_min <- min(lvn); list_max <- max(lvn)
      list_miss <- any(haven::is_tagged_na(as.numeric(lv)))
      if (length(lv) > 2) {
        js <- seq(list_min, list_max)
        list_vals <- .hfc_other_specify_numstr(js[js %in% lvn])
      } else {
        list_vals <- unique(.hfc_other_specify_numstr(c(list_min, list_max)))
      }
      if (list_miss) {
        for (letter in letters) {
          if (any(hfc_is_tag(p, letter))) {
            # NOTE(stata-bug): Stata appends `i' (the pair index), not .`letter'
            list_vals <- c(list_vals, as.character(i))
          }
        }
      }
      list_vals <- unique(c(vals, setdiff(list_vals, vals)))
    }
    if (!length(list_vals)) list_vals <- vals

    pn <- hfc_num(p)
    nm_cnt <- sum(!hfc_missing(p))
    for (val in list_vals) {
      if (p_is_str) {
        hay <- paste0("_", gsub(" ", "_", ifelse(is.na(p), "", p), fixed = TRUE), "_")
        pat <- paste0("_", val, "_")
        val_cnt <- sum(tryCatch(grepl(pat, hay), error = function(e) grepl(pat, hay, fixed = TRUE)))
        val_lab <- ""
      } else {
        vn <- suppressWarnings(as.numeric(val))
        val_cnt <- sum(!is.na(pn) & !is.na(vn) & pn == vn)
        # `:lab lbl val'` returns the value itself when unlabelled
        val_lab <- if (vallab != "") .hfc_other_specify_ifna(hfc_value_label(p, vn), val) else ""
      }
      choices[[length(choices) + 1]] <- data.frame(
        variable = substr(p_var, 1, 32), vartype = .hfc_other_specify_type(p),
        choice_label = substr(vallab, 1, 32), value = substr(val, 1, 80), label = substr(val_lab, 1, 80),
        frequency = as.numeric(val_cnt), percentage = val_cnt / nm_cnt, stringsAsFactors = FALSE)
    }

    meta[[as.character(i)]] <- list(parent = p_var, parent_label = hfc_varlabel(data, p_var, fallback = FALSE),
                                    child = c_var, child_label = hfc_varlabel(data, c_var, fallback = FALSE))
    pv <- if (p_is_str) p else .hfc_other_specify_string(p)
    cv <- if (is.character(cc)) cc else .hfc_other_specify_string(cc)
    # NOTE(stata-bug): for a numeric child, string(.) = "." is non-missing, so rows
    # with a missing numeric child are kept. Faithful to Stata.
    long <- rbind(long, data.frame(`__row` = seq_len(nrow(data)), index = i, parent_value = pv,
                                   child_value = cv, stringsAsFactors = FALSE, check.names = FALSE))
  }

  out <- NULL; ch <- NULL; N <- 0L; var_cnt <- 0L
  if (!is.null(long)) long <- long[!hfc_missing(long$child_value), , drop = FALSE]
  if (!is.null(long) && nrow(long) > 0) {
    long <- long[order(long$`__row`, long$index), , drop = FALSE]   # reshape long: i() then j()
    m <- meta[as.character(long$index)]
    out <- base[long$`__row`, , drop = FALSE]
    out$parent <- vapply(m, `[[`, "", "parent")
    out$parent_label <- vapply(m, `[[`, "", "parent_label")
    out$parent_value <- long$parent_value
    out$child <- vapply(m, `[[`, "", "child")
    out$child_label <- vapply(m, `[[`, "", "child_label")
    out$child_value <- long$child_value
    o <- order(out$parent, out$child, out$child_value, as.numeric(out[[date]]), method = "radix")
    out <- out[o, , drop = FALSE]
    out[[date]] <- hfc_to_date(out[[date]])

    out <- out[, unique(c(enumerator, date, keep, id, "parent", "parent_label", "parent_value",
                          "child", "child_label", "child_value")), drop = FALSE]
    for (v in names(out)) attr(out[[v]], "label") <- ""
    lbl <- c(parent = "parent variable", parent_label = "parent label", parent_value = "parent value",
             child = "child variable", child_label = "child label", child_value = "child value")
    for (v in names(lbl)) attr(out[[v]], "label") <- lbl[[v]]
    if (length(keep)) out <- hfc_labels(out, keep, nolabel)
    out <- hfc_labels(out, c(id, enumerator), nolabel)
    rownames(out) <- NULL
    hfc_write_sheet(outfile, outsheet1, out, header = "label", replace_sheet = sheetreplace,
                    formats = list(date_d_mon_yy = date))
    N <- nrow(out); var_cnt <- length(unique(out$child))

    # ---- choices sheet ----------------------------------------------------------
    ch <- do.call(rbind, choices)
    ch <- ch[order(ch$variable, ch$value, method = "radix"), , drop = FALSE]
    rownames(ch) <- NULL
    lbl2 <- c(variable = "variable", vartype = "variable type", choice_label = "choice list",
              value = "value", label = "label", frequency = "frequency", percentage = "percentage")
    for (v in names(lbl2)) attr(ch[[v]], "label") <- lbl2[[v]]
    wb <- hfc_write_sheet(outfile, outsheet2, ch, header = "label", replace_sheet = sheetreplace,
                          formats = list(percent_d2 = "percentage"), save = FALSE)
    # iparowline: thin bottom border on the last row of each variable
    last <- which(!duplicated(ch$variable, fromLast = TRUE)) + 1
    openxlsx::addStyle(wb, outsheet2, openxlsx::createStyle(border = "bottom", borderStyle = "thin"),
                       rows = last, cols = seq_len(ncol(ch)), gridExpand = TRUE, stack = TRUE)
    openxlsx::saveWorkbook(wb, outfile, overwrite = TRUE)

    hfc_msg("Found %d total specified values in %d variables.", N, var_cnt)
  } else {
    hfc_msg("Found 0 other specify values.")
  }

  invisible(list(N_specify = N, N_vars = var_cnt,
                 parentvarlist = paste(unab_parent, collapse = " "),
                 childvarlist = paste(unab_child, collapse = " "),
                 data = out, choices = ch))
}

.hfc_other_specify_ifna <- function(a, b) if (is.null(a) || length(a) == 0 || is.na(a)) b else a

# numbers as levelsof prints them
.hfc_other_specify_numstr <- function(x) vapply(x, function(z) trimws(formatC(z, format = "fg", digits = 15)), "", USE.NAMES = FALSE)

# Stata string(x) (%9.0g); extended missings -> ".d"
.hfc_other_specify_string <- function(x) {
  tag <- if (is.double(unclass(x))) haven::na_tag(as.vector(unclass(x))) else rep(NA_character_, length(x))
  v <- hfc_num(x)
  vapply(seq_along(v), function(i) {
    z <- v[i]
    if (is.na(z)) return(if (is.na(tag[i])) "." else paste0(".", tag[i]))
    for (d in 9:1) { s <- trimws(formatC(z, format = "fg", digits = d)); if (nchar(s) <= 9) return(sub("^(-?)0\\.", "\\1.", sub("\\.$", "", s))) }
    s <- formatC(z, format = "e", digits = 2); sub("e([+-])0?([0-9]{2})$", "e\\1\\2", s)
  }, "")
}

# Value label name: haven does not keep Stata's label-set name; use the
# "labels" name attr when present, else the variable name (Stata/SurveyCTO
# convention when names are not available).
.hfc_other_specify_vallabname <- function(x, var) attr(x, "labelname", exact = TRUE) %||% var

# Approximate Stata storage type (haven does not preserve byte/int/long/float)
.hfc_other_specify_type <- function(x) {
  if (is.character(x)) {
    w <- suppressWarnings(as.integer(sub("^%-?([0-9]+)s$", "\\1", attr(x, "format.stata", exact = TRUE) %||% "")))
    if (is.na(w)) w <- max(1L, nchar(x, type = "bytes"), na.rm = TRUE)
    return(if (w > 2045) "strL" else paste0("str", w))
  }
  v <- hfc_num(x); v <- v[!is.na(v)]
  if (!length(v)) return("byte")
  if (all(v == round(v))) {
    if (all(v >= -127 & v <= 100)) return("byte")
    if (all(v >= -32767 & v <= 32740)) return("int")
    if (all(v >= -2147483647 & v <= 2147483620)) return("long")
  }
  "double"
}
