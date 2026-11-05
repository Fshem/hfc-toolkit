# =============================================================================
#  hfc_specify_recode : recode other-specify values
# =============================================================================

#' Stata string(n) / strofreal(n): %9.0g display of a number ("." / ".d" for missing)
.hfc_specify_recode_strofreal <- function(x) {
  xv <- as.vector(unclass(x)); attributes(xv) <- NULL
  xv <- suppressWarnings(as.numeric(xv))
  out <- character(length(xv))
  tag <- haven::na_tag(xv)
  na <- is.na(xv)
  out[na] <- ifelse(is.na(tag[na]), ".", paste0(".", tag[na]))
  if (any(!na)) {
    v <- xv[!na]
    s <- sprintf("%.7g", v)
    e <- grepl("e", s)
    s[e] <- sprintf("%.2e", v[e])
    s <- sub("^(-?)0\\.", "\\1.", s)
    out[!na] <- s
  }
  out
}

#' Stata decode: value label text, "" when the value has no label
.hfc_specify_recode_decode <- function(x) {
  labs <- attr(x, "labels", exact = TRUE)
  if (is.null(labs)) return(rep("", length(x)))
  v <- hfc_num(x)
  y <- names(labs)[match(v, unname(hfc_num(labs)))]
  y[is.na(y)] <- ""
  y
}

#' Read the recode inputs (import excel ..., allstr firstrow | csv | dta)
.hfc_specify_recode_import <- function(file, sheet = NULL) {
  ext <- tolower(tools::file_ext(file))
  if (!file.exists(file)) hfc_stop("file %s not found", file)
  if (ext %in% c("xlsx", "xls", "xlsm")) {
    if (is.null(sheet) || !nzchar(sheet)) hfc_stop("sheet option required with .xlsx, .xls or .xlsm files")
    df <- as.data.frame(suppressMessages(readxl::read_excel(file, sheet = sheet, col_types = "text",
                                                            .name_repair = "minimal")), stringsAsFactors = FALSE)
  } else if (ext == "csv") {
    df <- utils::read.csv(file, colClasses = "character", check.names = FALSE, na.strings = character(0))
    names(df) <- tolower(names(df))
  } else {
    df <- as.data.frame(haven::read_dta(file))
    df[] <- lapply(df, function(z) if (is.character(z)) z else .hfc_specify_recode_strofreal(z))
  }
  names(df) <- gsub("[^A-Za-z0-9_]", "_", trimws(names(df)))
  df[] <- lapply(df, function(z) { z <- as.character(z); z[is.na(z)] <- ""; z })
  df
}

#' Recode other-specify responses into their parent variables
#'
#' Port of Stata `hfc_specify_recode`. Each row of the inputs sheet gives
#' parent (varlist), child (varlist), match_type ("exact", "contains",
#' "begins with", "ends with"), match_text, recode_from, recode_to and optional
#' new_label. For every parent/child pair, observations whose child value
#' matches have the parent recoded from `recode_from` to `recode_to` (numeric
#' parent: `recode`; string/select_multiple parent: first occurrence of the
#' space-separated token), and the child value is cleared.
#'
#' @param data      data.frame
#' @param using     inputs file (.xlsx/.xlsm/.xls, .csv or .dta)
#' @param sheet     sheet name (Excel)
#' @param id        id variable
#' @param keep      extra variables to show in the log (Stata varlist string)
#' @param logfile   optional .xlsx log file
#' @param logsheet  log sheet name (required with logfile)
#' @param sheetmodify,sheetreplace  export behaviour (sheet is always replaced in R)
#' @param nolabel   export values instead of value labels for id/keep vars
#' @return recoded data.frame with attr(, "r") = list(N_recoded =)
#' @examples
#' df <- hfc_specify_recode(df, "specifyrecode.xlsm", sheet = "other specify recode",
#'         id = "hhid", logfile = "specify_recode.xlsx", logsheet = "other specify recode",
#'         sheetreplace = TRUE)
hfc_specify_recode <- function(data, using, sheet = NULL, id, keep = NULL, logfile = NULL,
                                  logsheet = NULL, sheetmodify = FALSE, sheetreplace = TRUE,
                                  nolabel = FALSE) {
  data <- as.data.frame(data, stringsAsFactors = FALSE)
  id <- hfc_vars(data, id)
  rc <- .hfc_specify_recode_import(using, sheet)

  if (!is.null(logfile) && nzchar(logfile) && (is.null(logsheet) || !nzchar(logsheet)))
    hfc_stop("option logsheet required with option logfile")
  for (v in c("parent", "child")) if (!(v %in% names(rc))) hfc_stop("variable %s not found", v)

  rc <- rc[rc$parent != "" & rc$child != "", , drop = FALSE]
  recode_cnt <- nrow(rc)
  recoded <- 0L
  logs <- list()

  if (recode_cnt > 0) {
    for (v in c("parent", "child", "match_type", "match_text", "recode_from")) {
      if (!(v %in% names(rc))) hfc_stop("variable %s not found", v)
      if (any(rc[[v]] == "")) {
        print(rc[rc[[v]] == "", , drop = FALSE])
        hfc_stop("%s should never be missing", v)
      }
    }
    # NOTE(stata-bug): help says match_type defaults to "exact" but the ado errors when it is
    # missing (checked above, faithfully). recode_to is not validated.
    if (!("new_label" %in% names(rc))) hfc_stop("variable new_label not found")
    if (!("recode_to" %in% names(rc))) hfc_stop("variable recode_to not found")

    bw <- rc$match_type == "begins with"; rc$match_text[bw] <- paste0("^", rc$match_text[bw])
    ew <- rc$match_type == "ends with";   rc$match_text[ew] <- paste0(rc$match_text[ew], "$")

    keepv <- if (!is.null(keep) && length(hfc_tokens(keep))) hfc_vars(data, keep) else character(0)

    for (i in seq_len(recode_cnt)) {
      pvars <- hfc_vars(data, rc$parent[i])
      cvars <- hfc_vars(data, rc$child[i])
      vfrom <- rc$recode_from[i]; vto <- rc$recode_to[i]
      mtype <- rc$match_type[i];  mtext <- rc$match_text[i]; nlab <- rc$new_label[i]

      if (length(pvars) != length(cvars)) {
        message(sprintf("parent vars: %s", paste(pvars, collapse = " ")))
        message(sprintf("child vars: %s", paste(cvars, collapse = " ")))
        hfc_stop("number of vars specified in parent does not match the number of vars specified in child on row %d", i + 1)
      }

      for (j in seq_along(pvars)) {
        pvar <- pvars[j]; cvar <- cvars[j]
        cx <- data[[cvar]]
        if (!sum(!hfc_missing(cx))) next
        cxs <- if (is.character(cx)) { z <- cx; z[is.na(z)] <- ""; z } else .hfc_specify_recode_strofreal(cx)
        cond <- if (mtype == "exact") cxs == mtext else grepl(mtext, cxs)
        cond[is.na(cond)] <- FALSE

        px <- data[[pvar]]
        plab_var <- attr(px, "label", exact = TRUE) %||% ""
        if (is.character(px)) {
          oldval <- px; oldval[is.na(oldval)] <- ""
          p <- gsub(" +", " ", trimws(oldval))
          p <- paste0("//", gsub(" ", "//", p, fixed = TRUE), "//")
          p[cond] <- sub(paste0("//", vfrom, "//"), paste0("//", vto, "//"), p[cond], fixed = TRUE)
          # NOTE(stata-bug): "//" -> " " leaves a leading and trailing space on EVERY value of the
          # parent (and "" becomes "  "), not only on recoded observations. Kept faithfully.
          p <- gsub("//", " ", p, fixed = TRUE)
          attr(p, "label") <- attr(px, "label", exact = TRUE)
          data[[pvar]] <- p
          newval <- p; attr(newval, "label") <- NULL
          oldlab <- rep("", nrow(data)); newlab <- rep("", nrow(data))
        } else {
          haslab <- !is.null(attr(px, "labels", exact = TRUE))
          oldlab <- if (haslab) .hfc_specify_recode_decode(px) else rep("", nrow(data))
          oldval <- .hfc_specify_recode_strofreal(px)
          from <- suppressWarnings(as.numeric(hfc_tokens(vfrom)))
          to <- suppressWarnings(as.numeric(vto))
          att <- attributes(px); y <- unclass(px); attributes(y) <- NULL
          y <- as.double(y)
          hit <- cond & !is.na(y) & y %in% from
          y[hit] <- to
          if (nlab != "" && haslab) {
            labs <- att$labels
            labs <- labs[unname(hfc_num(labs)) != to]
            labs <- c(labs, structure(to, names = nlab))
            att$labels <- labs[order(unname(hfc_num(labs)))]
          }
          attributes(y) <- att
          data[[pvar]] <- y
          newlab <- if (haslab) .hfc_specify_recode_decode(y) else rep("", nrow(data))
          newval <- .hfc_specify_recode_strofreal(y)
        }

        if (any(cond)) {
          rows <- which(cond)
          lg <- data[rows, unique(c(id, keepv)), drop = FALSE]
          lg$recode_from <- oldval[rows]; lg$recode_from_lab <- oldlab[rows]
          lg$recode_to <- newval[rows];   lg$recode_to_lab <- newlab[rows]
          lg$child_value <- cxs[rows]
          lg$parent <- pvar; lg$parent_lab <- plab_var
          lg$child <- cvar;  lg$child_lab <- attr(cx, "label", exact = TRUE) %||% ""
          # replace cvar = "" if modified
          if (is.character(cx)) { cx[rows] <- ""; data[[cvar]] <- cx }
          else { att <- attributes(cx); z <- unclass(cx); z[rows] <- NA; attributes(z) <- att; data[[cvar]] <- z }
          # append using recodelog -> newest block first
          logs <- c(list(lg), logs)
          recoded <- sum(vapply(logs, nrow, integer(1)))
        }
      }
    }

    if (!is.null(logfile) && nzchar(logfile) && recoded > 0) {
      log <- do.call(rbind, lapply(logs, function(z) {
        for (v in names(z)) {
          x <- z[[v]]
          if (inherits(x, "haven_labelled")) x <- hfc_num(x) else attr(x, "label") <- NULL
          z[[v]] <- x
        }
        z
      }))
      # restore value labels for id/keep (rbind drops attributes)
      for (v in unique(c(id, keepv))) {
        if (inherits(data[[v]], "haven_labelled")) log[[v]] <- haven::labelled(hfc_num(log[[v]]), attr(data[[v]], "labels"))
        if (hfc_is_datetime(data[[v]]) || hfc_is_date(data[[v]])) class(log[[v]]) <- class(data[[v]])
      }
      ord <- c(unique(c(id, keepv)), "parent", "parent_lab", "child", "child_lab", "child_value",
               "recode_from", "recode_from_lab", "recode_to", "recode_to_lab")
      log <- log[, ord, drop = FALSE]
      for (v in c(unique(c(id, keepv)), "child_value")) attr(log[[v]], "label") <- v
      for (v in c("parent", "child", "recode_from", "recode_to")) attr(log[[paste0(v, "_lab")]], "label") <- paste(v, "label")
      # NOTE(stata-bug): ipalabels (nolabel handling) is only applied when keep() is specified
      if (length(keepv)) log <- hfc_labels(log, unique(c(id, keepv)), nolabel = nolabel)
      log <- hfc_labels(log, names(log))  # export excel default exports value labels
      hfc_write_sheet(logfile, logsheet, log, header = "label")
    }
  }

  # NOTE(stata-bug): in Stata `recoded` is undefined (empty return) when rows exist but nothing
  # matched; we return 0.
  attr(data, "r") <- list(N_recoded = recoded)
  data
}
