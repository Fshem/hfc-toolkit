# =============================================================================
#  hfc_collate_scto : collate SurveyCTO comments / text-audit media files
# =============================================================================

#' Collate SurveyCTO field-comments or text-audit CSV files into one dataset
#'
#' For every non-missing value of the media variable `var`, the matching CSV in
#' `folder` is imported (all columns as text, Stata `import delimited` naming:
#' lower case, invalid characters removed, e.g. "Field name" -> fieldname,
#' "Total duration (seconds)" -> totaldurationseconds), tagged with a column
#' named `var` holding the file id (e.g. "Comments-<uuid>" / "TA_<uuid>"), and
#' appended to the dataset `save` (".dta" is added if missing). Files already
#' present in an existing `save` dataset are skipped.
#'
#' Row/column order mirrors Stata's `append using`: each newly imported file is
#' placed ABOVE the files imported before it; old data from `save` comes last.
#'
#' @param type   "comments" or "textaudit" (Stata sub-command)
#' @param data   survey data.frame
#' @param var    name of the SurveyCTO media variable (e.g. "field_comments",
#'               "text_audit")
#' @param folder folder holding the media CSV files
#' @param save   path of the collated .dta dataset
#' @return invisibly list(N_total, N_successful, N_failed)
#' @examples
#' # hfc_collate_scto("comments", survey, "field_comments",
#' #                folder = "media", save = "comments_data.dta")
hfc_collate_scto <- function(type = c("comments", "textaudit"), data, var, folder, save) {
  type <- match.arg(type)

  if (!dir.exists(folder)) hfc_stop("folder %s not found", folder)
  if (!(var %in% names(data))) hfc_stop("variable %s not found", var)

  # add .dta to save
  if (substring(save, nchar(save) - 3) != ".dta") save <- paste0(save, ".dta")

  x <- data[[var]]
  x <- as.character(x)
  x <- x[!hfc_missing(x)]
  obs_cnt <- length(x)
  fail_cnt <- 0L
  succ_cnt <- 0L  # NOTE(stata-bug): Stata never increments succ_cnt, so r(N_successful) is always 0

  if (obs_cnt > 0) {
    x <- .hfc_collate_scto_mediaid(x, type)

    old <- NULL
    if (file.exists(save)) {
      old <- as.data.frame(haven::read_dta(save))
      if (!(var %in% names(old))) hfc_stop("variable %s not found in %s", var, save)
      oldids <- unique(as.character(old[[var]]))
      done <- x %in% oldids
      olddata_cnt <- sum(done)
      import_cnt <- obs_cnt - olddata_cnt
      hfc_msg("skipping %d files already imported", olddata_cnt)
      x <- x[!done]
    } else {
      import_cnt <- obs_cnt
      olddata_cnt <- 0L
    }

    if (import_cnt > 0) {
      hfc_msg("Importing %d %s files", import_cnt, var)
      acc <- NULL
      for (f in x) {
        path <- file.path(folder, paste0(f, ".csv"))
        if (!file.exists(path)) { fail_cnt <- fail_cnt + 1L; next }
        cur <- .hfc_collate_scto_import(path)
        if (is.null(cur)) { fail_cnt <- fail_cnt + 1L; next }
        cur[[var]] <- rep(f, nrow(cur))
        acc <- .hfc_collate_scto_append(cur, acc)   # append using: new file first
      }

      # NOTE(stata-bug): the Stata messages reference an undefined local `media`
      # (prints "... in ."); the folder is printed here instead.
      if (fail_cnt == import_cnt) {
        message(sprintf("\nNo Comments files in %s.", folder))
        hfc_stop("no media files found")
      } else if (fail_cnt > 0) {
        message(sprintf("\n%d of %d Comments files not found in %s", fail_cnt, obs_cnt, folder))
      }

      if (olddata_cnt > 0) acc <- .hfc_collate_scto_append(acc, old)
      hfc_save(acc, save)
    } else {
      hfc_msg("Imported 0 new files in %s.", var)
    }
  } else {
    hfc_msg("Found 0 files in %s.", var)
  }

  invisible(list(N_total = obs_cnt, N_successful = succ_cnt, N_failed = fail_cnt))
}

# Stata substr(s, n1, n2)
.hfc_collate_scto_substr <- function(s, n1, n2) {
  n <- nchar(s)
  n1 <- ifelse(n1 < 0, n + n1 + 1, n1)
  out <- ifelse(n1 < 1 | n1 > n | n2 <= 0, "", substr(s, n1, n1 + n2 - 1))
  out[is.na(s)] <- ""
  out
}

.hfc_collate_scto_strpos <- function(s, p) { r <- regexpr(p, s, fixed = TRUE); ifelse(r < 0, 0L, as.integer(r)) }

# Extract the media file id from the media variable (browser/API url or desktop path)
.hfc_collate_scto_mediaid <- function(x, type) {
  if (all(grepl("^https", x))) {
    # browser or API
    a <- .hfc_collate_scto_strpos(x, "uuid:"); b <- .hfc_collate_scto_strpos(x, "&e=")
    x <- .hfc_collate_scto_substr(x, a + 5, b - a - 5)
    x <- paste0(if (type == "comments") "Comments-" else "TA_", x)
  } else if (grepl("^media", x[1])) {
    # surveycto desktop. NOTE: Stata tests only the first observation (`if exp`)
    # and expects a backslash separator ("media\...").
    a <- .hfc_collate_scto_strpos(x, "media\\"); b <- .hfc_collate_scto_strpos(x, ".csv")
    x <- .hfc_collate_scto_substr(x, a + 6, b - a - 6)
  }
  x
}

# import delimited ..., stringcols(_all) varnames(1) : all text, Stata names
.hfc_collate_scto_import <- function(path) {
  df <- tryCatch(utils::read.csv(path, colClasses = "character", check.names = FALSE,
                                 na.strings = character(0), encoding = "UTF-8",
                                 stringsAsFactors = FALSE),
                 error = function(e) NULL)
  if (is.null(df)) return(NULL)
  orig <- names(df)
  names(df) <- .hfc_collate_scto_names(orig)
  for (j in seq_along(df)) {
    z <- df[[j]]; z[is.na(z)] <- ""
    # Stata keeps the original header as variable label when the name changed
    if (orig[j] != names(df)[j]) attr(z, "label") <- orig[j]
    df[[j]] <- z
  }
  df
}

# Stata import delimited variable naming (case(lower))
.hfc_collate_scto_names <- function(nms) {
  out <- tolower(nms)
  out <- gsub("[^a-z0-9_]", "", out)
  out <- substr(out, 1, 32)
  bad <- out == "" | grepl("^[0-9]", out) | duplicated(out)
  out[bad] <- paste0("v", which(bad))
  out
}

# Stata `append`: rows of `top` first, then `bottom`; columns of `top` first,
# followed by columns only in `bottom`. Missing cells are "".
.hfc_collate_scto_append <- function(top, bottom) {
  if (is.null(bottom)) return(top)
  if (is.null(top)) return(bottom)
  cols <- c(names(top), setdiff(names(bottom), names(top)))
  fill <- function(d) {
    for (c in setdiff(cols, names(d))) d[[c]] <- rep("", nrow(d))
    d[, cols, drop = FALSE]
  }
  out <- rbind(fill(top), fill(bottom))
  rownames(out) <- NULL
  out
}
