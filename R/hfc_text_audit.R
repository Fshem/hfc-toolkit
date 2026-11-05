# =============================================================================
#  hfc_text_audit : import and analyse SurveyCTO text audit data
# =============================================================================

# -----------------------------------------------------------------------------
#  private helpers (prefix .hfc_text_audit_)
# -----------------------------------------------------------------------------

#' Extract the media file id from a SurveyCTO media variable, exactly as the
#' Stata code does:  browser/API links (all obs start with "https") ->
#' "TA_" + uuid between "uuid:" and "&e=";  otherwise, if the FIRST obs starts
#' with "media" (SurveyCTO desktop, "media\TA_xxx.csv") -> text between
#' "media\" and ".csv".  `prefix` is "TA_" (text audits) or "Comments-".
.hfc_text_audit_mediaid <- function(x, prefix = "TA_") {
  if (length(x) && all(grepl("^https", x))) {
    a <- regexpr("uuid:", x, fixed = TRUE)
    b <- regexpr("&e=", x, fixed = TRUE)
    # Stata substr(s, a + 5, b - a - 5); strpos() = 0 when not found
    a0 <- ifelse(a < 0, 0L, a); b0 <- ifelse(b < 0, 0L, b)
    len <- b0 - a0 - 5
    x <- ifelse(len > 0, substr(x, a0 + 5, a0 + 5 + len - 1), "")
    x <- paste0(prefix, x)
  } else if (length(x) && grepl("^media", x[1])) {
    # NOTE(stata-bug): `if regexm(var, "^media")` is the Stata *if command*,
    # so only the first observation is tested. Kept faithfully.
    a <- regexpr("media\\", x, fixed = TRUE)
    b <- regexpr(".csv", x, fixed = TRUE)
    a0 <- ifelse(a < 0, 0L, a); b0 <- ifelse(b < 0, 0L, b)
    len <- b0 - a0 - 6
    x <- ifelse(len > 0, substr(x, a0 + 6, a0 + 6 + len - 1), "")
  }
  x
}

#' Resolve a (possibly abbreviated, Stata varabbrev) variable name in `nms`.
.hfc_text_audit_var <- function(nms, v) {
  if (v %in% nms) return(v)
  m <- nms[startsWith(nms, v)]
  if (length(m) == 1) return(m)
  NA_character_
}

#' Common front end of hfc_text_audit / hfc_time_use: keep non-missing
#' media obs, extract ids, isid, merge 1:m with the collated text audit data
#' (keep(match)).  Returns the merged data.frame sorted by the media id.
.hfc_text_audit_merge <- function(data, varname, keepvars, textauditdata) {
  d <- as.data.frame(data)[, unique(c(varname, keepvars)), drop = FALSE]
  d <- d[!hfc_missing(d[[varname]]), , drop = FALSE]
  obs_cnt <- nrow(d)
  if (obs_cnt == 0) return(NULL)
  d[[varname]] <- .hfc_text_audit_mediaid(as.character(d[[varname]]), "TA_")
  if (!hfc_isid(d, varname)) hfc_stop("variable %s does not uniquely identify the observations", varname)

  ta <- if (is.data.frame(textauditdata)) textauditdata else {
    if (!file.exists(textauditdata)) hfc_stop("file %s not found", textauditdata)
    hfc_load(textauditdata)
  }
  ta <- as.data.frame(ta)
  if (!(varname %in% names(ta))) hfc_stop("variable %s not found in %s", varname, textauditdata)
  # merge 1:m varname using textauditdata, nogen keep(match)
  ta <- ta[, setdiff(names(ta), setdiff(names(d), varname)), drop = FALSE]  # master wins on shared vars
  m <- merge(d, ta, by = varname, all = FALSE, sort = FALSE)
  # Stata merge leaves the result sorted by the key (using order kept within key)
  o <- order(m[[varname]], seq_len(nrow(m)), method = "radix")
  m <- m[o, , drop = FALSE]
  # NOTE(stata-bug): Stata tests `obs_cnt == 0` (count BEFORE the merge) so the
  # "does not match" error can never fire; with no matches Stata later fails
  # in collapse with "no observations". We raise the intended error instead.
  if (nrow(m) == 0)
    hfc_stop("Data in %s does not match current dataset on id variable %s",
             if (is.character(textauditdata)) textauditdata else "textauditdata", varname)
  rownames(m) <- NULL
  m
}

#' groupname = text before the last "/" (missing if no "/");
#' fieldname = text after the last "/".
.hfc_text_audit_split <- function(fieldname) {
  has <- grepl("/", fieldname, fixed = TRUE)
  groupname <- ifelse(has, sub("/[^/]*$", "", fieldname), "")
  fieldname <- sub("^.*/", "", fieldname)
  list(groupname = groupname, fieldname = fieldname)
}

#' collapse (count) (min) (max) (mean) (sd) (iqr) (p25) (p50) (p75) by(by)
.hfc_text_audit_collapse <- function(x, by, byname) {
  keys <- sort(unique(by), method = "radix")
  rows <- lapply(keys, function(k) {
    v <- x[by == k]; v <- v[!is.na(v)]
    data.frame(count = length(v), min = hfc_min(v), max = hfc_max(v), mean = hfc_mean(v),
               sd = hfc_sd(v), iqr = if (length(v)) hfc_iqr(v) else NA_real_,
               p25 = hfc_pctile(v, 25), p50 = hfc_pctile(v, 50), p75 = hfc_pctile(v, 75))
  })
  out <- cbind(setNames(data.frame(keys, stringsAsFactors = FALSE), byname), do.call(rbind, rows))
  out
}

#' collapse (mean) ta_ = duration, by(key enum) + reshape wide ta_, i(enum) j(index)
#' `key` values are indexed in sorted order (frame put + duplicates drop).
.hfc_text_audit_wide <- function(key, enum, duration, data_enum, enumerator, nolabel) {
  keys <- sort(unique(key), method = "radix")
  # enumerator values (keep labelled class for ipalabels)
  ekey <- if (is.character(enum)) enum else hfc_num(enum)
  eu <- sort(unique(ekey), method = "radix", na.last = TRUE)
  out <- data.frame(seq_along(eu))
  ev <- data_enum[match(eu, ekey)]
  out[[1]] <- ev
  names(out) <- enumerator
  attr(out[[enumerator]], "label") <- enumerator     # lab var enum "enum"
  for (i in seq_along(keys)) {
    sel <- key == keys[i]
    mm <- tapply(duration[sel], factor(ekey[sel], levels = unique(ekey[sel])), hfc_mean)
    col <- unname(mm[match(as.character(eu), names(mm))])
    col <- as.numeric(col)
    # Stata renames ta_i to ta_i_000.. so that ipacolwidth sizes the column
    nm <- paste0("ta_", i, "_", strrep("0", max(0, nchar(keys[i]) - nchar(paste0("ta_", i)) - 1)))
    attr(col, "label") <- keys[i]
    out[[nm]] <- col
  }
  out <- hfc_labels(out, enumerator, nolabel = nolabel)
  out
}

#' Import and analyse text audit data
#'
#' Port of Stata `hfc_text_audit`. Merges the survey data with collated
#' SurveyCTO text audit data (as saved by `hfc_collate_scto textaudit`) and
#' exports duration statistics per field, average duration per field by
#' enumerator, statistics per (repeat) group and average duration per group
#' by enumerator.
#'
#' @param data data.frame (survey data in memory)
#' @param varname text audit media variable (e.g. "text_audit")
#' @param enumerator enumerator variable
#' @param textauditdata path to the collated text audit dataset (.dta or .rds),
#'   or a data.frame. Must contain `varname` (file id), `fieldname` and
#'   `totalduration*` (or `devicetime` + `durationms` for newer SurveyCTO).
#' @param outfile output .xlsx file
#' @param stats statistics to export, any of "count mean max min sd iqr p25 p50
#'   p75" ("median" = p50). Default: all. Column order follows Stata's collapse
#'   order (count min max mean sd iqr p25 p50 p75).
#' @param sheetmodify,sheetreplace,replace accepted for compatibility; sheets
#'   are always replaced inside an existing workbook (other sheets kept).
#' @param nolabel export enumerator values instead of value labels
#' @return invisible list: N_obs (submissions with text audit), N_fields,
#'   N_groups and the four output tables.
#' @examples
#' hfc_text_audit(df, "text_audit", enumerator = "a_enum_id",
#'   textauditdata = "textaudit_data.dta", outfile = "textaudit.xlsx",
#'   stats = "count min max mean sd iqr p25 p50 p75", nolabel = TRUE)
hfc_text_audit <- function(data, varname, enumerator, textauditdata, outfile,
                              stats = NULL, sheetmodify = FALSE, sheetreplace = TRUE,
                              replace = FALSE, nolabel = FALSE) {
  varname <- hfc_vars(data, varname); enumerator <- hfc_vars(data, enumerator)
  if (length(varname) != 1) hfc_stop("too many variables specified")
  if (length(enumerator) != 1) hfc_stop("enumerator(): too many variables specified")

  all_stats <- c("count", "mean", "max", "min", "sd", "iqr", "p25", "p50", "p75")
  if (!is.null(stats) && length(hfc_tokens(stats))) {
    stats <- hfc_tokens(gsub("median", "p50", paste(stats, collapse = " "), fixed = TRUE))
    viol <- setdiff(stats, all_stats)
    if (length(viol)) {
      hfc_stop("%s invalid statistic. Allowed stats include %s", paste(viol, collapse = " "),
               sub("p50", "p50 (median)", paste(all_stats, collapse = " "), fixed = TRUE))
    }
  } else stats <- all_stats
  collapse_order <- c("count", "min", "max", "mean", "sd", "iqr", "p25", "p50", "p75")
  keep_stats <- collapse_order[collapse_order %in% stats]
  stat_fmt <- function(s) if (s %in% c("count", "min", "max")) "number_sep" else "number_d2"

  if (replace) hfc_rm(outfile)
  res <- list(N_obs = 0L, N_fields = 0L, N_groups = 0L)

  m <- .hfc_text_audit_merge(data, varname, enumerator, textauditdata)
  if (is.null(m)) return(invisible(res))
  nms <- names(m)
  if ("devicetime" %in% nms) {
    dvar <- .hfc_text_audit_var(nms, "durationms")
  } else {
    dvar <- .hfc_text_audit_var(nms, "totalduration")
  }
  if (is.na(dvar)) hfc_stop("duration variable not found in text audit data")
  if (!("fieldname" %in% nms)) hfc_stop("variable fieldname not found in text audit data")

  sp <- .hfc_text_audit_split(as.character(m$fieldname))
  ta <- data.frame(fieldname = sp$fieldname, groupname = sp$groupname, stringsAsFactors = FALSE)
  ta[[varname]] <- m[[varname]]
  enum_vec <- m[[enumerator]]
  duration <- suppressWarnings(as.numeric(as.character(m[[dvar]])))   # destring
  # drop negative duration: unreliable data caused by time travel in SurveyCTO app
  duration[!is.na(duration) & duration <= 0] <- NA
  ekey <- if (is.character(enum_vec)) enum_vec else hfc_num(enum_vec)
  res$N_obs <- length(unique(ta[[varname]]))

  # *** FIELD STATS ***
  fs <- .hfc_text_audit_collapse(duration, ta$fieldname, "fieldname")
  fs <- fs[, c("fieldname", keep_stats), drop = FALSE]
  fmts <- list(); for (s in keep_stats) fmts[[stat_fmt(s)]] <- c(fmts[[stat_fmt(s)]], s)
  hfc_write_sheet(outfile, "field stats", fs, header = "name", formats = fmts)
  res$N_fields <- nrow(fs)

  # *** FIELD AVERAGE BY ENUMERATOR ***
  fa <- .hfc_text_audit_wide(ta$fieldname, ekey, duration, enum_vec, enumerator, nolabel)
  hfc_write_sheet(outfile, "field average by enumerator", fa, header = "label")
  res$field_stats <- fs; res$field_average <- fa

  # *** GROUP STATS ***
  if (any(ta$groupname != "")) {
    g <- gsub("/", " ", ta$groupname, fixed = TRUE)
    g <- gsub("\\[[0-9]+\\]", " ", g)       # loop of regexr() until no "[" left
    lev <- sort(unique(g[g != ""]), method = "radix")       # levelsof, clean
    groups <- unique(hfc_tokens(paste(lev, collapse = " "))) # list uniq (tokenised)

    gstats <- list(); gavg_key <- character(0); gavg_idx <- integer(0)
    for (grp in groups) {
      sel <- grepl(paste0(grp, "\\["), ta$groupname, perl = TRUE)   # regexm(groupname, "grp\[")
      if (!any(sel)) next
      # NOTE(stata-bug): if the first group has 0 matches Stata appends to a
      # tempfile that does not exist yet (error); here empty groups are skipped.
      gstats[[length(gstats) + 1]] <- .hfc_text_audit_collapse(duration[sel], rep(grp, sum(sel)), "group")
      gavg_key <- c(gavg_key, rep(grp, sum(sel))); gavg_idx <- c(gavg_idx, which(sel))
    }
    if (length(gstats)) {
      gs <- do.call(rbind, rev(gstats))   # Stata appends new results on top
      gs <- gs[order(gs$group, method = "radix"), c("group", keep_stats), drop = FALSE]
      rownames(gs) <- NULL
      hfc_write_sheet(outfile, "group stats", gs, header = "name", formats = fmts)
      res$N_groups <- nrow(gs)

      # *** GROUP AVERAGE BY ENUMERATOR ***
      ga <- .hfc_text_audit_wide(gavg_key, ekey[gavg_idx], duration[gavg_idx], enum_vec[gavg_idx],
                                    enumerator, nolabel)
      tacols <- grep("^ta_", names(ga), value = TRUE)
      hfc_write_sheet(outfile, "group average by enumerator", ga, header = "label",
                      formats = list(number_d2 = tacols))
      res$group_stats <- gs; res$group_average <- ga
    }
  }
  invisible(res)
}
