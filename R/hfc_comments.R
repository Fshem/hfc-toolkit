# =============================================================================
#  hfc_comments : collate & export SurveyCTO field comments
# =============================================================================

#' Collate and export SurveyCTO field comments
#'
#' Merges the survey (1 row per comments file id derived from `var`) with the
#' collated comments dataset produced by hfc_collate_scto("comments", ...) and
#' exports enumerator, keepvars, field (last "/" segment of fieldname) and
#' comment, sorted by enumerator and field.
#'
#' @param data         survey data.frame
#' @param var          SurveyCTO comments media variable (Stata varname)
#' @param enumerator   enumerator variable
#' @param commentsdata collated comments dataset (.dta from hfc_collate_scto)
#' @param outfile      output workbook
#' @param outsheet     output sheet (default "field comments")
#' @param keepvars     additional variables to export (varlist string)
#' @param sheetreplace,nolabel as in Stata
#' @return invisibly list(N_comments, data = <exported rows>)
#' @examples
#' # hfc_collate_scto("comments", survey, "field_comments", "media", "comments_data.dta")
#' # hfc_comments(survey, "field_comments", enumerator = "a_enum_id",
#' #                  commentsdata = "comments_data.dta", outfile = "hfc_output.xlsx")
hfc_comments <- function(data, var, enumerator, commentsdata, outfile, outsheet = "field comments",
                             keepvars = NULL, sheetreplace = TRUE, nolabel = FALSE) {
  data <- as.data.frame(data)
  outsheet <- outsheet %||% "field comments"
  keepvars <- if (length(keepvars) && any(nzchar(keepvars))) hfc_vars(data, keepvars) else character(0)

  d <- data[, unique(c(var, enumerator, keepvars)), drop = FALSE]
  d <- d[!hfc_missing(d[[var]]), , drop = FALSE]
  obs_cnt <- nrow(d)
  out <- NULL; N <- 0L

  if (obs_cnt > 0) {
    x <- as.character(d[[var]])
    if (all(grepl("^https", x))) {
      a <- regexpr("uuid:", x, fixed = TRUE); b <- regexpr("&e=", x, fixed = TRUE)
      x <- paste0("Comments-", .hfc_comments_substr(x, a + 5, b - a - 5))
    } else if (grepl("^media", x[1])) {
      a <- regexpr("media\\", x, fixed = TRUE); b <- regexpr(".csv", x, fixed = TRUE)
      x <- .hfc_comments_substr(x, a + 6, b - a - 6)
    }
    d[[var]] <- x
    if (anyDuplicated(d[[var]])) hfc_stop("variable %s does not uniquely identify the observations", var)

    cm <- as.data.frame(haven::read_dta(commentsdata))
    if (!all(c(var, "fieldname", "comment") %in% names(cm)))
      hfc_stop("%s must contain variables %s, fieldname and comment", commentsdata, var)
    # merge 1:m var using commentsdata, keep(match): master order, then using order
    if ("comment" %in% names(d)) names(d)[names(d) == "comment"] <- "__commentkeep"
    cm <- cm[, c(var, "fieldname", "comment"), drop = FALSE]
    cm$`__u` <- seq_len(nrow(cm))
    d$`__m` <- seq_len(nrow(d))
    m <- merge(d, cm, by = var, sort = FALSE)
    m <- m[order(m$`__m`, m$`__u`), , drop = FALSE]
    if (!nrow(m)) {
      # NOTE(stata-bug): Stata tests obs_cnt (pre-merge count, always > 0 here),
      # so this error never fires there; an empty merge just exports nothing.
    }

    m$`__comment` <- as.character(m$comment)
    m$`__field` <- sub("^.*/", "", as.character(m$fieldname))   # egen ends(), last punct("/")
    for (v in c(enumerator, keepvars)) attributes(m[[v]]) <- attributes(data[[v]])

    out <- m[, unique(c(enumerator, keepvars, "__field", "__comment")), drop = FALSE]
    for (v in names(out)) attr(out[[v]], "label") <- v
    names(out)[match(c("__field", "__comment"), names(out))] <- c("field", "comment")
    attr(out$comment, "label") <- "comment"; attr(out$field, "label") <- "field"

    o <- order(.hfc_comments_sortkey(out[[enumerator]]), out$field, method = "radix")
    out <- out[o, , drop = FALSE]
    out <- out[!hfc_missing(out$comment), , drop = FALSE]

    if (length(keepvars)) out <- hfc_labels(out, keepvars, nolabel)
    out <- hfc_labels(out, enumerator, nolabel)
    rownames(out) <- NULL
    hfc_write_sheet(outfile, outsheet, out, header = "label", replace_sheet = sheetreplace)
    N <- nrow(out)
    hfc_msg("Found %d comments.", N)
  } else {
    hfc_msg("Found 0 Comments in %s.", var)
  }
  invisible(list(N_comments = N, data = out))
}

.hfc_comments_substr <- function(s, n1, n2) {
  n <- nchar(s)
  ifelse(n1 < 1 | n1 > n | n2 <= 0, "", substr(s, n1, n1 + n2 - 1))
}

.hfc_comments_sortkey <- function(x) { y <- hfc_num_or_chr(x); if (is.character(y)) y else { y[is.na(y)] <- Inf; y } }
