# =============================================================================
#  hfc_missingness : missingness and distinctness of variables
# =============================================================================

#' Stata float() rounding (single precision)
.hfc_missingness_float <- function(v) readBin(writeBin(as.double(v), raw(), size = 4), "double", n = length(v), size = 4)

#' Check variable missingness
#'
#' Port of Stata `hfc_missingness`. For every variable in `varlist` plus
#' `priority`, reports the number and percent of missing values (Stata
#' missing(): any numeric missing incl. .d/.r, or "" for strings) and the
#' number of distinct non-missing values (blank when all missing). Sorted by
#' priority, then percent missing desc, number missing desc, variable name.
#'
#' @param data      data.frame
#' @param varlist   variables to check (Stata varlist string, e.g. "_all")
#' @param priority  variables flagged "yes" and listed first
#' @param outfile   output .xlsx
#' @param show      threshold: "10%" (min percent missing) or "15" (min number)
#' @param outsheet  sheet (default "missing")
#' @param sheetmodify,sheetreplace  export options (sheet always replaced in R)
#' @return invisible list(N_vars, N_allmiss, N_miss)
#' @examples
#' hfc_missingness(df, "_all", priority = "hhid key starttime a_enum_id",
#'                 outfile = "hfc_output.xlsx", show = "0", sheetreplace = TRUE)
hfc_missingness <- function(data, varlist, priority = NULL, outfile, show = NULL, outsheet = NULL,
                            sheetmodify = FALSE, sheetreplace = TRUE) {
  data <- as.data.frame(data, stringsAsFactors = FALSE)
  show <- if (is.null(show)) "" else trimws(as.character(show))
  show_val <- 0
  if (show != "" && grepl("%", show)) {
    show_val <- suppressWarnings(as.numeric(gsub("%", "", show))) / 100
    if (is.na(show_val) || show_val < 0 || show_val > 1)
      hfc_stop("option show(%s) must be in range 0%% to 100%%. Use appropraite percentage value or use an absolute number. eg. show(10%%) or show(15)", show)
  }
  outsheet <- outsheet %||% "missing"

  prio <- if (!is.null(priority) && length(hfc_tokens(priority))) hfc_vars(data, priority) else character(0)
  vars <- hfc_vars(data, varlist)
  vars <- union(vars, prio)
  n <- nrow(data)

  res <- data.frame(variable = character(0), label = character(0), number_missing = numeric(0),
                    percent_missing = numeric(0), number_unique = numeric(0), important_var = character(0),
                    stringsAsFactors = FALSE)
  for (v in vars) {
    x <- data[[v]]
    miss <- hfc_missing(x)
    k <- if (is.character(x)) x else hfc_num(x)
    uniq <- length(unique(k[!miss]))
    lab <- substr(attr(x, "label", exact = TRUE) %||% "", 1, 80)
    res[nrow(res) + 1, ] <- list(substr(v, 1, 32), lab, sum(miss), sum(miss) / n, uniq,
                                 if (length(prio)) as.character(as.integer(v %in% prio)) else "")
  }

  varscount <- length(vars)
  allmisscount <- sum(res$percent_missing == 1)
  misscount <- sum(res$number_missing > 0)

  if (show != "" && grepl("%", show)) {
    res <- res[!(.hfc_missingness_float(res$percent_missing) < .hfc_missingness_float(show_val)), , drop = FALSE]
  } else if (show != "") {
    res <- res[!(res$number_missing < as.numeric(show)), , drop = FALSE]
  }

  # gsort -important_var -percent_missing -number_missing variable
  # (descending string: "1" before "0" before "")
  o <- order(-xtfrm(res$important_var), -res$percent_missing, -res$number_missing, res$variable, method = "radix")
  res <- res[o, , drop = FALSE]; rownames(res) <- NULL

  res$important_var <- ifelse(res$important_var == "1", "yes", "")
  res$number_unique[res$percent_missing == 1] <- NA

  attr(res$variable, "label") <- "variable"
  attr(res$label, "label") <- "label"
  attr(res$number_missing, "label") <- "# missing"
  attr(res$percent_missing, "label") <- "% missing"
  attr(res$number_unique, "label") <- "# distinct"
  attr(res$important_var, "label") <- "priority?"

  hfc_write_sheet(outfile, outsheet, res, header = "label", formats = list(percent_d2 = "percent_missing"))

  message(sprintf("Found %d of %d variables with all missing values.", allmisscount, varscount))
  message(sprintf("Found %d of %d variables with at least 1 missing values.", misscount, varscount))
  invisible(list(N_vars = varscount, N_allmiss = allmisscount, N_miss = misscount))
}
