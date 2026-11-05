# ==============================================================================
#  TITLE   : 3_prepsurvey.R
#  PURPOSE : Prepare survey data for HFCs
# ==============================================================================

#  import data ----------------------------------------------------------------
survey <- hfc_load(gl$rawsurvey)

#  recode extended missing values: dk -> .d, refused -> .r  (haven tagged NA)
if (nzchar(gl$dk_num))  survey <- hfc_recode_to_tag(survey, gl$dk_num,  "d")
if (nzchar(gl$ref_num)) survey <- hfc_recode_to_tag(survey, gl$ref_num, "r")

#  drop unwanted variables  (NB: edit as needed) -------------------------------
survey <- survey[, setdiff(names(survey),
                           c("deviceid", "subscriberid", "simid", "devicephonenum", "username")),
                 drop = FALSE]

#  destring numeric variables  (NB: edit as needed) ----------------------------
for (v in hfc_vars(survey, c(gl$duration, gl$enum), strict = FALSE)) {
  if (is.character(survey[[v]])) {
    lab <- attr(survey[[v]], "label")
    survey[[v]] <- suppressWarnings(as.numeric(survey[[v]]))
    attr(survey[[v]], "label") <- lab
  }
}

#  check key variable: no missing values, no duplicates ------------------------
hfc_check_unique(survey, gl$key)

#  generate short key (last 12 characters of the SurveyCTO key) -------------------
k <- survey[[gl$key]]
survey$skey <- substr(k, pmax(nchar(k) - 11, 1), nchar(k))
hfc_check_unique(survey, "skey")

#  check SurveyCTO date variables: no missing, none before 2016  (edit as needed)
for (v in intersect(c("starttime", "endtime", "submissiondate"), names(survey))) {
  nmiss <- sum(is.na(survey[[v]]))
  if (nmiss > 0) stop(sprintf("Variable %s has %d missing values", v, nmiss), call. = FALSE)
  bad <- as.integer(format(survey[[v]], "%Y")) < 2016
  if (any(bad)) {
    print(data.frame(row = which(bad), key = survey[[gl$key]][bad], value = survey[[v]][bad]), row.names = FALSE)
    stop(sprintf("variable %s has dates before 2016. Check that date variables are properly imported", v), call. = FALSE)
  }
}

#  generate date variables from SurveyCTO datetime variables -------------------
survey$startdate <- as.Date(survey$starttime)
survey$enddate   <- as.Date(survey$endtime)
survey$subdate   <- as.Date(survey$submissiondate)

#  drop observations with date before 1 Jan 2016  (edit date as needed) --------
survey <- hfc_rows(survey, survey$startdate > as.Date("2016-01-01"))

#  save data -------------------------------------------------------------------
hfc_save(survey, gl$preppedsurvey)
