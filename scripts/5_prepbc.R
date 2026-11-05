# ==============================================================================
#  TITLE   : 5_prepbc.R
#  PURPOSE : Prepare back check data for hfc_backcheck and build the
#            back-check master data from the checked survey
# ==============================================================================

#  import data ----------------------------------------------------------------
bc <- hfc_load(gl$rawbc)

#  recode extended missing values: dk -> .d, refused -> .r
if (nzchar(gl$dk_num))  bc <- hfc_recode_to_tag(bc, gl$dk_num,  "d")
if (nzchar(gl$ref_num)) bc <- hfc_recode_to_tag(bc, gl$ref_num, "r")

#  drop unwanted variables  (NB: edit as needed) -------------------------------
bc <- bc[, setdiff(names(bc), c("deviceid", "subscriberid", "simid", "devicephonenum", "username")),
         drop = FALSE]

#  destring numeric variables  (NB: edit as needed) ----------------------------
for (v in hfc_vars(bc, c(gl$duration, gl$bcer), strict = FALSE)) {
  if (is.character(bc[[v]])) {
    lab <- attr(bc[[v]], "label")
    bc[[v]] <- suppressWarnings(as.numeric(bc[[v]]))
    attr(bc[[v]], "label") <- lab
  }
}

#  check key and short key ------------------------------------------------------
hfc_check_unique(bc, gl$key)
k <- bc[[gl$key]]
bc$skey <- substr(k, pmax(nchar(k) - 11, 1), nchar(k))
hfc_check_unique(bc, "skey")

#  check SurveyCTO date variables -------------------------------------------------
for (v in intersect(c("starttime", "endtime", "submissiondate"), names(bc))) {
  nmiss <- sum(is.na(bc[[v]]))
  if (nmiss > 0) stop(sprintf("Variable %s has %d missing values", v, nmiss), call. = FALSE)
  bad <- as.integer(format(bc[[v]], "%Y")) < 2016
  if (any(bad)) {
    print(data.frame(row = which(bad), key = bc[[gl$key]][bad], value = bc[[v]][bad]), row.names = FALSE)
    stop(sprintf("variable %s has dates before 2016. Check that date variables are properly imported", v), call. = FALSE)
  }
}

#  date variables ---------------------------------------------------------------
bc$startdate <- as.Date(bc$starttime)
bc$enddate   <- as.Date(bc$endtime)
bc$subdate   <- as.Date(bc$submissiondate)

#  drop observations with date before 1 Jan 2016  (edit as needed) -------------
bc <- hfc_rows(bc, bc$startdate > as.Date("2016-01-01"))

#  save prepped back check data ------------------------------------------------
hfc_save(bc, gl$preppedbc)

#  generate master data for back checks -----------------------------------------
#  keep observations selected for back checks and the preload variables
checked <- hfc_load(gl$checkedsurvey)
checked <- hfc_rows(checked, !is.na(hfc_num(checked$bc_sel_yn)) & hfc_num(checked$bc_sel_yn) == 1)
mvars <- intersect(unique(c(gl$key, gl$id, "a_district_id", "a_district", "a_kg_id", "a_kg",
                            "a_community", "a_pl_hhh_fn", "a_pl_hhh_gnd", "a_pl_pgv_fn",
                            "a_pl_ch_fn", "a_pl_ch_gnd", "a_pl_ch_age")), names(checked))
master_bc <- checked[, mvars, drop = FALSE]
master_bc <- hfc_labels(master_bc, names(master_bc), nolabel = TRUE)   # export values (first(var))
openxlsx::write.xlsx(as.data.frame(lapply(master_bc, function(x) { attributes(x) <- NULL; x })),
                     gl$masterbc, overwrite = TRUE)
