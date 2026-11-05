# ==============================================================================
#  TITLE   : 4_checksurvey.R
#  PURPOSE : Run the high-frequency checks
# ==============================================================================

#  remove existing excel files ----------------------------------------------------
hfc_rm(gl$hfc_output, gl$corrlog_output, gl$id_dups_output, gl$textaudit_output,
       gl$surveydb_output, gl$enumdb_output, gl$timeuse_output, gl$tracking_output, gl$bc_output)

#  import prepped dataset --------------------------------------------------------
survey <- hfc_load(gl$preppedsurvey)

#  resolve survey duplicates (corrections by key) ----------------------------------
if (gl$run_corrections) {
  survey <- hfc_corrections(survey, using = gl$corrfile, sheet = gl$cr_dupsheet,
                                id = gl$key, logfile = gl$corrlog_output,
                                logsheet = gl$cr_dupslogsheet,
                                nolabel = gl$cr_nolabel, ignore = gl$cr_ignore)
}

#  find survey duplicates -----------------------------------------------------------
if (gl$run_ids) {
  survey <- hfc_ids(survey, varname = gl$id, enumerator = gl$enum, date = gl$date, key = gl$key,
                        outfile = gl$hfc_output, outsheet = "id duplicates",
                        keep = gl$id_keepvars, dupfile = gl$id_dups_output,
                        save = gl$checkedsurvey, nolabel = gl$id_nolabel, force = TRUE,
                        sheetreplace = TRUE)
} else {
  if (!hfc_isid(survey, gl$id)) stop(sprintf("variable %s does not uniquely identify the observations", gl$id))
  hfc_save(survey, gl$checkedsurvey)
}

#  other HFC corrections (by survey ID) ---------------------------------------------
if (gl$run_corrections) {
  survey <- hfc_corrections(survey, using = gl$corrfile, sheet = gl$cr_othersheet,
                                id = gl$id, logfile = gl$corrlog_output,
                                logsheet = gl$cr_otherlogsheet,
                                nolabel = gl$cr_nolabel, ignore = gl$cr_ignore)
  hfc_save(survey, gl$checkedsurvey)
}

#  recode other specify --------------------------------------------------------------
if (gl$run_specifyrecode) {
  survey <- hfc_specify_recode(survey, using = gl$recodefile, sheet = gl$rc_sheet,
                                  id = gl$id, keep = gl$rc_keepvars, logfile = gl$recodelog_output,
                                  logsheet = gl$rc_logsheet, sheetreplace = TRUE,
                                  nolabel = gl$rc_nolabel)
  hfc_save(survey, gl$checkedsurvey)
}

#  form versions -----------------------------------------------------------------------
if (gl$run_version) {
  hfc_versions(survey, varname = gl$formversion, enumerator = gl$enum, date = gl$date,
                   outfile = gl$hfc_output, outsheet1 = "form versions", outsheet2 = "outdated",
                   keep = gl$vs_keepvars, sheetreplace = TRUE, nolabel = gl$vs_nolabel)
}

#  variable duplicates -------------------------------------------------------------------
if (gl$run_dups) {
  hfc_duplicates(survey, varlist = gl$dp_vars, id = gl$id, enumerator = gl$enum, date = gl$date,
               outfile = gl$hfc_output, outsheet = "duplicates", keep = gl$dp_keepvars,
               nolabel = gl$dp_nolabel, sheetreplace = TRUE)
}

#  variable missingness ---------------------------------------------------------------------
if (gl$run_missing) {
  hfc_missingness(survey, varlist = gl$ms_vars, priority = gl$ms_pr_vars, show = gl$ms_show,
                  outfile = gl$hfc_output, outsheet = "missing", sheetreplace = TRUE)
}

#  outliers --------------------------------------------------------------------------------
if (gl$run_outliers) {
  hfc_outliers(survey, using = gl$inputfile, id = gl$id, enumerator = gl$enum, date = gl$date,
                   sheet = "outliers", outfile = gl$hfc_output, outsheet = "outliers",
                   nolabel = gl$ol_nolabel, sheetreplace = TRUE)
}

#  constraints --------------------------------------------------------------------------------
if (gl$run_constraints) {
  hfc_constraints(survey, using = gl$inputfile, id = gl$id, enumerator = gl$enum, date = gl$date,
                      sheet = "constraints", outfile = gl$hfc_output, outsheet = "constraints",
                      nolabel = gl$ct_nolabel, sheetreplace = TRUE)
}

#  logic ------------------------------------------------------------------------------------------
if (gl$run_logic) {
  hfc_logic(survey, using = gl$inputfile, id = gl$id, enumerator = gl$enum, date = gl$date,
                sheet = "logic", outfile = gl$hfc_output, outsheet = "logic",
                nolabel = gl$cl_nolabel, sheetreplace = TRUE)
}

#  other specify -------------------------------------------------------------------------------------
childvars <- character(0)
if (gl$run_specify) {
  res <- hfc_other_specify(survey, using = gl$inputfile, id = gl$id, enumerator = gl$enum, date = gl$date,
                         sheet = "other specify", outfile = gl$hfc_output,
                         outsheet1 = "other specify", outsheet2 = "other specify (choices)",
                         nolabel = gl$os_nolabel, sheetreplace = TRUE)
  childvars <- res$childvarlist
}

#  field comments -----------------------------------------------------------------------------------
if (gl$run_comments) {
  hfc_collate_scto("comments", data = survey, var = gl$fieldcomments, folder = gl$media_folder,
                 save = gl$commentsdata)
  hfc_comments(survey, var = gl$fieldcomments, enumerator = gl$enum,
                   commentsdata = gl$commentsdata, outfile = gl$hfc_output,
                   outsheet = "field comments", keepvars = gl$cm_keepvars,
                   nolabel = gl$cm_nolabel, sheetreplace = TRUE)
}

#  text audit & time use --------------------------------------------------------------------------
if (gl$run_textaudit || gl$run_timeuse) {
  hfc_collate_scto("textaudit", data = survey, var = gl$textaudit, folder = gl$media_folder,
                 save = gl$textauditdata)
}
if (gl$run_textaudit) {
  hfc_text_audit(survey, varname = gl$textaudit, enumerator = gl$enum,
                    textauditdata = gl$textauditdata, outfile = gl$textaudit_output,
                    stats = gl$ta_stats, nolabel = gl$ta_nolabel, sheetreplace = TRUE)
}
if (gl$run_timeuse) {
  hfc_time_use(survey, varname = gl$textaudit, enumerator = gl$enum, starttime = gl$starttime,
                  textauditdata = gl$textauditdata, outfile = gl$timeuse_output,
                  nolabel = gl$tu_nolabel, sheetreplace = TRUE)
}

#  survey dashboard -----------------------------------------------------------------------------------
if (gl$run_surveydb) {
  hfc_survey_dashboard(survey, by = gl$sv_by, enumerator = gl$enum, date = gl$date,
                   period = gl$sv_period, consent = gl$consent, cons_vals = gl$cons_vals,
                   dontknow_num = ".d", dontknow_str = gl$dk_str,
                   refuse_num = ".r", refuse_str = gl$ref_str,
                   otherspecify = childvars, duration = gl$duration,
                   formversion = gl$formversion, outfile = gl$surveydb_output,
                   nolabel = gl$sv_nolabel, sheetreplace = TRUE)
}

#  enumerator dashboard --------------------------------------------------------------------------------
if (gl$run_enumdb) {
  hfc_enum_dashboard(survey, using = gl$inputfile, sheetname = "enumstats", enumerator = gl$enum,
                 team = gl$team, date = gl$date, period = gl$en_period,
                 consent = gl$consent, cons_vals = gl$cons_vals,
                 dontknow_num = ".d", dontknow_str = gl$dk_str,
                 refuse_num = ".r", refuse_str = gl$ref_str,
                 otherspecify = childvars, duration = gl$duration,
                 formversion = gl$formversion, outfile = gl$enumdb_output,
                 nolabel = gl$en_nolabel, sheetreplace = TRUE)
}

#  track survey progress -----------------------------------------------------------------------------
if (gl$run_tracksurvey) {
  hfc_track_survey(surveydata = gl$checkedsurvey, id = gl$id, date = gl$date, by = gl$tr_by,
                 outcome = gl$tr_outcome, outcome_vals = gl$outc_vals, target = gl$tr_target,
                 masterdata = gl$mastersurvey, masterid = gl$tr_masterid,
                 trackingdata = gl$trackingsurvey, keepmaster = gl$tr_keepmaster,
                 keeptracking = gl$tr_keeptracking, keepsurvey = gl$tr_keepsurvey,
                 outfile = gl$tracking_output, save = gl$tr_save, nolabel = gl$tr_nolabel,
                 summaryonly = gl$tr_summaryonly, workbooks = gl$tr_workbooks,
                 surveyok = gl$tr_surveyok, replace = TRUE)
}
