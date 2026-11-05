# ==============================================================================
#  TITLE   : 6_checkbc.R
#  PURPOSE : Back check duplicates, tracking and comparison
# ==============================================================================

#  import prepped back check dataset -------------------------------------------------
bc <- hfc_load(gl$preppedbc)

#  resolve back check duplicates (corrections by key) --------------------------------
if (gl$run_corrections) {
  if (gl$cr_dupsheetbc %in% readxl::excel_sheets(gl$corrfile)) {
    bc <- hfc_corrections(bc, using = gl$corrfile, sheet = gl$cr_dupsheetbc,
                              id = gl$key, logfile = gl$corrlog_output,
                              logsheet = gl$cr_dupslogsheetbc,
                              nolabel = gl$cr_nolabel, ignore = gl$cr_ignore)
  } else {
    message(sprintf("Sheet '%s' not found in %s; skipping back check corrections", gl$cr_dupsheetbc, gl$corrfile))
  }
  hfc_save(bc, gl$checkedbc)
}

#  find back check duplicates ------------------------------------------------------
if (gl$run_ids) {
  bc <- hfc_ids(bc, varname = gl$id, enumerator = gl$bcer, date = gl$date, key = gl$key,
                    outfile = gl$hfc_output, outsheet = "BC id duplicates",
                    keep = gl$id_keepvars, dupfile = gl$id_dupsbc_output,
                    save = gl$checkedbc, nolabel = gl$id_nolabel, force = TRUE,
                    sheetreplace = TRUE)
} else {
  if (!hfc_isid(bc, gl$id)) stop(sprintf("variable %s does not uniquely identify the observations", gl$id))
  hfc_save(bc, gl$checkedbc)
}

#  track back check progress --------------------------------------------------------
if (gl$run_trackbc) {
  hfc_track_survey(surveydata = gl$checkedbc, id = gl$id, date = gl$date, by = gl$bs_tr_by,
                 outcome = gl$bs_tr_outcome, outcome_vals = gl$outc_vals, target = gl$bs_tr_target,
                 masterdata = gl$masterbc, masterid = gl$bs_tr_masterid,
                 trackingdata = gl$trackingbc, keepmaster = gl$bs_tr_keepmaster,
                 keeptracking = gl$bs_tr_keeptracking, keepsurvey = gl$bs_tr_keepsurvey,
                 outfile = gl$trackingbc_output, save = gl$bs_tr_save, nolabel = gl$bs_tr_nolabel,
                 summaryonly = gl$bs_tr_summaryonly, workbooks = gl$bs_tr_workbooks,
                 surveyok = gl$bs_tr_surveyok, replace = TRUE)
}

#  back check comparison ------------------------------------------------------------
if (gl$run_bc) {
  hfc_backcheck(surveydata = gl$checkedsurvey, bcdata = gl$checkedbc, id = gl$id,
             enumerator = gl$enum, backchecker = gl$bcer,
             enumteam = gl$team, bcteam = gl$bcerteam,
             t1vars = gl$bs_t1vars, t2vars = gl$bs_t2vars, t3vars = gl$bs_t3vars,
             ttest = gl$bs_ttest, prtest = gl$bs_prtest, signrank = gl$bs_signrank,
             reliability = gl$bs_reliability, filename = gl$bc_output,
             surveydate = gl$starttime, bcdate = gl$starttime,
             showid = gl$bs_showid, level = gl$bs_level,
             keepsurvey = gl$bs_keepsurvey, keepbc = gl$bs_keepbc,
             okrange = gl$bs_okrange, nodiffnum = gl$bs_nodiffnum, nodiffstr = gl$bs_nodiffstr,
             excludenum = gl$bs_excludenum, excludestr = gl$bs_excludestr,
             excludemissing = gl$bs_excludemissing, lower = gl$bs_lower, upper = gl$bs_upper,
             nosymbol = gl$bs_nosymbol, trim = gl$bs_trim, full = gl$bs_full,
             nolabel = gl$bs_nolabel, replace = TRUE)
}
