# ==============================================================================
#  TITLE   : 1_globals.R
#  PURPOSE : Settings for the high-frequency checks.
#            Every setting is an element of the list `gl`
#            (gl$name). Values below are set for the exercise data;
#            edit them for your project.
# ==============================================================================

gl <- list()
cwd <- gl_cwd   # set in 0_master.R

# ------------------------------------------------------------------------------
#  Run/Turn Off specific checks  (TRUE = run, FALSE = skip)
# ------------------------------------------------------------------------------
gl$run_corrections   <- TRUE   # Apply corrections
gl$run_specifyrecode <- TRUE   # Recode other specify
gl$run_version       <- TRUE   # Check for outdated survey form versions
gl$run_ids           <- TRUE   # Check Survey ID for duplicates
gl$run_dups          <- TRUE   # Check other Survey variables for duplicates
gl$run_missing       <- TRUE   # Check variable missingness
gl$run_outliers      <- TRUE   # Check numeric variables for outliers
gl$run_constraints   <- TRUE   # Check numeric variables for constraint violations
gl$run_logic         <- TRUE   # Check variables for logical violations
gl$run_specify       <- TRUE   # Check for other specify values
gl$run_comments      <- TRUE   # Collate and output field comments
gl$run_textaudit     <- TRUE   # Check Survey duration using text audit data
gl$run_timeuse       <- TRUE   # Check active survey hours using text audit data
gl$run_surveydb      <- TRUE   # Create survey Dashboard
gl$run_enumdb        <- TRUE   # Create enumerator Dashboard
gl$run_tracksurvey   <- TRUE   # Report on survey progress
gl$run_trackbc       <- TRUE   # Report on Back check progress
gl$run_bc            <- TRUE   # Back Check comparison

# ------------------------------------------------------------------------------
#  Input files
#    inputfile   Inputs for outliers, constraints, logic, specify, enumstats
#    corrfile    Inputs for hfc_corrections
#    recodefile  Inputs for hfc_specify_recode
# ------------------------------------------------------------------------------
gl$inputfile  <- file.path(cwd, "3_checks/1_inputs/hfc_inputs.xlsm")
gl$corrfile   <- file.path(cwd, "3_checks/1_inputs/corrections.xlsm")
gl$recodefile <- file.path(cwd, "3_checks/1_inputs/specifyrecode.xlsm")

# ------------------------------------------------------------------------------
#  Datasets  (.dta keeps value labels and .d/.r missing values; .rds also works)
# ------------------------------------------------------------------------------
gl$rawsurvey      <- file.path(cwd, "4_data/2_survey/household_survey.dta")
gl$preppedsurvey  <- file.path(cwd, "4_data/2_survey/household_survey_prepped.dta")
gl$checkedsurvey  <- file.path(cwd, "4_data/2_survey/household_survey_checked.dta")
gl$mastersurvey   <- file.path(cwd, "4_data/2_survey/household_preloads.xlsx")
gl$trackingsurvey <- ""

gl$rawbc          <- file.path(cwd, "4_data/3_backcheck/household_backcheck.dta")
gl$preppedbc      <- file.path(cwd, "4_data/3_backcheck/household_backcheck_prepped.dta")
gl$checkedbc      <- file.path(cwd, "4_data/3_backcheck/household_backcheck_checked.dta")
gl$masterbc       <- file.path(cwd, "4_data/3_backcheck/household_backcheck_preloads.xlsx")
gl$trackingbc     <- ""

gl$commentsdata   <- file.path(cwd, "4_data/2_survey/comments_data.dta")
gl$textauditdata  <- file.path(cwd, "4_data/2_survey/textaudit_data.dta")

# ------------------------------------------------------------------------------
#  SurveyCTO media folder (text audit / comments files)
# ------------------------------------------------------------------------------
gl$media_folder <- file.path(cwd, "4_data/2_survey/media")

# ------------------------------------------------------------------------------
#  Output files  (one dated folder per run, like the Stata template)
# ------------------------------------------------------------------------------
gl$folder_date <- format(Sys.Date(), "%Y-%m-%d")
out <- file.path(cwd, "3_checks/2_outputs", gl$folder_date)
dir.create(out, showWarnings = FALSE, recursive = TRUE)

gl$id_dups_output    <- file.path(out, "survey_duplicates.dta")
gl$corrlog_output    <- file.path(out, "correction_log.xlsx")
gl$recodelog_output  <- file.path(out, "specify_recode.xlsx")
gl$hfc_output        <- file.path(out, "hfc_output.xlsx")
gl$textaudit_output  <- file.path(out, "textaudit.xlsx")
gl$timeuse_output    <- file.path(out, "timeuse.xlsx")
gl$surveydb_output   <- file.path(out, "surveydb.xlsx")
gl$enumdb_output     <- file.path(out, "enumdb.xlsx")
gl$tracking_output   <- file.path(out, "tracking.xlsx")
gl$id_dupsbc_output  <- file.path(out, "bc_duplicates.dta")
gl$trackingbc_output <- file.path(out, "bc_tracking.xlsx")
gl$bc_output         <- file.path(out, "bc_comparison.xlsx")

# ------------------------------------------------------------------------------
#  Admin variables
#    key          Unique SurveyCTO submission key (not the survey ID)
#    id           Survey ID
#    enum         Enumerator variable
#    date         Survey date variable (datetime)
#    team         Enumerator team variable
#    starttime    Survey start time (datetime)
#    duration     Duration (numeric)
#    formversion  Form version (numeric; higher = more recent)
#    fieldcomments/textaudit  SurveyCTO comments / text audit variables
#    keepvars     Extra variables shown in all check outputs
#    consent      Consent variable (numeric) ; outcome  Survey outcome (numeric)
# ------------------------------------------------------------------------------
gl$key           <- "key"
gl$id            <- "hhid"
gl$enum          <- "a_enum_id"
gl$enumteam      <- "a_team_id"
gl$bcer          <- "a_bcer_id"
gl$bcerteam      <- ""            # back-checker team variable (must differ from gl$team)
gl$date          <- "starttime"
gl$team          <- "a_team_id"
gl$starttime     <- "starttime"
gl$duration      <- "duration"
gl$formversion   <- "formdef_version"
gl$fieldcomments <- "field_comments"
gl$textaudit     <- "text_audit"
gl$keepvars      <- "a_district"
gl$consent       <- "c_consent"
gl$outcome       <- ""

# ------------------------------------------------------------------------------
#  Missing values
#    cons_vals  values indicating consent      outc_vals  values = complete
#    dk_num / ref_num  numeric "don't know" / "refused" codes (become .d / .r)
#    dk_str / ref_str  string  "don't know" / "refused" codes
# ------------------------------------------------------------------------------
gl$cons_vals <- "1"
gl$outc_vals <- "1"
gl$dk_num    <- "-999 999 .999"
gl$dk_str    <- "-999"
gl$ref_num   <- "-888 888 .888"
gl$ref_str   <- "-888"

# ------------------------------------------------------------------------------
#  hfc_corrections
# ------------------------------------------------------------------------------
gl$cr_dupsheet       <- "duplicates"            # exercise sheet name; template default "duplicates corrections"
gl$cr_dupslogsheet   <- "duplicates corrections"
gl$cr_othersheet     <- "other issues"          # exercise sheet name; template default "other corrections"
gl$cr_otherlogsheet  <- "other corrections"
gl$cr_dupsheetbc     <- "bc duplicates corrections"
gl$cr_dupslogsheetbc <- "bc duplicates corrections"
gl$cr_nolabel        <- FALSE
gl$cr_ignore         <- TRUE

# ------------------------------------------------------------------------------
#  hfc_specify_recode
# ------------------------------------------------------------------------------
gl$rc_keepvars <- gl$keepvars
gl$rc_sheet    <- "other specify recode"
gl$rc_logsheet <- "other specify recode"
gl$rc_nolabel  <- FALSE

# ------------------------------------------------------------------------------
#  hfc_versions / hfc_ids / hfc_duplicates / hfc_missingness
# ------------------------------------------------------------------------------
gl$vs_keepvars <- gl$keepvars ; gl$vs_nolabel <- FALSE
gl$id_keepvars <- gl$keepvars ; gl$id_nolabel <- FALSE
gl$dp_vars     <- "gpslatitude gpslongitude"   # eg. "gps_* phonenumber"
gl$dp_keepvars <- gl$keepvars ; gl$dp_nolabel <- FALSE
gl$ms_vars     <- "_all"
gl$ms_pr_vars  <- paste(gl$id, gl$key, gl$date, gl$enum)
gl$ms_show     <- "0"

# ------------------------------------------------------------------------------
#  Input-sheet driven checks (set up in gl$inputfile)
# ------------------------------------------------------------------------------
gl$ol_nolabel <- FALSE   # hfc_outliers
gl$ct_nolabel <- FALSE   # hfc_constraints
gl$cl_nolabel <- FALSE   # hfc_logic
gl$os_nolabel <- FALSE   # hfc_other_specify
gl$cm_keepvars <- gl$keepvars ; gl$cm_nolabel <- FALSE   # hfc_comments

# ------------------------------------------------------------------------------
#  Text audit & time use
# ------------------------------------------------------------------------------
gl$ta_stats   <- "count min max mean sd iqr p25 p50 p75"
gl$ta_nolabel <- TRUE
gl$tu_nolabel <- FALSE

# ------------------------------------------------------------------------------
#  Dashboards  (period: daily, weekly, monthly, auto)
# ------------------------------------------------------------------------------
gl$sv_period <- "auto" ; gl$sv_by <- "" ; gl$sv_nolabel <- FALSE
gl$en_period <- "auto" ; gl$en_nolabel <- FALSE

# ------------------------------------------------------------------------------
#  hfc_track_survey (survey)
# ------------------------------------------------------------------------------
gl$tr_by           <- "a_district"
gl$tr_outcome      <- ""            # eg. "c_outcome"; values from gl$outc_vals
gl$tr_target       <- ""
gl$tr_masterid     <- gl$id
gl$tr_keepmaster   <- ""
gl$tr_keeptracking <- ""
gl$tr_keepsurvey   <- ""
gl$tr_summaryonly  <- TRUE
gl$tr_workbooks    <- FALSE
gl$tr_save         <- ""
gl$tr_surveyok     <- TRUE
gl$tr_nolabel      <- FALSE

# ------------------------------------------------------------------------------
#  hfc_track_survey (back checks)
# ------------------------------------------------------------------------------
gl$bs_tr_by           <- "a_district"
gl$bs_tr_outcome      <- ""
gl$bs_tr_target       <- ""
gl$bs_tr_masterid     <- gl$id
gl$bs_tr_keepmaster   <- ""
gl$bs_tr_keeptracking <- ""
gl$bs_tr_keepsurvey   <- ""
gl$bs_tr_summaryonly  <- TRUE
gl$bs_tr_workbooks    <- FALSE
gl$bs_tr_save         <- ""
gl$bs_tr_surveyok     <- TRUE
gl$bs_tr_nolabel      <- FALSE

# ------------------------------------------------------------------------------
#  hfc_backcheck: back check comparison
#    t1vars/t2vars/t3vars  type 1, 2, 3 variables
#    showid         flag IDs with error rate above this ("30%" or a count)
#    ttest/prtest/signrank/reliability  variables for each statistical test
#    okrange        acceptable ranges, eg. "f_hhm_cnt [-1, 1], j_land_size [-5%, 5%]"
# ------------------------------------------------------------------------------
gl$bs_t1vars         <- "a_pl_hhh_gnd e_hhh_relig e_hhh_native"
gl$bs_t2vars         <- "f_hhm_cnt h_rooms h_electricity h_cook_fuel h_toilet"
gl$bs_t3vars         <- "i_radio i_mosquito_net j_land_yn j_land_size m_mon_exp m_mon_inc"
gl$bs_showid         <- "30%"
gl$bs_ttest          <- "j_land_size m_mon_exp"
gl$bs_prtest         <- "j_land_yn"
gl$bs_signrank       <- "m_mon_inc"
gl$bs_level          <- 95
gl$bs_reliability    <- "f_hhm_cnt h_rooms"
gl$bs_keepsurvey     <- "a_district"
gl$bs_keepbc         <- ""
gl$bs_full           <- FALSE
gl$bs_nolabel        <- TRUE
gl$bs_okrange        <- "f_hhm_cnt [-1, 1], m_mon_exp [-10%, 10%]"
gl$bs_nodiffnum      <- ""
gl$bs_nodiffstr      <- ""
gl$bs_excludenum     <- paste(gl$dk_num, gl$ref_num)
gl$bs_excludestr     <- paste(gl$dk_str, gl$ref_str)
gl$bs_excludemissing <- TRUE
gl$bs_lower          <- TRUE
gl$bs_upper          <- FALSE
gl$bs_nosymbol       <- TRUE
gl$bs_trim           <- TRUE
