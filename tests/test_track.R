# Test hfc_track_survey() on the exercise data
pkg  <- Sys.getenv("HFC_TOOLKIT", ".")   # run from the repository root
source(file.path(pkg, "R/00_utils.R"))
source(file.path(pkg, "R/hfc_track_survey.R"))
dat <- file.path(pkg, "inst/data")
out <- file.path(pkg, "tests/out/track")
unlink(out, recursive = TRUE); dir.create(out, recursive = TRUE)

survey <- as.data.frame(haven::read_dta(file.path(dat, "household_survey.dta")))
survey <- hfc_recode_to_tag(survey, "-999 999 .999", "d")
survey <- hfc_recode_to_tag(survey, "-888 888 .888", "r")
survey <- .hfc_track_survey_rows(survey, !duplicated(survey$hhid))   # duplicates drop hhid, force (keeps labels)
cat("survey rows:", nrow(survey), "\n")
preloads <- file.path(dat, "household_preloads.xlsx")
targets  <- file.path(dat, "respondent_targets.xlsx")

show <- function(f, sheet, n = 6) {
  cat("\n---", basename(f), "/", sheet, "---\n")
  x <- readxl::read_excel(f, sheet = sheet)
  print(utils::head(as.data.frame(x), n)); if (nrow(x) > n) print(utils::tail(as.data.frame(x), 2))
  invisible(x)
}

# 1. master mode, by district, summaryonly ---------------------------------------
f1 <- file.path(out, "track_master_summary.xlsx")
r1 <- hfc_track_survey(survey, id = "hhid", date = "starttime", by = "a_district",
                     masterdata = preloads, outcome = "c_consent, 1", keepsurvey = "a_enum_name",
                     outfile = f1, summaryonly = TRUE, surveyok = TRUE,
                     save = file.path(out, "track_status.dta"))
cat("sheets:", readxl::excel_sheets(f1), "\n"); str(r1[1:5])
show(f1, "summary"); show(f1, "Unmatched IDs from Survey")
sv <- haven::read_dta(file.path(out, "track_status.dta")); print(table(sv[["_survey_status"]]))
stopifnot(r1$N_target == 923, r1$N_unmatched == 3, r1$N_submitted == 913)

# 2. master mode, summaryonly = FALSE -> one sheet per district ------------------
f2 <- file.path(out, "track_master_sheets.xlsx")
r2 <- hfc_track_survey(surveydata = survey, id = "hhid", date = "starttime", by = "a_district",
                     masterdata = preloads, keepmaster = "a_kg hhh_fullname",
                     keepsurvey = "a_enum_name c_consent", outfile = f2, summaryonly = FALSE,
                     surveyok = TRUE)
cat("sheets:", readxl::excel_sheets(f2), "\n")
show(f2, "summary"); x <- show(f2, "NORTH COCA"); print(table(x$status))

# 3. master mode, workbooks = TRUE, by = a_kg (80 groups > 30) ---------------------
f3 <- file.path(out, "track_wb.xlsx")
r3 <- hfc_track_survey(survey, id = "hhid", date = "submissiondate", by = "a_kg",
                     masterdata = preloads, keepmaster = "a_district", outfile = f3,
                     workbooks = TRUE, surveyok = TRUE, nolabel = TRUE)
wbs <- list.files(out, pattern = "^track_wb_.*\\.xlsx$")
cat("workbooks created:", length(wbs), "\n"); stopifnot(length(wbs) == 80)
show(file.path(out, wbs[1]), "status")
# > 30 groups without workbooks must error
e <- tryCatch(hfc_track_survey(survey, id = "hhid", date = "starttime", by = "a_kg",
                             masterdata = preloads, outfile = file.path(out, "err.xlsx"), surveyok = TRUE),
              error = function(e) conditionMessage(e))
cat("expected error:", e, "\n")

# 3b. numeric by (a_district_id), workbooks, masterdata as data.frame -------------
sv_num <- survey; sv_num$a_district_id <- as.numeric(sv_num$a_district_id)
f3b <- file.path(out, "track_num.xlsx")
hfc_track_survey(sv_num, id = "hhid", date = "starttime", by = "a_district_id",
               masterdata = as.data.frame(readxl::read_excel(preloads)), outfile = f3b,
               workbooks = TRUE, surveyok = TRUE)
print(list.files(out, pattern = "^track_num"))
show(file.path(out, "track_num_1.xlsx"), "status", 3)

# 4. tracking mode with respondent_targets.xlsx ----------------------------------
f4 <- file.path(out, "track_targets.xlsx")
r4 <- hfc_track_survey(survey, id = "hhid", date = "starttime", by = "a_kg", target = "hh_count",
                     trackingdata = targets, keeptracking = "district", outcome = "c_consent, 1",
                     outfile = f4)
cat("sheets:", readxl::excel_sheets(f4), "\n"); str(r4[1:5])
show(f4, "survey progress")
stopifnot(r4$N_groups == 80, r4$N_submitted == nrow(survey))

# 4b. tracking mode, surveyok + keepsurvey with an unmatched group ------------------
sv2 <- survey; sv2$a_kg[1:2] <- "UNKNOWN SCHOOL"
f4b <- file.path(out, "track_targets_ok.xlsx")
r4b <- hfc_track_survey(sv2, id = "hhid", date = "starttime", by = "a_kg", target = "hh_count",
                      trackingdata = targets, keepsurvey = "a_enum_name", outfile = f4b, surveyok = TRUE)
cat("sheets:", readxl::excel_sheets(f4b), " unmatched:", r4b$N_unmatched, "\n")
show(f4b, "Unmatched IDs from Survey")
e <- tryCatch(hfc_track_survey(sv2, date = "starttime", by = "a_kg", target = "hh_count",
                             trackingdata = targets, outfile = file.path(out, "err2.xlsx")),
              error = function(e) conditionMessage(e))
cat("expected error:", e, "\n")

# formats check
wb <- openxlsx::loadWorkbook(f4)
cat("\nstyles on survey progress:", length(wb$styleObjects), "\n")
cat("ALL TRACK TESTS PASSED\n")
