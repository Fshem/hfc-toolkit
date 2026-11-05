# Test for core1 ports: corrections, ids, specifyrecode, versions, dups, missing
# Runs the 4_checksurvey.do sequence end-to-end on the exercise data.
pkg  <- Sys.getenv("HFC_TOOLKIT", ".")   # run from the repository root
src  <- file.path(pkg, "inst")
source(file.path(pkg, "R/00_utils.R"))
for (f in c("hfc_corrections", "hfc_specify_recode", "hfc_versions",
            "hfc_ids", "hfc_duplicates", "hfc_missingness")) source(file.path(pkg, "R", paste0(f, ".R")))

out <- file.path(pkg, "tests/out/core1"); dir.create(out, recursive = TRUE, showWarnings = FALSE)
hfc_output     <- file.path(out, "hfc_output.xlsx")
corrlog_output <- file.path(out, "correction_log.xlsx")
recodelog      <- file.path(out, "specify_recode.xlsx")
id_dups_output <- file.path(out, "survey_duplicates.dta")
checkedsurvey  <- file.path(out, "household_survey_checked.dta")
hfc_rm(hfc_output, corrlog_output, recodelog, id_dups_output, checkedsurvey)

key <- "key"; id <- "hhid"; enum <- "a_enum_id"; date <- "starttime"; formversion <- "formdef_version"
corrfile   <- file.path(src, "excel/exercise/corrections_exercise.xlsm")

# ---- 3_prepsurvey.do -------------------------------------------------------
d <- as.data.frame(haven::read_dta(file.path(src, "data/household_survey.dta")))
d <- hfc_recode_to_tag(d, "-999 999 .999", "d")
d <- hfc_recode_to_tag(d, "-888 888 .888", "r")
d <- d[, setdiff(names(d), c("deviceid", "subscriberid", "simid", "devicephonenum", "username"))]
for (v in c("duration", enum)) { lab <- attr(d[[v]], "label"); d[[v]] <- as.numeric(d[[v]]); attr(d[[v]], "label") <- lab }
stopifnot(!anyNA(d[[key]]), !anyDuplicated(d[[key]]))
d$skey <- substr(d[[key]], nchar(d[[key]]) - 11, nchar(d[[key]]))
d$startdate <- as.Date(d$starttime); d$enddate <- as.Date(d$endtime); d$subdate <- as.Date(d$submissiondate)
d <- .hfc_corrections_subset(d, d$startdate > as.Date("2016-01-01"))   # keeps labels
cat("prepped:", nrow(d), "obs,", ncol(d), "vars\n")

show <- function(file, sheet, n = 6) {
  x <- as.data.frame(readxl::read_excel(file, sheet = sheet))
  cat(sprintf("\n--- %s [%s] : %d rows x %d cols\n", basename(file), sheet, nrow(x), ncol(x)))
  print(utils::head(x, n), right = FALSE)
  invisible(x)
}

# ---- corrections by key (duplicates sheet) ---------------------------------
d <- hfc_corrections(d, corrfile, sheet = "duplicates", id = key,
                         logfile = corrlog_output, logsheet = "duplicates", ignore = TRUE)
r <- attr(d, "r"); print(unlist(r))
stopifnot(r$N_obs == 4, r$N_failed == 0, nrow(d) == 916, !("23012" %in% d$hhid[duplicated(d$hhid) | duplicated(d$hhid, fromLast = TRUE)]))
show(corrlog_output, "duplicates")

# ---- ids ---------------------------------------------------------------------
# Inject a duplicate pair to exercise the comparison output (corrections resolved all exercise dups)
dd <- d
extra <- dd[dd$hhid == "10102", ]; extra$key <- "uuid:test-dup"; extra$f_hr_age_r1 <- extra$f_hr_age_r1 + 1
extra$starttime <- extra$starttime - 3600
dd <- .hfc_corrections_subset(dd, c(seq_len(nrow(dd)), which(dd$hhid == "10102")))
dd[nrow(dd), c("key", "f_hr_age_r1", "starttime")] <- extra[, c("key", "f_hr_age_r1", "starttime")]
d_ids <- hfc_ids(dd, id, enumerator = enum, date = date, key = key, outfile = hfc_output,
                     outsheet = "id duplicates", dupfile = id_dups_output, save = checkedsurvey,
                     force = TRUE, sheetreplace = TRUE)
print(unlist(attr(d_ids, "r")))
stopifnot(attr(d_ids, "r")$N_dups == 2, attr(d_ids, "r")$N_pairs == 1)
ids_x <- show(hfc_output, "id duplicates")
stopifnot(identical(names(ids_x)[1:3], c("a_enum_id", "serial", "starttime")), ids_x$`# different`[2] >= 1)
stopifnot(nrow(haven::read_dta(id_dups_output)) == 2)
stopifnot(tryCatch({ hfc_ids(dd, id, enumerator = enum, date = date, key = key, outfile = hfc_output,
                                 save = checkedsurvey); FALSE }, error = function(e) TRUE))  # force required
# On the real (corrected) data: no duplicates -> message only; save the checked dataset (pipeline)
d <- hfc_ids(d, id, enumerator = enum, date = date, key = key, outfile = hfc_output,
                 outsheet = "id duplicates", dupfile = id_dups_output, save = checkedsurvey,
                 force = TRUE, sheetreplace = TRUE)
d <- as.data.frame(haven::read_dta(checkedsurvey))      # use "${checkedsurvey}", clear
cat("checked survey:", nrow(d), "obs\n")

# ---- corrections by id (other issues sheet) --------------------------------
before <- d
d <- hfc_corrections(d, corrfile, sheet = "other issues", id = id,
                         logfile = corrlog_output, logsheet = "other issues", ignore = TRUE)
r <- attr(d, "r"); print(unlist(r))
log2 <- show(corrlog_output, "other issues", 10)
print(table(log2$status))
stopifnot("duplicates" %in% readxl::excel_sheets(corrlog_output))   # other sheet kept
cat("f_hr_age_r1 13719:", before$f_hr_age_r1[before$hhid == "13719"], "->", d$f_hr_age_r1[d$hhid == "13719"], "\n")
cat("k_sel 10512:", before$k_sel[before$hhid == "10512"], "->", d$k_sel[d$hhid == "10512"], "\n")
print(d[d[["_hfcokay"]] == 1, c("hhid", "_hfcokayvar")])
hfc_save(d, checkedsurvey)

# ---- specify recode ----------------------------------------------------------
# exercise sheet: empty -> 0 recodes
d <- hfc_specify_recode(d, file.path(src, "excel/exercise/specifyrecode_exercise.xlsm"),
                           sheet = "other specify recode", id = id, logfile = recodelog,
                           logsheet = "other specify recode", sheetreplace = TRUE)
stopifnot(attr(d, "r")$N_recoded == 0)
# custom recode inputs
rcfile <- file.path(out, "specifyrecode_test.xlsx")
wb <- openxlsx::createWorkbook(); openxlsx::addWorksheet(wb, "other specify recode")
openxlsx::writeData(wb, 1, data.frame(
  parent      = c("e_hhh_relig", "f_hr_mar_r*", "f_hr_lead_r*"),
  child       = c("e_hhh_relig_osp", "f_hr_mar_osp_r*", "f_hr_lead_osp_r*"),
  match_type  = c("exact", "begins with", "contains"),
  match_text  = c("Yoga Bakti", "Married", "[Uu]nit [Cc]ommit"),
  recode_from = c("-666", "-666", "-666"),
  recode_to   = c("4", "2", "12"),
  new_label   = c("Other: Yoga", "", "")))
openxlsx::saveWorkbook(wb, rcfile, overwrite = TRUE)
pre <- d
d <- hfc_specify_recode(d, rcfile, sheet = "other specify recode", id = id, keep = "a_enum_name",
                           logfile = recodelog, logsheet = "other specify recode", sheetreplace = TRUE)
print(unlist(attr(d, "r")))
show(recodelog, "other specify recode", 12)
i <- which(pre$e_hhh_relig_osp == "Yoga Bakti")
cat("e_hhh_relig:", hfc_num(pre$e_hhh_relig[i]), "->", hfc_num(d$e_hhh_relig[i]), "label:",
    hfc_value_label(d$e_hhh_relig, 4), " child now '", d$e_hhh_relig_osp[i], "'\n")
hfc_save(d, checkedsurvey)

# ---- versions ----------------------------------------------------------------
rv <- hfc_versions(d, formversion, enumerator = enum, date = date, outfile = hfc_output,
                       outsheet1 = "form versions", outsheet2 = "outdated", sheetreplace = TRUE)
print(unlist(rv))
show(hfc_output, "form versions", 12); show(hfc_output, "outdated")

# ---- dups (dp_vars empty in globals; exercise on gps + phone-like vars) ----
rd <- hfc_duplicates(d, "gpslatitude gpsaccuracy a_pl_pgv_fn", id = id, enumerator = enum, date = date,
                   outfile = hfc_output, outsheet = "duplicates", keep = "a_enum_name", sheetreplace = TRUE)
print(unlist(rd))
show(hfc_output, "duplicates", 8)

# ---- missing -----------------------------------------------------------------
rm_ <- hfc_missingness(d, "_all", priority = paste(id, key, date, enum), outfile = hfc_output,
                       show = "0", outsheet = "missing", sheetreplace = TRUE)
print(unlist(rm_))
show(hfc_output, "missing", 8)
rm2 <- hfc_missingness(d, "f_hr_*", outfile = file.path(out, "missing_show.xlsx"), show = "50%")
print(nrow(readxl::read_excel(file.path(out, "missing_show.xlsx"))))

cat("\nSheets in hfc_output:", paste(readxl::excel_sheets(hfc_output), collapse = " | "), "\n")
cat("ALL CORE1 TESTS DONE\n")
