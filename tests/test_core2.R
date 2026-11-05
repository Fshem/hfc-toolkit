# Test: hfc_outliers, hfc_constraints, hfc_logic, hfc_other_specify,
#       hfc_comments, hfc_collate_scto  (worker "core2")
pkg  <- Sys.getenv("HFC_TOOLKIT", ".")   # run from the repository root
src  <- file.path(pkg, "inst")
source(file.path(pkg, "R/00_utils.R"))
for (f in c("hfc_collate_scto", "hfc_outliers", "hfc_constraints", "hfc_logic",
            "hfc_other_specify", "hfc_comments")) source(file.path(pkg, "R", paste0(f, ".R")))

out_dir <- file.path(pkg, "tests/out/core2")
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
inputs  <- file.path(src, "excel/exercise/hfc_inputs_exercise.xlsm")
outfile <- file.path(out_dir, "hfc_output.xlsx")
media   <- file.path(out_dir, "media")
hfc_rm(outfile, file.path(out_dir, "comments_data.dta"), file.path(out_dir, "textaudit_data.dta"))
if (!dir.exists(media) || length(list.files(media)) == 0)
  utils::unzip(file.path(src, "data/media.zip"), exdir = out_dir)

# ---- prep survey like do/3_prepsurvey.do -------------------------------------
d <- as.data.frame(haven::read_dta(file.path(src, "data/household_survey.dta")))
d <- hfc_recode_to_tag(d, "-999 999 .999", "d")
d <- hfc_recode_to_tag(d, "-888 888 .888", "r")
d <- d[, setdiff(names(d), c("deviceid", "subscriberid", "simid", "devicephonenum", "username"))]
for (v in c("duration", "a_enum_id")) { lab <- attr(d[[v]], "label"); d[[v]] <- as.numeric(d[[v]]); attr(d[[v]], "label") <- lab }
d$skey <- substring(d$key, nchar(d$key) - 11)
d$startdate <- as.Date(d$starttime)

ID <- "hhid"; ENUM <- "a_enum_id"; DATE <- "starttime"
show <- function(sheet, n = 5) {
  x <- readxl::read_excel(outfile, sheet = sheet)
  cat(sprintf("\n--- sheet '%s': %d rows x %d cols\n", sheet, nrow(x), ncol(x)))
  print(names(x)); print(as.data.frame(head(x, n)))
}

# ---- outliers ---------------------------------------------------------------
r1 <- hfc_outliers(d, inputs, sheet = "outliers", outfile = outfile, outsheet = "outliers",
                       id = ID, enumerator = ENUM, date = DATE, sheetreplace = TRUE)
show("outliers")
r1b <- hfc_outliers(d, inputs, outfile = file.path(out_dir, "outliers_mult.xlsx"),
                        id = ID, enumerator = ENUM, date = DATE, use_multiplier = TRUE)
stopifnot(r1b$N_outliers == r1$N_outliers)  # multipliers in sheet are default 1.5

# ---- constraints ------------------------------------------------------------
r2 <- hfc_constraints(d, inputs, outfile = outfile, id = ID, enumerator = ENUM, date = DATE)
show("constraints")
print(table(r2$data$constraint))

# ---- logic ------------------------------------------------------------------
r3 <- hfc_logic(d, inputs, outfile = outfile, id = ID, enumerator = ENUM, date = DATE)
show("logic")
# extra hfc_eval checks with typical logic-sheet style expressions
for (e in c("duration <= 20000", "f_hr_age_r1 >= 18 | missing(f_hr_age_r1)",
            "!inlist(e_hhh_relig, -666) | e_hhh_relig_osp != \"\"", "a_district == \"NORTH COCA\" & duration > .",
            "c_consent == 1", "m_mon_inc ~= .d"))
  cat(sprintf("hfc_eval('%s'): %d TRUE\n", e, sum(hfc_eval(d, e))))

# ---- other specify ----------------------------------------------------------
r4 <- hfc_other_specify(d, inputs, outfile = outfile, id = ID, enumerator = ENUM, date = DATE)
show("other specify"); show("other specify (choices)", 12)
cat("childvarlist:", substr(r4$childvarlist, 1, 120), "...\n")

# ---- comments ---------------------------------------------------------------
cdat <- file.path(out_dir, "comments_data.dta")
r5a <- hfc_collate_scto("comments", d, "field_comments", folder = media, save = cdat)
str(r5a)
r5a2 <- hfc_collate_scto("comments", d, "field_comments", folder = media, save = cdat)  # skips all
cm <- haven::read_dta(cdat); print(as.data.frame(cm))
r5 <- hfc_comments(d, "field_comments", enumerator = ENUM, commentsdata = cdat,
                       outfile = outfile, outsheet = "field comments", keepvars = "hhid")
show("field comments", 10)

# ---- text audit collation -----------------------------------------------------
tdat <- file.path(out_dir, "textaudit_data.dta")
r6 <- hfc_collate_scto("textaudit", d, "text_audit", folder = media, save = tdat)
str(r6)
ta <- haven::read_dta(tdat)
cat("textaudit collated:", nrow(ta), "rows; columns:", paste(names(ta), collapse = ", "), "\n")
print(head(as.data.frame(ta), 3))

cat("\nSheets in output:", paste(readxl::excel_sheets(outfile), collapse = " | "), "\n")
cat(sprintf("SUMMARY outliers=%s/%s vars constraints=%s/%s vars logic=%s/%s vars specify=%s/%s vars comments=%s textaudit files=%s (failed %s)\n",
            r1$N_outliers, r1$N_vars, r2$N_constraints, r2$N_vars, r3$N_logic, r3$N_vars,
            r4$N_specify, r4$N_vars, r5$N_comments, r6$N_total, r6$N_failed))

# ---- synthetic outliers inputs: combine = yes, method sd, custom multiplier ----
syn <- file.path(out_dir, "syn_inputs.xlsx")
openxlsx::write.xlsx(list(outliers = data.frame(
  variable = c("l_hh_earn_*", "m_mon_inc", "j_land_size"),
  by = c(NA, "a_district", NA), method = c("iqr", "sd", "iqr"),
  multiplier = c("1.5", "2", "3"), combine = c("yes", NA, NA))), syn, overwrite = TRUE)
s1 <- hfc_outliers(d, syn, outfile = file.path(out_dir, "syn_out.xlsx"), id = ID, enumerator = ENUM, date = DATE)
s2 <- hfc_outliers(d, syn, outfile = file.path(out_dir, "syn_out_mult.xlsx"), id = ID, enumerator = ENUM, date = DATE,
                       use_multiplier = TRUE)
print(table(s1$data$variable)); print(table(s2$data$variable))
print(unique(s1$data[, c("combine", "range")])[1:4, ])
stopifnot(sum(s2$data$variable == "j_land_size") < sum(s1$data$variable == "j_land_size"))
