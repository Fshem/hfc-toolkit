# Test hfc_survey_dashboard() and hfc_enum_dashboard() on the exercise data
pkg  <- Sys.getenv("HFC_TOOLKIT", ".")   # run from the repository root
source(file.path(pkg, "R/00_utils.R"))
source(file.path(pkg, "R/hfc_survey_dashboard.R"))
source(file.path(pkg, "R/hfc_enum_dashboard.R"))

hfc <- file.path(pkg, "inst")
outdir <- file.path(pkg, "tests/out/db")
dir.create(outdir, recursive = TRUE, showWarnings = FALSE)
inputs <- file.path(hfc, "excel/exercise/hfc_inputs_exercise.xlsm")

# ---- prep like do/3_prepsurvey.do ----
d <- as.data.frame(haven::read_dta(file.path(hfc, "data/household_survey.dta")))
d <- hfc_recode_to_tag(d, "-999 999 .999", "d")
d <- hfc_recode_to_tag(d, "-888 888 .888", "r")
d <- d[, setdiff(names(d), c("deviceid", "subscriberid", "simid", "devicephonenum", "username"))]
for (v in c("duration", "a_enum_id")) {           # destring
  lab <- attr(d[[v]], "label"); d[[v]] <- as.numeric(d[[v]]); attr(d[[v]], "label") <- lab
}
stopifnot(hfc_isid(d, "key"))
d$skey <- substring(d$key, nchar(d$key) - 11)
d$startdate <- as.Date(d$starttime); d$enddate <- as.Date(d$endtime); d$subdate <- as.Date(d$submissiondate)
d <- d[d$startdate > as.Date("2016-01-01"), ]

# other specify child vars
os <- hfc_read_inputs(inputs, "other specify")
childvars <- hfc_vars(d, os$child)
cat("child vars:", length(childvars), "\n")

common <- list(date = "starttime", enumerator = "a_enum_id", consent = "c_consent", cons_vals = "1",
               dontknow_num = ".d", dontknow_str = "-999", refuse_num = ".r", refuse_str = "-888",
               otherspecify = childvars, duration = "duration", formversion = "formdef_version")

# ---- survey dashboard (auto period, no by) ----
f1 <- file.path(outdir, "surveydb.xlsx")
r1 <- do.call(hfc_survey_dashboard, c(list(data = d, outfile = f1), common))
str(r1)
print(readxl::excel_sheets(f1))
print(as.data.frame(readxl::read_excel(f1, "summary", col_names = FALSE)))
print(head(as.data.frame(readxl::read_excel(f1, paste(r1$period, "productivity")))))

# ---- survey dashboard by team, daily ----
f2 <- file.path(outdir, "surveydb_by.xlsx")
r2 <- do.call(hfc_survey_dashboard, c(list(data = d, outfile = f2, by = "a_team_id", period = "daily"), common))
print(readxl::excel_sheets(f2))
print(as.data.frame(readxl::read_excel(f2, "summary (grouped)")))
x <- as.data.frame(readxl::read_excel(f2, "daily productivity (grouped)")); print(x[, 1:6])

# monthly also
f2b <- file.path(outdir, "surveydb_monthly.xlsx")
r2b <- do.call(hfc_survey_dashboard, c(list(data = d, outfile = f2b, by = "a_team_id", period = "monthly"), common))
print(as.data.frame(readxl::read_excel(f2b, "monthly productivity")))
print(as.data.frame(readxl::read_excel(f2b, "monthly productivity (grouped)")))

# ---- enumerator dashboard ----
f3 <- file.path(outdir, "enumdb.xlsx")
hfc_rm(f3)
r3 <- do.call(hfc_enum_dashboard, c(list(data = d, using = inputs, sheetname = "enumstats",
                                     team = "a_team_id", outfile = f3), common))
str(r3)
print(readxl::excel_sheets(f3))
print(head(as.data.frame(readxl::read_excel(f3, "summary"))))
print(as.data.frame(readxl::read_excel(f3, "summary (team)")))
x <- as.data.frame(readxl::read_excel(f3, paste(r3$period, "productivity"))); print(dim(x)); print(tail(x[, 1:6], 3))
print(as.data.frame(readxl::read_excel(f3, paste(r3$period, "productivity (team)")))[, 1:6])
x <- as.data.frame(readxl::read_excel(f3, "enumstats", col_names = FALSE)); print(dim(x)); print(head(x[, 1:12], 5))

# rerun into the same file without team: other sheets kept
r4 <- do.call(hfc_enum_dashboard, c(list(data = d, outfile = f3), common))
print(readxl::excel_sheets(f3))
cat("ALL DB TESTS DONE\n")
