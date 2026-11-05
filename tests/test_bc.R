# Test for hfc_backcheck() -- R port of ipabcstats.ado
pkg  <- Sys.getenv("HFC_TOOLKIT", ".")   # run from the repository root
source(file.path(pkg, "R/00_utils.R"))
source(file.path(pkg, "R/hfc_backcheck.R"))
outdir <- file.path(pkg, "tests/out/bc")
dir.create(outdir, recursive = TRUE, showWarnings = FALSE)
datadir <- file.path(pkg, "inst/data")

dk_num <- "-999 999 .999"; ref_num <- "-888 888 .888"

# ---- prep like do/3_prepsurvey.do and do/5_prepbc.do ----------------------
prep <- function(file, destr) {
  d <- as.data.frame(haven::read_dta(file))
  d <- hfc_recode_to_tag(d, dk_num, "d")
  d <- hfc_recode_to_tag(d, ref_num, "r")
  d <- d[, setdiff(names(d), c("deviceid", "subscriberid", "simid", "devicephonenum", "username"))]
  for (v in destr) { l <- attr(d[[v]], "label"); d[[v]] <- suppressWarnings(as.numeric(d[[v]])); attr(d[[v]], "label") <- l }
  hfc_check_unique(d, "key")
  d$skey <- substring(d$key, nchar(d$key) - 11)
  hfc_check_unique(d, "skey")
  for (v in c("starttime", "endtime", "submissiondate")) {
    if (any(is.na(d[[v]]))) stop("Variable ", v, " has missing values")
    if (any(as.numeric(format(d[[v]], "%Y")) < 2016)) stop("variable ", v, " has dates before 2016")
  }
  d$startdate <- as.Date(d$starttime); d$enddate <- as.Date(d$endtime); d$subdate <- as.Date(d$submissiondate)
  d <- d[d$startdate > as.Date("2016-01-01"), ]
  d
}
survey <- prep(file.path(datadir, "household_survey.dta"), c("duration", "a_enum_id", "a_team_id"))
bc     <- prep(file.path(datadir, "household_backcheck.dta"), c("duration", "a_bcer_id", "a_team_id"))
# give enumerator / backchecker value labels from the name variables
mk_lab <- function(id, nm) { u <- unique(data.frame(id, nm)); u <- u[!is.na(u$id), ]; setNames(u$id, u$nm) }
survey$a_enum_id <- haven::labelled(survey$a_enum_id, mk_lab(survey$a_enum_id, survey$a_enum_name), label = "Enumerator ID")
bc$a_bcer_id     <- haven::labelled(bc$a_bcer_id, mk_lab(bc$a_bcer_id, bc$a_bcer_name), label = "Back checker ID")
# back-check team: the bc data has no own team variable (a_team_id is the survey
# team preload, a common variable that the merge takes from the survey), so build one
bc$bcer_team <- haven::labelled(ifelse(hfc_num(bc$a_bcer_id) %in% c(16087, 16256), 1, 2),
                                c("BC Team A" = 1, "BC Team B" = 2), label = "Back check team")
# checked data has hhid duplicates resolved (hfc_ids/corrections); here keep the first submission
survey <- survey[order(survey$hhid, survey$starttime), ]; survey <- survey[!duplicated(survey$hhid), ]
bc     <- bc[order(bc$hhid, bc$starttime), ];         bc     <- bc[!duplicated(bc$hhid), ]
cat("survey:", nrow(survey), " bc:", nrow(bc), "\n")
hfc_save(survey, file.path(outdir, "household_survey_prepped.dta"))
hfc_save(bc, file.path(outdir, "household_backcheck_prepped.dta"))

# ---- base-R cross-checks of the Stata statistical tests --------------------
set.seed(1); x <- rnorm(40); y <- x + rnorm(40, 0.2)
stopifnot(abs(.hfc_backcheck_ttest(x, y) - t.test(x, y, paired = TRUE)$p.value) < 1e-12)
a <- rbinom(50, 1, .5); b <- rbinom(50, 1, .6)
stopifnot(abs(.hfc_backcheck_prtest(a, b, "a") -
              suppressWarnings(prop.test(c(sum(a), sum(b)), c(50, 50), correct = FALSE)$p.value)) < 1e-12)
xr <- round(x * 3); yr <- round(y * 3); nz <- xr != yr
stopifnot(abs(.hfc_backcheck_signrank(xr[nz], yr[nz]) -
              suppressWarnings(wilcox.test(xr[nz], yr[nz], paired = TRUE, exact = FALSE, correct = FALSE)$p.value)) < 1e-12)
cat("signrank with zeros (Stata keeps zeros in ranking):", .hfc_backcheck_signrank(xr, yr), "\n")
stopifnot(.hfc_backcheck_fmt_g(c(121.199996948242, 0.5, 1234567, 12345678, -3)) == c("121.2", ".5", "1234567", "12345678", "-3"))

# ---- run 1: main pipeline call ---------------------------------------------
f1 <- file.path(outdir, "bc_output.xlsx")
r1 <- hfc_backcheck(surveydata = file.path(outdir, "household_survey_prepped.dta"),
                 bcdata = file.path(outdir, "household_backcheck_prepped.dta"),
                 id = "hhid", enumerator = "a_enum_id", enumteam = "a_team_id",
                 backchecker = "a_bcer_id", bcteam = "bcer_team",
                 t1vars = "e_hhh_relig e_hhh_native h_toilet a_pl_hhh_gnd",
                 t2vars = "f_hhm_cnt h_rooms h_electricity h_cook_fuel i_radio i_mosquito_net j_land_yn",
                 t3vars = "e_hhh_howlong j_land_size j_land_value m_mon_exp m_mon_inc",
                 ttest = "m_mon_exp m_mon_inc", prtest = "h_electricity i_radio",
                 signrank = "f_hhm_cnt h_rooms", reliability = "m_mon_exp j_land_size f_hhm_cnt",
                 okrange = "e_hhh_howlong [-1, 1], m_mon_exp [-5%, 5%]",
                 nodiffnum = "-666", excludenum = "0",
                 filename = f1, surveydate = "starttime", bcdate = "starttime",
                 showid = "30%", keepsurvey = "a_district", keepbc = "a_bcer_name", full = FALSE)
cat("\nr(): survey =", r1$survey, " bc =", r1$bc, " bc_only =", r1$bc_only, " showid =", r1$showid,
    " total_rate =", r1$total_rate, " avd =", r1$avd, "\n")
print(r1$rates)
print(names(r1))
sheets <- readxl::excel_sheets(f1); print(sheets)
for (s in sheets) {
  cat("\n=====", s, "\n")
  print(utils::head(as.data.frame(suppressMessages(readxl::read_excel(f1, sheet = s, col_names = FALSE))), 10))
}
cmp <- r1$comparisons
cat("\ndifferences by variable:\n"); print(tapply(cmp$`_vdiff`, cmp$`_vvar`, sum, na.rm = TRUE))
vs <- r1$varstats; print(vs)

# manual verification of a few numbers
mm <- merge(survey, bc, by = "hhid", suffixes = c("", "_bc"))
d_relig <- sum(haven::zap_labels(mm$e_hhh_relig) != haven::zap_labels(mm$e_hhh_relig_bc), na.rm = TRUE) +
           sum(xor(is.na(mm$e_hhh_relig), is.na(mm$e_hhh_relig_bc)))
stopifnot(vs$diffs[vs$variable == "e_hhh_relig"] == d_relig)
ex <- !is.na(hfc_num(mm$m_mon_exp_bc)) & hfc_num(mm$m_mon_exp_bc) == 0
sv <- as.numeric(.hfc_backcheck_tostring(mm$m_mon_exp[!ex])); bv <- as.numeric(.hfc_backcheck_tostring(mm$m_mon_exp_bc[!ex]))
stopifnot(abs(vs$pvalue[vs$variable == "m_mon_exp"] - t.test(sv, bv, paired = TRUE)$p.value) < 1e-12)
srv <- var(sv - bv, na.rm = TRUE) / 2
stopifnot(abs(vs$srv[vs$variable == "m_mon_exp"] - srv) < 1e-9)
cat("manual checks OK\n")

# ---- run 2: string options, full, nolabel, count showid, excludemissing ----
f2 <- file.path(outdir, "bc_output_full.xlsx")
r2 <- hfc_backcheck(surveydata = survey, bcdata = bc, id = "hhid", enumerator = "a_enum_id", backchecker = "a_bcer_id",
                 t1vars = "a_pl_hhh_fn a_pl_ch_fn a_pl_ch_gnd", t2vars = "h_rooms", t3vars = "j_land_value",
                 signrank = "j_land_value", excludestr = "\"zzz\"", nodiffstr = "\"female\"",
                 lower = TRUE, upper = FALSE, nosymbol = TRUE, trim = TRUE, excludemissing = TRUE,
                 full = TRUE, nolabel = TRUE, showid = "1", filename = file.path(outdir, "bc_output_full.xls"),
                 surveydate = "starttime", bcdate = "starttime")
print(readxl::excel_sheets(f2))
c2 <- as.data.frame(readxl::read_excel(f2, sheet = "comparison", skip = 3))
print(table(c2$variable, c2$result))
print(head(c2[c2$result == "different", 1:12]))
print(r2$varstats)
cat("showid (>1 differences):", r2$showid, "\n")

# ---- run 3: back-check-only IDs (drop 5 back-checked HHs from survey) -------
f3 <- file.path(outdir, "bc_output_bconly.xlsx")
s3 <- survey[!(survey$hhid %in% bc$hhid[1:5]), ]
r3 <- hfc_backcheck(surveydata = s3, bcdata = bc, id = "hhid", enumerator = "a_enum_id", backchecker = "a_bcer_id",
                 t2vars = "h_rooms f_hhm_cnt", reliability = "h_rooms", showid = "2",
                 filename = f3, surveydate = "starttime", bcdate = "starttime")
stopifnot(r3$bc_only == 5, "backcheck only" %in% readxl::excel_sheets(f3))
print(as.data.frame(readxl::read_excel(f3, sheet = "backcheck only", skip = 2)))

# ---- error checks -----------------------------------------------------------
e <- try(hfc_backcheck(survey, bc, id = "hhid", enumerator = "a_enum_id", backchecker = "a_bcer_id",
                    filename = f2, surveydate = "starttime", bcdate = "starttime"), silent = TRUE)
stopifnot(grepl("must specify, at minimum", e))
e <- try(hfc_backcheck(survey, bc, id = "hhid", enumerator = "a_enum_id", backchecker = "a_bcer_id", t1vars = "h_rooms",
                    ttest = "h_rooms", filename = f2, surveydate = "starttime", bcdate = "starttime"), silent = TRUE)
stopifnot(grepl("t2vars", e))
e <- try(hfc_backcheck(survey, bc, id = "hhid", enumerator = "a_enum_id", backchecker = "a_bcer_id", t2vars = "h_rooms",
                    nodiffnum = "1", excludenum = "1", filename = f2, surveydate = "starttime", bcdate = "starttime"), silent = TRUE)
stopifnot(grepl("both nodiffnum", e))
cat("\nALL TESTS PASSED\n")
