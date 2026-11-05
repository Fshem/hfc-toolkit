# Test: hfc_text_audit + hfc_time_use on the exercise data
pkg  <- Sys.getenv("HFC_TOOLKIT", ".")   # run from the repository root
source(file.path(pkg, "R/00_utils.R"))
source(file.path(pkg, "R/hfc_text_audit.R"))
source(file.path(pkg, "R/hfc_time_use.R"))

out <- file.path(pkg, "tests/out/ta")
dir.create(out, recursive = TRUE, showWarnings = FALSE)
media <- file.path(out, "media")
if (!dir.exists(file.path(media, "media")) && !length(list.files(media, "^TA_")))
  utils::unzip(file.path(pkg, "inst/data/media.zip"), exdir = media)
media_folder <- if (dir.exists(file.path(media, "media"))) file.path(media, "media") else media

# --- minimal private collator mimicking `hfc_collate_scto textaudit var, folder() save()` ---
# import delim ..., stringcols(_all) varnames(1): Stata names the columns from the
# header via strtoname()+lower: "Field name" -> fieldname, "Total duration (seconds)"
# -> totaldurationseconds, "First appeared (seconds into survey)" ->
# firstappearedsecondsintosurvey; all string. Then gen mediavar = file id and
# append (newest file on top).
.ta_collate <- function(data, mediavar, folder, save) {
  x <- as.character(data[[mediavar]]); x <- x[!hfc_missing(x)]
  ids <- .hfc_text_audit_mediaid(x, "TA_")
  stata_name <- function(h) { h <- tolower(gsub("[^A-Za-z0-9_]", "", h)); substr(h, 1, 32) }
  res <- NULL; fail <- 0
  for (f in ids) {
    p <- file.path(folder, paste0(f, ".csv"))
    if (!file.exists(p)) { fail <- fail + 1; next }
    d <- utils::read.csv(p, colClasses = "character", check.names = FALSE)
    names(d) <- stata_name(names(d))
    d[[mediavar]] <- f
    res <- if (is.null(res)) d else rbind(d, res)   # append using tmf_media (new on top)
  }
  message(sprintf("collated %d files, %d not found", length(ids) - fail, fail))
  hfc_save(res, save)
  invisible(res)
}

hh <- haven::read_dta(file.path(pkg, "inst/data/household_survey.dta"))
hh <- hfc_recode_to_tag(hh, "-999 999 .999", "d")
hh <- hfc_recode_to_tag(hh, "-888 888 .888", "r")
stopifnot(all(c("text_audit", "a_enum_id", "starttime") %in% names(hh)))

ta_file <- file.path(out, "textaudit_data.dta")
hfc_rm(ta_file)
coll <- .ta_collate(hh, "text_audit", media_folder, ta_file)
cat("collated rows:", nrow(coll), " cols:", paste(names(coll), collapse = ", "), "\n")

ta_out <- file.path(out, "textaudit.xlsx"); tu_out <- file.path(out, "timeuse.xlsx")
hfc_rm(ta_out, tu_out)

r1 <- hfc_text_audit(hh, "text_audit", enumerator = "a_enum_id", textauditdata = ta_file,
                        outfile = ta_out, stats = "count min max mean sd iqr p25 p50 p75",
                        nolabel = TRUE, sheetreplace = TRUE)
cat(sprintf("textaudit: %d submissions, %d fields, %d groups\n", r1$N_obs, r1$N_fields, r1$N_groups))
print(readxl::excel_sheets(ta_out))
for (s in readxl::excel_sheets(ta_out)) {
  x <- readxl::read_excel(ta_out, sheet = s)
  cat("\n==", s, ":", nrow(x), "x", ncol(x), "\n"); print(head(x[, 1:min(6, ncol(x))], 4))
}

# stats subset + median alias, rds input
saveRDS(coll, file.path(out, "textaudit_data.rds"))
r1b <- hfc_text_audit(hh, "text_audit", enumerator = "a_enum_id",
                         textauditdata = file.path(out, "textaudit_data.rds"),
                         outfile = file.path(out, "textaudit_sub.xlsx"), stats = "count mean median")
print(names(readxl::read_excel(file.path(out, "textaudit_sub.xlsx"), sheet = "field stats")))
err <- tryCatch(hfc_text_audit(hh, "text_audit", "a_enum_id", ta_file, tempfile(fileext = ".xlsx"), stats = "foo"),
                error = function(e) conditionMessage(e)); print(err)

r2 <- hfc_time_use(hh, "text_audit", enumerator = "a_enum_id", starttime = "starttime",
                      textauditdata = ta_file, outfile = tu_out, sheetreplace = TRUE)
cat(sprintf("\ntimeuse: %d submissions, %d dates, %d enumerators\n", r2$N_obs, r2$N_dates, r2$N_enums))
print(readxl::excel_sheets(tu_out))
print(head(r2$by_date[, 1:14], 4))
wb <- openxlsx::loadWorkbook(tu_out)
print(openxlsx::read.xlsx(tu_out, sheet = "timeuse by enumerator", colNames = FALSE, rows = 1:6, cols = 1:12, skipEmptyRows = FALSE))
cat("styles in workbook:", length(wb$styleObjects), "\n")
