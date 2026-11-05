# =============================================================================
#  hfc_new_project() — create a new HFC project
#  Creates the DMS project folder structure and copies the R scripts,
#  R functions and Excel input templates into it.  With exercise = TRUE it
#  also copies the exercise data, inputs and SurveyCTO media files so the
#  whole pipeline runs out of the box.
# =============================================================================

#' Set up a new high-frequency-check project
#'
#' @param folder    project folder (created if needed)
#' @param surveys   optional survey names; files are suffixed with each name
#' @param subfolders create one sub-folder per survey in inputs/outputs/data
#' @param filesonly  only copy scripts and input templates into `folder`
#' @param exercise   also copy the exercise data, filled-in inputs and media
#' @param toolkit    path of the hfc-toolkit folder (contains R/, scripts/, inst/)
#' @examples
#' hfc_new_project(folder = "C:/projects/hfc_exercise", exercise = TRUE)
hfc_new_project <- function(folder = getwd(), surveys = NULL, subfolders = FALSE,
                         filesonly = FALSE, exercise = FALSE,
                         toolkit = getOption("hfc_toolkit_dir", NULL)) {
  if (is.null(toolkit)) {
    toolkit <- tryCatch(dirname(dirname(normalizePath(sys.frame(1)$ofile))), error = function(e) NULL)
    if (is.null(toolkit) || !dir.exists(file.path(toolkit, "scripts")))
      stop("Set toolkit = '<path to hfc-toolkit folder>' (or options(hfc_toolkit_dir = ...))", call. = FALSE)
  }
  if (is.null(folder) || !nzchar(folder)) folder <- getwd()
  folder <- normalizePath(folder, mustWork = FALSE)
  if (exercise && (subfolders || filesonly)) stop("exercise cannot be combined with subfolders or filesonly", call. = FALSE)
  dir.create(folder, showWarnings = FALSE, recursive = TRUE)
  say <- function(...) message(sprintf(...))

  if (!filesonly) {
    folders <- c("0_archive", "1_instruments", "1_instruments/1_paper", "1_instruments/2_scto_print",
                 "1_instruments/3_scto_xls", "2_scripts", "2_scripts/R", "3_checks", "3_checks/1_inputs",
                 "3_checks/2_outputs", "4_data", "4_data/1_preloads", "4_data/2_survey", "4_data/3_backcheck",
                 "4_data/4_monitoring", "5_media", "5_media/1_audio", "5_media/2_images", "5_media/3_video",
                 "6_documentation", "7_field_manager", "8_reports")
    say("Setting up folders ...")
    for (f in folders) {
      p <- file.path(folder, f)
      if (dir.exists(p)) say("Skipped: Folder %s already exists", f)
      else { dir.create(p, recursive = TRUE); say("Successful: Folder %s created", f) }
    }
    if (subfolders && length(surveys)) {
      sfs <- c("3_checks/1_inputs", "3_checks/2_outputs", "4_data/1_preloads", "4_data/2_survey",
               "4_data/3_backcheck", "4_data/4_monitoring", "5_media/1_audio", "5_media/2_images", "5_media/3_video")
      for (i in seq_along(surveys)) for (sf in sfs)
        dir.create(file.path(folder, sf, paste0(i, "_", surveys[i])), showWarnings = FALSE, recursive = TRUE)
    }
  }

  copy1 <- function(from, to) {
    if (file.exists(to)) say("Skipped: File %s already exists", basename(to))
    else { file.copy(from, to); say("%s copied to %s", basename(to), dirname(to)) }
  }
  script_dir <- if (filesonly) folder else file.path(folder, "2_scripts")
  input_dir  <- if (filesonly) folder else file.path(folder, "3_checks/1_inputs")

  # master script goes in the project root, others in 2_scripts
  copy1(file.path(toolkit, "scripts/0_master.R"), file.path(folder, "0_master.R"))
  for (s in c("1_globals", "3_prepsurvey", "4_checksurvey", "5_prepbc", "6_checkbc")) {
    if (length(surveys)) for (sv in surveys) copy1(file.path(toolkit, "scripts", paste0(s, ".R")), file.path(script_dir, paste0(s, "_", sv, ".R")))
    else copy1(file.path(toolkit, "scripts", paste0(s, ".R")), file.path(script_dir, paste0(s, ".R")))
  }
  # R functions
  fdir <- if (filesonly) file.path(folder, "R") else file.path(folder, "2_scripts/R")
  dir.create(fdir, showWarnings = FALSE, recursive = TRUE)
  for (f in list.files(file.path(toolkit, "R"), pattern = "\\.R$", full.names = TRUE)) copy1(f, file.path(fdir, basename(f)))

  # input templates (or exercise inputs)
  say("Copying input files to %s ...", input_dir)
  for (f in c("hfc_inputs", "corrections", "specifyrecode")) {
    src <- if (exercise) file.path(toolkit, "inst/excel/exercise", paste0(f, "_exercise.xlsm"))
           else file.path(toolkit, "inst/excel/templates", paste0(f, ".xlsm"))
    if (length(surveys)) for (i in seq_along(surveys)) {
      d <- if (subfolders) file.path(input_dir, paste0(i, "_", surveys[i])) else input_dir
      copy1(src, file.path(d, paste0(f, "_", surveys[i], ".xlsm")))
    } else copy1(src, file.path(input_dir, paste0(f, ".xlsm")))
  }

  if (exercise) {
    say("Copying exercise files ...")
    d <- file.path(toolkit, "inst/data")
    for (f in c("household_survey.dta", "household_preloads.xlsx", "respondent_targets.xlsx"))
      file.copy(file.path(d, f), file.path(folder, "4_data/2_survey", f), overwrite = TRUE)
    file.copy(file.path(d, "household_backcheck.dta"), file.path(folder, "4_data/3_backcheck/household_backcheck.dta"), overwrite = TRUE)
    for (f in c("corrections", "hfc_inputs", "specifyrecode"))
      file.copy(file.path(toolkit, "inst/excel/exercise", paste0(f, "_exercise.xlsm")),
                file.path(folder, "0_archive", paste0(f, "_exercise.xlsm")), overwrite = TRUE)
    for (f in c("Household_Survey.xlsx", "Household_Back_Check_Survey.xlsx"))
      file.copy(file.path(toolkit, "inst/excel/exercise", f), file.path(folder, "1_instruments/3_scto_xls", f), overwrite = TRUE)
    say("Unpacking text audit and comment files ...")
    utils::unzip(file.path(d, "media.zip"), exdir = file.path(folder, "4_data/2_survey"))
  }
  say("Done. Open %s, check the settings in 2_scripts/1_globals.R, then run 0_master.R",
      file.path(folder, "0_master.R"))
  invisible(folder)
}
