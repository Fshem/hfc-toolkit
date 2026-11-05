# ==============================================================================
#  TITLE   : 0_master.R
#  PURPOSE : Master script — runs the high-frequency checks
#
#  Required packages: haven, openxlsx, readxl
#     install.packages(c("haven", "openxlsx", "readxl"))
# ==============================================================================

rm(list = setdiff(ls(), "gl_cwd"))
set.seed(87235)

# ---- working directory: the project folder created by hfc_new_project() --------
if (!exists("gl_cwd")) gl_cwd <- getwd()

# ---- load the hfc-toolkit functions -------------------------------------------
#  Set hfc_toolkit_dir to the folder that contains the R/ functions
#  (hfc_new_project() copies them into <project>/2_scripts/R).
hfc_toolkit_dir <- file.path(gl_cwd, "2_scripts", "R")
for (f in sort(list.files(hfc_toolkit_dir, pattern = "\\.R$", full.names = TRUE))) source(f)

# ---- Survey 1 -----------------------------------------------------------------
source(file.path(gl_cwd, "2_scripts", "1_globals.R"))      # settings
# source(file.path(gl_cwd, "2_scripts", "2_import.R"))     # import script (optional)
source(file.path(gl_cwd, "2_scripts", "3_prepsurvey.R"))   # prep survey
source(file.path(gl_cwd, "2_scripts", "4_checksurvey.R"))  # check survey
source(file.path(gl_cwd, "2_scripts", "5_prepbc.R"))       # prep back check
source(file.path(gl_cwd, "2_scripts", "6_checkbc.R"))      # check back check

## END ##
