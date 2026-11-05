# hfc-toolkit — High-Frequency Data Quality Checks in R

A toolkit for running high-frequency checks (HFCs) on survey data while it is
being collected. It reads Excel input workbooks, runs the checks, and writes
formatted Excel reports that field teams can act on.

## HFC Checker web app

`app/` holds a Shiny front end for the toolkit: upload survey data (`.dta`, `.csv` or `.xlsx`),
optionally an `hfc_inputs` workbook, match the ID, enumerator and date columns, and get back a
prioritised list of issues to follow up, an enumerator ranking and the full Excel reports.

The app wraps the functions in `R/` without changing them. On every push to `main`, the
`publish-app` workflow builds it with [Shinylive](https://posit-dev.github.io/r-shinylive/) and
publishes it to GitHub Pages, where it runs entirely in the browser: uploaded data never leaves
the user's computer.

To run it locally:

```r
install.packages(c("shiny", "bslib", "haven", "openxlsx", "readxl"))
shiny::runApp("hfc-toolkit/app")
```

## Requirements

R ≥ 4.1 and three packages:

```r
install.packages(c("haven", "openxlsx", "readxl"))
```

## Quick start (exercise data)

```r
source("hfc-toolkit/R/hfc_new_project.R")
hfc_new_project(folder = "C:/projects/hfc_exercise", exercise = TRUE, toolkit = "hfc-toolkit")

setwd("C:/projects/hfc_exercise")
gl_cwd <- getwd()
source("0_master.R")        # reports land in 3_checks/2_outputs/<today>/
```

For your own project:

1. Run `hfc_new_project(folder = "...")` without `exercise`.
2. Fill in the Excel input templates in `3_checks/1_inputs/`.
3. Edit the settings in `2_scripts/1_globals.R`.

## Functions

| Stage | Function | What it does |
|---|---|---|
| Setup | `hfc_new_project()` | Creates the project folders, scripts and input templates |
| Cleaning | `hfc_corrections()` | Applies corrections from the corrections workbook; **returns the corrected data** |
| | `hfc_specify_recode()` | Recodes "other, specify" answers into choices; **returns the recoded data** |
| Checks | `hfc_ids()` | Finds duplicate survey IDs; **returns the de-duplicated data** |
| | `hfc_versions()` | Summarises form versions and flags submissions on outdated forms |
| | `hfc_duplicates()` | Finds duplicate values in non-ID variables (e.g. GPS, phone numbers) |
| | `hfc_missingness()` | Missingness and distinctness of every variable |
| | `hfc_outliers()` | Outliers in numeric variables (IQR or SD method) |
| | `hfc_constraints()` | Values outside hard/soft limits |
| | `hfc_logic()` | Logical consistency rules from the inputs sheet |
| | `hfc_other_specify()` | Lists all "other, specify" answers |
| | `hfc_comments()` | SurveyCTO field comments |
| | `hfc_text_audit()` | Time spent per question/group, from SurveyCTO text audits |
| | `hfc_time_use()` | Heatmap of active survey hours by date and enumerator |
| Dashboards | `hfc_survey_dashboard()` | Survey-level rates: consent, missing, don't know, refusals, duration |
| | `hfc_enum_dashboard()` | The same rates and productivity by enumerator and team |
| Tracking | `hfc_track_survey()` | Progress against master data or targets |
| Back checks | `hfc_backcheck()` | Survey vs back-check comparison, error rates and statistical tests |
| Helpers | `hfc_collate_scto()` | Collates SurveyCTO comment/text-audit CSV files into one dataset |

Each function's arguments are documented in the comment block above it in `R/<function>.R`.
The shared helpers in `R/00_utils.R` cover Stata-style varlists, missing codes, Excel formatting and expression evaluation.

## Conventions

* **Settings**: all settings live in the list `gl` (`gl$id`, `gl$enum`, `gl$run_outliers`, …) in `1_globals.R`.
* **Varlists**: arguments take Stata-style variable lists as strings, e.g. `"hh_*"`, `"q1-q10"`, `"f_age_r?"`.
* **Missing codes**: "don't know" and "refused" codes become the extended missing values `.d` and `.r`, stored as
  haven tagged NAs. Saved `.dta` files keep them.
* **Expressions**: rules in the *logic* and *constraints* sheets are written in Stata syntax. They are evaluated
  with Stata semantics: a missing value is larger than any number, and `.` < `.d` < `.r`.
* **Return values**: checks that only report return their results invisibly as a list, e.g. `N_outliers`.

## Behaviour notes

Some behaviours carried over from the original code are marked `NOTE(stata-bug)` in the source.
The ones most likely to matter:

* **`hfc_outliers()`**: the IQR method always uses 1.5 × IQR, whatever multiplier the sheet sets.
  Set `use_multiplier = TRUE` to apply your multiplier.
* **`hfc_backcheck()`**:
  * `nodiffstr` only takes effect when `excludestr` is also set.
  * The variable-stats sheets always show value labels.
  * `bcteam` must be a different variable from `enumteam`.
* **`hfc_enum_dashboard()`**: the `sd` column of the *enumstats* sheet holds the median.
* **Weeks**: dashboards number weeks within the year (1–52 from 1 January).

## Credits and licence

MIT licence (see `LICENSE`). Third-party licence terms for adapted code, templates and
exercise data are in `LICENSE_ipacheck_original`, which must be kept with this toolkit.
The Excel templates and exercise data in `inst/` are covered by those terms.
