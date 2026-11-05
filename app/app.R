# ==============================================================================
#  HFC Checker — upload survey data, get back the issues to follow up.
#  Front end for hfc-toolkit.
#  Runs as a normal Shiny app or in the browser with Shinylive (webR).
# ==============================================================================

library(shiny)
library(bslib)
if (FALSE) { library(haven); library(readxl); library(openxlsx) }   # tells Shinylive which packages to bundle

app_dir <- if (dir.exists("hfc")) "hfc" else "../R"        # hfc/ is filled by the build step
source(if (file.exists("engine.R")) "engine.R" else "app/engine.R")
for (f in c("ml.R", "llm.R")) source(if (file.exists(f)) f else file.path("app", f))
hfc_app_load_toolkit(app_dir)
sample_dir <- if (dir.exists("sample")) "sample" else "../inst"
in_browser <- grepl("emscripten", R.version$platform, fixed = TRUE)
options(shiny.maxRequestSize = 200 * 1024^2)

`%or%` <- function(a, b) if (is.null(a) || !length(a) || identical(a, "")) b else a

# ---- issues list ---------------------------------------------------------------
excel_date <- function(x) {
  if (inherits(x, c("Date", "POSIXt"))) return(format(as.Date(x)))
  n <- suppressWarnings(as.numeric(x))
  ifelse(!is.na(n) & n > 20000 & n < 80000, format(as.Date(n, origin = "1899-12-30")), as.character(x))
}

build_issues <- function(r) {
  m <- r$map; s <- r$sheets
  col <- function(df, nm) if (!is.null(nm) && nm %in% names(df)) as.character(df[[nm]]) else rep("", nrow(df))
  mk <- function(df, check, variable, value, detail, severity) {
    if (is.null(df) || !nrow(df)) return(NULL)
    data.frame(check = check, severity = severity, id = col(df, m$id), enumerator = col(df, m$enum),
               date = excel_date(col(df, m$date)), variable = variable, value = value, detail = detail,
               stringsAsFactors = FALSE)
  }
  out <- list()
  d <- s[["id duplicates"]]
  if (!is.null(d) && nrow(d)) out$ids <- mk(d, "Duplicate ID", m$id, col(d, m$key),
      ifelse(is.na(d[["% different"]]), "first submission of this ID",
             sprintf("%s of %s answers differ from the first submission", d[["# different"]], d[["# compared"]])), "High")
  d <- s[["outdated"]]
  if (!is.null(d) && nrow(d)) out$ver <- mk(d, "Outdated form version", m$formversion, col(d, m$formversion),
      "submitted on an old version of the form", "Medium")
  d <- s[["duplicates"]]
  if (!is.null(d) && nrow(d)) out$dup <- mk(d, "Duplicate value", col(d, "variable"), col(d, "value"),
      "same value appears in another submission", "High")
  d <- s[["constraints"]]
  if (!is.null(d) && nrow(d)) out$con <- mk(d, "Constraint violation", col(d, "variable"), col(d, "value"), col(d, "constraint"),
      ifelse(grepl("hard", col(d, "constraint")), "High", "Medium"))
  d <- s[["logic"]]
  if (!is.null(d) && nrow(d)) out$log <- mk(d, "Logic violation", col(d, "variable"), col(d, "value"), col(d, "logic"), "High")
  d <- s[["outliers"]]
  if (!is.null(d) && nrow(d)) out$out <- mk(d, "Outlier", col(d, "variable"), col(d, "value"), col(d, "range"), "Medium")
  d <- s[["other specify"]]
  if (!is.null(d) && nrow(d)) out$os <- mk(d, "Other, specify", col(d, "child variable"), col(d, "child value"),
      "check whether it fits an existing choice", "Low")
  res <- do.call(rbind, out)
  if (is.null(res)) return(data.frame(check = character(), severity = character(), id = character(),
                                      enumerator = character(), date = character(), variable = character(),
                                      value = character(), detail = character()))
  res$severity <- factor(res$severity, c("High", "Medium", "Low"))
  res <- res[order(res$severity, res$check, res$enumerator, res$id), ]
  rownames(res) <- NULL
  res
}

enum_table <- function(r, issues) {
  m <- r$map
  subs <- table(as.character(r$data[[m$enum]]))
  checks <- unique(as.character(issues$check))
  e <- data.frame(enumerator = names(subs), submissions = as.integer(subs), stringsAsFactors = FALSE)
  for (ck in checks) {
    t <- table(issues$enumerator[issues$check == ck])
    e[[ck]] <- as.integer(t[e$enumerator]); e[[ck]][is.na(e[[ck]])] <- 0L
  }
  hi <- issues$severity == "High"
  e$`high-priority issues` <- as.integer(table(issues$enumerator[hi])[e$enumerator]); e$`high-priority issues`[is.na(e$`high-priority issues`)] <- 0L
  e$`issues per submission` <- round(rowSums(e[, setdiff(names(e), c("enumerator", "submissions", "high-priority issues")), drop = FALSE]) / e$submissions, 2)
  db <- r$enumdb[["summary"]]
  if (!is.null(db) && m$enum %in% names(db)) {
    keep <- intersect(c("% missing", "% dont know", "% refuse", "median duration"), names(db))
    x <- db[, c(m$enum, keep), drop = FALSE]; names(x)[1] <- "enumerator"
    for (k in intersect(c("% missing", "% dont know", "% refuse"), keep)) x[[k]] <- round(100 * as.numeric(x[[k]]), 1)
    if ("median duration" %in% keep) { x[["median minutes"]] <- round(as.numeric(x[["median duration"]]) / 60, 1); x[["median duration"]] <- NULL }
    x$enumerator <- as.character(x$enumerator)
    e <- merge(e, x, by = "enumerator", all.x = TRUE)
  }
  e[order(-e$`issues per submission`, -e$`high-priority issues`), ]
}

# ---- UI ---------------------------------------------------------------------------
theme <- bs_theme(version = 5, bg = "#F3F6F3", fg = "#15211C", primary = "#1E6B52",
                  base_font = font_collection("IBM Plex Sans", "Segoe UI", "system-ui", "sans-serif"),
                  heading_font = font_collection("Bricolage Grotesque", "Segoe UI", "system-ui", "sans-serif"),
                  "border-radius" = "8px")

col_select <- function(id, label, help = NULL) {
  tagList(selectInput(id, label, choices = c("(none)" = ""), width = "100%"),
          if (!is.null(help)) div(class = "form-text mt-n2 mb-2", help))
}

ui <- page_sidebar(
  title = div(strong("HFC Checker"), span(class = "text-muted ms-2 small", "high-frequency checks for survey data")),
  theme = theme,
  window_title = "HFC Checker",
  sidebar = sidebar(width = 360, open = "always",
    h6("1. Survey data"),
    fileInput("data", NULL, accept = c(".dta", ".csv", ".xlsx", ".xls", ".rds"),
              buttonLabel = "Upload", placeholder = ".dta, .csv or .xlsx"),
    actionLink("sample", "Or load a sample survey (917 submissions)"),
    hr(),
    h6("2. Check settings (optional)"),
    fileInput("inputs", NULL, accept = c(".xlsm", ".xlsx"), buttonLabel = "Upload", placeholder = "hfc_inputs workbook"),
    div(class = "form-text mb-2", "Adds outlier, constraint, logic and other-specify checks. ",
        downloadLink("template", "Download the blank template.")),
    hr(),
    h6("3. Match your columns"),
    col_select("id", "Respondent ID"),
    col_select("enum", "Enumerator"),
    col_select("date", "Interview date or start time"),
    col_select("key", "Submission key", "Unique per submission. Leave blank to use row numbers."),
    accordion(open = FALSE, multiple = TRUE,
      accordion_panel("More columns",
        col_select("team", "Team"),
        col_select("duration", "Duration (seconds)"),
        col_select("consent", "Consent"),
        textInput("cons_vals", "Consent given when value is", "1"),
        col_select("formversion", "Form version"),
        selectizeInput("dupvars", "Check for repeated values", choices = NULL, multiple = TRUE,
                       options = list(placeholder = "e.g. GPS, phone number"))),
      accordion_panel("Missing-value codes",
        textInput("dk_num", "Don't know (numeric)", "-999 .999"),
        textInput("ref_num", "Refused (numeric)", "-888 .888"),
        textInput("dk_str", "Don't know (text)", "-999"),
        textInput("ref_str", "Refused (text)", "-888"),
        checkboxInput("compat", "Classic outlier rule (always 1.5 × IQR)", FALSE))
    ),
    actionButton("run", "Run checks", icon = icon("play"), class = "btn-primary w-100 mt-2"),
    div(class = "form-text mt-3",
        if (in_browser) "The checks run in your browser: your data is not uploaded to a server. The optional AI review sends only what it reviews to Anthropic's API."
        else "Files are processed for this session only and are not stored.")
  ),
  hfc_llm_js(),
  uiOutput("main")
)

# ---- server -------------------------------------------------------------------------
server <- function(input, output, session) {
  rv <- reactiveValues(data = NULL, name = NULL, inputs = NULL, result = NULL, issues = NULL, error = NULL)

  set_columns <- function(d) {
    g <- hfc_app_guess(d); ch <- c("(none)" = "", names(d))
    for (k in c("id", "enum", "date", "key", "team", "duration", "consent", "formversion"))
      updateSelectInput(session, k, choices = ch, selected = g[[k]])
    updateSelectizeInput(session, "dupvars", choices = names(d), selected = strsplit(g$gps, " ")[[1]], server = TRUE)
  }

  observeEvent(input$data, {
    rv$error <- NULL; rv$result <- NULL
    d <- tryCatch(hfc_app_read(input$data$datapath, input$data$name), error = function(e) { rv$error <- conditionMessage(e); NULL })
    if (!is.null(d)) { rv$data <- d; rv$name <- input$data$name; set_columns(d) }
  })
  observeEvent(input$sample, {
    rv$error <- NULL; rv$result <- NULL
    d <- hfc_app_read(file.path(sample_dir, "data/household_survey.dta"))
    rv$data <- d; rv$name <- "Sample household survey (household_survey.dta)"
    rv$inputs <- file.path(sample_dir, "excel/exercise/hfc_inputs_exercise.xlsm")
    set_columns(d)
    updateTextInput(session, "dk_num", value = "-999 999 .999"); updateTextInput(session, "ref_num", value = "-888 888 .888")
  })
  observeEvent(input$inputs, {
    f <- file.path(tempdir(), paste0("inputs_", basename(input$inputs$name)))
    file.copy(input$inputs$datapath, f, overwrite = TRUE); rv$inputs <- f
  })

  observeEvent(input$run, {
    req(rv$data)
    rv$error <- NULL
    map <- list(key = input$key, id = input$id, enum = input$enum, team = input$team, date = input$date,
                duration = input$duration, consent = input$consent, formversion = input$formversion,
                dupvars = paste(input$dupvars, collapse = " "))
    opts <- list(dk_num = input$dk_num, ref_num = input$ref_num, dk_str = input$dk_str, ref_str = input$ref_str,
                 cons_vals = input$cons_vals, classic_outliers = input$compat)
    r <- tryCatch(suppressMessages(hfc_app_run(rv$data, map, inputs = rv$inputs, opts = opts)),
                  error = function(e) { rv$error <- conditionMessage(e); NULL })
    if (!is.null(r)) {
      rv$result <- r; iss <- build_issues(r)
      rv$iv <- tryCatch(hfc_ml_interviews(r, iss), error = function(e) NULL)
      fl <- if (is.null(rv$iv)) NULL else rv$iv[rv$iv$flagged, ]
      if (!is.null(fl) && nrow(fl)) {
        iss$severity <- as.character(iss$severity)
        iss <- rbind(iss, data.frame(check = "Unusual interview (ML)", severity = "Medium", id = fl$id,
                                     enumerator = fl$enumerator, date = fl$date, variable = "",
                                     value = sprintf("score %.2f", fl$`anomaly score`), detail = fl$`main reasons`,
                                     stringsAsFactors = FALSE))
        iss$severity <- factor(iss$severity, c("High", "Medium", "Low"))
        iss <- iss[order(iss$severity, iss$check, iss$enumerator, iss$id), ]; rownames(iss) <- NULL
      }
      rv$issues <- iss
      rv$ev <- tryCatch(hfc_ml_enumerators(r, rv$iv), error = function(e) NULL)
      rv$enums <- enum_table(r, rv$issues)
      rv$llm_os <- NULL; rv$llm_brief <- NULL; rv$llm_status <- NULL; rv$coded <- NULL
    }
  })

  output$template <- downloadHandler("hfc_inputs.xlsm", function(file)
    file.copy(file.path(sample_dir, "excel/templates/hfc_inputs.xlsm"), file))
  output$dl_issues <- downloadHandler(function() paste0("hfc_issues_", Sys.Date(), ".csv"),
    function(file) utils::write.csv(rv$issues, file, row.names = FALSE, na = ""))
  output$dl_hfc <- downloadHandler(function() paste0("hfc_output_", Sys.Date(), ".xlsx"),
    function(file) file.copy(rv$result$files[["hfc"]], file))
  output$dl_enum <- downloadHandler(function() paste0("enumdb_", Sys.Date(), ".xlsx"),
    function(file) file.copy(rv$result$files[["enumdb"]], file))

  output$main <- renderUI({
    if (!is.null(rv$error)) return(card(class = "border-danger", card_body(strong("The checks could not run. "), rv$error)))
    if (is.null(rv$data)) return(intro_ui())
    if (is.null(rv$result)) return(card(card_body(
      h4(rv$name), p(sprintf("%s submissions, %s variables.", format(nrow(rv$data), big.mark = ","), ncol(rv$data))),
      p(class = "text-muted", "Check the column matches on the left, then run the checks."),
      if (!is.null(rv$inputs)) p(icon("check"), " Check settings workbook loaded.") else
        p(class = "text-muted", "No check settings workbook: outlier, constraint, logic and other-specify checks will be skipped."))))
    results_ui()
  })

  intro_ui <- function() card(card_body(class = "p-4",
    h3("Find the survey problems to follow up today"),
    p("Upload the data your enumerators have submitted so far. The checker flags duplicate IDs, outdated forms,",
      "repeated GPS or phone values, missing data, outliers, constraint and logic violations, and other-specify",
      "answers, then ranks enumerators by how many issues their interviews produce."),
    p("Built on hfc-toolkit, an R package for high-frequency checks. The same checks run in R scripts",
      "and produce formatted Excel reports for field teams."),
    p(class = "text-muted mb-0", "No data to hand? Load the sample survey from the sidebar.")))

  results_ui <- function() {
    r <- rv$result; iss <- rv$issues
    n <- function(ck) sum(iss$check == ck)
    tiles <- list(
      value_box("Submissions checked", format(r$n, big.mark = ","), if (r$n_dup_dropped) sprintf("%d repeat IDs set aside", r$n_dup_dropped)),
      value_box("High-priority issues", format(sum(iss$severity == "High"), big.mark = ","), "duplicate IDs and values, hard limits, logic", theme = "danger"),
      value_box("To review", format(sum(iss$severity != "High"), big.mark = ","), "soft limits, outliers, old forms, other-specify", theme = "warning"),
      value_box("Enumerators to follow up", sum(rv$enums$`issues per submission` > stats::median(rv$enums$`issues per submission`) * 2 & rv$enums$`issues per submission` > 0),
                "issue rate over twice the median"))
    skipped <- if (length(r$errors)) div(class = "alert alert-warning",
      strong("Not run: "), paste(sprintf("%s (%s)", names(r$errors), unlist(r$errors)), collapse = "; "))
    tagList(
      layout_column_wrap(width = "210px", fill = FALSE, !!!tiles),
      skipped,
      navset_card_tab(
        nav_panel("Issues to follow up",
          div(class = "d-flex flex-wrap gap-2 mb-2 align-items-end",
            selectInput("f_check", NULL, c("All checks" = "", sort(unique(as.character(iss$check)))), width = "220px"),
            selectInput("f_enum", NULL, c("All enumerators" = "", sort(unique(iss$enumerator))), width = "200px"),
            div(class = "ms-auto", downloadButton("dl_issues", "Issues list (CSV)", class = "btn-sm"),
                downloadButton("dl_hfc", "Full HFC report (Excel)", class = "btn-sm"))),
          tableOutput("t_issues"), uiOutput("issues_note")),
        nav_panel("By enumerator",
          p(class = "text-muted small", "Issues per enumerator from the checks above, with dashboard rates. Sorted by issues per submission."),
          div(class = "mb-2", downloadButton("dl_enum", "Enumerator dashboard (Excel)", class = "btn-sm")),
          div(class = "table-responsive", tableOutput("t_enum"))),
        nav_panel("Unusual interviews (ML)",
          p(class = "text-muted small", "An isolation forest scores every interview on its overall pattern: length, don't know and",
            "refused answers, issues found by the checks, start time, how typical the answers are, how closely it matches",
            "another interview by the same enumerator, and distance from the enumerator's usual area. It finds interviews",
            "that pass every single rule but look unlike the rest. Flagged: the top 5%, plus near-identical copies."),
          if (is.null(rv$iv)) p("Not enough data to score interviews.") else div(class = "table-responsive", tableOutput("t_iv"))),
        nav_panel("Enumerator patterns (ML)",
          p(class = "text-muted small", "How far each enumerator's answers sit from everyone else's, averaged over the closed",
            "questions (Jensen-Shannon divergence). Large differences can mean fabrication, leading questions or a",
            "misunderstood question, so they are a reason to look, not proof. Review when the divergence z-score is 3 or",
            "more, or when 20% or more of their interviews are flagged."),
          if (is.null(rv$ev)) p("Needs at least three enumerators with 10 or more interviews.") else div(class = "table-responsive", tableOutput("t_ev"))),
        nav_panel("AI review", ai_ui()),
        nav_panel("Missing data",
          p(class = "text-muted small", "Variables with missing values, most missing first."),
          div(class = "table-responsive", tableOutput("t_missing"))),
        nav_panel("Survey summary", div(class = "table-responsive", tableOutput("t_survey"))),
        nav_panel("All report sheets",
          selectInput("sheet", NULL, names(r$sheets), width = "300px"),
          div(class = "table-responsive", tableOutput("t_sheet")))
      )
    )
  }

  ai_ui <- function() tagList(
    p("Optional. Uses a large language model through your own Anthropic API key. The key stays in this browser",
      "tab and is sent only to Anthropic."),
    div(class = "d-flex flex-wrap gap-2 align-items-end mb-3",
      div(tags$label(`for` = "llm_key", class = "form-label small mb-1", "Anthropic API key"),
          tags$input(id = "llm_key", type = "password", class = "form-control", style = "width:300px",
                     autocomplete = "off", placeholder = "sk-ant-...")),
      selectInput("llm_model", "Model", hfc_llm_models, width = "260px")),
    div(class = "d-flex flex-wrap gap-2 mb-2",
      actionButton("llm_os", "Review other-specify answers", class = "btn-outline-primary btn-sm"),
      actionButton("llm_brief", "Write supervisor briefing", class = "btn-outline-primary btn-sm")),
    div(class = "form-text mb-3",
      "Other-specify review sends the question text, its choices and the typed answers (no IDs or names columns,",
      "though respondents sometimes type names into answers). The briefing sends only issue counts per enumerator."),
    uiOutput("llm_status"),
    uiOutput("llm_os_ui"),
    uiOutput("llm_brief_ui"),
    hr(),
    h5("Evaluate the AI review"),
    p(class = "text-muted small", "Measure how often the AI agrees with a person. Download the blank coding sheet and code each",
      "answer yourself before looking at the AI's suggestions (human_action: recode, new category, not an answer or unclear;",
      "human_choice: the choice or category name). Then upload it here. About 100-200 answers gives a reliable figure."),
    div(class = "d-flex flex-wrap gap-2 align-items-end",
      downloadButton("dl_coding_sheet", "Blank coding sheet (CSV)", class = "btn-sm"),
      fileInput("coded", NULL, accept = ".csv", buttonLabel = "Upload coded sheet", placeholder = "coded CSV", width = "320px")),
    uiOutput("llm_eval_ui"))

  observeEvent(input$llm_os, {
    items <- hfc_llm_specify_items(rv$result)
    if (is.null(items)) { rv$llm_status <- "No other-specify answers to review. Upload a check settings workbook with an 'other specify' sheet."; return() }
    rv$llm_items <- items; rv$llm_status <- sprintf("Reviewing %d distinct answers...", nrow(items))
    session$sendCustomMessage("hfc_llm", list(task = "os", model = input$llm_model, json = TRUE,
                                              prompt = hfc_llm_specify_prompt(items), max_tokens = 12000))
  })
  observeEvent(input$llm_brief, {
    req(rv$enums); rv$llm_status <- "Writing the briefing..."
    session$sendCustomMessage("hfc_llm", list(task = "brief", model = input$llm_model, json = FALSE,
                                              prompt = hfc_llm_briefing_prompt(rv$enums, rv$iv), max_tokens = 3000))
  })
  observeEvent(input$llm_result, {
    x <- input$llm_result
    if (!is.null(x$error)) { rv$llm_status <- paste("The AI review did not run:", x$error); return() }
    rv$llm_status <- NULL
    if (identical(x$task, "os")) rv$llm_os <- hfc_llm_parse_items(x$items, rv$llm_items) else rv$llm_brief <- x$text
  })
  output$llm_status <- renderUI(if (!is.null(rv$llm_status)) div(class = "alert alert-info py-2", rv$llm_status))
  output$llm_os_ui <- renderUI({ req(rv$llm_os); tagList(h5("Other-specify suggestions"),
    p(class = "text-muted small", "Suggestions to check before recoding. Paste the ones you accept into the specifyrecode workbook."),
    downloadButton("dl_llm_os", "Suggestions (CSV)", class = "btn-sm mb-2"),
    div(class = "table-responsive", tableOutput("t_llm_os"))) })
  output$t_llm_os <- renderTable(rv$llm_os, striped = TRUE, spacing = "s", na = "")
  output$dl_coding_sheet <- downloadHandler(function() paste0("other_specify_coding_sheet_", Sys.Date(), ".csv"), function(file) {
    items <- hfc_llm_specify_items(rv$result, max_items = 300)
    utils::write.csv(if (is.null(items)) data.frame(note = "No other-specify answers found") else hfc_llm_coding_sheet(items),
                     file, row.names = FALSE, na = "")
  })
  observeEvent(input$coded, {
    rv$coded <- tryCatch(utils::read.csv(input$coded$datapath, stringsAsFactors = FALSE, check.names = FALSE),
                         error = function(e) data.frame())
  })
  llm_eval <- reactive({ req(rv$coded); if (is.null(rv$llm_os)) return(list(error = "Run the other-specify review first, then upload the coded sheet.")); hfc_llm_evaluate(rv$llm_os, rv$coded) })
  output$llm_eval_ui <- renderUI({
    req(rv$coded); e <- llm_eval()
    if (!is.null(e$error)) return(div(class = "alert alert-warning py-2", e$error))
    tagList(
      div(class = "alert alert-success py-2", HTML(sprintf(
        "On <b>%d</b> hand-coded answers, the AI agreed with the human coder on <b>%.0f%%</b> (Cohen's kappa <b>%.2f</b>).%s",
        e$n, 100 * e$accuracy, e$kappa,
        if (is.na(e$choice_agreement)) "" else sprintf(" Where both chose to recode, they picked the same choice %.0f%% of the time (%d answers).", 100 * e$choice_agreement, e$n_choice)))),
      downloadButton("dl_eval", "Evaluation results (CSV)", class = "btn-sm mb-2"),
      h6("Human vs AI decisions"), tableOutput("t_eval_cm"),
      h6("By decision"), tableOutput("t_eval_per"),
      h6(sprintf("Disagreements (%d)", nrow(e$disagreements))), div(class = "table-responsive", tableOutput("t_eval_dis")))
  })
  output$t_eval_cm <- renderTable(llm_eval()$confusion, spacing = "s")
  output$t_eval_per <- renderTable(llm_eval()$per_action, spacing = "s", na = "")
  output$t_eval_dis <- renderTable(llm_eval()$disagreements, striped = TRUE, spacing = "s", na = "")
  output$dl_eval <- downloadHandler(function() paste0("ai_review_evaluation_", Sys.Date(), ".csv"), function(file) {
    e <- llm_eval(); d <- e$disagreements
    utils::write.csv(rbind(data.frame(variable = "SUMMARY", answer = sprintf("n=%d; agreement=%.3f; kappa=%.3f", e$n, e$accuracy, e$kappa),
                                      human = "", human_choice = "", ai = "", ai_choice = ""), d), file, row.names = FALSE, na = "")
  })
  output$dl_llm_os <- downloadHandler(function() paste0("other_specify_suggestions_", Sys.Date(), ".csv"),
    function(file) utils::write.csv(rv$llm_os, file, row.names = FALSE, na = ""))
  output$llm_brief_ui <- renderUI({ req(rv$llm_brief); tagList(h5("Supervisor briefing"),
    p(class = "text-muted small", "Generated from the counts above. Check it against the tables before sharing."),
    div(class = "border rounded p-3 bg-white", HTML(markdown(rv$llm_brief)))) })
  output$t_iv <- renderTable({ x <- rv$iv; x$flagged <- ifelse(x$flagged, "Yes", ""); utils::head(x, 100) }, striped = TRUE, spacing = "s", na = "")
  output$t_ev <- renderTable(rv$ev, striped = TRUE, spacing = "s", na = "")

  filtered <- reactive({
    x <- rv$issues; req(x)
    if (nzchar(input$f_check %or% "")) x <- x[x$check == input$f_check, ]
    if (nzchar(input$f_enum %or% "")) x <- x[x$enumerator == input$f_enum, ]
    x
  })
  output$t_issues <- renderTable({ x <- filtered(); utils::head(x, 300) }, striped = TRUE, hover = TRUE, spacing = "s", na = "")
  output$issues_note <- renderUI({ n <- nrow(filtered()); if (n > 300) p(class = "text-muted small", sprintf("Showing 300 of %s. Download the CSV for the full list.", format(n, big.mark = ","))) })
  output$t_enum <- renderTable(rv$enums, striped = TRUE, hover = TRUE, spacing = "s", na = "")
  output$t_missing <- renderTable({
    m <- rv$result$sheets[["missing"]]; req(m)
    m <- m[as.numeric(m[["# missing"]]) > 0, ]
    m[["% missing"]] <- round(100 * as.numeric(m[["% missing"]]), 1)
    utils::head(m[order(-as.numeric(m[["% missing"]])), c("variable", "label", "# missing", "% missing", "# distinct")], 200)
  }, striped = TRUE, spacing = "s", na = "")
  output$t_survey <- renderTable(rv$result$surveydb[["summary"]], spacing = "s", na = "")
  output$t_sheet <- renderTable({ s <- rv$result$sheets[[input$sheet]]; req(s); utils::head(s, 300) }, striped = TRUE, spacing = "s", na = "")
}

shinyApp(ui, server)
