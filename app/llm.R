# ==============================================================================
#  llm.R — optional AI review for the HFC Checker
#
#  Two tasks, both run only when the user supplies their own Anthropic API key:
#   * Other-specify review: suggests whether each "other, specify" answer belongs
#     to an existing choice, is a genuine new category, or is not a real answer.
#   * Supervisor briefing: turns the issue counts and ML flags per enumerator into
#     short follow-up notes for field supervisors. Sends counts only, no answers.
#
#  The API call is made from the browser (JavaScript below). The key is read from
#  a plain HTML field and never passed to R or stored.
# ==============================================================================

hfc_llm_models <- c("Claude Haiku (fast, low cost)" = "claude-haiku-5-5",
                    "Claude Sonnet (more careful)" = "claude-sonnet-5-5")

# Unique other-specify answers with the question and its existing choices
hfc_llm_specify_items <- function(r, max_items = 150) {
  os <- r$sheets[["other specify"]]; d <- r$data
  if (is.null(os) || !nrow(os)) return(NULL)
  os <- os[!is.na(os[["child value"]]) & nzchar(trimws(os[["child value"]])), ]
  key <- paste(os[["parent variable"]], tolower(trimws(os[["child value"]])))
  os$n <- as.integer(table(key)[key]); os <- os[!duplicated(key), ]
  os <- os[order(-os$n), ][seq_len(min(nrow(os), max_items)), ]
  choices <- lapply(os[["parent variable"]], function(v) {
    lab <- attr(d[[v]], "labels"); if (is.null(lab)) return(character(0))
    lab <- lab[!(haven::is_tagged_na(lab) | is.na(lab))]; names(lab)
  })
  q <- ifelse(is.na(os[["parent label"]]), os[["parent variable"]], os[["parent label"]])
  q <- trimws(gsub("\\s+", " ", gsub("<[^>]+>|\\$\\{[^}]*\\}", "", q)))
  data.frame(i = seq_len(nrow(os)), question = q,
             variable = os[["parent variable"]], answer = os[["child value"]], times = os$n,
             choices = vapply(choices, function(x) paste(x, collapse = " | "), ""), stringsAsFactors = FALSE)
}

hfc_llm_specify_prompt <- function(items) {
  rows <- vapply(split(items, items$variable), function(g)
    sprintf('{"question":%s,"choices":%s,"answers":[%s]}', .json_str(g$question[1]), .json_str(g$choices[1]),
            paste(sprintf('{"i":%d,"answer":%s}', g$i, .json_str(g$answer)), collapse = ",")), "")
  paste0(
    "You are helping a survey data manager clean 'other, specify' answers from a household survey.\n",
    "For each item, decide:\n",
    "- \"recode\": the answer means one of the existing choices (give that choice exactly as written)\n",
    "- \"new category\": a genuine answer that no existing choice covers (suggest a short category name)\n",
    "- \"not an answer\": gibberish, a test entry, 'none', 'no', or text unrelated to the question\n",
    "- \"unclear\": cannot tell without more context\n",
    "Answers may be in English, Swahili or another local language; translate in your reason when needed.\n",
    "Return ONLY a JSON array, one object per item: ",
    "{\"i\": <number>, \"action\": <one of the four>, \"choice\": <existing choice or suggested category or \"\">, \"reason\": <under 15 words>}.\n\n",
    "Questions, each with its existing choices (separated by |) and the answers to classify:\n[", paste(rows, collapse = ",\n"), "]")
}

hfc_llm_briefing_prompt <- function(enums, interviews = NULL, top = 8) {
  e <- utils::head(enums, top)
  if (!is.null(interviews)) {
    rs <- tapply(interviews$`main reasons`[interviews$flagged], interviews$enumerator[interviews$flagged],
                 function(x) paste(utils::head(unique(x), 3), collapse = " / "))
    e$`unusual interview reasons` <- as.character(rs[as.character(e$enumerator)])
  }
  tbl <- utils::capture.output(print(e, row.names = FALSE))
  paste0(
    "You are a survey field coordinator. Below are high-frequency check results per enumerator ",
    "(counts of issues found in their interviews, rates, and reasons a model flagged some interviews as unusual).\n",
    "Write a short briefing for field supervisors: for each enumerator listed, 2-3 sentences on what stands out ",
    "and one concrete follow-up action (for example back-check specific interviews, retrain on a module, or ",
    "accompany them in the field). Be factual, do not accuse anyone of fraud, and note when the numbers are small. ",
    "Use a heading per enumerator in Markdown. Finish with a 3-bullet summary of team-wide patterns.\n\n",
    paste(tbl, collapse = "\n"))
}

.json_str <- function(x) {
  x <- gsub("\\\\", "\\\\\\\\", as.character(x)); x <- gsub('"', '\\\\"', x)
  x <- gsub("[\r\n\t]+", " ", x); paste0('"', x, '"')
}

# Browser side: calls the Anthropic Messages API and returns text (or parsed JSON) to Shiny
hfc_llm_js <- function() tags$script(HTML("
(function () {
  function extractJSON(t) { var a = t.indexOf('['), b = t.lastIndexOf(']'); return (a >= 0 && b > a) ? JSON.parse(t.slice(a, b + 1)) : null; }
  Shiny.addCustomMessageHandler('hfc_llm', async function (msg) {
    var keyEl = document.getElementById('llm_key'), key = keyEl ? keyEl.value.trim() : '';
    var done = function (o) { o.task = msg.task; o.nonce = Math.random(); Shiny.setInputValue('llm_result', o); };
    if (!key) { done({ error: 'Enter your Anthropic API key first.' }); return; }
    try {
      var res = await fetch('https://api.anthropic.com/v1/messages', {
        method: 'POST',
        headers: { 'x-api-key': key, 'anthropic-version': '2023-06-01', 'content-type': 'application/json',
                   'anthropic-dangerous-direct-browser-access': 'true' },
        body: JSON.stringify({ model: msg.model, max_tokens: msg.max_tokens || 8000,
                               messages: [{ role: 'user', content: msg.prompt }] })
      });
      var j = await res.json();
      if (!res.ok) throw new Error((j.error && j.error.message) || ('HTTP ' + res.status));
      var text = j.content.map(function (c) { return c.text || ''; }).join('');
      if (msg.json) { var arr = extractJSON(text); if (!arr) throw new Error('The model did not return a list.'); done({ items: JSON.stringify(arr) }); }
      else done({ text: text });
    } catch (e) { done({ error: String(e.message || e) }); }
  });
})();
"))

hfc_llm_parse_items <- function(json, items) {
  # minimal JSON array-of-flat-objects parser (avoids a jsonlite dependency in the browser build)
  objs <- regmatches(json, gregexpr("\\{[^{}]*\\}", json))[[1]]
  get <- function(o, k) {
    m <- regmatches(o, regexec(paste0('"', k, '"\\s*:\\s*("((?:[^"\\\\]|\\\\.)*)"|[-0-9.]+)'), o))[[1]]
    if (!length(m)) return(NA_character_)
    v <- if (startsWith(m[2], '"')) m[3] else m[2]; gsub('\\\\"', '"', v)
  }
  res <- data.frame(i = as.integer(vapply(objs, get, "", k = "i")), action = vapply(objs, get, "", k = "action"),
                    choice = vapply(objs, get, "", k = "choice"), reason = vapply(objs, get, "", k = "reason"),
                    stringsAsFactors = FALSE)
  out <- merge(items[, c("i", "variable", "question", "answer", "times")], res, by = "i", all.x = TRUE)
  out$i <- NULL
  names(out) <- c("variable", "question", "answer", "times given", "suggestion", "choice / category", "reason")
  out[order(out$suggestion, -out$`times given`), ]
}

# ---- Evaluating the other-specify review against hand coding ---------------------
# A person codes a blank sheet without seeing the AI's suggestions (blind coding);
# the app then compares the two and reports agreement and Cohen's kappa.

hfc_llm_actions <- c("recode", "new category", "not an answer", "unclear")

hfc_llm_coding_sheet <- function(items) {
  data.frame(variable = items$variable, question = items$question, existing_choices = items$choices,
             answer = items$answer, times_given = items$times, human_action = "", human_choice = "",
             stringsAsFactors = FALSE)
}

.norm_action <- function(x) {
  x <- tolower(trimws(as.character(x)))
  x[grepl("^recod", x)] <- "recode"
  x[grepl("^new", x)] <- "new category"
  x[grepl("not.*answer|^no answer|^invalid|^gibberish", x)] <- "not an answer"
  x[grepl("^unclear|^unsure|^don", x)] <- "unclear"
  x[!x %in% hfc_llm_actions] <- NA
  x
}

hfc_llm_evaluate <- function(ai, coded) {
  key <- function(v, a) paste(v, tolower(trimws(a)), sep = "\r")
  names(coded) <- tolower(gsub("[^A-Za-z]+", "_", names(coded)))
  need <- c("variable", "answer", "human_action")
  if (!all(need %in% names(coded)))
    return(list(error = "The coded sheet needs the columns variable, answer and human_action. Use the blank coding sheet from this tab."))
  h <- data.frame(k = key(coded$variable, coded$answer), human = .norm_action(coded$human_action),
                  human_choice = if ("human_choice" %in% names(coded)) as.character(coded$human_choice) else "",
                  stringsAsFactors = FALSE)
  h <- h[!is.na(h$human), ]
  a <- data.frame(k = key(ai$variable, ai$answer), variable = ai$variable, answer = ai$answer,
                  ai = .norm_action(ai$suggestion), ai_choice = as.character(ai$`choice / category`),
                  stringsAsFactors = FALSE)
  m <- merge(a, h, by = "k")
  m <- m[!is.na(m$ai), ]
  if (nrow(m) < 10) return(list(error = sprintf("Only %d coded answers match the AI review. Code at least 10 (100 or more gives a reliable figure).", nrow(m))))
  lv <- hfc_llm_actions
  cm <- table(factor(m$human, lv), factor(m$ai, lv), dnn = c("Human", "AI"))
  po <- sum(diag(cm)) / sum(cm); pe <- sum(rowSums(cm) * colSums(cm)) / sum(cm)^2
  kappa <- if (pe < 1) (po - pe) / (1 - pe) else NA_real_
  per <- data.frame(action = lv, `human count` = as.integer(rowSums(cm)), `AI count` = as.integer(colSums(cm)),
                    precision = round(ifelse(colSums(cm) > 0, diag(cm) / colSums(cm), NA), 2),
                    recall = round(ifelse(rowSums(cm) > 0, diag(cm) / rowSums(cm), NA), 2), check.names = FALSE)
  both_recode <- m$human == "recode" & m$ai == "recode" & nzchar(trimws(m$human_choice))
  choice_agree <- if (any(both_recode)) mean(tolower(trimws(m$human_choice[both_recode])) == tolower(trimws(m$ai_choice[both_recode]))) else NA_real_
  dis <- m[m$human != m$ai | (both_recode & tolower(trimws(m$human_choice)) != tolower(trimws(m$ai_choice))),
           c("variable", "answer", "human", "human_choice", "ai", "ai_choice")]
  cmdf <- as.data.frame.matrix(cm); cmdf <- cbind(`Human ↓ / AI →` = rownames(cmdf), cmdf)
  list(n = nrow(m), accuracy = round(po, 3), kappa = round(kappa, 3), choice_agreement = round(choice_agree, 3),
       n_choice = sum(both_recode), confusion = cmdf, per_action = per, disagreements = dis)
}
