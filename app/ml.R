# ==============================================================================
#  ml.R — machine learning layer for the HFC Checker
#
#  1. Interview anomaly score: an isolation forest over per-submission features
#     (duration, don't know / refused rates, issues found by the rule-based
#     checks, interview hour, answer typicality, distance from the enumerator's
#     usual area). Rules catch single bad values; the forest catches interviews
#     whose overall pattern is unlike the rest.
#  2. Enumerator answer patterns: for each enumerator, how far their answer
#     distributions sit from everyone else's (Jensen-Shannon divergence), a
#     common sign of fabrication, leading questions or a misunderstood question.
#
#  Base R only, so it also runs in the browser through webR.
# ==============================================================================

# ---- isolation forest (Liu, Ting & Zhou 2008) -----------------------------------
.if_cn <- function(n) ifelse(n > 2, 2 * (log(n - 1) + 0.5772156649) - 2 * (n - 1) / n, ifelse(n == 2, 1, 0))

.if_tree <- function(X, depth, maxdepth) {
  n <- nrow(X)
  if (depth >= maxdepth || n <= 1) return(list(size = n))
  q <- sample.int(ncol(X), 1); x <- X[, q]; r <- range(x)
  if (r[1] == r[2]) return(list(size = n))
  p <- stats::runif(1, r[1], r[2]); left <- x < p
  list(q = q, p = p, l = .if_tree(X[left, , drop = FALSE], depth + 1, maxdepth),
       r = .if_tree(X[!left, , drop = FALSE], depth + 1, maxdepth))
}

.if_path <- function(x, t) {
  h <- 0
  while (is.null(t$size)) { t <- if (x[t$q] < t$p) t$l else t$r; h <- h + 1 }
  h + .if_cn(t$size)
}

#' Isolation forest anomaly scores in (0, 1); higher = more unusual (> 0.6 is notable)
hfc_iforest <- function(X, ntree = 100, psi = 256, seed = 20261105) {
  X <- as.matrix(X); n <- nrow(X); psi <- min(psi, n)
  old <- if (exists(".Random.seed", globalenv())) get(".Random.seed", globalenv()) else NULL
  set.seed(seed); on.exit(if (!is.null(old)) assign(".Random.seed", old, globalenv()))
  maxdepth <- ceiling(log2(max(psi, 2)))
  trees <- lapply(seq_len(ntree), function(i) .if_tree(X[sample.int(n, psi), , drop = FALSE], 0, maxdepth))
  depth <- vapply(trees, function(t) apply(X, 1, .if_path, t = t), numeric(n))
  if (is.null(dim(depth))) depth <- matrix(depth, nrow = n)
  2^(-rowMeans(depth) / .if_cn(psi))
}

.robust_z <- function(x) {
  m <- stats::median(x, na.rm = TRUE); s <- stats::mad(x, na.rm = TRUE)
  if (!is.finite(s) || s == 0) s <- stats::sd(x, na.rm = TRUE)
  if (!is.finite(s) || s == 0) return(rep(0, length(x)))
  (x - m) / s
}

.km <- function(lat1, lon1, lat2, lon2) {
  r <- pi / 180
  h <- sin((lat2 - lat1) * r / 2)^2 + cos(lat1 * r) * cos(lat2 * r) * sin((lon2 - lon1) * r / 2)^2
  12742 * asin(sqrt(pmin(1, h)))
}

# Columns that look like closed questions: numeric, 2-12 distinct values, mostly answered
.closed_vars <- function(d, exclude) {
  keep <- vapply(names(d), function(v) {
    if (v %in% exclude) return(FALSE)
    x <- unclass(d[[v]]); if (!is.numeric(x)) return(FALSE)
    x <- x[!is.na(x)]
    length(x) >= max(30, 0.3 * nrow(d)) && length(unique(x)) >= 2 && length(unique(x)) <= 12
  }, logical(1))
  names(d)[keep]
}

#' Per-submission features and anomaly scores
hfc_ml_interviews <- function(r, issues, top_share = 0.05) {
  d <- r$data; m <- r$map; n <- nrow(d)
  idv <- as.character(d[[m$id]])
  admin <- unlist(m[c("key", "id", "enum", "team", "date", "duration", "consent", "formversion")])
  admin <- admin[nzchar(admin)]
  f <- data.frame(row.names = seq_len(n))

  if (nzchar(m$duration %||% "")) f$`interview length` <- log1p(pmax(as.numeric(unclass(d[[m$duration]])), 0))
  tagged <- vapply(d, function(x) is.double(unclass(x)), logical(1))
  if (any(tagged)) {
    dd <- d[, tagged, drop = FALSE]
    f$`don't know answers` <- rowSums(vapply(dd, function(x) haven::is_tagged_na(as.vector(unclass(x)), "d"), logical(n)))
    f$`refused answers`    <- rowSums(vapply(dd, function(x) haven::is_tagged_na(as.vector(unclass(x)), "r"), logical(n)))
  }
  if (nrow(issues)) {
    for (ck in intersect(c("Outlier", "Constraint violation", "Logic violation", "Other, specify"), unique(as.character(issues$check)))) {
      t <- table(issues$id[issues$check == ck]); x <- as.numeric(t[idv]); x[is.na(x)] <- 0
      f[[c(Outlier = "outliers", `Constraint violation` = "constraint violations", `Logic violation` = "logic violations",
           `Other, specify` = "other-specify answers")[[ck]]]] <- x
    }
  }
  dt <- d[[m$date]]
  if (inherits(dt, "POSIXt")) {
    h <- as.numeric(format(dt, "%H")) + as.numeric(format(dt, "%M")) / 60
    f$`unusual start hour` <- abs(h - stats::median(h, na.rm = TRUE))
  }
  closed <- .closed_vars(d, admin)
  if (length(closed) >= 5) {
    modal <- vapply(closed, function(v) { x <- unclass(d[[v]]); as.numeric(names(which.max(table(x)))) }, numeric(1))
    same <- vapply(seq_along(closed), function(i) { x <- unclass(d[[closed[i]]]); ifelse(is.na(x), NA, x == modal[i]) }, logical(n))
    f$`answer typicality` <- rowMeans(same, na.rm = TRUE)
    # near-copies: highest share of identical closed answers with another interview by the same enumerator
    A <- vapply(closed, function(v) as.numeric(unclass(d[[v]])), numeric(n))
    en <- as.character(d[[m$enum]]); sim <- rep(NA_real_, n)
    for (g in split(seq_len(n), en)) {
      if (length(g) < 2) next
      B <- A[g, , drop = FALSE]
      for (i in seq_along(g)) {
        both <- !is.na(B) & matrix(!is.na(B[i, ]), nrow(B), ncol(B), byrow = TRUE)
        eq <- (B == matrix(B[i, ], nrow(B), ncol(B), byrow = TRUE)) & both
        s <- rowSums(eq, na.rm = TRUE) / pmax(rowSums(both), 1); s[i] <- NA
        sim[g[i]] <- max(s, na.rm = TRUE)
      }
    }
    f$`match with another interview` <- sim
  }
  ll <- strsplit(m$dupvars %||% "", " ")[[1]]
  lat <- ll[grepl("lat", ll, ignore.case = TRUE)][1]; lon <- ll[grepl("lon", ll, ignore.case = TRUE)][1]
  if (!is.na(lat) && !is.na(lon) && all(c(lat, lon) %in% names(d))) {
    la <- as.numeric(unclass(d[[lat]])); lo <- as.numeric(unclass(d[[lon]])); en <- as.character(d[[m$enum]])
    cla <- stats::ave(la, en, FUN = function(x) stats::median(x, na.rm = TRUE))
    clo <- stats::ave(lo, en, FUN = function(x) stats::median(x, na.rm = TRUE))
    f$`distance from usual area (km)` <- .km(la, lo, cla, clo)
  }
  f <- f[, vapply(f, function(x) sum(!is.na(x)) > 1 && length(unique(x[!is.na(x)])) > 1, logical(1)), drop = FALSE]
  if (ncol(f) < 2 || n < 20) return(NULL)

  # impute and standardise robustly, then score
  Z <- vapply(f, function(x) { x[is.na(x)] <- stats::median(x, na.rm = TRUE); .robust_z(x) }, numeric(n))
  Z[!is.finite(Z)] <- 0
  score <- hfc_iforest(Z)

  # explain each score by its most extreme features
  dir_word <- function(nm, z) {
    if (nm == "interview length") return(if (z < 0) "very short interview" else "very long interview")
    if (nm == "answer typicality") return(if (z > 0) "answers unusually uniform" else "answers unusually atypical")
    if (nm == "match with another interview") return(if (z > 0) "near-copy of another interview by the same enumerator" else "unlike the enumerator's other interviews")
    if (nm == "distance from usual area (km)") return("far from the enumerator's usual area")
    if (nm == "unusual start hour") return("started at an unusual time")
    paste(if (z > 0) "many" else "few", nm)
  }
  why <- vapply(seq_len(n), function(i) {
    z <- Z[i, ]; o <- order(-abs(z)); o <- o[abs(z[o]) >= 2][1:2]; o <- o[!is.na(o)]
    if (!length(o)) return("unusual combination of answers")
    paste(vapply(o, function(j) dir_word(colnames(Z)[j], z[j]), ""), collapse = "; ")
  }, "")
  cut <- stats::quantile(score, 1 - top_share)
  # clustered fabrication can mask itself in the forest, so near-identical interviews are flagged directly
  sim <- f$`match with another interview`
  near_copy <- if (is.null(sim)) rep(FALSE, n) else !is.na(sim) & sim >= 0.95
  why[near_copy] <- paste0(sprintf("%.0f%% of answers identical to another interview by the same enumerator", 100 * sim[near_copy]),
                           ifelse(grepl("near-copy", why[near_copy]), "", paste0("; ", why[near_copy])))
  out <- data.frame(id = idv, enumerator = as.character(d[[m$enum]]),
                    date = format(as.Date(d[[m$date]])), `anomaly score` = round(score, 3),
                    flagged = (score >= cut & score > 0.55) | near_copy, `main reasons` = why,
                    check.names = FALSE, stringsAsFactors = FALSE)
  out <- out[order(-out$flagged, -out$`anomaly score`), ]; rownames(out) <- NULL
  attr(out, "features") <- colnames(Z)
  out
}

#' Enumerator answer-pattern divergence (Jensen-Shannon, base 2, in [0, 1])
hfc_ml_enumerators <- function(r, interviews = NULL, min_n = 10) {
  d <- r$data; m <- r$map
  admin <- unlist(m[c("key", "id", "enum", "team", "date", "duration", "consent", "formversion")]); admin <- admin[nzchar(admin)]
  closed <- .closed_vars(d, admin)
  en <- as.character(d[[m$enum]]); ens <- names(which(table(en) >= min_n))
  if (!length(closed) || length(ens) < 3) return(NULL)
  js <- function(p, q) { mm <- (p + q) / 2; k <- function(a, b) sum(ifelse(a > 0, a * log2(a / b), 0)); (k(p, mm) + k(q, mm)) / 2 }
  res <- lapply(ens, function(e) {
    me <- en == e
    div <- vapply(closed, function(v) {
      x <- unclass(d[[v]]); lv <- sort(unique(x[!is.na(x)]))
      a <- table(factor(x[me], lv)); b <- table(factor(x[!me], lv))
      if (sum(a) < 5 || sum(b) < 5) return(NA_real_)
      js(as.numeric(a) / sum(a), as.numeric(b) / sum(b))
    }, numeric(1))
    top <- sort(div, decreasing = TRUE)[1:3]; top <- top[!is.na(top)]
    data.frame(enumerator = e, interviews = sum(me), `answer divergence` = round(mean(div, na.rm = TRUE), 4),
               `most different questions` = paste(names(top), collapse = ", "),
               check.names = FALSE, stringsAsFactors = FALSE)
  })
  out <- do.call(rbind, res)
  out$`divergence z` <- round(.robust_z(out$`answer divergence`), 2)
  if (!is.null(interviews)) {
    a <- tapply(interviews$`anomaly score`, interviews$enumerator, mean)
    f <- tapply(interviews$flagged, interviews$enumerator, mean)
    out$`mean anomaly score` <- round(as.numeric(a[out$enumerator]), 3)
    out$`% interviews flagged` <- round(100 * as.numeric(f[out$enumerator]), 1)
  }
  hi_flag <- if (is.null(out$`% interviews flagged`)) FALSE else out$`% interviews flagged` >= 20
  out$`review?` <- ifelse(out$`divergence z` >= 3 | hi_flag, "Yes", "")
  out <- out[order(-out$`divergence z`), ]; rownames(out) <- NULL
  out
}
