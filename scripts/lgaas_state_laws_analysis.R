################################################################################
# What did Mexico's states do with the General Law on Adequate and Sustainable
# Food (LGAAS)? A text comparison of the Colima and Sonora state laws against
# the federal General Law.
#
# Author: Karina Sánchez Bazán
# Companion to: "A bright spot or a cautionary tale? Mexico's states take up
#   the right to food, but not the budget" (IFPRI blog post draft, Oct. 2026).
# Cutoff date for all statements about status and deadlines: 2026-10-26.
#
# ------------------------------------------------------------------------------
# WHAT THIS SCRIPT DOES
#
# It starts from the official PDFs and produces every number and figure cited
# in the blog post. All methods are transparent counting and comparison
# procedures; no language model or machine-learning classifier is used.
#
#   A. Verbatim text reuse with word n-grams. A word in a state law counts as
#      "copied" if it belongs to at least one sequence of n consecutive words
#      that also appears in the LGAAS (n = 5 in the main specification; n = 3
#      and n = 8 reported as sensitivity checks). This is the "shingling"
#      approach (Broder 1997) used in the legislative text-reuse literature
#      (Wilkerson, Smith and Stramp 2015; Linder et al. 2020).
#
#   B. Article matching with TF-IDF weighted cosine similarity. Each state
#      article is compared with all 111 LGAAS articles and paired with the
#      most similar one. Articles are labelled "reproduced", "adapted" or
#      "no LGAAS equivalent" using two thresholds (0.80 and 0.40 in the main
#      specification; alternative thresholds reported as sensitivity checks).
#      The articles closest to each threshold are exported for manual review.
#
#   C. Budget and implementation dictionary. Counts phrases such as "subject to
#      the resources expressly approved", "no budget increases will be
#      authorized" or "honorary" (unpaid posts), and exports every match with
#      its surrounding words (keyword in context) so each can be read.
#
#   D. Strength of obligations (deontic modality). Counts verbs that impose a
#      duty ("deberá" = shall, "queda prohibido" = is prohibited) against verbs
#      that leave discretion ("promoverá" = shall promote, "procurará" = shall
#      endeavour, "podrá" = may, "fomentará" = shall encourage).
#
#   E. Anchoring of sanctions. The LGAAS (art. 109), Colima (art. 113) and
#      Sonora (art. 119) each list the articles whose breach carries a fine.
#      For every article sanctioned in the LGAAS, the script finds its state
#      equivalent (method B) and checks whether the state also sanctions it.
#
#   F. Thematic vocabulary. Counts mentions of 12 policy themes (e.g.,
#      sustainability, climate change, food donation, conflict of interest)
#      per 10,000 words in each law, and tests each state's difference from
#      the LGAAS with the log-likelihood (G2) statistic (Rayson and Garside
#      2000).
#
#   G. Governance architecture. A structured coding of each law's governance
#      bodies (chair, membership, executive body, conflict-of-interest
#      safeguards, municipal councils). Every coded cell cites an article and
#      is verified against the text by the script.
#
#   H. Content added by the states. (1) Automatic: runs of 8 or more words
#      not shared with the LGAAS, grouped by topic. (2) Curated: a verified
#      catalogue of the main provisions with no LGAAS counterpart.
#
#   It also computes the deadlines set by each law's transitional articles.
#
# NOTE ON LANGUAGE
#   The laws are in Spanish, so the search patterns in methods C and D are
#   Spanish words. Each pattern is followed by an English gloss in comments.
#   Text is lower-cased and accents are removed before matching.
#
# ------------------------------------------------------------------------------
# HOW TO RUN
#
#   1. Keep the repository structure: data/ (the four PDFs, file names in
#      section 0), output/ and scripts/ (this file).
#   2. Open R with the repository root as the working directory and run:
#        source("scripts/lgaas_state_laws_analysis.R")
#   3. All outputs are written to "output/". See README.md for a description
#      of each file.
#
#   Packages: pdftools, dplyr, tidyr, stringr, purrr, readr, ggplot2.
#   Tested with R 4.3.3 on Ubuntu 24.04 (poppler 24.02). Session details are
#   saved to output/session_info.txt.
#
# SOURCES
#   LGAAS:  Diario Oficial de la Federación, April 17, 2024 (Chamber of
#           Deputies consolidated text).
#   Colima: Periódico Oficial "El Estado de Colima", no. 76, suppl. 3,
#           August 1, 2026 (Colima Congress, Legislative Process Office copy).
#   Sonora: Law no. 160, published in the Boletín Oficial, vol. CCXVIII,
#           no. 21, sec. I, September 10, 2026. The analysis uses the
#           born-digital text released by the Sonora Congress (pages 16-46,
#           containing the law and an appendix citing the gazette).
#           The scanned gazette (OCR text layer) is kept in data/ and compared
#           article by article with the Congress text (quality check 1).
################################################################################


# 0. Setup ---------------------------------------------------------------------

suppressPackageStartupMessages({
  library(pdftools)
  library(dplyr)
  library(tidyr)
  library(stringr)
  library(purrr)
  library(readr)
  library(ggplot2)
})

Sys.setlocale("LC_TIME", "C")  # English month names in figure labels

# Paths are relative to the repository root. If the script is run from
# scripts/, move up one level.
if (!dir.exists("data") && dir.exists("../data")) setwd("..")
if (!dir.exists("data")) stop("Run this script from the repository root (the folder with data/).")

files <- c(
  LGAAS  = "data/LGAAS_DOF_2024-04-17.pdf",
  Colima = "data/Colima_POE_2026-08-01.pdf",
  Sonora = "data/Sonora_Congress_Ley160_2026_law_pp16-46.pdf"
)
# The Sonora gazette (Boletín Oficial) is a scanned image with an OCR text
# layer. The main analysis uses the born-digital text published by the Sonora
# Congress: pages 16-46 of a 48-page document that contains the committee
# report (pp. 1-16), the law as enacted (pp. 16-46) and an appendix citing
# B.O. no. 21, sec. I, September 10, 2026, followed by an index (pp. 46-48). The gazette is used only to
# check that both versions contain the same law (quality check 1).
gazette_file <- "data/Sonora_BO_2026-09-10.pdf"

dir.create("output", showWarnings = FALSE)

N_GRAM         <- 5          # n-gram length for verbatim reuse
HIGH_THRESHOLD <- 0.80       # minimum cosine similarity for "reproduced"
LOW_THRESHOLD  <- 0.40       # below this: "no LGAAS equivalent"
CUTOFF_DATE    <- as.Date("2026-10-26")


# 1. Reading and cleaning the PDFs ---------------------------------------------

# Running headers, footers and watermarks that are not part of the law text.
noise_patterns <- list(
  LGAAS = c("^LEY GENERAL DE LA ALIMENTACIÓN ADECUADA Y SOSTENIBLE$",
            "^CÁMARA DE DIPUTADOS", "^Secretaría General$",
            "^Secretaría de Servicios Parlamentarios$", "Nueva Ley DOF",
            "^\\d+ de 32$"),
  Colima = c("^Ley de Alimentación Adecuada y Sostenible del Estado Libre y Soberano de Colima$",
             "^Dirección de Proceso Legislativo", "^\\d+$"),
  Sonora = c("^\\d+$", "^\uFFFC$"),
  Sonora_gazette = c("^Tomo CCXVIII", "^BOLETÍN OFICIAL$", "^\\d+$",
                     "^Publicación electrónica", "^sin validez oficial")
)

read_law <- function(path, patterns) {
  lines <- pdf_text(path) |>
    str_split("\n") |>
    unlist() |>
    str_squish()
  is_noise <- str_detect(lines, paste(patterns, collapse = "|"))
  # Sonora's diagonal watermark ("Publicación electrónica sin validez oficial")
  # is extracted as stray fragments of 1-3 letters on their own line ("Pu",
  # "si bl", "lid ió"). Lines made only of such fragments are dropped.
  is_fragment <- str_detect(lines, "^[[:alpha:]]{1,3}( [[:alpha:]]{1,3}){0,2}$") &
    !str_detect(lines, "^(LEY|y|Ley)$")
  lines[!(is_noise | is_fragment | lines == "")]
}

# Drops title, chapter and section headings and their subtitles. A heading is
# a line starting with TÍTULO/CAPÍTULO/Sección, or an all-caps line without
# digits; an all-caps single word ending in ";", ":" or "." is legal text, not
# a heading (e.g., "CIE;" at the end of a wrapped sentence). Subtitles can span
# several lines, so every line from a heading up to the next article is
# dropped. All dropped lines are exported for review (output/00_dropped_lines.csv).
is_article_start <- function(lines) str_detect(lines, "^(ART[ÍI]CULO|Art[íi]culo) \\d+\\s?\\.")
heading_flags <- function(lines) {
  all_caps <- lines == str_to_upper(lines) & !str_detect(lines, "[0-9]") & nchar(lines) > 3 &
    !(str_count(lines, " ") == 0 & str_detect(lines, "[;:.]$"))
  is_heading <- str_detect(lines, "^(T[ÍI]TULO|CAP[ÍI]TULO|Cap[íi]tulo|Secci[óo]n|SECCI[ÓO]N)\\b") | all_caps
  in_block <- FALSE; drop <- logical(length(lines))
  for (i in seq_along(lines)) {
    if (is_article_start(lines[i])) in_block <- FALSE
    if (is_heading[i]) in_block <- TRUE
    drop[i] <- in_block
  }
  drop
}
drop_headings <- function(lines) lines[!heading_flags(lines)]

# Drops list markers at the start of a line ("I.", "XIV.-", and "1.-", which
# is how Sonora's OCR reads the Roman numeral I).
drop_list_markers <- function(lines) {
  marker <- "^([IVXLl1]{1,6}|[A-Z]\\))\\s?\\.?\\s?-?\\s+(?=[[:alpha:]])"
  # Applied twice because some lines carry a doubled marker (Colima art. 36: "I. I.-").
  str_remove(str_remove(lines, marker), marker)
}

# Keeps the body of the law (article 1 to the signatures) and splits off the
# transitional articles ("Transitorios"). For the Sonora Congress version, the
# committee report before article 1 and the appendix and index after the
# transitional articles are excluded.
split_law <- function(lines) {
  start <- which(str_detect(lines, "^(ART[ÍI]CULO|Art[íi]culo) 1\\s?\\.-?"))[1]
  trans <- which(str_detect(lines, "^(TRANSITORIOS|Transitorios)$") & seq_along(lines) > start)[1]
  end   <- which(str_detect(lines, paste0(
    "^(Ciudad de México, a 6 de marzo",                       # LGAAS signatures
    "|La Gobernadora del Estado dispondrá",                   # Colima signatures
    "|Comuníquese al Titular del Poder Ejecutivo para su sanción",  # Sonora gazette
    "|APÉNDICE)")) & seq_along(lines) > trans)[1] - 1          # Sonora Congress text
  list(articles     = lines[start:(trans - 1)],
       transitional = lines[(trans + 1):end])
}

raw_texts <- imap(files, \(path, law) read_law(path, noise_patterns[[law]]) |> split_law())
gazette_raw <- read_law(gazette_file, noise_patterns$Sonora_gazette) |> split_law()


# 2. Normalization (and OCR correction for the Sonora gazette cross-check) -----

strip_accents <- function(x) chartr("áéíóúüñÁÉÍÓÚÜÑ", "aeiouunAEIOUUN", x)

normalize <- function(text) {
  text |>
    str_to_lower() |>
    strip_accents() |>
    str_replace_all("-\\s+", "") |>          # words hyphenated across lines
    str_replace_all("[^a-z0-9% ]", " ") |>
    str_squish()
}

tokenize <- function(text) str_split(normalize(text), " ")[[1]]

# Reference vocabulary: every word in the two born-digital texts (no OCR).
reference_vocab <- c(raw_texts$LGAAS$articles, raw_texts$LGAAS$transitional,
                     raw_texts$Colima$articles, raw_texts$Colima$transitional) |>
  paste(collapse = " ") |> tokenize() |> unique()

# The OCR splits words ("Estab lecer", "atenci ó n"), reads "ñ" as "fl"
# ("niflas" for "niñas") and capital "I" as "l" ("lntersectorial"). A fix is
# applied only when the result is a word in the reference vocabulary and the
# original token is not, so genuine Sonora-only words are left untouched.
fix_ocr <- function(tk, vocab) {
  tk <- ifelse(!tk %in% vocab & str_replace_all(tk, "fl", "n") %in% vocab,
               str_replace_all(tk, "fl", "n"), tk)
  tk <- ifelse(!tk %in% vocab & str_replace(tk, "^l", "i") %in% vocab,
               str_replace(tk, "^l", "i"), tk)
  for (pass in 1:3) {
    i <- 1; out <- character(0)
    while (i <= length(tk)) {
      three <- if (i + 2 <= length(tk)) paste0(tk[i], tk[i + 1], tk[i + 2]) else NA
      two   <- if (i + 1 <= length(tk)) paste0(tk[i], tk[i + 1]) else NA
      if (!is.na(three) && three %in% vocab && !tk[i + 1] %in% vocab) {
        out <- c(out, three); i <- i + 3
      } else if (!is.na(two) && two %in% vocab &&
                 (!tk[i] %in% vocab || !tk[i + 1] %in% vocab)) {
        out <- c(out, two); i <- i + 2
      } else {
        out <- c(out, tk[i]); i <- i + 1
      }
    }
    tk <- out
  }
  tk
}

# Splits each law into articles plus one "transitional" block, and normalizes.
segment <- function(parts, law_name) {
  needs_ocr_fix <- law_name == "Sonora_gazette"
  lines <- parts$articles |> drop_headings() |> drop_list_markers()
  art_no <- str_match(lines, "^(?:ART[ÍI]CULO|Art[íi]culo) (\\d+)\\s?\\.")[, 2]
  articles <- tibble(line = lines, article = as.integer(art_no)) |>
    fill(article) |>
    group_by(article) |>
    summarise(text = paste(line, collapse = " "), .groups = "drop") |>
    mutate(text = str_remove(text, "^(ART[ÍI]CULO|Art[íi]culo) \\d+\\s?\\.?-?\\s?"))
  transitional <- tibble(article = NA_integer_,
                         text = paste(drop_list_markers(parts$transitional), collapse = " "))
  bind_rows(articles, transitional) |>
    mutate(law = law_name,
           section = if_else(is.na(article), "transitional", "body"),
           tk = map(text, \(t) {
             x <- tokenize(t)
             if (needs_ocr_fix) fix_ocr(x, reference_vocab) else x
           }),
           text_norm = map_chr(tk, paste, collapse = " "),
           words = lengths(tk))
}

corpus <- imap(raw_texts, segment) |> bind_rows()

# Segmentation checks: article numbers must appear once each, in order, and
# every dropped heading line is exported so a reader can confirm no legal text
# was removed.
walk(names(raw_texts), \(l) {
  kept <- drop_list_markers(drop_headings(raw_texts[[l]]$articles))
  nums <- as.integer(str_match(kept, "^(?:ART[ÍI]CULO|Art[íi]culo) (\\d+)\\s?\\.")[, 2])
  nums <- nums[!is.na(nums)]
  if (!identical(nums, seq_len(length(nums)))) stop("Article sequence broken in ", l)
})
dropped_lines <- imap_dfr(raw_texts, \(parts, l) {
  tibble(law = l, line = parts$articles[heading_flags(parts$articles)])
})
write_csv(dropped_lines, "output/00_dropped_lines.csv")

# Quality check: articles and words per law. The article count must equal
# the last article number (111 for the LGAAS, 114 for Colima, 120 for Sonora).
corpus_summary <- corpus |>
  group_by(law) |>
  summarise(articles = sum(!is.na(article)),
            last_article = max(article, na.rm = TRUE),
            words = sum(words))
print(corpus_summary)
stopifnot(all(corpus_summary$articles == corpus_summary$last_article))
write_csv(corpus_summary, "output/00_corpus_summary.csv")


# 3. Method A: verbatim reuse with n-grams -------------------------------------

ngrams <- function(tk, n) {
  if (length(tk) < n) return(character(0))
  vapply(seq_len(length(tk) - n + 1),
         \(i) paste(tk[i:(i + n - 1)], collapse = " "), character(1))
}

# Logical vector: is each word covered by an n-gram that also appears in ref?
covered <- function(tk, ref_ngrams, n) {
  ng <- ngrams(tk, n)
  hit <- logical(length(tk))
  for (i in which(ng %in% ref_ngrams)) hit[i:(i + n - 1)] <- TRUE
  hit
}

# Quality check 1: do the Sonora Congress text and the scanned gazette contain
# the same law? The gazette is segmented the same way, its OCR errors are
# corrected (split words, "fl" read for "ñ", "l" read for "I"), and each
# article of the Congress text is compared with the gazette article of the
# same number: share of its words covered by 5-word sequences found in that
# gazette article. Values near 100 mean identical text up to OCR noise.
gazette <- segment(gazette_raw, "Sonora_gazette")
sonora_check <- corpus |>
  filter(law == "Sonora") |>
  select(article, tk_congress = tk) |>
  left_join(select(gazette, article, tk_gazette = tk), by = "article") |>
  mutate(pct_matching_gazette = map2_dbl(tk_congress, tk_gazette, \(a, b) {
    if (is.null(b)) return(NA_real_)
    round(100 * mean(covered(a, unique(ngrams(b, N_GRAM)), N_GRAM)), 1)
  }),
  words_congress = lengths(tk_congress), words_gazette = lengths(tk_gazette)) |>
  transmute(article = if_else(is.na(article), "Transitional", as.character(article)),
            words_congress, words_gazette, pct_matching_gazette)
ocr_summary <- tibble(
  articles_compared        = nrow(sonora_check),
  median_pct_matching      = median(sonora_check$pct_matching_gazette, na.rm = TRUE),
  articles_below_90_pct    = sum(sonora_check$pct_matching_gazette < 90, na.rm = TRUE),
  words_congress           = sum(sonora_check$words_congress),
  words_gazette            = sum(sonora_check$words_gazette),
  gazette_out_of_vocab_before_fix = sum(!unlist(map(gazette$text, tokenize)) %in% reference_vocab),
  gazette_out_of_vocab_after_fix  = sum(!unlist(gazette$tk) %in% reference_vocab)
)
print(ocr_summary)
print(filter(sonora_check, pct_matching_gazette < 90))
write_csv(sonora_check, "output/00_sonora_congress_vs_gazette.csv")
write_csv(ocr_summary, "output/00_sonora_congress_vs_gazette_summary.csv")

tokens_by_law <- corpus |> group_by(law) |> summarise(tk = list(unlist(tk)))
law_tokens <- \(l) tokens_by_law$tk[[which(tokens_by_law$law == l)]]
lgaas_ngrams <- unique(ngrams(law_tokens("LGAAS"), N_GRAM))

verbatim_reuse <- map_dfr(c("Colima", "Sonora"), \(l) {
  tk <- law_tokens(l)
  map_dfr(c(3, 5, 8), \(n) tibble(
    law = l, n = n, words = length(tk),
    pct_copied = round(100 * mean(covered(tk, unique(ngrams(law_tokens("LGAAS"), n)), n)), 1)
  ))
})
print(verbatim_reuse)
write_csv(verbatim_reuse, "output/01_verbatim_reuse.csv")

# Verbatim reuse per article (main specification).
reuse_by_article <- corpus |>
  filter(law != "LGAAS") |>
  mutate(pct_copied = map_dbl(tk, \(x) round(100 * mean(covered(x, lgaas_ngrams, N_GRAM)), 1)))


# 4. Method B: article matching (TF-IDF and cosine similarity) ------------------

# Spanish stopwords: articles, prepositions, conjunctions and frequent legal
# filler ("ley" = law, "artículo" = article, "fracción" = clause).
stopwords_es <- c(
  "a","al","ante","bajo","con","contra","de","del","desde","durante","en","entre",
  "hacia","hasta","mediante","para","por","segun","sin","sobre","tras","el","la",
  "los","las","lo","un","una","unos","unas","y","e","o","u","ni","que","se","su",
  "sus","como","cual","cuales","cuando","donde","este","esta","estos","estas",
  "ese","esa","esos","esas","dicho","dicha","dichos","dichas","asi","mismo",
  "misma","mismos","mismas","tal","otro","otra","otros","otras","sea","ser","son",
  "es","seran","sera","fin","efecto","efectos","caso","cada","todo","toda","todos",
  "todas","ley","presente","articulo","articulos","fraccion","parrafo","demas")

term_counts <- corpus |>
  filter(section == "body") |>
  mutate(doc = paste(law, article, sep = "_")) |>
  select(doc, law, article, tk) |>
  unnest(tk) |>
  filter(!tk %in% stopwords_es, !str_detect(tk, "^[0-9]+$"), nchar(tk) > 1) |>
  count(doc, law, article, tk, name = "tf")

# Sublinear term frequency, inverse document frequency over all three laws,
# and L2 normalization, so the dot product of two articles is their cosine.
tfidf <- term_counts |>
  group_by(tk) |> mutate(df = n_distinct(doc)) |> ungroup() |>
  mutate(weight = (1 + log(tf)) * log(n_distinct(doc) / df)) |>
  group_by(doc) |> mutate(weight = weight / sqrt(sum(weight^2))) |> ungroup()

# Cosine similarity for every pair of articles between law_a and law_b.
similarity <- function(law_a, law_b) {
  a <- filter(tfidf, law == law_a) |> select(art_a = article, tk, w_a = weight)
  b <- filter(tfidf, law == law_b) |> select(art_b = article, tk, w_b = weight)
  inner_join(a, b, by = "tk", relationship = "many-to-many") |>
    group_by(art_a, art_b) |>
    summarise(cosine = sum(w_a * w_b), .groups = "drop")
}

classify <- function(x, high = HIGH_THRESHOLD, low = LOW_THRESHOLD) case_when(
  x >= high ~ "Reproduced from LGAAS",
  x >= low  ~ "Adapted from LGAAS",
  TRUE      ~ "No LGAAS equivalent")

sim <- map(set_names(c("Colima", "Sonora")), \(l) similarity(l, "LGAAS"))

# Best LGAAS match for each state article.
best_match <- imap_dfr(sim, \(s, l) {
  all_articles <- tibble(art_a = sort(unique(corpus$article[corpus$law == l])))
  best <- s |> group_by(art_a) |> slice_max(cosine, n = 1, with_ties = FALSE) |> ungroup()
  left_join(all_articles, best, by = "art_a") |>
    mutate(law = l, cosine = replace_na(cosine, 0))
}) |>
  rename(state_article = art_a, lgaas_article = art_b) |>
  mutate(class = classify(cosine))

composition <- best_match |> count(law, class) |>
  group_by(law) |> mutate(pct = round(100 * n / sum(n), 1)) |> ungroup()
print(composition)

# Sensitivity: composition under alternative thresholds.
threshold_grid <- tibble(high = c(0.70, 0.80, 0.90, 0.80, 0.80),
                         low  = c(0.40, 0.40, 0.40, 0.30, 0.50))
threshold_sensitivity <- pmap_dfr(threshold_grid, \(high, low) {
  best_match |>
    mutate(class = classify(cosine, high, low)) |>
    count(law, class) |>
    mutate(high = high, low = low)
}) |>
  pivot_wider(names_from = class, values_from = n, values_fill = 0)
print(threshold_sensitivity)
write_csv(threshold_sensitivity, "output/02_threshold_sensitivity.csv")

# Validation sample: the 8 articles closest to each threshold in each state,
# with the beginning of both texts, for manual review of the classification.
lgaas_text <- filter(corpus, law == "LGAAS", section == "body") |> select(article, text)
state_text <- filter(corpus, law != "LGAAS", section == "body") |> select(law, article, text)
validation_sample <- best_match |>
  mutate(dist_high = abs(cosine - HIGH_THRESHOLD), dist_low = abs(cosine - LOW_THRESHOLD)) |>
  pivot_longer(c(dist_high, dist_low), names_to = "threshold", values_to = "distance") |>
  group_by(law, threshold) |> slice_min(distance, n = 8, with_ties = FALSE) |> ungroup() |>
  left_join(state_text, by = c("law", "state_article" = "article")) |>
  left_join(rename(lgaas_text, lgaas_text = text), by = c("lgaas_article" = "article")) |>
  transmute(law, threshold, state_article, lgaas_article, cosine = round(cosine, 2), class,
            state_text = str_trunc(text, 300), lgaas_text = str_trunc(lgaas_text, 300))
write_csv(validation_sample, "output/02_validation_sample.csv")

# Reverse direction: LGAAS articles with no equivalent in each state law.
lgaas_not_carried <- imap_dfr(sim, \(s, l) {
  all_articles <- tibble(art_b = sort(unique(corpus$article[corpus$law == "LGAAS"])))
  best <- s |> group_by(art_b) |> slice_max(cosine, n = 1, with_ties = FALSE) |> ungroup()
  left_join(all_articles, best, by = "art_b") |>
    mutate(law = l, cosine = replace_na(cosine, 0))
}) |>
  filter(cosine < LOW_THRESHOLD) |>
  left_join(lgaas_text, by = c("art_b" = "article")) |>
  transmute(law, lgaas_article = art_b, cosine = round(cosine, 2),
            lgaas_text_start = str_trunc(text, 160))

write_csv(left_join(best_match,
                    select(reuse_by_article, law, article, pct_copied),
                    by = c("law", "state_article" = "article")) |>
            mutate(cosine = round(cosine, 3)),
          "output/02_article_matching.csv")
write_csv(composition, "output/02_composition.csv")
write_csv(lgaas_not_carried, "output/03_lgaas_articles_without_state_equivalent.csv")


# 5. Method C: budget and implementation dictionary ----------------------------

# Patterns run on normalized text (lower case, no accents, no punctuation).
dictionary <- tribble(
  ~category,                                  ~pattern,
  # "subject to the resources (expressly) approved", "budget limits",
  # "budget availability"
  "Spending conditional on approved resources", "sujecion a los recursos|recursos (economicos )?aprobados( expresamente)?|limites presupuestales|disponibilidad presupuest",
  # "no budget increases will be authorized"
  "Ban on budget increases",                  "no se autorizaran ampliaciones",
  # "honorary" (unpaid posts)
  "Honorary (unpaid) posts",                  "honorific",
  # "regulations" (secondary rules the executive must issue)
  "References to regulations",                "reglamento",
  # "financing", "accessible credit", "fund", "budget line", "investment"
  "Financing instruments",                    "financiamiento|creditos? accesibles|fondo|partida|inversion",
  # "N calendar days"
  "Deadlines in days",                        "\\d+ dias naturales",
  # "municipality", "town council"
  "References to municipalities",             "municipi|ayuntamiento",
  # federal bodies of the LGAAS: SINSAMAC, National Intersectoral Council,
  # National Food Strategy, "National System" (excluding the existing national
  # civil protection and family development systems)
  "References to federal LGAAS bodies",       "sinsamac|consejo intersectorial nacional|estrategia nacional de alimentacion|sistema nacional(?! (de proteccion|para el desarrollo))"
)

count_matches <- function(text, pattern) str_count(text, regex(pattern))

dictionary_counts <- corpus |>
  group_by(law) |>
  summarise(text = paste(text_norm, collapse = " "), words = sum(words)) |>
  crossing(dictionary) |>
  mutate(matches = map2_int(text, pattern, count_matches),
         per_10k_words = round(1e4 * matches / words, 1)) |>
  select(law, category, matches, per_10k_words)
print(pivot_wider(dictionary_counts, id_cols = category, names_from = law, values_from = matches))
write_csv(dictionary_counts, "output/04_budget_implementation_counts.csv")

# Keyword in context: every match with 15 words on each side.
kwic <- corpus |>
  crossing(dictionary) |>
  mutate(pos = map2(text_norm, pattern, \(t, p) str_locate_all(t, regex(p))[[1]][, "start"])) |>
  unnest(pos) |>
  mutate(context = map2_chr(text_norm, pos, \(t, p) {
    before <- str_sub(t, 1, p - 1)
    before <- coalesce(word(before, -15, -1, sep = " "), before)
    after  <- str_sub(t, p)
    after  <- coalesce(word(after, 1, 15, sep = " "), after)
    paste(before, "[", after)
  })) |>
  transmute(law, article = if_else(is.na(article), "Transitional", as.character(article)),
            category, context)
write_csv(kwic, "output/05_keyword_in_context.csv")

# Articles (excluding transitional ones) whose spending is made conditional on
# "approved resources".
conditional_articles <- corpus |>
  filter(section == "body") |>
  mutate(is_conditional = str_detect(text_norm, dictionary$pattern[1])) |>
  group_by(law) |>
  summarise(articles = n(), conditional_articles = sum(is_conditional),
            article_list = paste(article[is_conditional], collapse = ", "))
print(conditional_articles)
write_csv(conditional_articles, "output/06_budget_conditional_articles.csv")


# 6. Method D: strength of obligations -----------------------------------------

modal_verbs <- tribble(
  ~type,           ~pattern,
  # shall / must / has the obligation / is prohibited / shall guarantee /
  # negated "may" (recoded as "prohibicionmodal" below)
  "Mandatory",     "\\b(debera|deberan|debe|deben|tendra la obligacion|tienen la obligacion|es obligacion|queda prohibid[oa]|se prohibe|garantizara|garantizaran|prohibicionmodal)\\b",
  # shall promote / shall endeavour / may / shall encourage / shall foster /
  # "to the extent possible"
  "Discretionary", "\\b(promovera|promoveran|procurara|procuraran|podra|podran|fomentara|fomentaran|impulsara|impulsaran|en la medida de lo posible)\\b"
)

# Negated "may" ("no podrá", "en ningún caso podrá", "bajo ningún supuesto…
# podrán") expresses a prohibition, so it is recoded as mandatory before
# counting.
negated_may_pattern <- "\\b(?:no(?: se)?|ningun caso(?: se)?|ningun supuesto(?: \\w+){0,3})\\s+podran?\\b"
obligation_strength <- corpus |>
  filter(section == "body") |>
  group_by(law) |>
  summarise(text = paste(text_norm, collapse = " "), words = sum(words)) |>
  mutate(negated_may = str_count(text, negated_may_pattern),
         text = str_replace_all(text, negated_may_pattern, "prohibicionmodal")) |>
  crossing(modal_verbs) |>
  mutate(n = map2_int(text, pattern, count_matches)) |>
  select(law, type, n, words, negated_may) |>
  pivot_wider(names_from = type, values_from = n) |>
  mutate(pct_mandatory = round(100 * Mandatory / (Mandatory + Discretionary), 1),
         mandatory_per_1k_words = round(1e3 * Mandatory / words, 1),
         discretionary_per_1k_words = round(1e3 * Discretionary / words, 1))
print(obligation_strength)
write_csv(obligation_strength, "output/07_obligation_strength.csv")


# 7. Method E: anchoring of sanctions ------------------------------------------

# Article lists transcribed from LGAAS art. 109, Colima art. 113 and Sonora
# art. 119. Group A: fines of 50-20,000 UMA; group B: 22,000-50,000 UMA plus
# possible closure (UMA = Mexico's daily unit of account for fines).
sanctions <- list(
  LGAAS  = list(A = c(3, 9, 11, 13, 21, 27, 38, 107), B = c(22, 24, 29, 35)),
  Colima = list(A = c(3, 9, 12, 26, 33, 45, 11),      B = c(28, 30, 36, 42)),
  Sonora = list(A = c(3, 11, 20, 26, 34, 55, 59, 61, 66, 115), B = c(22, 35, 36, 65))
)

sanction_anchoring <- imap_dfr(sim, \(s, l) {
  equivalents <- s |> group_by(art_b) |> slice_max(cosine, n = 1, with_ties = FALSE) |> ungroup()
  sanctioned_in_state <- unlist(sanctions[[l]])
  imap_dfr(sanctions$LGAAS, \(arts, grp) tibble(lgaas_article = arts, group = grp)) |>
    left_join(equivalents, by = c("lgaas_article" = "art_b")) |>
    transmute(law = l, group, lgaas_article,
              state_equivalent = art_a, cosine = round(cosine, 2),
              sanctioned_in_state = state_equivalent %in% sanctioned_in_state)
}) |>
  left_join(lgaas_text, by = c("lgaas_article" = "article")) |>
  mutate(lgaas_text_start = str_trunc(text, 110)) |>
  select(-text)
# Reading notes for reviewers:
# - LGAAS art. 107 is the article that sends public servants' breaches to the
#   administrative-responsibility law; it is excluded from the summary below.
# - For LGAAS art. 13 (2nd paragraph: water infrastructure), the closest Colima
#   match by vocabulary is art. 61, but the paragraph also reappears in Colima
#   art. 14. Neither article is on Colima's sanctions list, so the result does
#   not depend on which one is taken as the equivalent.
print(select(sanction_anchoring, -lgaas_text_start), n = 30)
write_csv(sanction_anchoring, "output/08_sanction_anchoring.csv")

sanction_summary <- sanction_anchoring |>
  filter(lgaas_article != 107) |>
  group_by(law) |>
  summarise(lgaas_sanctioned_obligations = n(),
            still_sanctioned_in_state = sum(sanctioned_in_state),
            not_sanctioned = paste(lgaas_article[!sanctioned_in_state], collapse = ", "))
print(sanction_summary)
write_csv(sanction_summary, "output/08_sanction_summary.csv")

# Reverse direction: obligations fined by each state whose LGAAS equivalent is
# not fined in the LGAAS (or that have no LGAAS equivalent at all).
lgaas_sanctioned <- unlist(sanctions$LGAAS)
new_state_sanctions <- map_dfr(c("Colima", "Sonora"), \(l) {
  tibble(state_article = unlist(sanctions[[l]])) |>
    left_join(filter(best_match, law == l), by = "state_article") |>
    mutate(law = l, cosine = round(cosine, 2),
           lgaas_equivalent_sanctioned = class != "No LGAAS equivalent" &
             lgaas_article %in% lgaas_sanctioned)
}) |>
  left_join(state_text, by = c("law", "state_article" = "article")) |>
  transmute(law, state_article, lgaas_article, cosine, class,
            lgaas_equivalent_sanctioned, state_text_start = str_trunc(text, 140))
print(count(new_state_sanctions, law, lgaas_equivalent_sanctioned))
write_csv(new_state_sanctions, "output/08_state_sanctions_mapped_to_lgaas.csv")


# 7b. Method F: thematic vocabulary --------------------------------------------

# Where do the laws differ in substance, not only in wording? Each theme is a
# set of Spanish stems; rates are mentions per 10,000 words of each law.
# Fixed legal formulas are removed first so they do not inflate counts:
# "adecuada y sostenible" (part of the name of the right and of the laws) and
# "Estado Libre y Soberano" (part of Colima's official name).
themes <- tribble(
  ~theme,                               ~pattern,
  # sustainable / sustainability
  "Sustainability",                     "sostenib|sustentab",
  # environmental impact
  "Environmental impact",               "impacto ambiental",
  # climate change, hurricanes, drought, resilience, disasters
  "Climate change and disasters",       "cambio climatico|huracan|sequia|hidrometeorologic|resilien|desastre",
  # agroecology
  "Agroecology",                        "agroecolog",
  # water, water resources, watersheds, aquifers
  "Water and watersheds",               "\\bagua\\b|hidric|cuenca|acuifer",
  # legumes, beans, chickpeas, plant-based foods
  "Legumes and plant-based food",       "leguminosa|legumbre|frijol|garbanzo|origen vegetal",
  # breastfeeding, first 1,000 days, early childhood
  "Breastfeeding and early childhood",  "lactancia|primera infancia|mil dias|1000 dias",
  # food sovereignty, self-sufficiency
  "Food sovereignty and self-sufficiency", "soberania|autosuficien",
  # food donation, donors, donees, food banks, food loss and waste
  "Food donation, loss and waste",      "donaci|donant|donatari|bancos? de alimentos|desperdicio|perdidas? (y el desperdicio )?de alimentos",
  # labeling, genetically modified organisms
  "Labeling and GMOs",                  "etiquet|geneticamente modificad|transgenic",
  # public purchases, small and medium-scale producers, short supply chains
  "Public procurement and local producers", "compras (gubernamentales|publicas)|pequena y mediana escala|productores locales|cadena corta",
  # conflict of interest
  "Conflict of interest",               "conflictos? de interes"
)

theme_text <- corpus |>
  group_by(law) |>
  summarise(text = paste(text_norm, collapse = " "), words = sum(words)) |>
  mutate(text = str_remove_all(text, "adecuad[ao]s? y sostenibles?|estado libre y soberano"))

theme_counts <- theme_text |>
  crossing(themes) |>
  mutate(matches = map2_int(text, pattern, count_matches),
         per_10k_words = round(1e4 * matches / words, 1)) |>
  select(law, theme, matches, words, per_10k_words)

# Log-likelihood (G2) test of each state against the LGAAS for each theme
# (Rayson and Garside 2000), the standard keyness statistic in corpus
# linguistics. Words in a legal text are not independent draws, so p-values
# are a guide to which differences are large relative to text length, not a
# formal inference.
g2 <- function(a, b, c, d) {
  e1 <- c * (a + b) / (c + d); e2 <- d * (a + b) / (c + d)
  term <- function(o, e) ifelse(o > 0, o * log(o / e), 0)
  2 * (term(a, e1) + term(b, e2))
}
lgaas_theme <- filter(theme_counts, law == "LGAAS") |>
  select(theme, lgaas_matches = matches, lgaas_words = words, lgaas_per_10k = per_10k_words)
theme_comparison <- theme_counts |>
  filter(law != "LGAAS") |>
  left_join(lgaas_theme, by = "theme") |>
  mutate(ratio_to_lgaas = round(per_10k_words / lgaas_per_10k, 2),
         g2 = round(g2(matches, lgaas_matches, words, lgaas_words), 2),
         p_value = signif(pchisq(g2, df = 1, lower.tail = FALSE), 2)) |>
  group_by(law) |>
  # Bonferroni correction for the 12 themes tested for each state.
  mutate(p_bonferroni = signif(pmin(1, p_value * n()), 2)) |>
  ungroup() |>
  mutate(direction = case_when(p_bonferroni >= 0.05 ~ "No clear difference",
                               per_10k_words > lgaas_per_10k ~ "More than LGAAS",
                               TRUE ~ "Less than LGAAS")) |>
  select(law, theme, matches, per_10k_words, lgaas_matches, lgaas_per_10k,
         ratio_to_lgaas, g2, p_value, p_bonferroni, direction)
print(theme_comparison, n = 30)
write_csv(theme_counts, "output/10_theme_counts.csv")
write_csv(theme_comparison, "output/10_theme_comparison.csv")


# 7c. Method G: governance architecture -----------------------------------------

# A structured legal coding of how each law organizes governance. Each cell
# cites the article it comes from. The script verifies every coding against
# the text: "evidence" must appear in the cited article, and for features
# coded as absent, "absent_if" must not appear anywhere in that law's body
# (or, for council membership, in the article that lists the members).
article_text <- \(l, a) corpus$text_norm[corpus$law == l & corpus$article %in% a] |> paste(collapse = " ")
law_body <- \(l) corpus$text_norm[corpus$law == l & corpus$section == "body"] |> paste(collapse = " ")

governance <- tribble(
  ~feature,                              ~law,     ~value,                                                         ~article, ~evidence,                                      ~absent_if,
  "System that groups all bodies",        "LGAAS",  "SINSAMAC (national system)",                                   "56",  "se crea el sinsamac",                          NA,
  "System that groups all bodies",        "Colima", "None; a single state council (CIE)",                           "67",  "el cie sera presidido",                        "se crea el s",
  "System that groups all bodies",        "Sonora", "SIESAMAC (state system)",                                      "69",  "se crea el siesamac",                          NA,
  "Who chairs the council",               "LGAAS",  "President; may delegate to Health Ministry",                   "60",  "podra delegar esta funcion en la direccion",   NA,
  "Who chairs the council",               "Colima", "Governor; no delegation clause",                               "69",  "fungira en la presidencia",                    NA,
  "Who chairs the council",               "Sonora", "Governor; may delegate to State Health Ministry",              "73",  "podra delegar esta funcion en la direccion",   NA,
  "Government bodies on the council",     "LGAAS",  "15",                                                           "61",  "consejo nacional de humanidades",              NA,
  "Government bodies on the council",     "Colima", "13 (incl. 3 federal health institutions)",                     "69",  "subdelegacion medica",                         NA,
  "Government bodies on the council",     "Sonora", "9",                                                            "74",  "secretaria de hacienda",                       NA,
  "Finance ministry on the council",      "LGAAS",  "Yes",                                                          "61",  "hacienda y credito publico",                   NA,
  "Finance ministry on the council",      "Colima", "Yes",                                                          "69",  "planeacion finanzas y administracion",         NA,
  "Finance ministry on the council",      "Sonora", "Yes",                                                          "74",  "secretaria de hacienda",                       NA,
  "Environment agency on the council",    "LGAAS",  "Yes (SEMARNAT)",                                               "61",  "medio ambiente y recursos naturales",          NA,
  "Environment agency on the council",    "Colima", "Yes (IMADES)",                                                 "69",  "el imades",                                    NA,
  "Environment agency on the council",    "Sonora", "No",                                                           "74",  "secretaria de hacienda",                       "ARTICLE:ecologia|medio ambiente|sustentable|cedes",
  "Civil society and academia",           "LGAAS",  "Permanent representatives",                                    "61",  "contara de manera permanente con representantes del sector social", NA,
  "Civil society and academia",           "Colima", "Permanent invitees (art. 69; art. 67 also names them as members)", "69", "seran invitados permanentes del consejo",  NA,
  "Civil society and academia",           "Sonora", "Permanent representatives",                                    "74",  "contara de manera permanente con representantes del sector social", NA,
  "Executive body",                       "LGAAS",  "Board of Directors",                                           "62",  "nombrar a una junta directiva",                NA,
  "Executive body",                       "Colima", "Technical Committee",                                          "71",  "nombrar a un comite tecnico",                  NA,
  "Executive body",                       "Sonora", "Board of Directors",                                           "75",  "nombrar a una junta directiva",                NA,
  "Working commissions",                  "LGAAS",  "At least 2",                                                   "69",  "por lo menos con las siguientes comisiones",  NA,
  "Working commissions",                  "Colima", "None",                                                         "71",  "comite tecnico",                               "comisiones de trabajo",
  "Working commissions",                  "Sonora", "At least 2",                                                   "82",  "por lo menos con las siguientes comisiones",  NA,
  "Conflict-of-interest committee",       "LGAAS",  "Yes",                                                          "70",  "comite de prevencion de conflicto de interes", NA,
  "Conflict-of-interest committee",       "Colima", "No",                                                           "80",  "avalados por el cie",                          "comite de prevencion",
  "Conflict-of-interest committee",       "Sonora", "Yes",                                                          "83",  "comite de prevencion de conflicto de interes", NA,
  "Who vets citizen food committees",     "LGAAS",  "Conflict-of-interest committee",                               "77",  "avalados por el comite de prevencion",         NA,
  "Who vets citizen food committees",     "Colima", "State council (CIE); wording also mentions municipal councils",                                          "80",  "avalados por el cie",                          NA,
  "Who vets citizen food committees",     "Sonora", "State conflict-of-interest committee",                         "88",  "avalados por el comite de prevencion",         NA,
  "Members paid?",                        "LGAAS",  "No (honorary)",                                                "59",  "honorifica",                                   NA,
  "Members paid?",                        "Colima", "No (honorary)",                                                "68",  "honorificos",                                  NA,
  "Members paid?",                        "Sonora", "No (honorary, incl. Technical Secretariat)",                  "72",  "incluyendo el secretariado tecnico es honorifica", NA,
  "Minimum council meetings per year",    "LGAAS",  "2",                                                            "64",  "por lo menos dos veces al ano",                NA,
  "Minimum council meetings per year",    "Colima", "2",                                                            "72",  "por lo menos dos veces al ano",                NA,
  "Minimum council meetings per year",    "Sonora", "2",                                                            "77",  "por lo menos dos veces al ano",                NA,
  "Municipal councils: membership guidelines", "LGAAS", "Set by SINSAMAC",                                          "73",  "lineamientos que determine el sinsamac",       NA,
  "Municipal councils: membership guidelines", "Colima", "Set by each municipality; non-official members need knowledge, experience or training",           "75",  "lineamientos que determinen los mismos municipios", NA,
  "Municipal councils: membership guidelines", "Sonora", "Set by National System",                                   "84",  "lineamientos que determine el sistema nacional", NA,
  "Municipal councils: minimum meetings", "LGAAS",  "Not set",                                                      "73,74", "consejo de alimentacion municipal",          "ARTICLE:veces al ano",
  "Municipal councils: minimum meetings", "Colima", "3 per year",                                                   "77",  "tres veces al ano",                            NA,
  "Municipal councils: minimum meetings", "Sonora", "Not set",                                                      "84,85", "consejo de alimentacion municipal",          "ARTICLE:veces al ano",
  "Food committee leaders sit on municipal council", "LGAAS",  "Yes",                                               "73",  "presidentes de dichos comites deberan ser parte", NA,
  "Food committee leaders sit on municipal council", "Colima", "Not specified",                                     "75",  "consejo intersectorial municipal",             "presidentes de dichos comites",
  "Food committee leaders sit on municipal council", "Sonora", "Yes",                                               "84",  "presidentes de dichos comites deberan ser parte", NA,
  "Members trained to form a food committee", "LGAAS", "1",                                                         "78",  "al menos uno de sus miembros",                 NA,
  "Members trained to form a food committee", "Colima","2",                                                         "81",  "al menos dos de sus miembros",                 NA,
  "Members trained to form a food committee", "Sonora","1",                                                         "89",  "al menos uno de sus miembros",                 NA,
  "Depends on federal bodies to operate", "Colima", "Strategy proposed 'before the SINSAMAC'; emergency requests to its Board", "95,104", "ante el sinsamac", NA,
  "Depends on federal bodies to operate", "Sonora", "Municipal councils follow National System guidelines", "84", "lineamientos que determine el sistema nacional", NA,
  "Monitoring sources named in the law",  "LGAAS",  "Not specified",                                                "88",  "indicadores de impacto",                       "ensanut",
  "Monitoring sources named in the law",  "Colima", "ENSANUT survey; soil and water use; food loss at wholesale market", "74", "ensanut",                                 NA,
  "Monitoring sources named in the law",  "Sonora", "Food loss and waste information system",                       "57",  "sistema de informacion sobre la perdida",      NA
)

governance_check <- governance |>
  mutate(arts = str_split(article, ","),
         cited_text = map2_chr(law, arts, \(l, a) article_text(l, as.integer(a))),
         evidence_found = map2_lgl(cited_text, evidence, \(t, e) str_detect(t, e)),
         absence_holds = pmap_lgl(list(law, absent_if, cited_text), \(l, a, t) {
           if (is.na(a)) return(TRUE)
           if (str_starts(a, "ARTICLE:")) !str_detect(t, str_remove(a, "^ARTICLE:"))
           else !str_detect(law_body(l), a)
         })) |>
  select(-arts, -cited_text)
failed <- filter(governance_check, !evidence_found | !absence_holds)
if (nrow(failed) > 0) { print(failed); stop("Governance coding not supported by the text.") }
governance_table <- governance |>
  mutate(cell = paste0(value, " [art. ", article, "]")) |>
  select(feature, law, cell) |>
  pivot_wider(names_from = law, values_from = cell, values_fill = "")
print(governance_table, n = 30, width = 200)
write_csv(governance_check, "output/11_governance_coding_verified.csv")
write_csv(governance_table, "output/11_governance_table.csv")


# 7d. Method H: content added by the states -------------------------------------

# Step 1 (automatic). "Novel spans": runs of at least 8 consecutive words in a
# state law that are not part of any 5-word sequence shared with the LGAAS.
# This captures both new articles and new clauses inserted into copied ones.
MIN_SPAN <- 8
novel_spans <- corpus |>
  filter(law != "LGAAS", section == "body") |>
  mutate(sp = map(tk, \(x) {
    hit <- covered(x, lgaas_ngrams, N_GRAM)
    r <- rle(!hit); ends <- cumsum(r$lengths); starts <- ends - r$lengths + 1
    keep <- r$values & r$lengths >= MIN_SPAN
    tibble(start = starts[keep], end = ends[keep],
           span = map2_chr(starts[keep], ends[keep], \(a, b) paste(x[a:b], collapse = " ")))
  })) |>
  select(law, article, words_in_law = words, sp) |>
  unnest(sp) |>
  mutate(words = end - start + 1)

# Each span gets one category, assigned by the first matching pattern in this
# order; spans matching none are rewordings, local names or references.
span_categories <- tribble(
  ~category,                                   ~pattern,
  # donation, donors, donees, food banks, food loss and waste
  "Food donation, loss and waste",             "donaci|donant|donatari|bancos? de alimentos|donativ|desperdicio|perdidas? de alimentos",
  # low environmental impact foods, plant-based options, legumes, dietary inclusion
  "Sustainable and plant-based diets",         "bajo impacto ambiental|menor impacto|origen vegetal|legumbre|garbanzo|soya|inclusion dietaria|dietas|sostenibles? en los eventos",
  # climate change, drought, heat, resilience, restoration, soils, mangroves,
  # watersheds, wastewater reuse, nature-based solutions, agroecology
  "Environment, climate and agroecology",      "cambio climatico|huracan|sequia|hidrometeorolog|resilien|calor|restauracion|suelos?|tierras|manglar|reforest|microcuenca|labranza|agroforestal|reuso|aguas residuales|soluciones basadas|infraestructura verde|hidric|acuifer|agroecolog|impacto ambiental|medio ambiente",
  # breastfeeding, first 1,000 days, school food education, labels, nutrition services
  "Early childhood and nutrition education",   "mil dias|1000 dias|lactancia|educacion alimentaria|nueva escuela|sellos|etiquet|servicio social|orientacion alimentaria|nutricion",
  # local producers, markets, women and rural youth, land tenure
  "Local producers, women and youth",          "productos? locales|comercializacion|agroproductor|productores locales|cadena corta|mujeres|juventud|tierra",
  # councils, committees, secretariats, institutions, monitoring sources
  "Governance and monitoring",                 "consejo|\\bcie\\b|siesamac|secretaria tecnica|comite|imades|imss|issste|inifap|invitados|ensanut|indicador|monitoreo|seguimiento"
)
assign_category <- function(x) {
  hit <- map_lgl(span_categories$pattern, \(p) str_detect(x, p))
  if (any(hit)) span_categories$category[which(hit)[1]] else "Rewording, local names and references"
}
novel_spans <- mutate(novel_spans, category = map_chr(span, assign_category))

body_words <- corpus |> filter(section == "body") |> group_by(law) |> summarise(total_words = sum(words))
novel_summary <- novel_spans |>
  group_by(law, category) |>
  summarise(spans = n(), words = sum(words), .groups = "drop") |>
  left_join(body_words, by = "law") |>
  mutate(pct_of_body = round(100 * words / total_words, 1))
novel_totals <- novel_spans |> group_by(law) |>
  summarise(novel_words = sum(words)) |>
  left_join(body_words, by = "law") |>
  mutate(pct_of_body = round(100 * novel_words / total_words, 1))
print(novel_totals)
print(arrange(novel_summary, law, desc(words)), n = 20)
write_csv(novel_spans, "output/12_novel_spans.csv")
write_csv(novel_summary, "output/12_novel_content_by_category.csv")

# Step 2 (curated). A catalogue of the main provisions with no LGAAS
# counterpart, identified by reading the novel spans and the articles with no
# LGAAS equivalent (method B). Each entry is verified against the cited text.
state_only <- tribble(
  ~law,     ~article, ~topic,                         ~provision,                                                                                           ~evidence,
  "Colima", "54",     "Climate adaptation",           "Sectoral climate change adaptation program for the food chain, incl. research on drought- and heat-resilient crops", "adaptacion al cambio climatico",
  "Colima", "55",     "Land restoration",             "Program to restore degraded farmland, prioritizing state investment in the Sierra de Manantlán watersheds and the coast", "restauracion productiva de tierras",
  "Colima", "56",     "Climate adaptation",           "Economic resilience mechanisms: 'differentiated financing' and food reserves based on purchases from local producers, linked to agroecological practices", "financiamiento diferenciado",
  "Colima", "14,53",  "Water and nature-based solutions", "Wastewater reuse in agriculture, nature-based solutions, mangrove restoration and reforestation of micro-watersheds", "soluciones basadas en la naturaleza|manglares",
  "Colima", "19",     "Agricultural burning",         "Gradual phase-out of agricultural burning",                                                          "quema agricola",
  "Colima", "4",      "Agroecology",                  "Links the right to food to the State Agroecological Transition Plan and to dietary guidelines favoring low-impact foods", "plan estatal de transicion agroecologica",
  "Colima", "1,5",    "Sustainable diets",            "'Dietary inclusion' principle and sustainable food at public events and services",                   "inclusion dietaria",
  "Colima", "31",     "Sustainable diets",            "Legumes (beans, chickpeas, soy, peas, fava beans) as the main protein source in normative food baskets", "legumbres como fuente principal de proteina",
  "Colima", "36,41",  "Sustainable diets",            "Plant-based options: free swap in restaurants; at least one plant-based option in workplace cafeterias", "origen vegetal",
  "Colima", "15",     "Early childhood",              "Recognizes the first 1,000 days of life in breastfeeding and complementary feeding policies",       "primeros 1000 dias",
  "Colima", "10",     "Program integrity",            "Ethical and transparent guidelines to protect food programs from private interests when receiving donations", "lineamientos eticos y transparentes",
  "Colima", "22",     "Nutrition education",          "Education on choosing processed foods with the fewest front-of-pack warning seals",                  "minimo de sellos de advertencia",
  "Colima", "24",     "Nutrition services",           "Nutrition students' social service in public food guidance programs",                                "servicio social",
  "Colima", "52,89",  "Rural equity",        "Gender perspective in rural projects: priority for those led by rural women, men and youth, incl. access to land title or use rights", "juventudes rurales",
  "Colima", "74",     "Monitoring",                   "Council's technical secretariat tracks food insecurity (ENSANUT), soil and water use, and food loss at wholesale markets", "ensanut",
  "Sonora", "56,57",  "Food donation",                "State policy to prevent food loss and waste through altruistic donation to priority groups",        "donacion altruista",
  "Sonora", "57",     "Food donation",                "Support for food banks' infrastructure and equipment",                                                "bancos de alimentos",
  "Sonora", "57",     "Information system",           "State information system on food loss and waste",                                                    "sistema de informacion sobre la perdida",
  "Sonora", "59,61,62","Food donation",               "Duties of donors (food safety, labeling information) and annual agreements with authorized recipients", "convenios anuales",
  "Sonora", "63",     "Food donation",                "People lacking access to food may request donations directly, without going through an organization", "sin necesidad de vincularse",
  "Sonora", "64",     "Food donation",                "Tax-deductible receipts for donated food",                                                            "deduccion fiscal",
  "Sonora", "65,68",  "Program integrity",            "Recipients may not sell donated food or condition its delivery; quarterly reporting by food banks",  "podran comercializar los alimentos",
  "Sonora", "66",     "Data protection",              "Beneficiaries' data collected under data protection law; refusal cannot bar access to food",       "no sera condicionante",
  "Sonora", "2",      "Nutrition education",          "School food education aligned with the 'Nueva Escuela Mexicana' curriculum",                         "nueva escuela mexicana"
)
state_only_check <- state_only |>
  mutate(cited_text = map2_chr(law, str_split(article, ","), \(l, a) article_text(l, as.integer(a))),
         evidence_found = map2_lgl(cited_text, evidence, \(t, e) str_detect(t, e))) |>
  select(-cited_text)
if (any(!state_only_check$evidence_found)) {
  print(filter(state_only_check, !evidence_found)); stop("Catalogue entry not supported by the text.")
}
write_csv(state_only_check, "output/13_state_only_provisions_verified.csv")


# 8. Deadlines in the transitional articles ------------------------------------

deadlines <- tribble(
  ~law,     ~milestone,                           ~base_date,    ~days,     ~source,
  "LGAAS",  "Entry into force",                   "2024-04-17",  1,         "1st transitional art.",
  "LGAAS",  "Federal regulations",                "2024-04-18",  180,       "2nd transitional art.",
  "LGAAS",  "State laws harmonized",              "2024-04-18",  360,       "5th transitional art.",
  "Colima", "Entry into force",                   "2026-08-01",  1,         "1st transitional art.",
  "Colima", "State regulations",                  "2026-08-02",  180,       "2nd transitional art.",
  "Colima", "State council installed*",           "2026-08-02",  180 + 90,  "3rd transitional art.",
  "Sonora", "Entry into force",                   "2026-09-10",  1,         "1st transitional art.",
  "Sonora", "State regulations",                  "2026-09-11",  365,       "3rd transitional art.",
  "Sonora", "Municipal regulations updated",      "2026-09-11",  365,       "5th transitional art.",
  "Sonora", "State council installed*",           "2026-09-11",  365 + 160, "4th transitional art."
) |>
  mutate(deadline = as.Date(base_date) + days)
# * Assuming the state regulations are issued on the last day allowed.
print(select(deadlines, law, milestone, deadline))
write_csv(deadlines, "output/09_deadlines.csv")


# 9. Figures -------------------------------------------------------------------

base_theme <- theme_minimal(base_size = 13, base_family = "sans") +
  theme(panel.grid.major.y = element_blank(), panel.grid.minor = element_blank(),
        plot.title.position = "plot", plot.caption.position = "plot",
        legend.position = "top", legend.justification = "left",
        plot.caption = element_text(color = "grey40", size = 9, hjust = 0))

class_colors <- c("Reproduced from LGAAS" = "#2a6f97",
                  "Adapted from LGAAS"    = "#89c2d9",
                  "No LGAAS equivalent"   = "#e07a5f")

reuse_labels <- verbatim_reuse |> filter(n == N_GRAM) |>
  mutate(label = paste0(round(pct_copied), "% of the text copied word for word"))

fig1 <- composition |>
  mutate(class = factor(class, levels = rev(names(class_colors)))) |>
  ggplot(aes(x = pct, y = law, fill = class)) +
  geom_col(width = 0.55) +
  geom_text(aes(label = n), position = position_stack(vjust = 0.5),
            color = "white", fontface = "bold", size = 4.2) +
  geom_text(data = reuse_labels, aes(x = 0, y = law, label = label),
            inherit.aes = FALSE, nudge_y = 0.42, hjust = 0, size = 3.6, color = "grey25") +
  scale_fill_manual(values = class_colors, breaks = names(class_colors), name = NULL) +
  scale_x_continuous(labels = \(x) paste0(x, "%"), expand = expansion(mult = c(0, 0.03))) +
  labs(title = "How closely do the state laws follow Mexico's General Law?",
       subtitle = "Articles in each state law by similarity to the LGAAS (number of articles shown in bars)",
       x = NULL, y = NULL,
       caption = paste0("Note: Each state article is paired with the most similar LGAAS article (TF-IDF cosine similarity): ",
                        "reproduced ≥ ", HIGH_THRESHOLD, "; adapted ", LOW_THRESHOLD, " to ", HIGH_THRESHOLD, ".\n",
                        "Verbatim copy: words that are part of a ", N_GRAM,
                        "-word sequence identical to one in the LGAAS.\n",
                        "Counts by category depend on the thresholds; see output/02_threshold_sensitivity.csv.\n",
                        "Source: Author's analysis of the LGAAS (DOF 2024), Colima's law (POE 2026) and Sonora's law (BO 2026).")) +
  base_theme
ggsave("output/figure1_similarity_to_lgaas.png", fig1, width = 9.5, height = 4.6, dpi = 200, bg = "white")

# Figure 3: transitional deadlines against the 2027 political calendar.
# Gubernatorial elections in both states: June 6, 2027. New governors take
# office in Sonora on September 13, 2027, and in Colima on November 1, 2027.
fig2_data <- deadlines |>
  filter(!milestone %in% c("Entry into force", "State laws harmonized")) |>
  mutate(label = paste(law, "·", milestone),
         label = factor(label, levels = rev(label)),
         status = if_else(deadline < CUTOFF_DATE, "Deadline missed", "Deadline ahead"))

fig2 <- ggplot(fig2_data, aes(x = deadline, y = label)) +
  annotate("rect", xmin = as.Date("2027-06-06"), xmax = as.Date("2027-11-01"),
           ymin = -Inf, ymax = Inf, fill = "#f2e8cf", alpha = 0.8) +
  annotate("text", x = as.Date("2027-08-20"), y = Inf, vjust = 1.3, size = 3.1,
           label = "State elections and\ntransfer of power", lineheight = 0.9, color = "grey30") +
  geom_vline(xintercept = CUTOFF_DATE, linetype = "dashed", color = "grey50") +
  annotate("text", x = CUTOFF_DATE, y = Inf, label = "Today", vjust = 1.5, hjust = 1.2,
           size = 3.1, color = "grey30") +
  geom_segment(aes(x = as.Date("2024-07-01"), xend = deadline, yend = label),
               color = "grey85", linewidth = 0.6) +
  geom_point(aes(color = status), size = 3.6) +
  geom_text(aes(label = format(deadline, "%b %d, %Y")), hjust = -0.2, size = 3.2) +
  scale_color_manual(values = c("Deadline missed" = "#e07a5f", "Deadline ahead" = "#2a6f97"),
                     name = NULL) +
  scale_x_date(date_labels = "%Y", date_breaks = "1 year",
               limits = as.Date(c("2024-07-01", "2028-12-31")), expand = c(0, 0)) +
  labs(title = "Implementation deadlines and the 2027 political calendar",
       subtitle = "Deadlines set by the transitional articles of each law",
       x = NULL, y = NULL,
       caption = paste0("Note: * Assuming the state regulations are issued on the last day allowed. ",
                        "Today = October 26, 2026.\n",
                        "Source: Author's analysis of the transitional articles of the LGAAS and the Colima and Sonora laws.")) +
  base_theme
ggsave("output/figure3_deadlines.png", fig2, width = 9.5, height = 4.5, dpi = 200, bg = "white")


# Figure 2: thematic emphasis of each law (mentions per 10,000 words).
law_colors <- c(LGAAS = "#9e9e9e", Colima = "#2a6f97", Sonora = "#e07a5f")
theme_order <- theme_counts |>
  select(law, theme, per_10k_words) |>
  pivot_wider(names_from = law, values_from = per_10k_words) |>
  arrange(Colima - LGAAS) |>
  pull(theme)
# Mark themes where a state differs clearly from the LGAAS (p < 0.05).
sig_marks <- theme_comparison |>
  filter(direction != "No clear difference") |>
  group_by(theme) |>
  summarise(mark = paste0(" *", paste(str_sub(law, 1, 1), collapse = ",")))
theme_labels <- tibble(theme = theme_order) |>
  left_join(sig_marks, by = "theme") |>
  mutate(label = paste0(theme, coalesce(mark, "")))
fig3_data <- theme_counts |>
  mutate(theme = factor(theme, levels = theme_order, labels = theme_labels$label),
         law = factor(law, levels = c("LGAAS", "Colima", "Sonora")))
fig3_ranges <- fig3_data |> group_by(theme) |>
  summarise(lo = min(per_10k_words), hi = max(per_10k_words))
fig3 <- ggplot(fig3_data, aes(x = per_10k_words, y = theme)) +
  geom_segment(data = fig3_ranges, aes(x = lo, xend = hi, y = theme, yend = theme),
               inherit.aes = FALSE, color = "grey85", linewidth = 2.2) +
  geom_point(aes(color = law), size = 3.4, alpha = 0.95) +
  scale_color_manual(values = law_colors, name = NULL) +
  labs(title = "Where the laws differ in substance",
       subtitle = "Mentions of each theme per 10,000 words",
       x = "Mentions per 10,000 words", y = NULL,
       caption = paste0("Note: * marks a clear difference from the LGAAS (log-likelihood test, Bonferroni-adjusted p < 0.05); C = Colima, S = Sonora.\n",
                        "Fixed formulas (\"adecuada y sostenible\", \"Estado Libre y Soberano\") are excluded. ",
                        "Search terms and tests in output/10_theme_comparison.csv.\n",
                        "Source: Author's analysis of the LGAAS (DOF 2024), Colima's law (POE 2026) and Sonora's law (BO 2026).")) +
  base_theme + theme(panel.grid.major.y = element_line(color = "grey95"),
                     axis.title.x = element_text(size = 10, color = "grey30"))
ggsave("output/figure2_themes.png", fig3, width = 9.5, height = 5.6, dpi = 200, bg = "white")

writeLines(capture.output(sessionInfo()), "output/session_info.txt")
message("Done. All outputs are in output/.")
