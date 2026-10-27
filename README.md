# Right to food state laws in Mexico: Colima and Sonora vs. the General Law on Adequate and Sustainable Food (LGAAS)

Replication files for the blog post *A bright spot or a cautionary tale? Mexico’s states take up the right to food, but not the budget* by Karina Sánchez Bazán (draft, October 2026).

The analysis compares the two state laws published so far under the LGAAS, Colima (August 1, 2026) and Sonora (September 10, 2026), with the federal General Law (April 17, 2024). Everything in the post can be reproduced from the official PDFs with one script. Cutoff date for statements about status and deadlines: October 26, 2026.

## Contents

| Path | Description |
|---|---|
| `scripts/lgaas_state_laws_analysis.R` | The full analysis, from PDF to figures |
| `data/LGAAS_DOF_2024-04-17.pdf` | LGAAS, Chamber of Deputies consolidated text of the DOF publication |
| `data/Colima_POE_2026-08-01.pdf` | Colima law (Decree 345), Periódico Oficial no. 76, suppl. 3 |
| `data/Sonora_Congress_Ley160_2026_law_pp16-46.pdf` | Sonora law (Law 160) as released by the Sonora Congress, pages 16 to 46 of the original document: the enacted text and an appendix citing Boletín Oficial no. 21, sec. I, September 10, 2026. Page 16 opens with the last paragraphs of the committee report, which precede article 1 and are excluded by the script. Used for the analysis |
| `data/Sonora_BO_2026-09-10.pdf` | Sonora law as published in the Boletín Oficial vol. CCXVIII, no. 21, sec. I (scanned, with OCR text layer). Used only to cross-check the Congress text |
| `output/` | All tables and figures produced by the script (`figure1_similarity_to_lgaas.png`, `figure2_themes.png`, `figure3_deadlines.png` match Figures 1 to 3 in the post) |

## Sources

All source documents are public legal texts, included here so the analysis can be reproduced exactly.

- **LGAAS.** *Diario Oficial de la Federación*, April 17, 2024. https://dof.gob.mx/nota_detalle.php?codigo=5723626&fecha=17/04/2024 (file: consolidated text published by the Chamber of Deputies).
- **Colima.** Decree 345, *Periódico Oficial "El Estado de Colima"*, no. 76, suppl. 3, August 1, 2026. https://congresocol.gob.mx/web/Sistema/uploads/LegislacionEstatal/LeyesEstatales/Nueva_LEY_ALIMENTACION_ADECUADA_01agosto2026.pdf
- **Sonora.** Law 160, *Boletín Oficial del Gobierno del Estado de Sonora*, vol. CCXVIII, no. 21, sec. I, September 10, 2026 (https://boletinoficial.sonora.gob.mx). The born-digital text used in the analysis is an excerpt of the version released by the Sonora Congress (https://congresoson.gob.mx).

## How to run

```r
# Working directory = the repository root (the folder that contains data/, output/ and scripts/)
source("scripts/lgaas_state_laws_analysis.R")
```

Requires R (tested on 4.3.3) and the packages `pdftools`, `dplyr`, `tidyr`, `stringr`, `purrr`, `readr`, `ggplot2`. Runtime is under two minutes. Session details are written to `output/session_info.txt`.

## Methods in brief

| | Method | What it measures | Main output |
|---|---|---|---|
| A | Verbatim reuse with word n-grams (n = 5; 3 and 8 as checks) | Share of each state law's words that belong to a 5-word sequence also found in the LGAAS | `01_verbatim_reuse.csv` |
| B | Article matching, TF-IDF cosine similarity | Each state article paired with its closest LGAAS article; labelled reproduced (≥ 0.80), adapted (0.40 to 0.80) or no equivalent (< 0.40) | `02_article_matching.csv`, `02_composition.csv` |
| C | Budget and implementation dictionary | Mentions of budget conditionality, bans on budget increases, unpaid posts, financing instruments, municipalities and federal bodies | `04_…counts.csv`, `05_keyword_in_context.csv`, `06_…articles.csv` |
| D | Deontic modality | Share of mandatory verbs ("deberá", "queda prohibido") vs. discretionary ones ("promoverá", "procurará", "podrá") | `07_obligation_strength.csv` |
| E | Sanction anchoring | Whether each obligation fined in the LGAAS (art. 109) is still fined in the state law (Colima art. 113, Sonora art. 119), and which state fines have no fined LGAAS counterpart | `08_sanction_anchoring.csv`, `08_sanction_summary.csv`, `08_state_sanctions_mapped_to_lgaas.csv` |
| F | Thematic vocabulary | Mentions of 12 policy themes per 10,000 words, with a log-likelihood (G2) test of each state against the LGAAS | `10_theme_counts.csv`, `10_theme_comparison.csv` |
| G | Governance architecture | Structured coding of each law's governance bodies (system, chair, members, executive body, conflict-of-interest safeguards, municipal councils). Each cell cites an article and is verified against the text | `11_governance_table.csv`, `11_governance_coding_verified.csv` |
| H | Content added by the states | (1) Novel passages: runs of 8+ words not shared with the LGAAS, grouped into topic categories; (2) curated catalogue of the main state-only provisions, each verified against the cited article | `12_novel_spans.csv`, `12_novel_content_by_category.csv`, `13_state_only_provisions_verified.csv` |
| | Deadlines | Dates implied by the transitional articles | `09_deadlines.csv` |

All text is lower-cased and stripped of accents and punctuation before comparison. Search patterns are in Spanish because the laws are; each is glossed in English in the script.

## Robustness and quality checks

- **Segmentation.** The script stops if article numbers are not found exactly once each and in order (1 to 111, 114 and 120). Headings and their subtitles, which can span several lines, are removed from each heading up to the next article; every removed line is listed in `00_dropped_lines.csv` so readers can confirm that no legal text was dropped. See also `00_corpus_summary.csv`.
- **Sonora source text.** The analysis uses the born-digital text released by the Sonora Congress. To confirm it is the law as published, each article is compared with the scanned gazette after correcting OCR errors (split words, "fl" read for "ñ", "l" read for "I"): all 121 segments (120 articles plus the transitional block) match at 90 percent or more of their words, with a median of 100 percent (`00_sonora_congress_vs_gazette.csv`, `00_sonora_congress_vs_gazette_summary.csv`). An earlier version of this analysis used the OCR text directly; results changed by at most one article in any category and 0.4 percentage points in verbatim reuse.
- **n-gram length.** Verbatim reuse is 61 to 80 percent for Colima and 80 to 89 percent for Sonora across n = 3, 5 and 8 (`01_verbatim_reuse.csv`).
- **Similarity thresholds.** `02_threshold_sensitivity.csv` reports the article classification under five threshold pairs. Sonora's classification is stable; Colima's "no equivalent" count ranges from 9 to 24, so the post reports the substance of Colima's new provisions rather than a single count.
- **Manual review.** `02_validation_sample.csv` lists, for each state, the eight articles closest to each threshold with both texts side by side. Review shows that some short Colima articles below 0.40 are reworded versions of LGAAS provisions (e.g., Colima art. 23 vs. LGAAS art. 19), so the "no equivalent" category slightly overstates new content.
- **Strength of obligations.** Negated "may" ("no podrá", "en ningún caso podrá", "bajo ningún supuesto… podrán") expresses a prohibition and is counted as mandatory, not discretionary (2 to 4 instances per law; `07_obligation_strength.csv`, column `negated_may`).
- **Thematic vocabulary.** Fixed legal formulas ("adecuada y sostenible", part of the name of the right, and "Estado Libre y Soberano") are removed before counting so they do not inflate the sustainability or sovereignty themes. The G2 test treats words as independent draws, which legal text is not; p-values indicate which differences are large relative to text length rather than formal inference. A Bonferroni correction for the 12 themes tested per state is reported (`p_bonferroni`) and used to flag differences in Figure 2; the agroecology difference in Colima (p = 0.029) does not survive it. Themes with few mentions (e.g., conflict of interest: 5 in the LGAAS, 2 in Colima) show no clear difference, so the post relies on structural provisions rather than word rates for those topics.
- **Governance coding.** Coding legal structures requires judgment, so every coded cell in `11_governance_coding_verified.csv` lists the cited article and an evidence phrase; the script stops if the phrase is not in that article. Features coded as absent (e.g., Colima's conflict-of-interest committee, Sonora's environment agency) are also checked: the script stops if the corresponding term appears in the law (or, for council membership, in the article listing members). One inconsistency is reported rather than resolved: Colima art. 67 names civil society and universities as council members, while art. 69 lists them as permanent invitees.
- **Novel content.** Novel passages are a mechanical measure: they include genuine new provisions but also rewording, local institution names and legal cross-references (reported as a separate category). The curated catalogue in `13_state_only_provisions_verified.csv` was built by reading those passages and the articles with no LGAAS equivalent; each entry is checked against the cited text.
- **Sanction anchoring.** LGAAS art. 107 is excluded from the summary because it refers public servants' breaches to the administrative-responsibility law rather than creating a substantive obligation. For LGAAS art. 13 the closest Colima match is art. 61, but the same paragraph reappears in art. 14; neither is sanctioned, so the result does not depend on the choice.

## Limitations

These are measures of legal text, not of implementation. A copied article can be implemented well and an original one ignored. Budget conditionality is identified by phrasing; it does not capture what state budgets actually allocate, which requires reviewing 2027 budget bills once published.

## References

- Broder, A. Z. (1997). On the resemblance and containment of documents. *Proceedings of Compression and Complexity of Sequences*, 21–29.
- Kyle, J., & Resnick, D. (2019). Delivering more with less: Subnational service provision in low capacity states. *Studies in Comparative International Development*, 54(1), 133–163. https://doi.org/10.1007/s12116-018-9276-z
- Linder, F., Desmarais, B., Burgess, M., & Giraudy, E. (2020). Text as policy: Measuring policy similarity through bill text reuse. *Policy Studies Journal*, 48(2), 546–574.
- Rayson, P., & Garside, R. (2000). Comparing corpora using frequency profiling. *Proceedings of the Workshop on Comparing Corpora*, 1–6. Association for Computational Linguistics. https://aclanthology.org/W00-0901/
- Resnick, D., Haggblade, S., Babu, S. C., Hendriks, S. L., & Mather, D. (2018). The Kaleidoscope Model of policy change: Applications to food security policy in Zambia. *World Development*, 109, 101–120. http://hdl.handle.net/2263/65231
- Wilkerson, J., Smith, D., & Stramp, N. (2015). Tracing the flow of policy ideas in legislatures: A text reuse approach. *American Journal of Political Science*, 59(4), 943–956.

## License and citation

This repository is licensed under the [Creative Commons Attribution 4.0 International License (CC BY 4.0)](https://creativecommons.org/licenses/by/4.0/); see `LICENSE`. You may share and adapt the code, data outputs and figures for any purpose, provided you give appropriate credit. The legal texts in `data/` are official public documents of the Mexican federal government and the states of Colima and Sonora and are included for reproducibility only; the license does not apply to them.

Suggested citation: Sánchez Bazán, K. (2026). *Right to food state laws in Mexico: Replication files* [Data and code]. GitHub. https://github.com/ksanchezbazan-dpp/righttofoodstatelawsinmexico

