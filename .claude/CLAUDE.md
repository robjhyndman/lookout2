# lookout 2

LaTeX source for the paper accompanying the `lookout` R package (Hyndman, Kandanaarachchi, Turner). The draft began as a JACT submission (`JACT_Submission.pdf`; referee report `JACT-2026-47-review-mod.pdf`) and is being rewritten for the Journal of Computational and Graphical Statistics around the Euclidean minimum spanning tree instead of Rips death diameters.

## Read the plan before editing

`tasks/todo.md` is the authority on what happens next. Its §1 lists the known errors, §2 the verified theory facts and the proof simplifications, §3 the ordered steps with checkboxes, and a log at the end. Work through §3 in order, tick items as they are finished, and add a dated log entry. Do not restructure, renumber or retitle sections ahead of that order.

When a step's scope is a list, the list is the scope. Fix the listed items and whatever keeps them coherent, then stop. Errors found nearby go in the final message, not into the diff. If a reply ends with a recommendation and an optional preparatory step, a bare "go ahead" from Rob approves the recommendation, not the preparatory step.

## Layout

`main.tex` carries the preamble, title, abstract, keywords and bibliography, and inputs the body files in order: `1_introduction.tex` (`sec:intro`), `2_mst.tex` (`sec:mst`), `3_newlookout.tex` (`sec:modified`), `4_experiments.tex` (`sec:experiments`), `5_applications.tex` (`sec:applications`) and `6_discussion.tex` (`sec:conclusion`).

The online appendix is a separate document: `supplement.tex` inputs `7_proofs.tex` (proofs, sections S1–S3) and `8_experiments_supp.tex` (Experiments S1 and S2, the showcase examples and the details of the bandwidth sensitivity study, section S4) and builds `supplement.pdf`. ASA style forbids LaTeX cross-references between paper and appendix, so `7_proofs.tex` restates `prop:order` and `cex:fattails` (same labels, same numbers as in the paper) and defines its own notation, and the paper refers to it only as "the online Appendix". Keep the restated statements identical to those in `2_mst.tex`. References cited only in the appendix appear only in its reference list.

Results in the paper are `prop:order`, `cex:fattails` and `cor:critical`, each kind on its own counter. Refer to them by label, never by number.

The experiments are numbered the same way everywhere: 1 to 5 in the paper (Gamma rates; ring anomalies as n grows; six-dimensional normal against other methods; annulus; unit cube in twenty dimensions) and S1 and S2 in the appendix, with matching script names (`R/Experiment1.R` to `R/Experiment5.R`, `R/ExperimentS1.R`, `R/ExperimentS2.R`), target names (`exp2_results`, `fig_expS1`, ...), figure files and, for Experiments 3 to 5, timing CSVs. Renaming a target changes its random seed in `targets`, so any renaming needs a rerun of the pipeline and a recheck of the realisation-specific sentences in §4.

`R/` holds the experiment scripts, `_targets.R` the pipeline that runs them, `Figures/` the generated PDFs, `Data_Output/` the timing tables, and `References.bib` the bibliography. `kde.tex` is an old standalone note and is not part of the paper. `MST_anomaly_detection_references.pdf` is a reference list; one of its titles is wrong (see `tasks/todo.md` §2).

## Building

For text-only edits, `ratex --keep-logs main.tex` and `ratex --keep-logs supplement.tex` are enough and do not touch R; without `--keep-logs`, ratex writes no `.log` file. Positron rebuilds automatically with ratex whenever a `.tex` file is saved (LaTeX Workshop, set in the global Positron settings). `make` runs `targets::tar_make()` first to regenerate figures, then compiles both documents. After any edit, rebuild and check `main.log` and `supplement.log` for errors and undefined references.

The document uses the JCGS submission format from the Taylor & Francis template (`files.taylorandfrancis.com/ucgs-template.zip`): plain `article`, `\spacingset{1.8}`, `agsm.bst` through `natbib`. Do not switch to the `interact` class or to `tfs.bst`. Both `main.tex` and `supplement.tex` set `12pt` with 2cm margins; Rob kept the 2cm margins on 18 September 2026 (no template-margin requirement).

JCGS review is single-anonymous: author names, affiliations and emails are on the title page, and the old `\anon` switch is gone. The official format requirements are the ASA style guide at https://files.taylorandfrancis.com/asa-style-guide.pdf: 12pt fully double-spaced, abstract of at most 200 words, three to five keywords, a separate counter for each kind of result, appendices as an online supplement cited as "the online Appendix".

R packages are managed by `uvr`; the project `.Rprofile` puts its library on the path, so `Rscript` from the repo root works directly. `uvr doctor` and `uvr sync` diagnose and repair the library. Add dependencies with `uvr add <pkg>`, never by editing `uvr.toml` or `uvr.lock` by hand.

## Check claims about the algorithm against the package, not the paper

Read the function before describing what it does:

```bash
Rscript -e 'print(lookout::lookout); print(lookout::find_tda_bw); print(lookout:::lookde); print(lookout::mvscale)'
```

What the pinned build (2.0.2.9000 at commit `3ae6ac4`, 18 September 2026) does: `mlpack::emst` edge lengths, type-8 quantile at `gamma`, multiplied by `sqrt(m + 4)` and used as the support radius of an Epanechnikov kernel, so H = (m+4) h_n² I; MCD scaling with `alpha = 0.9` and median centring; GPD fitted unconstrained and refitted with shape 0 if the estimate is positive; p_i multiplied by (1 − β); default `alpha = 0.01`. The exact KDE (`fast = FALSE`, the default for n ≤ 10⁴) uses a fixed-radius search, `dbscan::frNN`, so its cost is proportional to the number of pairs inside the kernel support; `fast = TRUE` sums over between 100 and 500 nearest neighbours via dbscan. Results are identical to the earlier RANN-based build up to rounding (checked on three targets on 18 September 2026).

`old_version = TRUE` is not the original package. It keeps min–max scaling, the largest-gap rule (restricted to edges at or above the median) and the unconstrained GPD fit (no refit with shape 0) but shares everything else with the new version. The original CRAN releases 0.1.0–0.1.4 used a √5 multiplier in every dimension, no (1 − β) factor, and `alpha = 0.05`; the median restriction appeared in 0.1.4. Algorithm 1 in the paper describes the 2022 publication; the median restriction is noted in the paragraph after it (Rob, 18 September 2026). The archive is at `https://cran.r-project.org/src/contrib/Archive/lookout/` if a detail needs checking.

## LaTeX style

**Delimiters.** Use plain `(` `)` and `[` `]`. Reach for `\left` and `\right` only when what they enclose is genuinely taller than ordinary delimiters: a displayed `\frac`, a `\sqrt` of a fraction, a `\sum` or `\int` with limits, a matrix. For function arguments, conditioning bars and subscripted terms, plain delimiters are correct. Prefer `\big`, `\Big` and `\bigg` when a little extra height helps; the algorithm blocks already use `\big`.

```latex
\exp\left(\frac{\log n}{\log\log n}\right)   % correct, the fraction is tall
\mathbb{P}\left(Y\left(n, C_n, g_n\right)\right)  % wrong, the inner arguments are ordinary height
\mathbb{P}\big(Y(n, C_n, g_n)\big)           % what that should be
```

**Line wrapping.** `panache.toml` sets `wrap = "reflow"` with a line width of 9999, so each paragraph is one physical line. Do not hand-wrap paragraphs, and do not reflow paragraphs you are not otherwise changing.

**Edge lengths, not death times.** The paper works with the ordered MST edge lengths e_(1) ≤ … ≤ e_(n−1). Rips death diameters survive only in the description of the original algorithm in §1, where the diameter convention applies: a death time is a distance between two sample points.

## Working habits

Keep diffs minimal and confined to what was asked. This is a manuscript under revision with a planned sequence of changes; an unrequested improvement elsewhere is a cost, not a bonus. If you notice an error outside the current task, report it and leave it alone.

## Prose for the paper

Match the register of the sections the authors wrote themselves (§1, §4, §5): plain declarative sentences about the method and the data, first person plural where the authors act, one statement per sentence. Rob rejected the following on 18 September 2026 as AI wording, so do not write them:

- Aphoristic sentences that sound like a conclusion, such as "Both facts have one explanation, and it is what anomaly detection wants" or "The rule sits at the boundary by choice".
- Attributing wants, needs, decisions or questions to methods: "what an anomaly detector needs", "anomaly detection asks a different question", "the signal the detector needs to keep".
- Contrast constructions of the form "not X but Y", "X, not Y", "not X. Y." State Y.
- Metaphor and dramatisation: "throws off", "rescue", "puts at risk", "invisible".
- Labels and slogans invented for the paper's own ideas.

Corrections from Rob go in `tasks/lessons.md`; read it at the start of a session.
