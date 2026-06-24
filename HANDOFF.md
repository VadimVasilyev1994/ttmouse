# TimeTeller mouse projection tool — handoff / context-reset notes

Paste this into a new chat (with the standing "Working Instructions") to continue
with full context. It records what is decided, built, verified, and remaining.
Treat "verified" items as reliable; re-verify anything marked "assumed/TBD".

This project is now an **R package** (`ttmouse`) — the "app-as-package" pattern,
chosen for longevity/maintainability (`R CMD check`, declared deps, namespacing,
roxygen docs, standard test harness). Reproducibility is carried by renv + Docker
+ a pinned TimeTeller commit, used regardless of layout.

---

## 0. How to behave (carry over the standing rules)
Do only what is explicitly asked; answer questions without building; ask before
adding/extending; use only the authoritative repo; clean, commented code matching
the existing style; test everything and state results; never invent
functions/arguments — verify from source or ask; separate verified from assumed;
correct mistakes plainly.

## 1. Goal
A public web tool: a researcher uploads a **raw mouse RNA-seq count matrix** (and,
only for the time-series path, a **separate metadata file** for group structure),
and the app projects it onto a **pre-built TimeTeller model**, returning a **CSV
of results** (primary deliverable) plus a few plots. R Shiny, then Dockerized for
public hosting (Hugging Face Spaces or Google Cloud Run).

## 2. Authoritative source
- App/engine repo: this package, `ttmouse`.
- Engine dependency: **`TimeTeller`** package (the `VadimVasilyev1994/TimeTeller-v2`
  repo, branch **master**; package name confirmed `TimeTeller` v2.0.0). Pulled via
  `Remotes:` — **pin to a commit** for reproducibility. Reading its R source is
  permitted; do not clone/fetch/pull beyond reading.

## 3. The models (provided by the user)
Two pre-built models, **40 MB each** as `.rds` (size is a non-issue for any host):
**intergene** (default; per-sample, single-sample OK) and **timecourse** (for
time-series with group structure). Each trained object carries (verified, with
source locations):
- `object[['Normalisation_choice']]` — Training_functions.R:92 / Testing_functions.R:3.
- `object[['Metadata']][['Train']][['Genes_Used']]` — Training_functions.R:34, hard-checked Testing_functions.R:67.
- `object[['Train_Data']][['LogThresh_Train']]` — scalar log threshold; Training_functions.R:540, read :723.

Valid `Normalisation_choice` (from the `normalise_test_data` dispatch):
`intergene`, `clr`, `timecourse`, `timecourse_matched`, `combined`.
Models live **outside** the package (too large to bundle); pass their paths to
`load_model()`. In Docker they are copied in as assets.

## 4. Frozen preprocessing contract (must match training)
log2(TPM + 1); Ensembl, versionless IDs; gene length = longest transcript;
GRCm39 / Ensembl release 107; TPM via `DGEobj.utils::convertCounts`. At
**projection** use `skip_filter = TRUE` (never drop genes). Train used
`skip_filter = FALSE`, so train vs projection TPM differ slightly — expected small
for the per-sample intergene z-score but must be confirmed by the round-trip test.

## 5. Verified facts about projection (`TimeTeller::test_model`)
- Signature (Main_functions.R): `test_model(object, exp_matrix, test_grouping_vars,
  test_group_1, test_group_2, test_group_3, test_replicate, test_time,
  mat_normalised_test = TRUE, log_thresh, parallel_comp = FALSE, cores = 4,
  minpeakheight = -Inf, minpeakdistance = 1, nups = 1, ndowns = 0, threshold = 0,
  npeaks = 2, diagnose_pd = FALSE)`.
- `mat_normalised_test = TRUE` (default) → no re-transform; the app supplies log2(TPM+1).
- `log_thresh` is **required** → the app passes `model_log_thresh(model)` by default.
- `add_test_data()` uses `missing()` → **omit** unsupplied optional args (never `NULL`).
- Results in `object[['Test_Data']][['Results_df']]` (columns referenced:
  `time_1st_peak`, `time_2nd_peak`, `Actual_Time`, `Pred_Error`, `Theta`;
  `Corrected_Time`/`Was_Shifted` only after the separate `shift_outlier_times()`,
  which `test_model` does NOT call). Exact final column set not yet run-confirmed.
- intergene/clr per sample (single sample OK); timecourse family needs grouping.

## 6. Verified facts about the plot functions (Phase D targets)
- `plot_3d_projection(object, selected_local_projection, density = FALSE, ...)`
  → **plotly** object; **training** projection only (from the loaded model).
- `plot_3d_projection_with_test(object, selected_local_projection, density = FALSE, ...)`
  → **plotly** object; needs the object **after projection**.
  Pure plotly at `density = FALSE`; `rgl`/OpenGL only if `density = TRUE` → keep FALSE.
- `plot_raw_likelis(object, sample_num, logthresh, train_or_test = 'test')` → **base graphics**.
- `plot_ind_curve(object, sample_num, logthresh, train_or_test = 'test')` → **base graphics**,
  returns `recordPlot()`; needs a completed projection + `Train_Data$epsilon`/`eta`.
- Shiny: plotly → `plotlyOutput`/`renderPlotly`; base graphics → `plotOutput`/`renderPlot`.
  UI selectors required: a **local-projection** chooser (`selected_local_projection`)
  and a **sample** chooser (`sample_num`); `logthresh` from `model_log_thresh()`.

## 7. Package structure & status
```
ttmouse/
├── DESCRIPTION              # deps; Remotes: VadimVasilyev1994/TimeTeller-v2 (PIN the commit)
├── NAMESPACE                # hand-written to match @export; regenerate via devtools::document()
├── README.md                # build/test/run + the counts_to_tpm requirement
├── HANDOFF.md               # this file
├── .Rbuildignore
├── R/                       # the engine (built + verified)
│   ├── read_counts.R        # read_counts() — CSV/XLSX -> numeric matrix
│   ├── preprocess_counts.R  # preprocess_counts() — frozen TPM contract wrapper
│   ├── load_model.R         # load_model() + model_normalisation/genes/log_thresh()
│   ├── validate_genes.R     # validate_genes() — pre-projection gene check
│   ├── project.R            # project_test_data() + internal .call_test_model() seam
│   └── counts_to_tpm.R      # *** USER MUST ADD THIS (their own function) ***
├── tests/
│   ├── testthat.R
│   └── testthat/
│       ├── test-read_counts.R        # 25 pass
│       ├── test-preprocess_counts.R  # 11 pass, 4 skip (need edgeR/DGEobj.utils)
│       ├── test-phaseB.R             # 36 pass (load_model + validate_genes)
│       └── test-project.R            # mocks the .call_test_model seam (local_mocked_bindings)
└── data-raw/
    └── build_gene_lengths.R          # run ONCE offline -> gene_lengths .rds (not runtime)
```
- All exported functions carry roxygen; `NAMESPACE` is hand-written to match and
  should be regenerated with `devtools::document()` in a full environment.
- **One required user file:** `R/counts_to_tpm.R` (the user's own TPM function).
  `preprocess_counts()` depends on it; without it the package builds but
  `preprocess_counts()` cannot run and check notes an undefined `counts_to_tpm`.
  Add it with a roxygen `@export` header; it uses `edgeR` + `DGEobj.utils` (already Imports).
- **The `.call_test_model()` seam:** `project_test_data()` calls
  `TimeTeller::test_model()` through this tiny internal wrapper so the external
  dependency is `R CMD check`-clean and mockable in tests without TimeTeller installed.

## 8. Build / document / test / run (full environment)
```r
devtools::document()   # regenerate NAMESPACE + man/
devtools::test()       # run tests/testthat
devtools::check()      # R CMD check
```
Requires all deps installed (`edgeR`, `DGEobj.utils`, `openxlsx`, `TimeTeller`, …).

## 9. Runtime pipeline (how the pieces connect)
Startup (once): load both models via `load_model()`, `readRDS()` the gene_lengths
table — held in memory, shared across sessions. Per upload: `read_counts()` →
`preprocess_counts()` → `validate_genes()` (block with a clear message if genes
missing) → `project_test_data()` → `Results_df` → CSV download + plots. Model
choice: intergene by default; timecourse when a metadata file with group/time
structure is supplied.

## 10. Not yet built — remaining work
- **Phase D** — plot wrappers around the four functions in §6 (default
  `density = FALSE`, `logthresh = model_log_thresh(model)`) + `Results_df → CSV`
  export + the local-projection / sample selectors. Add `plotly` to Imports then.
- **`R/read_metadata.R`** — reader for the **separate uploaded metadata file**
  (group/time/replicate) for the timecourse path. **Format/columns TBD** — confirm.
- **Phase E** — `inst/app/` + an exported `run_app()` (Shiny): startup preload +
  per-upload pipeline + downloads.
- **Phase F** — `Dockerfile` (R+Shiny base; install deps + TimeTeller at a pinned
  commit; copy package + app + model assets) + `renv.lock` + deploy (HF Spaces / Cloud Run).

## 11. Decisions made / still open
- **DECIDED: R package** (`ttmouse`, app-as-package). Not a plain project.
- Open: **renv.lock** vs DESCRIPTION-only for pinning (recommend renv.lock).
- Open: **pin the TimeTeller commit** in `Remotes:` (must do for reproducibility).
- Open: metadata file format (exact columns for group_1/2/3, time, replicate).
- Open: whether `project_test_data()` should self-validate genes (currently it does
  not; `validate_genes` is the separate pre-check).
- Open: package name `ttmouse` is a placeholder — rename freely.

## 12. Sandbox testing caveat
In the build sandbox, base R + `testthat` + `openxlsx` install via apt; `edgeR`,
`DGEobj.utils`, `plotly`, `TimeTeller`, and `roxygen2`'s full toolchain do **not**
(no CRAN/Bioconductor network), so the package cannot be loaded as a package and
`devtools::test()`/`R CMD check` cannot run here. What was verified here: every R
and test file parses; the dependency-light suites pass in **sourced mode**
(read_counts 25; preprocess 11 + 4 skip; load_model+validate_genes 36); and the
`project_test_data` wiring + `.call_test_model` seam pass a **sourced smoke**
(default log_thresh applied, `mat_normalised_test = TRUE`, optional args omitted,
group guard fires). The canonical package test run (`devtools::test()`) and the
TPM/projection/round-trip paths must be run in a full-deps environment.
