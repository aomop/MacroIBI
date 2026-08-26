# macroibi 1.2.0

## Bug fixes

* Fixed a copy-paste error in the Full Report PDF template where the Snail Taxa
  narrative section was incorrectly referencing EOT Taxa scores and conditions
  instead of Snail Taxa values.
* `calculate_corixids_ratio()` no longer hardcodes section IDs (`"section_2"`,
  `"section_4"`). It now resolves beetles and true-bugs sections via
  `group_defs`, consistent with the batch export path.
* `quality_class` is now passed as a reactive to `results_download_server()`
  instead of being evaluated eagerly at session startup. The Data Summary PDF
  now reflects the quality class at the time of download.
* The comparison metrics pool in `results_download_server()` now uses a proper
  0-row data frame instead of a scalar `NA` when no prior metric files exist,
  removing a fragile special-case in `prepare_results_data()`.

## Improvements

* PDF export handlers now check for a LaTeX installation before attempting to
  render and show a clear notification if none is found.
* CSV upload now wraps `readr::read_csv()` in error handling and shows a
  notification to the user on failure, instead of crashing the reactive chain.
* The "No data to download" condition in the CSV download handler now shows a
  user-visible notification instead of a silent console warning.
* The "How are these calculated?" tooltip on the Results tab now uses a CSS
  `:hover` rule instead of inline `onmouseover`/`onmouseout` attributes.
* IBI quality class thresholds (≥38 Excellent, ≥28 Good, ≥20 Fair, ≥10 Poor)
  are now defined in a single `ibi_quality_class()` helper used by the app
  server, batch export, and report templates.
* `refresh_taxonomy()` default `input_dir` changed from a hardcoded
  developer path to `NULL`. Calling without an explicit path now gives an
  informative error.

## Taxonomy data selection

* `load_taxonomy()` and `refresh_taxonomy()` now choose the newest taxonomy file
  by the date in its filename rather than by file modification time. Installing
  a package rewrites mtimes, so on a fresh install the previous behaviour could
  silently load an older bundled snapshot than the one intended.
* `refresh_taxonomy()` now matches only `taxonomy_<YYYYMMDD>.csv`. Its previous
  pattern (`"*.csv$"`) was a malformed regex that matched any CSV in the input
  directory, so an unrelated file could be adopted as the taxonomy.
* `refresh_taxonomy()` now names its output for the source CSV's date instead of
  the date the function ran, so rebuilding from an older CSV no longer produces a
  file that appears current.
* Removed the stale bundled `inst/extdata/taxonomy_2026-02-24.rds`, which was
  never reachable once a newer snapshot existed.

## Package structure

* Removed stale `inst/extdata/taxonomy_2_18_26.rds` (non-ISO date format,
  never matched by `load_taxonomy()`).
* Removed duplicate `"_PACKAGE"` roxygen sentinel from `R/app_state.R`.
* Removed self-referential `Remotes: github::aomop/MacroIBI` from DESCRIPTION.
* Removed `LazyData: true` from DESCRIPTION (no `data/` directory exists).
* Fixed mismatched email address in `Authors@R` field of DESCRIPTION.
* Added `tinytex` to `Suggests` to support optional PDF generation.
* Replaced broad `@import stats` / `@import graphics` in `generate_tree.R`
  with targeted `@importFrom graphics` directives.

## Documentation

* Consolidated documentation that had been maintained in two places. The README,
  code of conduct, contributing guide, security policy, and changelog now live at
  the repository root; `docs/` is left to the generated pkgdown site, which
  previously collided with the hand-written files kept there.
* The field guide is now maintained solely as the `field-and-app-guide` vignette,
  with its screenshots included. The standalone copy had drifted from it, and its
  table of contents linked to a path that does not exist.
* Added a "Taxonomy data" section to the README describing the companion
  [macro-taxonomy](https://github.com/aomop/macro-taxonomy) pipeline that builds
  the bundled dataset.
* Added `R CMD check` CI on pull requests.

* Vignettes now document LaTeX (TinyTeX) and Chrome as optional dependencies
  for PDF and PNG exports respectively.
* Corrected troubleshooting advice for PNG export: Chrome/Chromium is required
  (not PhantomJS).
* Clarified that the Raw Data CSV download button is on the **Data tab
  sidebar**, not the Results tab.

---

# macroibi 1.1.3
### 02/24/2026
- **Fix:** `generate_reports()` data summary and full report outputs are now rendered properly.
- **Improvement:** `generate_reports()` now includes detailed outputs with error handling instead of hiding logs in `tryCatch()`.

---

# macroibi 1.1.2
### 02/16/2026
- **New:** Common names are now included in raw data outputs
- **Fix:** `generate_reports()` output formats are now properly aligned with the app updated outputs.

---

# macroibi 1.1.1
### 02/10/2026
- **New:** Taxonomy dataset now includes common names for taxa where it is available. Users may search for taxa by the latin name, common name, or taxon level. Section tables now display common names.
- **New:** Raw data outputs now include taxon level information. 

---

# macroibi 1.1.0
### 02/03/2026
- **New:** Added `generate_reports()` exported function for batch report generation from autosave files without running the Shiny app. Supports output types: `"csv"` (metric scores), `"csv_data"` (raw taxa), `"pdf_report"` (full report), `"pdf_summary"` (data summary), or `"all"`.
- **Improvement:** Replaced free-text date input with a calendar date picker (`dateInput`), providing proper date validation while maintaining backwards compatibility with existing autosave files and uploaded CSV data containing string dates.
- **Fix:** Download filenames now use ISO date format (YYYY-MM-DD) instead of raw user input, preventing invalid filenames when dates contain slashes (e.g., `07/10/2024`). Affected downloads: PNG table image, PDF full report, PDF data summary, and CSV data export.
- **Docs:** Updated user documentation to be much more helpful.

---

# macroibi 1.0.1
### 12/09/2025
- **Fix:** Taxonomic tree layouts no longer crash for parent-less taxa selections.
- **Improvement:** Harden tree layout logic against incomplete taxonomic hierarchies.

---

# macroibi 1.0.0
### 12/08/2025
- **New:** Added full documentation and user-friendly installation instructions to support new users.
- **New:** Added practical guides and explanatory material for the field sampling protocol used to collect MacroIBI data.
- **New:** Regenerated the internal taxonomy dataset with `in_region` flags to identify taxa unlikely to occur in North America, improving data entry and potential misidentification checks.
- **New (UI):** Added visual flags and a show/hide toggle for out-of-region taxa within the species selection interface.
- **Stability:** This is the first **stable**, user-oriented release of the MacroIBI application.

---

# macroibi 0.3.0
### 12/05/2025
- **Fix:** Updated LaTeX dependencies in report templates, restoring reliable PDF report generation across environments.
- **Refactor:** Removed dependencies on `treeio`, `ggtree`, `phylo`, and `ape` packages and reimplemented tree-generation logic internally, reducing heavy dependencies and improving render stability.
- **Refactor:** Reworked demo autosave behavior to prevent writing demo-state data to the user's real autosave directory.
- **Improvement:** General cleanup and stability improvements across modules.

---

# macroibi 0.2.0
### 12/03/2025
- **New:** Added `demo_mode` argument to `run_macroibi()` allowing a safe, limited-feature demonstration version of the application.
- **New:** Included shippable demo autosave files for training and demonstration.
- **Improvement:** Disabled or restricted certain features (uploads, autosave, downloads) in demo mode to prevent user confusion.
- **Fix:** Removed `pkgload` dependency to eliminate recurring CLI installation/locking issues during deployment or installation.

---

# macroibi 0.1.0
### 11/26/2025
- **Initial package version.**
- Converted the original standalone Shiny app into an installable R package.
- Resolved early dependency issues that prevented the app from starting reliably.
- Changed autosave behavior so that files are saved to a **user-local directory** instead of inside the installed package structure.
