# macroibi 1.1.3

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

* Vignettes now document LaTeX (TinyTeX) and Chrome as optional dependencies
  for PDF and PNG exports respectively.
* Corrected troubleshooting advice for PNG export: Chrome/Chromium is required
  (not PhantomJS).
* Clarified that the Raw Data CSV download button is on the **Data tab
  sidebar**, not the Results tab.
