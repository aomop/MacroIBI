# MacroIBI

[![R-CMD-check](https://github.com/aomop/MacroIBI/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/aomop/MacroIBI/actions/workflows/R-CMD-check.yaml)

A Shiny application for calculating a macroinvertebrate-based Index of Biotic
Integrity (IBI) for wetlands, packaged for easy installation.

See the [Field Sampling & App Guide](https://aomop.github.io/MacroIBI/articles/field-and-app-guide.html)
for the complete workflow — field sampling through IBI calculation — with
step-by-step installation screenshots and troubleshooting tips.

A demo version is available at <https://smsc2.shinyapps.io/MacroIBI/> (some
features disabled). Install locally for full functionality.

---

## Quick Start

### Requirements

- **R 4.2.0 or newer** — <https://cran.r-project.org>
- **RStudio** (recommended) — <https://posit.co/download/rstudio-desktop/>
- **Rtools** (Windows only) — <https://cran.r-project.org/bin/windows/Rtools/>

Optional, for exports:

- **TinyTeX or another LaTeX distribution** — required for PDF reports
  (`tinytex::install_tinytex()`)
- **Chrome or Chromium** — required for PNG table images

### Install MacroIBI

Open RStudio and type these commands in the Console (press Enter after each):

```r
install.packages("remotes")
remotes::install_github("aomop/MacroIBI")
```

### Launch the app

```r
library(macroibi)
run_macroibi()
```

A browser window will open with the Wetland IBI Dashboard.

### Where are my files saved?

Autosaves are stored in your user data folder:

- **Windows:** `C:\Users\[YourName]\AppData\Local\R\macroibi\data\`
- **Mac:** `~/Library/Application Support/macroibi/data/`

---

## Key Features

- Interactive taxon entry by group with inline counts, dynamic summaries, and an
  optional taxonomic hierarchy view
- Built-in metrics module computing EOT taxa, snail taxa, corixid ratio,
  abundance of EOT, and the overall IBI score
- Optional autosave that periodically writes taxa and metric data to a
  user-specific cache and reloads saved datasets
- Import previously saved CSV data to repopulate taxon tables
- Export results as CSV, PNG table image, and PDF reports
- Visualize selected taxa as an annotated phylogenetic tree
- Search taxa by scientific name, common name, or taxonomic level

> **Raw/imported CSV scope:** the exported Raw Data CSV is intended only for
> datasets created within MacroIBI. Avoid modifying it externally or feeding
> unrelated data from other systems back into the app.

---

## For Experienced R Users

All dependencies install automatically. The package exports three functions:

| Function | Purpose |
|---|---|
| `run_macroibi()` | Launch the app (`demo_mode = TRUE` for bundled demo data) |
| `generate_reports()` | Batch-generate PDF/CSV/PNG outputs without the UI |
| `refresh_taxonomy()` | Rebuild the bundled taxonomy from a pipeline CSV |

Further reading:

- [Getting Started](https://aomop.github.io/MacroIBI/articles/getting-started.html)
- [Batch Report Generation](https://aomop.github.io/MacroIBI/articles/batch-export.html)
- [Field Sampling & App Guide](https://aomop.github.io/MacroIBI/articles/field-and-app-guide.html)

---

## Taxonomy Data

The taxon list, hierarchy, regional occurrence flags, and common names shipped in
`inst/extdata/` are built by the companion
**[macro-taxonomy](https://github.com/aomop/macro-taxonomy)** pipeline, which
queries the [ITIS](https://www.itis.gov/) and
[iNaturalist](https://www.inaturalist.org/) APIs and writes a dated CSV.

The currently bundled snapshot is `taxonomy_2026-07-01.rds`. To load a newer
build:

```r
refresh_taxonomy(
  input_dir   = "path/to/macro-taxonomy/data/output",
  output_path = "inst/extdata/"
)
```

The app picks up the newest snapshot automatically on next launch. See
[CONTRIBUTING.md](CONTRIBUTING.md) for details.

---

## Project Structure

```
R/               # Package functions, Shiny modules, app entry point
inst/app/www/    # Bundled app assets (report templates, styles, images)
inst/extdata/    # Bundled taxonomy dataset
inst/docs/       # Reference documents shipped with the package
vignettes/       # Long-form guides, also published to the pkgdown site
tests/testthat/  # Test suite
tools/           # Deployment and rendering scripts
```

## Testing

```r
devtools::test()
```

The suite covers metric calculation, autosave and upload handling, download and
report assembly, taxonomy selection, and tree generation.

---

## Support & Contributions

- **Questions and issues:** open an issue at
  <https://github.com/aomop/MacroIBI/issues>
- **Contributions:** see [CONTRIBUTING.md](CONTRIBUTING.md)
- **Security:** please report vulnerabilities privately — see
  [SECURITY.md](SECURITY.md)
- **Conduct:** see [CODE_OF_CONDUCT.md](CODE_OF_CONDUCT.md)

## License

Released under the [MIT License](LICENSE.md).
