# MacroIBI

A Shiny application for calculating a macroinvertebrate-based Index of Biotic Integrity (IBI) for wetlands, packaged for easy installation.

**New to R?** See the [FIELD AND APP GUIDE](FIELD_AND_APP_GUIDE_v2.md) for step-by-step installation instructions with screenshots and troubleshooting tips.

**Try before installing:** A demo version is available at [https://smsc2.shinyapps.io/MacroIBI/](https://smsc2.shinyapps.io/MacroIBI/) (some features disabled). Install locally for full functionality.

---

## Quick Start

### Requirements
- **R 4.2.0 or newer** — Download from <https://cran.r-project.org>
- **RStudio** (recommended) — Download from <https://posit.co/download/rstudio-desktop/>
- **Rtools** (Windows only) — Download from <https://cran.r-project.org/bin/windows/Rtools/>

### Install MacroIBI
Open RStudio and type these commands in the Console (press Enter after each):

```r
install.packages("remotes")
remotes::install_github("aomop/MacroIBI")
```

### Launch the App
```r
library(macroibi)
run_macroibi()
```

A browser window will open with the Wetland IBI Dashboard.

### Where Are My Files Saved?
Autosaves are stored in your user data folder:
- **Windows:** `C:\Users\[YourName]\AppData\Local\R\macroibi\data\`
- **Mac:** `~/Library/Application Support/macroibi/data/`

---

## For Experienced R Users

All dependencies install automatically. The package exports two functions:
- `run_macroibi()` — Launch the app
- `run_macroibi(demo_mode = TRUE)` — Launch with bundled demo data (no file writes)

## Key Features

- Interactive taxon entry by group with inline counts, dynamic summaries, and optional taxonomic hierarchy view
- Built-in metrics module computes EOT taxa, snail taxa, corixid ratio, abundance of EOT, and overall IBI score
- Optional auto-save that periodically writes taxa and metric data to a user-specific cache and reloads saved datasets
- Import previously saved CSV data to repopulate taxon tables
- Export results as CSV, PNG table image, and PDF reports
- Visualize selected taxa as an annotated phylogenetic tree
- **Raw/Imported CSV scope:** The exported Raw Data CSV is intended only for datasets created within MacroIBI. Avoid modifying it externally or feeding unrelated/prepared data from other systems back into the app.

## Project Structure

- `R/` – package functions, modules, and app entry point
- `inst/app/www/` – bundled assets (templates, styles, images)
- `inst/extdata/` – bundled taxonomy data
- `tests/testthat/` – smoke tests for the package entry point

## Testing

```r
devtools::test()
```

## Support & contributions

- **Questions and issues:** Please open an issue at <https://github.com/aomop/MacroIBI/issues>.
- **Contributions:** Fork the repo, open a pull request with a concise description of the change, and include any relevant testing notes. Find more details in the [contributing guidelines](https://github.com/aomop/MacroIBI/blob/main/CONTRIBUTING.md).
- **Security:** I cannot fathom what security issues may arise from use of this app, but if you find one, please follow the steps outlined in the [security](SECURITY.md) document.

## License

This project is licensed under the [MIT License](LICENSE).
