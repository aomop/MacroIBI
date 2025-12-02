# MacroIBI

A Shiny application for calculating a macroinvertebrate-based Index of Biotic Integrity (IBI) for wetlands, packaged for easy installation.

**Please see the [FIELD AND APP GUIDE](inst/docs/FIELD_AND_APP_GUIDE_v2.md) for questions about sampling protocol or how to use the app**

## Installation
Ensure R and RStudio are installed.
```r
# from the project root
install.packages("devtools")
devtools::install_local()
```

## Running the App

```r
library(macroibi)
run_macroibi()
# launch in a browser:
run_macroibi(browser = TRUE)
```

The app bundles its static assets and taxonomy reference data inside the package. Autosave files are written to a per-user data directory obtained via `tools::R_user_dir("macroibi", "data")`.

## Key Features

- Interactive taxon entry by group with inline counts, dynamic summaries, and optional taxonomic hierarchy view
- Built-in metrics module computes EOT taxa, snail taxa, corixid ratio, abundance of EOT, and overall IBI score
- Optional auto-save that periodically writes taxa and metric data to a user-specific cache and reloads saved datasets
- Import previously saved CSV data to repopulate taxon tables
- Export results as CSV, PNG table image, and PDF reports
- Visualize selected taxa as an annotated phylogenetic tree

## Project Structure

- `R/` – package functions, modules, and app entry point
- `inst/app/www/` – bundled assets (templates, styles, images)
- `inst/extdata/` – bundled taxonomy data
- `tests/testthat/` – smoke tests for the package entry point

## Testing

```r
devtools::test()
```

## License

This project is licensed under the [MIT License](LICENSE).
