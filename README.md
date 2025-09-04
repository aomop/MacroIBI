# Wetland IBI Dashboard

A Shiny application for calculating a macroinvertebrate-based Index of Biotic Integrity (IBI) for wetlands.

## Key Features

- Interactive taxon entry by group with inline counts, dynamic summaries, and optional taxonomic hierarchy view
- Built-in metrics module computes EOT taxa, snail taxa, corixid ratio, abundance of EOT, and overall IBI score
- Optional auto-save that periodically writes taxa and metric data to disk and reloads saved datasets
- Import previously saved CSV data to repopulate taxon tables
- Export results as CSV, PNG table image, and PDF reports
- Visualize selected taxa as an annotated phylogenetic tree

## Screenshots

![Screenshot of initial app state](www/init_screenshot.png)
![Screenshot of data tab](www/table_screenshot.png)
![Screenshot of results tab](www/results_screenshot.png)

## Quick Start

```r
# install dependencies from lockfile
renv::restore()

# run the dashboard
shiny::runApp()
```

## Requirements

- R 4.4.2 or later
- R packages handled via `renv`; core packages include `shiny`, `ggtree`, `DT`, `magick`, and others
- External tools: `magick` requires ImageMagick and `webshot` needs PhantomJS (TODO: confirm installation steps)

## Installation & Setup

1. Install [renv](https://rstudio.github.io/renv/).
2. From the project root run `renv::restore()` to install package versions pinned in `renv.lock`.
3. Ensure system dependencies like ImageMagick and PhantomJS are available (TODO).

## Configuration

No special configuration is required. Adjust port or host when running locally, e.g.:

```r
shiny::runApp(port = 1234, host = "0.0.0.0")
```

## Running the App

- **R console**: set working directory to the project root and call `shiny::runApp()`.
- **RStudio**: open `Wetland_IBI.Rproj` and click *Run App*.

## Testing & QA

Run the smoke tests:

```r
testthat::test_dir("tests")
```

These tests confirm the app and taxonomy data exist and attempt to launch the app with `shinytest2` if installed.

## Project Structure

- `app.R` – main application script
- `modules/` – Shiny modules for data entry, metrics, auto-save, uploads, downloads, and tree visualization
- `utils/` – setup scripts for package installation
- `www/` – reports, styles, helper functions, and assets
- `tests/` – testthat smoke tests
- `auto_saves/` & `metric_autosaves/` – directories used for auto-save files

## Usage Guide

1. Provide a wetland name and sampling date when prompted.
2. Search and add taxa using the selection input; counts can be edited directly in the group tables.
3. View updated metrics in the *Metrics* section.
4. Enable auto-save to periodically persist your work, or upload a saved CSV to resume.
5. Download raw data or reports from the *Results* section.

## Performance & Ops

TODO

## Security & Privacy

TODO

## Troubleshooting

- Missing ImageMagick or PhantomJS may cause report or image downloads to fail.
- If dependencies are missing, run `renv::restore()` again.

## Contributing

Contributions are welcome. For now, please contact Sam Swanson before submitting pull requests. TODO: add contribution guidelines.

## Changelog

TODO

## License

This project is licensed under the [MIT License](LICENSE).

## Acknowledgments / Citation

TODO

