# Contributing to MacroIBI

Thank you for your interest in contributing! This project is primarily developed
and maintained internally, but community contributions are welcome.

Please read the [Code of Conduct](CODE_OF_CONDUCT.md) before contributing.

## Reporting issues

Check the [existing issues](https://github.com/aomop/MacroIBI/issues) first to
avoid duplicates, then open a new one including:

- A clear description of the problem
- Steps to reproduce
- Expected vs. actual behavior
- Relevant error messages, logs, or screenshots
- Your R version, operating system, and package version
- The output of `sessionInfo()` if relevant

## Requesting features

Feature requests are welcome, especially where they improve usability or
clarity. Explain *why* the feature would be useful, provide example workflows or
UI suggestions where applicable, and say whether you are able to help implement
it.

## Submitting changes

1. Open or find an issue discussing the proposed change.
2. Fork the repository and create a branch from `main`:
   ```bash
   git checkout -b feature/my-feature
   ```
3. Make your changes. Keep commits focused — one logical change per commit.
4. Run `devtools::check()` and ensure there are no new errors or warnings.
5. Run `devtools::test()` and ensure all tests pass.
6. Open a pull request against `main` with a concise description of the change
   and why it is needed.

Both `devtools::check()` and `devtools::test()` must pass before a change can be
merged; CI runs `R CMD check` on every pull request.

### Pull request checklist

- References an issue (e.g. `Fixes #123`)
- No failing `R CMD check`
- Tests provided for new functions where practical

## Code style

- Follow the existing style (base pipe `|>` or magrittr `%>%` as used in
  context, `snake_case` function and variable names).
- Use clear function names and descriptive comments. Keep changes focused.
- Keep helper functions `@keywords internal`.
- Export only user-facing functions (`run_macroibi()`, `generate_reports()`,
  `refresh_taxonomy()`).

## Testing and documentation

The project uses **testthat**; please include basic tests for any new function.

```r
devtools::load_all()   # local testing
devtools::test()       # run the suite
devtools::check()      # full package check
macroibi::run_macroibi()  # exercise the app itself
```

Documentation is written with roxygen2. After changing any roxygen block,
regenerate the `.Rd` files:

```r
devtools::document()
```

## Project structure

```
R/               # Core functions, Shiny modules, app entry point
inst/app/www/    # Bundled app assets (report templates, styles, images)
inst/extdata/    # Bundled taxonomy dataset (taxonomy_YYYY-MM-DD.rds)
inst/docs/       # Reference documents shipped with the package
vignettes/       # Long-form guides (also published to the pkgdown site)
tests/testthat/  # Test suite
tools/           # Deployment and rendering scripts
```

## Taxonomy updates

The bundled taxonomy dataset is built by the companion
[macro-taxonomy](https://github.com/aomop/macro-taxonomy) pipeline, which queries
ITIS and iNaturalist and writes a dated CSV to its `data/output/` directory.

To load a newly built CSV into the package, use `refresh_taxonomy()`:

```r
refresh_taxonomy(
  input_dir   = "path/to/macro-taxonomy/data/output",
  output_path = "inst/extdata/"
)
```

This reads the most recent `taxonomy_YYYYMMDD.csv` from `input_dir` — most
recent by the date in the filename — and writes
`inst/extdata/taxonomy_YYYY-MM-DD.rds`, carrying the source CSV's date across.
`load_taxonomy()` picks up the newest such file automatically on next launch.

## Working with `selected_genera`

The app stores all user-entered taxa and dipnet counts in a nested reactive
structure called `selected_genera`. It is central to the IBI workflow and is a
reactive list of reactive lists, so it is worth understanding before you touch
it.

`selected_genera` is a `reactiveValues` container with one entry per taxon
section in the UI (Beetles, Dragonflies, Gastropods, and so on). Each section
entry is *either* a `reactive()` returning a `reactiveValues` object, *or* a
`reactiveValues` object directly, depending on how that section was initialized.
Inside each section object, the field of interest is `$data`: a list of row-lists
representing user input for that group.

```
selected_genera  # reactiveValues

|- "section_1" -> reactive() -> reactiveValues:
|      $data = list(
|        list(taxon="...", dipnet1=..., dipnet2=..., tsn=..., parentTsn=...),
|        list(...),
|        ...
|      )
|- "section_2" -> reactive() -> reactiveValues:
|      $data = list(...)
|- etc. (one section per taxon group)
```

Because an entry may be either a function or a `reactiveValues` object, always
check the type and handle both:

```r
section_obj <- selected_genera[[section_id]]

# If it's a reactive() / reactiveVal(), call it
if (is.function(section_obj)) {
  section_obj <- section_obj()
}

# Now section_obj is the underlying reactiveValues
rows <- section_obj$data
```

## Questions

Open an issue for general questions, or contact the maintainer at
<sam.swanson@shakopeedakota.org>.
