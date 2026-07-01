# Contributing to macroibi

Thank you for your interest in contributing!

## Reporting issues

Please open an issue at <https://github.com/aomop/MacroIBI/issues> with:

- A clear description of the problem or feature request
- Steps to reproduce (for bugs)
- Your R version and operating system
- The output of `sessionInfo()` if relevant

## Submitting changes

1. Fork the repository and create a branch from `main`.
2. Make your changes. Keep commits focused — one logical change per commit.
3. Run `devtools::check()` and ensure there are no new errors or warnings.
4. Run `devtools::test()` and ensure all tests pass.
5. Open a pull request against `main` with a concise description of the
   change and why it is needed.

## Taxonomy updates

Taxonomy data lives in `inst/extdata/` as a dated RDS file. To regenerate
it from a source CSV directory, use `refresh_taxonomy()`:

```r
refresh_taxonomy(
  input_dir   = "path/to/taxonomy/csvs",
  output_path = "inst/extdata/"
)
```

The output file must follow the naming convention `taxonomy_YYYY-MM-DD.rds`
(ISO date format) to be picked up by `load_taxonomy()`.

## Code style

- Follow the existing style (base pipe `|>` or magrittr `%>%` as used in
  context, `snake_case` function and variable names).
- Keep helper functions `@keywords internal`.
- Export only user-facing functions (`run_macroibi()`, `generate_reports()`,
  `refresh_taxonomy()`).

## Questions

For general questions, open an issue or contact the maintainer at
<sam.swanson@shakopeedakota.org>.
