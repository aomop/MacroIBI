# Render the Field Sampling & App Guide vignette to a standalone PDF.
#
# The guide is maintained solely as vignettes/field-and-app-guide.Rmd. This
# script produces the printable copy for field crews, intended to be attached to
# a GitHub Release rather than committed to the repository.
#
# Usage (from the package root):
#   Rscript tools/render_field_guide.R [output_dir]

output_dir <- commandArgs(trailingOnly = TRUE)[1]
if (is.na(output_dir)) {
  output_dir <- "."
}

vignette_path <- "vignettes/field-and-app-guide.Rmd"

if (!file.exists(vignette_path)) {
  stop(
    "Cannot find ", vignette_path,
    ". Run this script from the package root.",
    call. = FALSE
  )
}

# Reuse the package's own LaTeX detection so the failure mode matches the app's.
if (!macroibi:::is_latex_available()) {
  stop(
    "No LaTeX installation found. Install one with:\n",
    "  install.packages(\"tinytex\"); tinytex::install_tinytex()",
    call. = FALSE
  )
}

dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

output_file <- sprintf(
  "MacroIBI-Field-and-App-Guide_%s.pdf",
  format(Sys.Date(), "%Y-%m-%d")
)

rmarkdown::render(
  input         = vignette_path,
  output_format = rmarkdown::pdf_document(toc = TRUE, toc_depth = 2),
  output_file   = output_file,
  output_dir    = normalizePath(output_dir),
  quiet         = FALSE
)

message("Wrote ", file.path(output_dir, output_file))
