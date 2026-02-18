## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  eval = FALSE
)

## ----basic--------------------------------------------------------------------
# library(macroibi)
# generate_reports()

## ----types--------------------------------------------------------------------
# # Only metric CSVs
# generate_reports(output_type = "metrics")
# 
# # Only PDF reports
# generate_reports(output_type = "pdf_report")

## ----specific-----------------------------------------------------------------
# generate_reports(
#   autosave_file = "autosave_MyWetland.rds",
#   output_type = "pdf_report",
#   output_path = "~/reports"
# )

## ----output-path--------------------------------------------------------------
# generate_reports(output_path = "~/wetland_exports/2025")

## ----autosave-path------------------------------------------------------------
# generate_reports(
#   autosave_path = "/shared/macroibi_data",
#   output_path = "~/reports"
# )

## ----results------------------------------------------------------------------
# results <- generate_reports(output_type = "metrics")
# print(results)

## ----script-------------------------------------------------------------------
# #!/usr/bin/env Rscript
# library(macroibi)
# 
# results <- generate_reports(
#   output_type = "all",
#   output_path = "/output/macroibi"
# )
# 
# failures <- results[!results$success, ]
# if (nrow(failures) > 0) {
#   warning("Some reports failed:\n", paste(failures$message, collapse = "\n"))
# }

