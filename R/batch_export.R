#' Generate reports from autosave files
#'
#' Batch-generate IBI reports and data exports from autosave files without
#' running the Shiny application. This is useful for programmatic report
#' generation and automation.
#'
#' @param autosave_file Character. Either the filename of a specific autosave
#'   file (e.g., "autosave_MyWetland.rds"), or `"all"` to process all available
#'   autosave files. Defaults to `"all"`.
#' @param output_type Character. The type of output to generate. One of:
#'   \itemize{
#'     \item `"metrics"` - Metric scores as CSV
#'     \item `"raw_data"` - Raw taxa data as CSV
#'     \item `"pdf_report"` - Full PDF report
#'     \item `"pdf_summary"` - Data summary PDF
#'     \item `"all"` - Generate all output types
#'   }
#'   Defaults to `"all"`.
#' @param output_path Character. Directory path where output files will be
#'   saved. The directory will be created if it does not exist. Defaults to
#'   the current working directory.
#' @param autosave_path Character. Path to the directory containing autosave
#'   files. Defaults to the package's autosave directory.
#'
#' @return A data frame with columns:
#'   \itemize{
#'     \item `autosave_file` - Name of the processed autosave file
#'     \item `output_type` - Type of output generated
#'     \item `output_file` - Path to the generated file
#'     \item `success` - Logical indicating whether generation succeeded
#'     \item `message` - Status or error message
#'   }
#'
#' @examples
#' \dontrun{
#' # Generate all outputs for all autosave files
#' generate_reports()
#'
#' # Generate only PDF reports for a specific file
#' generate_reports(
#'   autosave_file = "autosave_MyWetland.rds",
#'   output_type = "pdf_report",
#'   output_path = "~/reports"
#' )
#'
#' # Generate CSV exports for all files
#' generate_reports(output_type = "metrics", output_path = "~/exports")
#' }
#'
#' @export
generate_reports <- function(
    autosave_file = "all",
    output_type = "all",
    output_path = getwd(),
    autosave_path = NULL
) {
  # Validate output_type
  valid_types <- c("metrics", "raw_data", "pdf_report", "pdf_summary", "all")
  if (!output_type %in% valid_types) {
    stop(
      "Invalid output_type: '", output_type, "'. ",
      "Must be one of: ", paste(valid_types, collapse = ", "),
      call. = FALSE
    )
  }

  # Initialize app state to ensure paths are available
  init_app_state()

  # Set autosave path
  if (is.null(autosave_path)) {
    autosave_path <- get_app_path("autosave_dir")
  }

  if (!dir.exists(autosave_path)) {
    stop("Autosave directory does not exist: ", autosave_path, call. = FALSE)
  }

  # Create output directory if needed
  if (!dir.exists(output_path)) {
    dir.create(output_path, recursive = TRUE, showWarnings = FALSE)
  }

  # Load taxonomy and build group definitions
  taxonomy <- load_taxonomy()
  group_list <- levels(taxonomy$Group)
  group_defs <- build_group_defs(group_list)

  # Find autosave files to process
  if (autosave_file == "all") {
    files <- list.files(autosave_path, pattern = "^autosave_.*\\.rds$", full.names = FALSE)
    if (length(files) == 0L) {
      message("No autosave files found in: ", autosave_path)
      return(invisible(data.frame(
        autosave_file = character(),
        output_type = character(),
        output_file = character(),
        success = logical(),
        message = character(),
        stringsAsFactors = FALSE
      )))
    }
  } else {
    if (!file.exists(file.path(autosave_path, autosave_file))) {
      stop("Autosave file not found: ", file.path(autosave_path, autosave_file), call. = FALSE)
    }
    files <- autosave_file
  }

  # Determine which output types to generate
  if (output_type == "all") {
    types_to_generate <- c("metrics", "raw_data", "pdf_report", "pdf_summary")
  } else {
    types_to_generate <- output_type
  }

  # Process each file
  results <- lapply(files, function(file) {
    process_autosave_file(
      file = file,
      autosave_path = autosave_path,
      output_path = output_path,
      types_to_generate = types_to_generate,
      taxonomy = taxonomy,
      group_defs = group_defs
    )
  })

  # Combine results
  result_df <- do.call(rbind, results)
  rownames(result_df) <- NULL

  # Print summary
  n_success <- sum(result_df$success)
  n_total <- nrow(result_df)
  message(sprintf("Generated %d/%d outputs successfully.", n_success, n_total))

  invisible(result_df)
}

#' Process a single autosave file and generate outputs
#'
#' @param file Autosave filename.
#' @param autosave_path Path to autosave directory.
#' @param output_path Path to output directory.
#' @param types_to_generate Character vector of output types.
#' @param taxonomy Taxonomy data frame.
#' @param group_defs Group definitions data frame.
#' @return Data frame of results.
#' @keywords internal
process_autosave_file <- function(
    file,
    autosave_path,
    output_path,
    types_to_generate,
    taxonomy,
    group_defs
) {
  file_path <- file.path(autosave_path, file)

  # Try to load the autosave data
  data <- tryCatch(
    readRDS(file_path),
    error = function(e) NULL
  )

  if (is.null(data)) {
    return(data.frame(
      autosave_file = file,
      output_type = types_to_generate,
      output_file = NA_character_,
      success = FALSE,
      message = "Failed to read autosave file",
      stringsAsFactors = FALSE
    ))
  }

  # Extract metadata
  user_title <- if ("Title" %in% names(data)) data$Title[1] else "Unknown"
  user_date <- if ("Date" %in% names(data)) data$Date[1] else NA

  # Parse date for filename
  date_for_filename <- format_date_for_filename(user_date)

  # Remove metadata columns before processing
  taxa_data <- data[, !(names(data) %in% c("Title", "Date", "schema_version")), drop = FALSE]

  # Rebuild section data structure
  section_data <- tryCatch(
    build_sections_from_autosave(taxa_data, group_defs),
    error = function(e) NULL
  )

  if (is.null(section_data) || length(section_data) == 0L) {
    return(data.frame(
      autosave_file = file,
      output_type = types_to_generate,
      output_file = NA_character_,
      success = FALSE,
      message = "Failed to parse autosave data structure",
      stringsAsFactors = FALSE
    ))
  }

  # Calculate metrics
  metric_scores <- calculate_metrics_from_sections(section_data, taxonomy, group_defs)

  # Generate each output type
  results <- lapply(types_to_generate, function(type) {
    generate_single_output(
      type = type,
      output_path = output_path,
      user_title = user_title,
      user_date = user_date,
      date_for_filename = date_for_filename,
      metric_scores = metric_scores,
      taxa_data = taxa_data,
      autosave_file = file,
      taxonomy = taxonomy
    )
  })

  do.call(rbind, results)
}

#' Calculate metrics from section data without reactives
#'
#' @param section_data Named list of section data (from build_sections_from_autosave).
#' @param taxonomy Taxonomy data frame.
#' @param group_defs Group definitions data frame.
#' @return Data frame of metric scores.
#' @keywords internal
calculate_metrics_from_sections <- function(section_data, taxonomy, group_defs) {
  # Find section IDs for EOT and Snails
  eot_section <- group_defs$section_id[
    group_defs$group_id == "dragonflies_mayflies_damselflies_and_caddisflies_eot_orders"
  ]
  snail_section <- group_defs$section_id[
    group_defs$group_id == "snails_class_gastropoda"
  ]
  beetles_section <- group_defs$section_id[
    group_defs$group_id == "beetles_order_coleoptera"
  ]
  bugs_section <- group_defs$section_id[
    group_defs$group_id == "true_bugs_order_hemiptera"
  ]

  # Calculate unique taxa counts per section
  unique_taxa_counts <- sapply(names(section_data), function(sid) {
    section <- section_data[[sid]]
    if (is.null(section$data) || length(section$data) == 0L) return(0L)
    length(unique(sapply(section$data, function(row) row$taxon)))
  })

  # Calculate group totals (sum of dipnet1 + dipnet2) per section
  group_totals <- sapply(names(section_data), function(sid) {
    section <- section_data[[sid]]
    if (is.null(section$data) || length(section$data) == 0L) return(0)
    sum(sapply(section$data, function(row) {
      sum(row$dipnet1 %||% 0, row$dipnet2 %||% 0, na.rm = TRUE)
    }))
  })

  # Calculate individual metrics
  eot_taxa <- if (eot_section %in% names(unique_taxa_counts)) unique_taxa_counts[[eot_section]] else 0
  snail_taxa <- if (snail_section %in% names(unique_taxa_counts)) unique_taxa_counts[[snail_section]] else 0
  all_taxa <- sum(unique_taxa_counts, na.rm = TRUE)

  grand_total <- sum(group_totals, na.rm = TRUE)
  eot_abundance <- if (eot_section %in% names(group_totals) && grand_total > 0) {
    group_totals[[eot_section]] / grand_total
  } else {
    0
  }

  # Calculate corixid ratio
  corixid_ratio <- calculate_corixid_ratio_static(
    section_data = section_data,
    group_totals = group_totals,
    taxonomy = taxonomy,
    beetles_section = beetles_section,
    bugs_section = bugs_section
  )

  # Build metric scores data frame
  metric_data <- data.frame(
    metric_name = c(
      "EOT Taxa: ",
      "Snail Taxa: ",
      "All Taxa: ",
      "Corixid Metric: ",
      "Abundance EOT: "
    ),
    response = c("Decrease", "Decrease", "Decrease", "Increase", "Decrease"),
    min = c(1, 1, 10, 0, 0),
    fifth = c(2, 2, 20, 0, 0.002),
    ninety_fifth = c(12, 10, 40, 0.82, 0.16),
    max = c(14, 12, 50, 1, 0.26),
    metric_value = c(eot_taxa, snail_taxa, all_taxa, corixid_ratio, eot_abundance),
    stringsAsFactors = FALSE
  )

  # Calculate scores
  increase_rows <- grepl("Increase", metric_data$response)

  metric_data$metric_score <- NA_real_
  metric_data$metric_score[!increase_rows] <- calculate_metric_score(
    values = metric_data$metric_value[!increase_rows],
    min_values = metric_data$min[!increase_rows],
    max_values = metric_data$ninety_fifth[!increase_rows]
  )
  metric_data$metric_score[increase_rows] <- calculate_metric_score(
    values = metric_data$metric_value[increase_rows],
    min_values = metric_data$fifth[increase_rows],
    max_values = metric_data$max[increase_rows],
    inverse = TRUE
  )

  # Adjust scores to [0, 10] range
  metric_data$adj_score <- pmin(pmax(metric_data$metric_score, 0), 10)

  # Add IBI summary row
  summarize_metric_scores(metric_data)
}

#' Calculate corixid ratio without reactives
#'
#' @param section_data Section data list.
#' @param group_totals Named vector of group totals.
#' @param taxonomy Taxonomy data frame.
#' @param beetles_section Section ID for beetles.
#' @param bugs_section Section ID for bugs.
#' @return Numeric corixid ratio.
#' @keywords internal
calculate_corixid_ratio_static <- function(
    section_data,
    group_totals,
    taxonomy,
    beetles_section,
    bugs_section
) {
  count_beetles <- if (beetles_section %in% names(group_totals)) group_totals[[beetles_section]] else 0
  count_bugs <- if (bugs_section %in% names(group_totals)) group_totals[[bugs_section]] else 0

  # Get corixid taxa from taxonomy
  corixids_taxa <- taxonomy$taxon[taxonomy$Family == "Corixidae" & !is.na(taxonomy$Family)]

  # Count corixids in bugs section
  count_corixids <- 0
  if (bugs_section %in% names(section_data)) {
    bugs_data <- section_data[[bugs_section]]$data
    if (!is.null(bugs_data) && length(bugs_data) > 0L) {
      count_corixids <- sum(sapply(bugs_data, function(row) {
        if (row$taxon %in% corixids_taxa) {
          sum(row$dipnet1 %||% 0, row$dipnet2 %||% 0, na.rm = TRUE)
        } else {
          0
        }
      }))
    }
  }

  total_beetles_bugs <- count_beetles + count_bugs
  if (total_beetles_bugs > 0) {
    count_corixids / total_beetles_bugs
  } else {
    0
  }
}

#' Generate a single output file
#'
#' @param type Output type.
#' @param output_path Output directory path.
#' @param user_title User title from autosave.
#' @param user_date User date from autosave.
#' @param date_for_filename Formatted date for filename.
#' @param metric_scores Calculated metric scores data frame.
#' @param taxa_data Raw taxa data.
#' @param autosave_file Original autosave filename.
#' @param taxonomy Taxonomy data frame for Level lookup.
#' @return Data frame with result.
#' @keywords internal
generate_single_output <- function(
    type,
    output_path,
    user_title,
    user_date,
    date_for_filename,
    metric_scores,
    taxa_data,
    autosave_file,
    taxonomy
) {
  # Sanitize title for filename
  safe_title <- gsub("[^A-Za-z0-9_-]", "_", user_title)

  result <- tryCatch({
    switch(type,
      "metrics" = {
        filename <- paste0("results_", safe_title, "__", date_for_filename, ".csv")
        filepath <- file.path(output_path, filename)
        readr::write_csv(metric_scores, filepath)
        list(file = filepath, success = TRUE, message = "OK")
      },
      "raw_data" = {
        filename <- paste0("IBI_", safe_title, "_", date_for_filename, ".csv")
        filepath <- file.path(output_path, filename)
        # Add Level column by looking up tsn in taxonomy
        export_data <- taxa_data
        export_data$Level <- taxonomy$level[match(export_data$tsn, taxonomy$tsn)]
        export_data$Common <- taxonomy$common_names[match(export_data$tsn, taxonomy$tsn)]
        # Add back metadata columns
        export_data$Title <- user_title
        export_data$Date <- user_date
        export_data$schema_version <- "2.0.0"
        # Reorder columns to match download_module output structure
        export_data <- export_data[, c(
          "group_id", "group_name", "section_id", "Taxon", "Common", "Level",
          "Dipnet1", "Dipnet2", "tsn", "parentTsn", "Title", "Date", "schema_version"
        )]
        readr::write_csv(export_data, filepath)
        list(file = filepath, success = TRUE, message = "OK")
      },
      "pdf_report" = {
        filename <- paste0("Report_", safe_title, "_", date_for_filename, ".pdf")
        filepath <- file.path(output_path, filename)
        generate_pdf_report(
          filepath = filepath,
          user_title = user_title,
          user_date = user_date,
          metric_scores = metric_scores,
          template_name = "full_report_template.Rmd"
        )
        list(file = filepath, success = TRUE, message = "OK")
      },
      "pdf_summary" = {
        filename <- paste0("DataSummary_", safe_title, "_", date_for_filename, ".pdf")
        filepath <- file.path(output_path, filename)
        generate_pdf_report(
          filepath = filepath,
          user_title = user_title,
          user_date = user_date,
          metric_scores = metric_scores,
          template_name = "datasum_report_template.Rmd"
        )
        list(file = filepath, success = TRUE, message = "OK")
      },
      list(file = NA_character_, success = FALSE, message = paste("Unknown type:", type))
    )
  }, error = function(e) {
    list(file = NA_character_, success = FALSE, message = conditionMessage(e))
  })

  data.frame(
    autosave_file = autosave_file,
    output_type = type,
    output_file = result$file,
    success = result$success,
    message = result$message,
    stringsAsFactors = FALSE
  )
}

#' Generate a PDF report from template
#'
#' @param filepath Output file path.
#' @param user_title Report title.
#' @param user_date Sampling date.
#' @param metric_scores Metric scores data frame.
#' @param template_name Name of the Rmd template file.
#' @keywords internal
generate_pdf_report <- function(
    filepath,
    user_title,
    user_date,
    metric_scores,
    template_name
) {
  template_path <- system.file("app/www", template_name, package = "macroibi")

  if (!nzchar(template_path) || !file.exists(template_path)) {
    stop("Report template not found: ", template_name, call. = FALSE)
  }

  temp_rmd <- tempfile(fileext = ".Rmd")
  file.copy(template_path, temp_rmd, overwrite = TRUE)

  # Prepare params based on template
  params <- list(
    user_title = user_title,
    user_date = user_date,
    report_date = add_ordinal_suffix(Sys.Date()),
    data = metric_scores,
    comparison_metrics = metric_scores[0, , drop = FALSE]  # Empty comparison
  )

  # For datasum template, add additional params
  if (grepl("datasum", template_name)) {
    params$metric_data <- metric_scores
    params$raw_data <- NULL
    params$taxonomy <- load_taxonomy()
    # Calculate quality class
    total_score <- metric_scores$adj_score[metric_scores$metric_name == "IBI Score (0-50)"]
    params$quality_class <- dplyr::case_when(
      total_score >= 38 ~ "4 (Excellent)",
      total_score >= 28 ~ "3 (Good)",
      total_score >= 20 ~ "2 (Fair)",
      total_score >= 10 ~ "1 (Poor)",
      TRUE ~ NA_character_
    )
  }

  rmarkdown::render(
    input = temp_rmd,
    output_file = filepath,
    params = params,
    envir = new.env(parent = asNamespace("macroibi")),
    quiet = TRUE
  )

  invisible(filepath)
}

#' Null-coalescing operator
#'
#' Returns the left-hand side if not NULL, otherwise the right-hand side.
#'
#' @param x Left-hand side value.
#' @param y Right-hand side value (default).
#' @return x if not NULL, otherwise y.
#' @keywords internal
`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}
