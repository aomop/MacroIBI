#' Append IBI summary row to metric scores
#'
#' @param metric_df Data frame of metric scores with an `adj_score` column.
#'
#' @return Data frame with an additional "IBI Score (0-50)" row.
#' @keywords internal
summarize_metric_scores <- function(metric_df) {
  ibi_row <- data.frame(
    metric_name  = "IBI Score (0-50)",
    response     = "Decrease",
    min          = NA_real_,
    fifth        = NA_real_,
    ninety_fifth = NA_real_,
    max          = NA_real_,
    metric_value = NA_real_,
    metric_score = NA_real_,
    adj_score    = sum(metric_df$adj_score, na.rm = TRUE),
    stringsAsFactors = FALSE
  )
  
  # Keep the same columns/order as metric_df and append
  common_cols <- union(names(metric_df), names(ibi_row))
  metric_df   <- dplyr::as_tibble(metric_df)[, common_cols]
  ibi_row     <- dplyr::as_tibble(ibi_row)[, common_cols]
  
  dplyr::bind_rows(metric_df, ibi_row)
}

#' Prepare results data for reports
#'
#' @param summarized_df Data frame of current-location metrics.
#' @param combined_df Data frame of comparison metrics (possibly NA).
#' @param raw_data Optional raw taxa data (from the download module).
#'
#' @return A list with `summarized_data`, `combined_metrics`, and `raw_data`,
#'   with `metric_name` factor levels aligned across data frames.
#' @keywords internal
prepare_results_data <- function(summarized_df, combined_df, raw_data = NULL) {
  # Handle case where combined_df might be NA or NULL
  if (is.null(combined_df) || (is.atomic(combined_df) && length(combined_df) == 1L && is.na(combined_df))) {
    combined_df <- summarized_df[0, , drop = FALSE]  # empty with same cols
  }
  
  common_levels <- unique(c(summarized_df$metric_name, combined_df$metric_name))
  
  summarized_df$metric_name <- factor(summarized_df$metric_name, levels = common_levels)
  combined_df$metric_name   <- factor(combined_df$metric_name,   levels = common_levels)
  
  list(
    summarized_data  = summarized_df,
    combined_metrics = combined_df,
    raw_data         = raw_data
  )
}

#' Parse a date string flexibly
#'
#' Attempts to parse a date string using multiple common formats including
#' ISO, US, and European date formats. Returns NA if no format matches.
#'
#' @param date_string A character string representing a date.
#' @return A Date object, or NA if parsing fails.
#'
#' @details
#' Supported formats (in order of precedence):
#' \itemize{
#'   \item ISO: YYYY-MM-DD, YYYY/MM/DD
#'   \item US: MM-DD-YYYY, MM/DD/YYYY, MM-DD-YY, MM/DD/YY
#'   \item Underscore: MM_DD_YYYY, MM_DD_YY
#'   \item European: DD-MM-YYYY, DD/MM/YYYY
#' }
#'
#' @keywords internal
parse_flexible_date <- function(date_string) {
    # Clean up the input
    date_string <- trimws(date_string)
    
    # List of formats to try (order matters—more specific first)
    formats <- c(
      "%Y-%m-%d",   # 2024-07-10
      "%Y/%m/%d",   # 2024/07/10
      "%m-%d-%Y",   # 07-10-2024
      "%m/%d/%Y",   # 07/10/2024
      "%m-%d-%y",   # 07-10-24
      "%m/%d/%y",   # 07/10/24
      "%m_%d_%Y",   # 07_10_2024
      "%m_%d_%y",   # 07_10_24
      "%d-%m-%Y",   # 10-07-2024 (European)
      "%d/%m/%Y"    # 10/07/2024 (European)
    )
    
    for (fmt in formats) {
      parsed <- as.Date(date_string, format = fmt)
      if (!is.na(parsed)) {
        return(parsed)
      }
    }
    
    # If nothing worked, return NA
    NA
  }

#' Format a date for safe use in filenames
#'
#' Converts a Date object or date string to ISO format (YYYY-MM-DD)
#' suitable for use in filenames. Falls back to sanitizing the raw
#' string if parsing fails.
#'
#' @param date_input A Date object, character string, or NULL.
#' @return A character string in YYYY-MM-DD format, or a sanitized
#'   version of the input if parsing fails, or "unknown-date" if NULL.
#' @keywords internal
format_date_for_filename <- function(date_input) {
  # Handle NULL/empty
  if (is.null(date_input) || length(date_input) == 0L ||
      identical(date_input, "") || identical(date_input, NA)) {
    return("unknown-date")
  }

  # If already a Date, format directly
  if (inherits(date_input, "Date")) {
    return(format(date_input, "%Y-%m-%d"))
  }

  # Try parsing as string
  date_parsed <- parse_flexible_date(as.character(date_input))

  if (!is.na(date_parsed)) {
    return(format(date_parsed, "%Y-%m-%d"))
  }

  # Fallback: sanitize raw input
  gsub("[^A-Za-z0-9_-]", "_", as.character(date_input))
}