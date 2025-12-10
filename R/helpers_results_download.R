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
