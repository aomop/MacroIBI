testthat::test_that("summarize_metric_scores appends IBI row with correct total", {
  # Minimal fake metric scores
  metric_df <- data.frame(
    metric_name  = c("Metric A", "Metric B"),
    response     = c("Increase", "Decrease"),
    min          = c(0, 0),
    fifth        = c(1, 1),
    ninety_fifth = c(9, 9),
    max          = c(10, 10),
    metric_value = c(5.5, 3.2),
    metric_score = c(8, 7),
    adj_score    = c(5, 10),
    stringsAsFactors = FALSE
  )
  
  res <- summarize_metric_scores(metric_df)
  
  # Should have original rows + 1 IBI row
  testthat::expect_s3_class(res, "data.frame")
  testthat::expect_equal(nrow(res), nrow(metric_df) + 1L)
  
  # IBI row should be the last row
  ibi_row <- res[nrow(res), , drop = FALSE]
  
  testthat::expect_equal(ibi_row$metric_name, "IBI Score (0-50)")
  testthat::expect_equal(ibi_row$response, "Decrease")
  
  # adj_score should be the sum of input adj_scores
  testthat::expect_equal(
    ibi_row$adj_score,
    sum(metric_df$adj_score, na.rm = TRUE)
  )
  
  # Other numeric columns on the IBI row should be NA
  numeric_cols <- c("min", "fifth", "ninety_fifth", "max", "metric_value", "metric_score")
  for (col in numeric_cols) {
    testthat::expect_true(is.na(ibi_row[[col]]))
  }
  
  # Original rows unchanged in order and values
  testthat::expect_equal(
    as.data.frame(res[seq_len(nrow(metric_df)), names(metric_df)], stringsAsFactors = FALSE),
    metric_df
  )
})

testthat::test_that("summarize_metric_scores handles NA adj_score safely", {
  metric_df <- data.frame(
    metric_name  = c("Metric A", "Metric B"),
    response     = c("Increase", "Decrease"),
    min          = NA_real_,
    fifth        = NA_real_,
    ninety_fifth = NA_real_,
    max          = NA_real_,
    metric_value = c(1.1, 2.2),
    metric_score = c(NA_real_, 3),
    adj_score    = c(NA_real_, 4),
    stringsAsFactors = FALSE
  )
  
  res <- summarize_metric_scores(metric_df)
  
  ibi_row <- res[nrow(res), , drop = FALSE]
  
  # Sum should ignore NA
  testthat::expect_equal(
    ibi_row$adj_score,
    sum(metric_df$adj_score, na.rm = TRUE)
  )
})

testthat::test_that("prepare_results_data aligns metric_name factor levels", {
  summarized_df <- data.frame(
    metric_name  = c("Metric A", "Metric B", "IBI Score (0-50)"),
    response     = c("Increase", "Decrease", "Decrease"),
    metric_value = c(1.0, 2.0, NA),
    stringsAsFactors = FALSE
  )
  
  combined_df <- data.frame(
    metric_name  = c("Metric B", "Metric C"),
    response     = c("Decrease", "Increase"),
    metric_value = c(1.5, 3.2),
    stringsAsFactors = FALSE
  )
  
  raw_data <- list(foo = "bar")
  
  res <- prepare_results_data(
    summarized_df = summarized_df,
    combined_df   = combined_df,
    raw_data      = raw_data
  )
  
  # Structure checks
  testthat::expect_true(all(c("summarized_data", "combined_metrics", "raw_data") %in% names(res)))
  
  df1 <- res$summarized_data
  df2 <- res$combined_metrics
  
  # metric_name should be factors in both
  testthat::expect_s3_class(df1$metric_name, "factor")
  testthat::expect_s3_class(df2$metric_name, "factor")
  
  # Factor levels should be identical and equal to union of names
  expected_levels <- unique(c(summarized_df$metric_name, combined_df$metric_name))
  testthat::expect_equal(levels(df1$metric_name), expected_levels)
  testthat::expect_equal(levels(df2$metric_name), expected_levels)
  
  # Raw data should be passed through unchanged
  testthat::expect_identical(res$raw_data, raw_data)
})

testthat::test_that("prepare_results_data handles combined_df = NA by creating empty comparison", {
  summarized_df <- data.frame(
    metric_name  = c("Metric A", "Metric B"),
    response     = c("Increase", "Decrease"),
    metric_value = c(1.0, 2.0),
    stringsAsFactors = FALSE
  )
  
  # Simulate the `combined_metrics <- NA` case from results_download_server
  combined_df <- NA
  
  res <- prepare_results_data(
    summarized_df = summarized_df,
    combined_df   = combined_df,
    raw_data      = NULL
  )
  
  df1 <- res$summarized_data
  df2 <- res$combined_metrics
  
  # summarized_data should match input (apart from metric_name being factor)
  testthat::expect_equal(
    as.character(df1$metric_name),
    summarized_df$metric_name
  )
  
  # combined_metrics should be an empty data frame with same columns as summarized_df
  testthat::expect_s3_class(df2, "data.frame")
  testthat::expect_equal(nrow(df2), 0L)
  testthat::expect_equal(names(df2), names(df1))
  
  # metric_name factor levels still aligned
  testthat::expect_s3_class(df1$metric_name, "factor")
  testthat::expect_s3_class(df2$metric_name, "factor")
  testthat::expect_equal(levels(df1$metric_name), levels(df2$metric_name))
})
