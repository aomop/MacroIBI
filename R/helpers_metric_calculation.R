#' Canonical IBI metric calibration table
#'
#' Returns the five-metric calibration data frame with fixed boundary values
#' (min, 5th-percentile, 95th-percentile, max) derived from reference
#' wetland data. This is the single source of truth used by both the Shiny
#' server and the headless batch export path.
#'
#' @return A data.frame with columns: metric_name, response, min, fifth,
#'   ninety_fifth, max.
#' @keywords internal
metric_calibration_df <- function() {
  data.frame(
    metric_name = c(
      "EOT Taxa: ",
      "Snail Taxa: ",
      "All Taxa: ",
      "Corixid Metric: ",
      "Abundance EOT: "
    ),
    response = c("Decrease", "Decrease", "Decrease", "Increase", "Decrease"),
    min          = c(1,    1,  10, 0,     0),
    fifth        = c(2,    2,  20, 0,     0.002),
    ninety_fifth = c(12,  10,  40, 0.82,  0.16),
    max          = c(14,  12,  50, 1,     0.26),
    stringsAsFactors = FALSE
  )
}

#' Calculate a metric score
#'
#' @param values Metric values.
#' @param min_values Minimum values for scaling.
#' @param max_values Maximum values for scaling.
#' @param scale_factor Scaling factor.
#' @param inverse Whether to invert the scale.
#' @return Numeric scores.
#' @keywords internal
calculate_metric_score <- function(values, min_values, max_values, scale_factor = 10, inverse = FALSE) {
  ifelse(
    !is.na(values) & !is.na(min_values) & !is.na(max_values) & (max_values - min_values != 0),
    if (inverse) {
      scale_factor - ((values - min_values) / (max_values - min_values) * scale_factor)
    } else {
      ((values - min_values) / (max_values - min_values) * scale_factor)
    },
    NA
  )
}

#' Calculate the Corixids ratio metric
#'
#' @param selected_genera Reactive values of selected taxa.
#' @param group_totals Reactive values of group totals.
#' @param taxonomy Taxonomy data frame.
#' @return Numeric ratio.
#' @keywords internal
calculate_corixids_ratio <- function(selected_genera, group_totals, taxonomy) {
  tryCatch({
    count_beetles <- safe_reactive_value(group_totals[["section_2"]])
    count_bugs <- safe_reactive_value(group_totals[["section_4"]])
    
    corixids_taxa <- dplyr::pull(dplyr::filter(taxonomy, .data$Family == "Corixidae"), .data$taxon)
    
    selected_data <- selected_genera[["section_4"]]()
    if (is.list(selected_data$data) & length(selected_data$data) == 0) {
      return(0)
    }
    
    count_corixids <- sum(sapply(selected_data$data, function(row) {
      tryCatch({
        if (row[["taxon"]] %in% corixids_taxa) {
          dipnet1 <- row[["dipnet1"]]
          dipnet2 <- row[["dipnet2"]]
          sum(dipnet1, dipnet2, na.rm = TRUE)
        } else {
          0
        }
      }, error = function(e) {
        0
      })
    }), na.rm = TRUE)
    
    total_beetles_bugs <- count_beetles + count_bugs
    if (total_beetles_bugs > 0) {
      count_corixids / total_beetles_bugs
    } else {
      0
    }
  }, error = function(e) {
    0
  })
}
