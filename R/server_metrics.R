#' Metric scores UI
#'
#' @param id Module identifier.
#' @return A Shiny UI fragment.
#' @keywords internal
ui_metric_scores <- function(id) {
  ns <- shiny::NS(id)  # Namespace for the module
  shiny::tagList(
    DT::DTOutput(ns("metric_scores_table"))  # Placeholder for the metric scores table
  )
}

#' Metric scores server
#'
#' @param id Module identifier.
#' @param selected_genera Reactive values of selected genera.
#' @param taxonomy Taxonomy data frame.
#' @param unique_taxa_counts Reactive values of unique taxa counts.
#' @param group_totals Reactive values of group totals.
#' @param grand_total_observations Reactive expression for grand total observations.
#' @return Reactive values for metric scores.
#' @keywords internal
server_metrics <- function(id, selected_genera, taxonomy, unique_taxa_counts, group_totals, grand_total_observations) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns # Namespace for managing IDs within this module

    # Initialize reactive values to store data for metrics
    metric_scores <- shiny::reactiveValues(
      data = data.frame(
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
        metric_value = NA,
        metric_score = NA,
        adj_score = NA
      )
    )
    
    # Reactive observer to calculate metric values
    shiny::observe({
      # Set metric values based on data retrieved from various sections
      metric_scores$data$metric_value <- c(
        safe_reactive_value(unique_taxa_counts[["section_1"]]),
        safe_reactive_value(unique_taxa_counts[["section_7"]]),
        sum(purrr::map_dbl(shiny::reactiveValuesToList(unique_taxa_counts), safe_reactive_value), na.rm = TRUE),
        calculate_corixids_ratio(selected_genera, group_totals, taxonomy),
        if(grand_total_observations() > 0){
          safe_reactive_value(group_totals[["section_1"]]) / grand_total_observations()
        } else (0)
      )
      # Determine which rows require inverse scoring
      increase_rows <- grepl("Increase", metric_scores$data$response)
      
      # Calculate metric scores, adjusting for response type
      metric_scores$data$metric_score[!increase_rows] <- calculate_metric_score(
        values = metric_scores$data$metric_value[!increase_rows],
        min_values = metric_scores$data$min[!increase_rows],
        max_values = metric_scores$data$ninety_fifth[!increase_rows]
      )
      metric_scores$data$metric_score[increase_rows] <- calculate_metric_score(
        values = metric_scores$data$metric_value[increase_rows],
        min_values = metric_scores$data$fifth[increase_rows],
        max_values = metric_scores$data$max[increase_rows],
        inverse = TRUE
      )
      
      # Adjust scores to ensure they fall within the range [0, 10]
      metric_scores$data$adj_score <- pmin(pmax(metric_scores$data$metric_score, 0), 10)
    })
    
    # Render the metric scores table with formatted outputs
    output$metric_scores_table <- DT::renderDT({
      summarized_data <- rbind(
        metric_scores$data,
        data.frame(
          metric_name = "IBI Score (0-50)",
          response = "Decrease",
          min = NA, fifth = NA, ninety_fifth = NA, max = NA,
          metric_value = NA, metric_score = NA,
          adj_score = sum(metric_scores$data$adj_score, na.rm = TRUE)
        )
      )
      
      DT::datatable(
        summarized_data[, c("metric_name", "response", "metric_value", "metric_score", "adj_score")],
        options = list(dom = 't', paging = FALSE, ordering = FALSE),
        rownames = FALSE,
        colnames = c("Metric Name", "Response to Disturbance", "Metric Value", "Metric Score", "Adjusted Score")
      ) |>
        DT::formatStyle("metric_name", target = "row", fontWeight = DT::styleEqual("IBI Score (0-50)", "bold")) |>
        DT::formatRound(c("metric_value", "metric_score", "adj_score"), digits = 2)
    })
    
    return(
        metric_scores
      )
  })
}