#' Metric scores UI
#'
#' @param id Module identifier.
#' @return A Shiny UI fragment.
#' @keywords internal
ui_metric_scores <- function(id) {
  ns <- shiny::NS(id)  # Namespace for the module
  shiny::tagList(
    shinycssloaders::withSpinner(
      DT::DTOutput(ns("metric_scores_table"))
    )  # Placeholder for the metric scores table
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
server_metrics <- function(id, selected_genera, taxonomy,
                           unique_taxa_counts, group_totals,
                           grand_total_observations, group_defs) {
  
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    EOT_SECTION <- group_defs$section_id[
      group_defs$group_id == "dragonflies_mayflies_damselflies_and_caddisflies_eot_orders"
    ]
    SNAIL_SECTION <- group_defs$section_id[
      group_defs$group_id == "snails_class_gastropoda"
      ]
    
    # Initialize reactive values to store data for metrics
    cal <- metric_calibration_df()
    cal$metric_value <- NA_real_
    cal$metric_score <- NA_real_
    cal$adj_score    <- NA_real_
    metric_scores <- shiny::reactiveValues(data = cal)
    
    # Reactive observer to calculate metric values
    shiny::observe({
      # Set metric values based on data retrieved from various sections
      metric_scores$data$metric_value <- c(
        safe_reactive_value(unique_taxa_counts[[EOT_SECTION]]),       # EOT
        safe_reactive_value(unique_taxa_counts[[SNAIL_SECTION]]),     # Snails (now via constant)
        sum(purrr::map_dbl(shiny::reactiveValuesToList(unique_taxa_counts), safe_reactive_value), na.rm = TRUE),
        calculate_corixids_ratio(selected_genera, group_totals, taxonomy),
        if (grand_total_observations() > 0) {
          safe_reactive_value(group_totals[[EOT_SECTION]]) / grand_total_observations()
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
      summarized_data <- summarize_metric_scores(metric_scores$data)
      
      DT::datatable(
        summarized_data[, c("metric_name", "response", "metric_value", "metric_score", "adj_score")],
        options = list(dom = 't', paging = FALSE, ordering = FALSE),
        rownames = FALSE,
        colnames = c("Metric Name", "Response to Disturbance", "Metric Value", "Metric Score", "Adjusted Score")
      ) %>%
        DT::formatStyle("metric_name", target = "row", fontWeight = DT::styleEqual("IBI Score (0-50)", "bold")) %>%
        DT::formatRound(c("metric_value", "metric_score", "adj_score"), digits = 2)
    })
    
    return(
        metric_scores
      )
  })
}
