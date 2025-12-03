#' Results download UI
#'
#' @param id Module identifier.
#' @return A Shiny UI fragment.
#' @keywords internal
results_download_ui <- function(id, demo_mode) {
  ns <- shiny::NS(id)
  shinyjs::useShinyjs()
  shinybusy::use_busy_spinner(spin = "fading-circle")
  shiny::tagList(
    shiny::downloadButton(ns("download_csv"), "Download Metric Data (CSV)", style = "background-color: #25b3ff", class = "but1"),
    if(!demo_mode){
      shiny::downloadButton(ns("download_img"), "Download Table Image (PNG)", style = "background-color: #6eacbd", class = "but2")
    },
    shiny::downloadButton(ns("download_small_report"), "Download Data Summary (PDF)", style = "background-color: #b6a67b", class = "but3"),
    shiny::downloadButton(ns("download_full_report"), "Download Full Report (PDF)", style = "background-color: #ff9f39", class = "but4")
  )
}

#' Results download server
#'
#' @param id Module identifier.
#' @param metric_scores Reactive values for metric scores.
#' @param shared_reactives Shared reactive values.
#' @param selected_genera Reactive values of selected genera.
#' @param taxonomy Taxonomy data frame.
#' @param metric_save_path Path where metric autosave files are stored.
#' @param group_defs Data frame with columns section_id, group_id, group_name
#'   mapping section modules to stable groups.
#' @keywords internal
results_download_server <- function(
    id,
    metric_scores,
    shared_reactives,
    selected_genera,
    taxonomy,
    metric_save_path = get_app_path("metric_autosave_dir"),
    group_defs
) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    all_metric_files <- list.files(metric_save_path, pattern = "^metrics_", full.names = TRUE)

    current_metric_file <- file.path(metric_save_path, paste0("metrics_", isolate(shared_reactives$user_title), ".rds"))
    
    # Exclude the current location's metric file from the list
    filtered_metric_files <- all_metric_files[all_metric_files != current_metric_file]
    
    if (length(filtered_metric_files) > 0) {
      metric_data_list <- lapply(filtered_metric_files, readRDS)
      combined_metrics <- do.call(rbind, metric_data_list)
    } else {
      combined_metrics <- NA  # or set defaults as needed
    }
    
    summarized_data <- shiny::reactive(
      rbind(
      metric_scores$data,
      data.frame(
        metric_name = "IBI Score (0-50)",
        response = "Decrease",
        min = NA, fifth = NA, ninety_fifth = NA, max = NA,
        metric_value = NA, metric_score = NA,
        adj_score = sum(metric_scores$data$adj_score, na.rm = TRUE)
      )
    ))
    
    shared_data <- shiny::reactiveVal()

    full_report_template <- system.file("app/www", "full_report_template.Rmd", package = "macroibi")
    datasum_report_template <- system.file("app/www", "datasum_report_template.Rmd", package = "macroibi")
    
    download_module_server(
      id = "exposed_data",
      selected_genera = selected_genera,
      shared_reactives = shared_reactives,
      group_defs = group_defs,
      expose_data_reactive = shared_data
    )
    
    prepared_data <- shiny::reactive({
      # Get the summarized data (current location's metrics)
      df1 <- summarized_data()  
      
      df2 <- combined_metrics
  
      # Determine common levels across both datasets
      common_levels <- unique(c(df1$metric_name, df2$metric_name))
      
      # Set the factor levels for metric_name in both data frames
      df1$metric_name <- factor(df1$metric_name, levels = common_levels)
      df2$metric_name <- factor(df2$metric_name, levels = common_levels)
      
      # Return a list containing both
      list(summarized_data = df1, combined_metrics = df2, raw_data = shared_data())
    })
    
    table <- shiny::reactive(
      DT::datatable(
        summarized_data()[, c("metric_name", "response", "metric_value", "metric_score", "adj_score")],
        options = list(dom = 't', paging = FALSE, ordering = FALSE),
        rownames = FALSE,
        colnames = c("Metric Name", "Response to Disturbance", "Metric Value", "Metric Score", "Adjusted Score")
      ) |>
        DT::formatStyle("metric_name", target = "row", fontWeight = DT::styleEqual("IBI Score (0-50)", "bold")) |>
        DT::formatRound(c("metric_value", "metric_score", "adj_score"), digits = 2)
    )
    
    # Download CSV
    output$download_csv <- shiny::downloadHandler(
      filename = function() {
        paste0("results_", shared_reactives$user_title, "_", shared_reactives$user_date, ".csv")
      },
      content = function(file) {
        shiny::withProgress(message = "Preparing CSV...", value = 0, {
          shiny::incProgress(0.5, detail = "Processing data...")
          readr::write_csv(summarized_data(), file)
          shiny::incProgress(0.9, detail = "Finalizing...")
        })
      }
    )
    
    # Download Table as Image
    output$download_img <- shiny::downloadHandler(
      filename = function() {
        paste0(
          "table_",
          shared_reactives$user_title, "_",
          shared_reactives$user_date,
          ".png"
        )
      },
      content = function(file) {
        shiny::withProgress(message = "Generating Image...", value = 0, {
          shiny::incProgress(0.1, detail = "Rendering title...")
          
          title_text <- shiny::tags$h2(
            shiny::HTML(paste0(
              "<span style='color: #2C3E50; font-size: 24px; font-weight: bold; display: block; margin-bottom: 0px;'>
Macroinvertebrate Index of Biotic Integrity for</span>", 
              "<span style='color: #2C3E50; font-size: 32px; font-weight: bold; text-decoration: underline; display: block; margin-top: 5px; margin-bottom: 5px;'>",
              shared_reactives$user_title, "</span>"
            )),
            style = "text-align: center; margin-bottom: 5px;"
          )
          
          shiny::incProgress(0.3, detail = "Rendering date...")
          date_text <- shiny::tags$p(
            shiny::HTML(paste0(
              "<span style='color: #555555; font-size: 18px; display: block; margin-top: 0px;'>
Sampled on<strong> ",
              add_ordinal_suffix(
                format(
                  as.Date(shared_reactives$user_date, format = "%m/%d/%y"),
                  "%B %d, %Y"
                ),
                style = "%B %d, %Y"
              ),
              "</strong>",
              "  |  Calculated on<strong> ",
              add_ordinal_suffix(format(Sys.Date(), "%B %d, %Y"), style = "%B %d, %Y"),
              "</strong>",
              "</span>"
            )),
            style = "text-align: center; margin-top: 5px;"
          )
          
          shiny::incProgress(0.5, detail = "Putting it all together...")
          full_html <- shiny::tags$html(
            shiny::tags$head(shiny::tags$style("#results_section { padding: 20px; }")),
            shiny::tags$body(
              shiny::tags$div(id = "results_section", title_text, date_text, table())
            )
          )
          
          shiny::incProgress(0.7, detail = "Saving html file...")
          temp_html <- tempfile(fileext = ".html")
          htmltools::save_html(full_html, temp_html)
          
          shiny::incProgress(0.9, detail = "Converting html to png...")
          
          # Use file:// URL so Chrome can open the local file
          url <- paste0("file://", normalizePath(temp_html))
          
          webshot2::webshot(
            url,
            file      = file,
            vwidth    = 1200,
            vheight   = 800,
            selector  = "#results_section",  # <- key bit
            delay     = 0.5,
            zoom      = 2
          )
          
          shiny::incProgress(1, detail = "Rendering complete!")
        })
      }
    )
    
    output$download_full_report <- shiny::downloadHandler(
      filename = function() {
        paste0("Report_", shared_reactives$user_title, "_", shared_reactives$user_date, ".pdf")
      },
      content = function(file) {
        shiny::withProgress(message = "Generating report...", value = 0, {
          
          # Step 1: Copy the Rmd template
          shiny::incProgress(0.1, detail = "Preparing template...")
          temp_rmd <- tempfile(fileext = ".Rmd")
          file.copy(full_report_template, temp_rmd, overwrite = TRUE)
          
          # Step 2: Render the R Markdown file
          shiny::incProgress(0.5, detail = "Rendering report...")
          rmarkdown::render(
            input = temp_rmd,
            output_file = file,
            params = list(
              user_title = shared_reactives$user_title,
              user_date = shared_reactives$user_date,
              data = prepared_data()$summarized_data,
              comparison_metrics = prepared_data()$combined_metrics
            ),
            envir = new.env(parent = asNamespace("macroibi"))
          )
          
          shiny::incProgress(1, detail = "Completed!")
        })
      }
    )
    
    output$download_small_report <- shiny::downloadHandler(
      filename = function() {
        paste0("DataSummary_", shared_reactives$user_title, "_", shared_reactives$user_date, ".pdf")
      },
      content = function(file) {
        shiny::withProgress(message = "Generating report...", value = 0, {
          
          # Step 1: Copy the Rmd template
          shiny::incProgress(0.1, detail = "Preparing template...")
          temp_rmd <- tempfile(fileext = ".Rmd")
          file.copy(datasum_report_template, temp_rmd, overwrite = TRUE)
          
          # Step 2: Render the R Markdown file
          shiny::incProgress(0.5, detail = "Rendering report...")
          rmarkdown::render(
            input = temp_rmd,
            output_file = file,
            params = list(
              user_title = shared_reactives$user_title,
              user_date = shared_reactives$user_date,
              metric_data = prepared_data()$summarized_data,
              comparison_metrics = prepared_data()$combined_metrics,
              raw_data = prepared_data()$raw_data,
              taxonomy = taxonomy
            ),
            envir = new.env(parent = asNamespace("macroibi"))
          )
          
          shiny::incProgress(1, detail = "Completed!")
        })
      }
    )
  })
}


