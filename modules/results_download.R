results_download_ui <- function(id) {
  ns <- NS(id)
  useShinyjs()  
  use_busy_spinner(spin = "fading-circle")
  tagList(
    downloadButton(ns("download_csv"), "Download Metric Data (CSV)", style = "background-color: #25b3ff", class = "but1"),
    downloadButton(ns("download_img"), "Download Table Image (PNG)", style = "background-color: #6eacbd", class = "but2"),
    downloadButton(ns("download_small_report"), "Download Data Summary (PDF)", style = "background-color: #b6a67b", class = "but3"),
    downloadButton(ns("download_full_report"), "Download Full Report (PDF)", style = "background-color: #ff9f39", class = "but4")
  )
}


results_download_server <- function(id, metric_scores, shared_reactives, selected_genera, taxonomy) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Retrieve metric autosave files
    metric_autosave_path <- "metric_autosaves"
    all_metric_files <- list.files(metric_autosave_path, pattern = "^metrics_", full.names = TRUE)
    
    # Define the file corresponding to the current location
    current_metric_file <- file.path(metric_autosave_path, paste0("metrics_", isolate(shared_reactives$user_title), ".rds"))
    
    # Exclude the current location's metric file from the list
    filtered_metric_files <- all_metric_files[all_metric_files != current_metric_file]
    
    if (length(filtered_metric_files) > 0) {
      metric_data_list <- lapply(filtered_metric_files, readRDS)
      combined_metrics <- do.call(rbind, metric_data_list)
    } else {
      combined_metrics <- NA  # or set defaults as needed
    }
    
    summarized_data <- reactive(
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
    
    shared_data <- reactiveVal()
    
    download_module_server(
      id = "exposed_data",
      selected_genera = selected_genera,
      shared_reactives = shared_reactives,
      expose_data_reactive = shared_data
    )
    
    prepared_data <- reactive({
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
    
    table <- reactive(
      datatable(
        summarized_data()[, c("metric_name", "response", "metric_value", "metric_score", "adj_score")],
        options = list(dom = 't', paging = FALSE, ordering = FALSE),
        rownames = FALSE,
        colnames = c("Metric Name", "Response to Disturbance", "Metric Value", "Metric Score", "Adjusted Score")
      ) %>%
        formatStyle("metric_name", target = "row", fontWeight = styleEqual("IBI Score (0-50)", "bold")) %>%
        formatRound(c("metric_value", "metric_score", "adj_score"), digits = 2)
    )
    
    # Download CSV
    output$download_csv <- downloadHandler(
      filename = function() {
        paste0("results_", shared_reactives$user_title, "_", shared_reactives$user_date, ".csv")
      },
      content = function(file) {
        withProgress(message = "Preparing CSV...", value = 0, {
          incProgress(0.5, detail = "Processing data...")
          write.csv(summarized_data(), file, row.names = FALSE)
          incProgress(0.9, detail = "Finalizing...")
        })
      }
    )
    
    # Download Table as Image
    output$download_img <- downloadHandler(
      filename = function() {
        paste0("table_", shared_reactives$user_title, "_", shared_reactives$user_date, ".png")
      },
      content = function(file) {
        withProgress(message = "Generating Image...", value = 0, {
          incProgress(0.1, detail = "Rendering title...")
        title_text <- tags$h2(
          HTML(paste0(
            "<span style='color: #2C3E50; font-size: 24px; font-weight: bold; display: block; margin-bottom: 0px;'>
    Macroinvertebrate Index of Biotic Integrity for</span>", 
            "<span style='color: #2C3E50; font-size: 32px; font-weight: bold; text-decoration: underline; display: block; margin-top: 5px; margin-bottom: 5px;'>",
            shared_reactives$user_title, "</span>"
          )),
          style = "text-align: center; margin-bottom: 5px;"
        )
        incProgress(0.3, detail = "Rendering date...")
        date_text <- tags$p(
          HTML(paste0(
            "<span style='color: #555555; font-size: 18px; display: block; margin-top: 0px;'>
    Sampled on<strong> ", add_ordinal_suffix(format(as.Date(shared_reactives$user_date, format = "%m/%d/%y"), "%B %d, %Y"), style = "%B %d, %Y"), "</strong>", 
            "  |  Calculated on<strong> ", add_ordinal_suffix(format(Sys.Date(), "%B %d, %Y"), style = "%B %d, %Y"), "</strong>",
            "</span>"
          )),
          style = "text-align: center; margin-top: 5px;"
        )
        incProgress(0.5, detail = "Putting it all together...")
        # Wrap everything in a div with an ID
        full_html <- tags$html(
          tags$head(tags$style("#results_section { padding: 20px; }")),
          tags$body(
            tags$div(id = "results_section", title_text, date_text, table())
          )
        )
        incProgress(0.7, detail = "Saving html file...")
        # Save as an HTML file
        temp_html <- tempfile(fileext = ".html")
        save_html(full_html, temp_html)
        incProgress(0.9, detail = "Converting html to png...")
        # Capture as PNG using 'selector' argument
        webshot::webshot(temp_html, file = file, delay = 0.5, zoom = 2, selector = "#results_section")
        incProgress(1, detail = "Rendering complete!")
      }
        )}
    )
    
    output$download_full_report <- downloadHandler(
      filename = function() {
        paste0("Report_", shared_reactives$user_title, "_", shared_reactives$user_date, ".pdf")
      },
      content = function(file) {
        withProgress(message = "Generating report...", value = 0, {
          
          # Step 1: Copy the Rmd template
          incProgress(0.1, detail = "Preparing template...")
          temp_rmd <- tempfile(fileext = ".Rmd")
          file.copy("www/full_report_template.Rmd", temp_rmd, overwrite = TRUE)
          
          # Step 2: Render the R Markdown file
          incProgress(0.5, detail = "Rendering report...")
          rmarkdown::render(
            input = temp_rmd,
            output_file = file,
            params = list(
              user_title = shared_reactives$user_title,
              user_date = shared_reactives$user_date,
              data = prepared_data()$summarized_data,
              comparison_metrics = prepared_data()$combined_metrics
            ),
            envir = new.env(parent = globalenv())
          )
          
          incProgress(1, detail = "Completed!")
        })
      }
    )
    
    output$download_small_report <- downloadHandler(
      filename = function() {
        paste0("DataSummary_", shared_reactives$user_title, "_", shared_reactives$user_date, ".pdf")
      },
      content = function(file) {
        withProgress(message = "Generating report...", value = 0, {
          
          # Step 1: Copy the Rmd template
          incProgress(0.1, detail = "Preparing template...")
          temp_rmd <- tempfile(fileext = ".Rmd")
          file.copy("www/datasum_report_template.Rmd", temp_rmd, overwrite = TRUE)
          
          # Step 2: Render the R Markdown file
          incProgress(0.5, detail = "Rendering report...")
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
            envir = new.env(parent = globalenv())
          )
          
          incProgress(1, detail = "Completed!")
        })
      }
    )
  })
}


