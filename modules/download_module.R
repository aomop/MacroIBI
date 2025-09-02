# --------------------------------------------------------------------
# UI Module for Download and Auto-Save Functionality
# --------------------------------------------------------------------
# Provides UI for saving data as a CSV file and includes an optional status message for auto-saves.
download_module_ui <- function(id) {
  ns <- NS(id)
  tagList(
    tags$div(
      tags$h4("Save Data", style = "font-size: 16px;"),  # Title above the button
      downloadButton(ns("save_data"), "Download Data (CSV)")  # Button label
    )
  )
}

# --------------------------------------------------------------------
# Server Module for Download and Auto-Save Functionality
# --------------------------------------------------------------------
download_module_server <- function(
    id,
    selected_genera,
    shared_reactives,
    auto_save_interval = 30,
    auto_save_path = "auto_saves",
    expose_data_reactive = NULL # reactiveVal to share data with other modules
) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Function to assemble data from selected_genera
    assemble_data <- function() {
      req(selected_genera)
      
      group_names <- names(reactiveValuesToList(selected_genera))
      
      all_data <- lapply(group_names, function(group_name) {
        if (startsWith(group_name, "section_")) {
          group_reactive <- selected_genera[[group_name]]
          group_data <- isolate(group_reactive()$data)
          
          if (is.null(group_data) || length(group_data) == 0) return(NULL)
          
          do.call(rbind, lapply(group_data, function(row) {
            data.frame(
              Group     = group_name,
              Taxon     = row$taxon,
              Dipnet1   = row$dipnet1,
              Dipnet2   = row$dipnet2,
              tsn       = row$tsn,
              parentTsn = row$parentTsn
            )
          }))
        } else {
          return(NULL)
        }
      })
      
      combined_data <- do.call(rbind, all_data)
      
      if (is.null(combined_data) || nrow(combined_data) == 0) {
        return(NULL)
      }
      
      combined_data$Title <- shared_reactives$user_title
      combined_data$Date  <- as.character(shared_reactives$user_date)
      
      combined_data
    }
    
    # Reactive for internal access
    if (!is.null(expose_data_reactive)) {
      observe({
        data <- assemble_data()
        if (!is.null(data)) {
          expose_data_reactive(data)
        }
      })
    }
    
    # Manual download functionality
    output$save_data <- downloadHandler(
      filename = function() {
        paste0("IBI_", shared_reactives$user_title, "_", shared_reactives$user_date, ".csv")
      },
      content = function(file) {
        data <- assemble_data()
        
        if (is.null(data)) {
          warning("No data to save. Skipping file creation.")
          return()
        }
        
        write.csv(data, file, row.names = FALSE)
      }
    )
  })
}