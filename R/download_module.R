#' Download module UI
#'
#' @param id Module identifier.
#' @return A Shiny UI fragment.
#' @keywords internal
download_module_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::tags$div(
      shiny::tags$h4("Save Data", style = "font-size: 16px;"),  # Title above the button
      shiny::downloadButton(ns("save_data"), "Download Data (CSV)")  # Button label
    )
  )
}

#' Download module server
#'
#' @param id Module identifier.
#' @param selected_genera Reactive values of selected genera.
#' @param shared_reactives Shared reactive values.
#' @param auto_save_interval Interval used for autosave (reserved for future use).
#' @param auto_save_path Path to autosave directory (reserved for future use).
#' @param expose_data_reactive Optional reactiveVal used to expose assembled data.
#' @keywords internal
download_module_server <- function(
    id,
    selected_genera,
    shared_reactives,
    auto_save_interval = 30,
    auto_save_path = get_app_path("autosave_dir"),
    expose_data_reactive = NULL
) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Function to assemble data from selected_genera
    assemble_data <- function() {
      shiny::req(selected_genera)

      group_names <- names(shiny::reactiveValuesToList(selected_genera))

      all_data <- lapply(group_names, function(group_name) {
        if (startsWith(group_name, "section_")) {
          group_reactive <- selected_genera[[group_name]]
          group_data <- shiny::isolate(group_reactive()$data)
          
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
      shiny::observe({
        data <- assemble_data()
        if (!is.null(data)) {
          expose_data_reactive(data)
        }
      })
    }
    
    # Manual download functionality
    output$save_data <- shiny::downloadHandler(
      filename = function() {
        paste0("IBI_", shared_reactives$user_title, "_", shared_reactives$user_date, ".csv")
      },
      content = function(file) {
        data <- assemble_data()
        
        if (is.null(data)) {
          warning("No data to save. Skipping file creation.")
          return()
        }
        
        readr::write_csv(data, file)
      }
    )
  })
}