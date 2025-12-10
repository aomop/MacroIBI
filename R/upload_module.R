#' Upload module UI
#'
#' @param id Module identifier.
#' @return A Shiny UI fragment.
#' @keywords internal
upload_module_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::fileInput(ns("reload_data"), "Upload Saved Data (CSV)")
}

#' Upload module server
#'
#' @param id Module identifier.
#' @param taxonomy Taxonomy data frame.
#' @param selected_genera Reactive values of selected genera.
#' @param shared_reactives Shared reactive values.
#' @param toggle_state Reactive value controlling editing state.
#' @param group_defs Data frame with columns section_id, group_id, group_name
#'   mapping groups to section modules.
#' @keywords internal
upload_module_server <- function(
    id,
    taxonomy,
    selected_genera,
    shared_reactives,
    toggle_state,
    group_defs) {
  
  shiny::moduleServer(id, function(input, output, session) {
    
    ## --- Handle File Upload -------------------------------------------------
    shiny::observeEvent(input$reload_data, {
      shiny::req(input$reload_data)
      
      data <- readr::read_csv(input$reload_data$datapath)
      
      shared_reactives$server_update <- TRUE
      
      split <- split_uploaded_results(data)
      
      shared_reactives$user_title   <- split$meta$title
      shared_reactives$user_date    <- split$meta$date
      shared_reactives$uploaded_data <- split$taxa
    })
    
    ## --- Process Uploaded Data ----------------------------------------------
    shiny::observeEvent(shared_reactives$uploaded_data, {
      shiny::req(shared_reactives$uploaded_data)
      data <- shared_reactives$uploaded_data
      
      # 1) Turn raw data frame into per-section row-lists
      taxa_by_section <- tryCatch(
        normalize_uploaded_taxa(data, group_defs),
        error = function(e) {
          shiny::showNotification(
            "Uploaded file does not contain expected grouping columns.",
            type = "error",
            closeButton = TRUE
          )
          return(NULL)
        }
      )
      
      if (is.null(taxa_by_section)) {
        return()
      }
      
      # 2) For each section_id in the normalized list, update selected_genera
      for (section_id in names(taxa_by_section)) {
        if (!is.null(selected_genera[[section_id]])) {
          shiny::isolate({
            section_obj <- selected_genera[[section_id]]
            
            # Handle reactive() wrapper if present, consistent with the rest of the app
            if (is.function(section_obj)) {
              section_obj <- section_obj()
            }
            
            if (is.null(section_obj)) {
              section_obj <- shiny::reactiveValues(data = list())
            }
            
            # Overwrite only the $data field with the uploaded rows
            section_obj$data <- taxa_by_section[[section_id]]
          })
        }
      }
      
      shiny::showNotification(
        "Successfully updated with user uploaded data!",
        closeButton = TRUE,
        type = "message"
      )
      message(paste0("Uploaded data from ", input$reload_data$name))
    })
    
    ## --- Toggle State Based on Title and Date --------------------------------
    shiny::observe({
      shiny::req(shared_reactives$user_title, shared_reactives$user_date)
      if (nzchar(shared_reactives$user_title) && nzchar(shared_reactives$user_date)) {
        toggle_state(FALSE)
      }
    })
  })
}
