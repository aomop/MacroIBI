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
#' @keywords internal
upload_module_server <- function(id, taxonomy, selected_genera, shared_reactives, toggle_state) {
  shiny::moduleServer(id, function(input, output, session) {
    
    ## --- Handle File Upload ---
    shiny::observeEvent(input$reload_data, {
      shiny::req(input$reload_data) # Ensure file is uploaded before proceeding

      # Read the uploaded CSV
      data <- readr::read_csv(input$reload_data$datapath)
      
      shared_reactives$server_update <- TRUE
      
      # Update Title and Date from the uploaded data, if present
      if ("Title" %in% colnames(data)) {
        shared_reactives$user_title <- data$Title[1]
      } 
      
      if ("Date" %in% colnames(data)) {
        shared_reactives$user_date <- data$Date[1]
      } 
      
      # Store remaining data in shared_reactives, excluding Title and Date
      other_data <- data[, !(colnames(data) %in% c("Title", "Date")), drop = FALSE]
      shared_reactives$uploaded_data <- other_data
    })
    
    ## --- Process Uploaded Data ---
    observeEvent(shared_reactives$uploaded_data, {
      req(shared_reactives$uploaded_data)
      data <- shared_reactives$uploaded_data
      
      # Split data by 'Group' column
      split_data <- split(data, data$Group)
      
      # Iterate through each group and update selected_genera
      for (group_name in names(split_data)) {
        # Process only valid groups starting with "section_"
        if (startsWith(group_name, "section_")) { 
          group_data <- split_data[[group_name]]
          
          if (!is.null(selected_genera[[group_name]])) {
            shiny::isolate({
              current_group <- selected_genera[[group_name]]()
              
              # Initialize current group data structure if null
              if (is.null(current_group)) {
                current_group <- list(data = list())
              }
              
              # Update the group's data with uploaded values
              current_group$data <- lapply(seq_len(nrow(group_data)), function(i) {
                list(
                  id = i,
                  taxon = group_data$Taxon[i],
                  dipnet1 = group_data$Dipnet1[i],
                  dipnet2 = group_data$Dipnet2[i],
                  tsn = group_data$tsn[i],
                  parentTsn = group_data$parentTsn[i]
                )
              })
              
              # Reassign the updated structure back to selected_genera
              selected_genera[[group_name]] <- shiny::reactiveVal(current_group)
            })
          }
        }
      }

      # Notify the user about the successful update
      shiny::showNotification("Successfully updated with user uploaded data!", closeButton = TRUE, type = "message")
      message(paste0("Uploaded data from ", input$reload_data$name))
    })
    
    ## --- Toggle State Based on Title and Date ---
    shiny::observe({
      shiny::req(shared_reactives$user_title, shared_reactives$user_date)  # Ensure both Title and Date are present
      # Toggle state based on presence of Title and Date
      if (nzchar(shared_reactives$user_title) && nzchar(shared_reactives$user_date)) {
        toggle_state(FALSE)
        
      } 
    })
  })
}