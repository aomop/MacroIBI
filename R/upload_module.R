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
      
      # Store remaining data in shared_reactives, excluding meta columns
      other_data <- data[, !(colnames(data) %in% c("Title", "Date", "schema_version")), drop = FALSE]
      shared_reactives$uploaded_data <- other_data
    })
    
    ## --- Process Uploaded Data ----------------------------------------------
    shiny::observeEvent(shared_reactives$uploaded_data, {
      shiny::req(shared_reactives$uploaded_data)
      data <- shared_reactives$uploaded_data
      
      # New schema: group_id / group_name / section_id
      if ("group_id" %in% colnames(data) && "section_id" %in% colnames(data)) {
        
        split_by_group <- split(data, data$group_id)
        
        for (gid in names(split_by_group)) {
          group_data <- split_by_group[[gid]]
          
          meta <- group_defs[group_defs$group_id == gid, , drop = FALSE]
          if (nrow(meta) == 0L) {
            next
          }
          
          section_id <- meta$section_id[1]
          
          if (!is.null(selected_genera[[section_id]])) {
            shiny::isolate({
              current_group <- selected_genera[[section_id]]()
              
              if (is.null(current_group)) {
                current_group <- list(data = list())
              }
              
              current_group$data <- lapply(seq_len(nrow(group_data)), function(i) {
                list(
                  id        = i,
                  taxon     = group_data$Taxon[i],
                  dipnet1   = group_data$Dipnet1[i],
                  dipnet2   = group_data$Dipnet2[i],
                  tsn       = group_data$tsn[i],
                  parentTsn = group_data$parentTsn[i]
                )
              })
              
              selected_genera[[section_id]] <- shiny::reactiveVal(current_group)
            })
          }
        }
        
      } else if ("Group" %in% colnames(data)) {
        # Legacy schema: Group column contains section IDs (e.g., "section_1")
        split_data <- split(data, data$Group)
        
        for (group_name in names(split_data)) {
          if (startsWith(group_name, "section_")) {
            group_data <- split_data[[group_name]]
            
            if (!is.null(selected_genera[[group_name]])) {
              shiny::isolate({
                current_group <- selected_genera[[group_name]]()
                
                if (is.null(current_group)) {
                  current_group <- list(data = list())
                }
                
                current_group$data <- lapply(seq_len(nrow(group_data)), function(i) {
                  list(
                    id        = i,
                    taxon     = group_data$Taxon[i],
                    dipnet1   = group_data$Dipnet1[i],
                    dipnet2   = group_data$Dipnet2[i],
                    tsn       = group_data$tsn[i],
                    parentTsn = group_data$parentTsn[i]
                  )
                })
                
                selected_genera[[group_name]] <- shiny::reactiveVal(current_group)
              })
            }
          }
        }
        
      } else {
        shiny::showNotification(
          "Uploaded file does not contain expected grouping columns.",
          type = "error",
          closeButton = TRUE
        )
      }
      
      # Notify the user about the successful update
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
