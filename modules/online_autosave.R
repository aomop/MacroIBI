# --------------------------------------------------------------------
# UI Function for the Auto-Save Reload Module (Load-only version)
# --------------------------------------------------------------------
autosave_module_ui <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(ns("autosave_warning")),
    actionButton(ns("load_autosave"), "Load Auto-Save"),
    textOutput(ns("autosave_status"))
  )
}

# --------------------------------------------------------------------
# Server Function for the Auto-Save Reload Module (Load-only version)
# --------------------------------------------------------------------
autosave_module_server <- function(
    id, auto_save_path = "auto_saves", selected_genera,
    shared_reactives) {
  
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    output$autosave_warning <- renderUI({
      HTML("The autosave feature is not available online. You may still load old autosaves as sample data. Please run the app locally if you would like to generate new autosaves.")
    })
    
    update_autosave_status <- function(message) {
      output$autosave_status <- renderText({
        paste0(message, " (Last updated: ", Sys.time(), ")")
      })
    }
    
    if (!dir.exists(auto_save_path)) dir.create(auto_save_path, recursive = TRUE)
    
    observeEvent(input$load_autosave, {
      files <- list.files(auto_save_path, pattern = "autosave_", full.names = FALSE)
      
      if (length(files) > 0) {
        showModal(
          modalDialog(
            title = "Manage Auto-Saves",
            selectInput(ns("selected_file"), "Available Files", choices = files),
            footer = tagList(
              modalButton("Cancel"),
              actionButton(ns("confirm_load"), "Load Selected File"),
              actionButton(ns("delete_file"), "Delete Selected File", class = "btn-danger")
            )
          )
        )
      } else {
        showModal(
          modalDialog(
            title = "No Auto-Saves Available",
            "There are no auto-saved files to load or delete.",
            easyClose = TRUE,
            footer = modalButton("Close")
          )
        )
      }
    })
    
    observeEvent(input$confirm_load, {
      req(input$selected_file)
      selected_file_path <- file.path(auto_save_path, input$selected_file)
      
      shared_reactives$server_update <- TRUE
      
      if (file.exists(selected_file_path)) {
        data <- readRDS(selected_file_path)
        
        if ("Title" %in% colnames(data)) {
          shared_reactives$user_title <- data$Title[1]
        }
        if ("Date" %in% colnames(data)) {
          shared_reactives$user_date <- data$Date[1]
        }
        
        other_data <- data[, !(colnames(data) %in% c("Title", "Date")), drop = FALSE]
        split_data <- split(other_data, other_data$Group)
        
        for (group_name in names(split_data)) {
          if (startsWith(group_name, "section_")) {
            group_data <- split_data[[group_name]]
            
            if (!is.null(selected_genera[[group_name]])) {
              isolate({
                current_group <- selected_genera[[group_name]]()
                if (is.null(current_group)) {
                  current_group <- list(data = list())
                }
                
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
                
                selected_genera[[group_name]] <- reactiveVal(current_group)
              })
            }
          }
        }
        
        removeModal()
        showNotification("Auto-save loaded successfully!", type = "message")
        message(paste("Loaded data from file:", input$selected_file))
      } else {
        showNotification("The selected file no longer exists.", type = "error")
      }
    })
    
    observeEvent(input$delete_file, {
      req(input$selected_file)
      selected_file_path <- file.path(auto_save_path, input$selected_file)
      
      showModal(
        modalDialog(
          title = "Confirm Deletion",
          paste("Are you sure you want to delete the file", input$selected_file, "?",
                "<strong>The data will be permanently deleted - <span style='color: red;'>there is no method for recovery.</strong></span>"),
          footer = tagList(
            modalButton("Cancel"),
            actionButton(ns("confirm_delete"), "Yes, Delete", class = "btn-danger")
          )
        )
      )
    })
    
    observeEvent(input$confirm_delete, {
      req(input$selected_file)
      selected_file_path <- file.path(auto_save_path, input$selected_file)
      
      if (file.exists(selected_file_path)) {
        file.remove(selected_file_path)
        showNotification("Selected auto-save file has been deleted.", type = "warning")
        message(paste("Deleted file", selected_file_path))
        
        files <- list.files(auto_save_path, pattern = "\\.rds$", full.names = FALSE)
        removeModal()
        
        if (length(files) > 0) {
          showModal(
            modalDialog(
              title = "Manage Auto-Saves",
              selectInput(ns("selected_file"), "Available Files", choices = files),
              footer = tagList(
                modalButton("Cancel"),
                actionButton(ns("confirm_load"), "Load Selected File"),
                actionButton(ns("delete_file"), "Delete Selected File", class = "btn-danger")
              )
            )
          )
        } else {
          showNotification("No auto-save files remain.", type = "message")
        }
      } else {
        showNotification("The selected file no longer exists.", type = "error")
        removeModal()
      }
    })
  })
}