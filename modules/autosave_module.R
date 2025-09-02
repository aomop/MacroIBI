 # --------------------------------------------------------------------
# UI Function for the Auto-Save Reload Module
# --------------------------------------------------------------------
autosave_module_ui <- function(id) {
  ns <- NS(id)
  tagList(
    checkboxInput(ns("enable_autosave"), "Enable Auto-Save", value = FALSE),  # Toggle for auto-save
    actionButton(ns("load_autosave"), "Load Auto-Save"),  # Button to open file selection modal
    textOutput(ns("autosave_status"))  # Display status updates
  )
}

# --------------------------------------------------------------------
# Server Function for the Auto-Save Reload Module
# --------------------------------------------------------------------
autosave_module_server <- function(
    id, auto_save_path = "auto_saves", metric_save_path = "metric_autosaves", selected_genera, 
    shared_reactives, metric_scores, auto_save_interval = 30) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
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
    
    # Ensure the auto-save directory exists
    if (!dir.exists(auto_save_path)) dir.create(auto_save_path, recursive = TRUE)
    
    # Create the metric auto-save directory if it doesn't exist
    if (!dir.exists(metric_save_path)) dir.create(metric_save_path, recursive = TRUE)
    
    # Function to update the auto-save status message
    update_autosave_status <- function(message) {
      output$autosave_status <- renderText({
        paste0(message, " (Last updated: ", Sys.time(), ")")
      })
    }
    
    observeEvent(input$enable_autosave, {
      if (input$enable_autosave == FALSE) {
        message("Auto-save feature disabled by user")
      } else {
        message("Auto-save feature enabled by user")
      }
    }, ignoreInit = TRUE)
    
    
    # Auto-save timer
    auto_save_timer <- reactiveTimer(auto_save_interval * 1000)
    
    # Auto-save logic: Triggered periodically based on `auto_save_timer`
    observe({
      if (!input$enable_autosave) {
        update_autosave_status("Auto-save feature is disabled.")
        return()
      }
      
      auto_save_file <- file.path(auto_save_path, paste0("autosave_", shared_reactives$user_title, ".csv"))
      auto_save_timer()  # Trigger periodically
      
      # Collect data and save
      group_names <- names(reactiveValuesToList(selected_genera))
      all_data <- lapply(group_names, function(group_name) {
        if (startsWith(group_name, "section_")) {
          group_reactive <- selected_genera[[group_name]]
          group_data <- isolate(group_reactive()$data)
          if (is.null(group_data) || length(group_data) == 0) return(NULL)
          do.call(rbind, lapply(group_data, function(row) {
            data.frame(
              Group = group_name,
              Taxon = row$taxon,
              Dipnet1 = row$dipnet1,
              Dipnet2 = row$dipnet2,
              tsn = row$tsn,
              parentTsn = row$parentTsn
            )
          }))
        } else {
          return(NULL)
        }
      })
      
      combined_data <- do.call(rbind, all_data)
      # Update autosave status based on conditions
      if (!is.null(combined_data) && nrow(combined_data) > 0) {
        if (!is.null(shared_reactives$user_title) && shared_reactives$user_title != "") {
          # Title is present, perform auto-save
          auto_save_file <- file.path(auto_save_path, paste0("autosave_", shared_reactives$user_title, ".rds"))
          combined_data$Title <- shared_reactives$user_title
          combined_data$Date <- as.character(shared_reactives$user_date)
          saveRDS(combined_data, auto_save_file)
          
          # Save pre-calculated metrics (summarized_data) to a separate folder
          metric_file <- file.path(metric_save_path, paste0("metrics_", shared_reactives$user_title, ".rds"))
          saveRDS(summarized_data(), metric_file)
          
          update_autosave_status("Auto-save completed.")
        } else {
          # Title is missing
          update_autosave_status("Please add a title to enable auto-save.")
        }
      } else {
        # No data to auto-save
        update_autosave_status("No data to auto-save.")
      }
    })
    
    # Separate logic for loading auto-saves (independent of auto-save state)
    observeEvent(input$load_autosave, {
      # List available auto-save files
      files <- list.files(auto_save_path, pattern = "autosave_", full.names = FALSE)
      
      # Dynamically set modal content based on file availability
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
    
    # Load the selected auto-save file
    observeEvent(input$confirm_load, {
      req(input$selected_file)  # Ensure a file is selected
      selected_file_path <- file.path(auto_save_path, input$selected_file)
      
      shared_reactives$server_update <- TRUE
      
      if (file.exists(selected_file_path)) {
        # Read the auto-save file
        data <- readRDS(selected_file_path)
        
        # Update shared_reactives with Title and Date
        if ("Title" %in% colnames(data)) {
          shared_reactives$user_title <- data$Title[1]
        }
        if ("Date" %in% colnames(data)) {
          shared_reactives$user_date <- data$Date[1]
        }
        
        # Process data and update `selected_genera`
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
                
                # Update data structure
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
                
                # Update `selected_genera`
                selected_genera[[group_name]] <- reactiveVal(current_group)
              })
            }
          }
        }
        
        removeModal()  # Close the modal
        showNotification("Auto-save loaded successfully!", type = "message")
        message(paste("Loaded data from file:", input$selected_file))
      } else {
        showNotification("The selected file no longer exists.", type = "error")
      }
    })
    
    observeEvent(input$delete_file, {
      req(input$selected_file)  # Ensure a file is selected
      selected_file_path <- file.path(auto_save_path, input$selected_file)
      
      # Show confirmation modal
      showModal(
        modalDialog(
          title = "Confirm Deletion",
          paste("Are you sure you want to delete the file", input$selected_file, "?", 
                "<strong>The data will be permanantly deleted - <span style='color: red;'>there is no method for recovery.</strong></span>"),
          footer = tagList(
            modalButton("Cancel"),
            actionButton(ns("confirm_delete"), "Yes, Delete", class = "btn-danger")
          )
        )
      )
    })
    
    # Handle file deletion after confirmation
    observeEvent(input$confirm_delete, {
      req(input$selected_file)  # Ensure a file is selected
      selected_file_path <- file.path(auto_save_path, input$selected_file)
      
      if (file.exists(selected_file_path)) {
        file.remove(selected_file_path)  # Delete the file
        showNotification("Selected auto-save file has been deleted.", type = "warning")
        message(paste("Deleted file", selected_file_path))
        
        # Refresh the file list
        files <- list.files(auto_save_path, pattern = "\\.rds$", full.names = FALSE)
        
        # Close the confirmation modal
        removeModal()
        
        # Reopen the main modal with the updated file list
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
        removeModal()  # Close the confirmation modal
      }
    })
    
  })
}