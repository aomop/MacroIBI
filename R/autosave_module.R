#' Autosave module UI
#'
#' @param id Module identifier.
#' @return A Shiny UI fragment.
#' @keywords internal
autosave_module_ui <- function(id, demo_mode = FALSE) {
  ns <- shiny::NS(id)
  shiny::tagList(
    if (demo_mode) {
      shiny::div(
        class = "small text-muted mb-2",
        shiny::strong("Demo mode:"),
        " Auto-save is disabled on shinyapps.io. Use the sample auto-saves below to explore the workflow."
      )
    } else {
      shiny::checkboxInput(ns("enable_autosave"), "Enable Auto-Save", value = FALSE)
    },
    shiny::actionButton(ns("load_autosave"), "Load Auto-Save"),
    shiny::textOutput(ns("autosave_status"))
  )
}

#' Autosave module server
#'
#' @param id Module identifier.
#' @param auto_save_path Path where autosave RDS files are stored.
#' @param metric_save_path Path where metric autosave RDS files are stored.
#' @param selected_genera Reactive values of selected genera.
#' @param shared_reactives Shared reactive values for the session.
#' @param metric_scores Reactive values for metric scores.
#' @param auto_save_interval Interval in seconds between autosaves.
#' @param group_defs Data frame with columns section_id, group_id, group_name
#'   mapping modules to stable taxonomic groups.
#' @param demo_mode Logical indicating whether the app is running in demo mode
#'   on shinyapps.io.
#' @keywords internal
autosave_module_server <- function(
    id,
    auto_save_path   = get_app_path("autosave_dir"),
    metric_save_path = get_app_path("metric_autosave_dir"),
    selected_genera,
    shared_reactives,
    metric_scores,
    auto_save_interval = 30,
    group_defs,
    demo_mode = FALSE) {
  
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # ------------------------------------------------------------------------
    # Demo directories inside the installed package (read-only)
    # ------------------------------------------------------------------------
    pkg            <- utils::packageName()
    demo_auto_dir  <- system.file("demo_autosaves",        package = pkg)
    demo_metric_dir <- system.file("demo_metric_autosaves", package = pkg)
    
    # ------------------------------------------------------------------------
    # Summarized metrics (used for metric autosaves in non-demo mode)
    # ------------------------------------------------------------------------
    summarized_data <- shiny::reactive(
      build_summarized_metrics(metric_scores$data)
    )
    
    # ------------------------------------------------------------------------
    # Ensure autosave directories exist (non-demo only)
    # ------------------------------------------------------------------------
    if (!demo_mode) {
      if (!dir.exists(auto_save_path)) {
        dir.create(auto_save_path, recursive = TRUE, showWarnings = FALSE)
      }
      if (!dir.exists(metric_save_path)) {
        dir.create(metric_save_path, recursive = TRUE, showWarnings = FALSE)
      }
    }
    
    # ------------------------------------------------------------------------
    # Helpers
    # ------------------------------------------------------------------------
    update_autosave_status <- function(message) {
      output$autosave_status <- shiny::renderText({
        paste0(message, " (Last updated: ", Sys.time(), ")")
      })
    }
    
    # In non-demo, autosave is governed by the checkbox.
    is_autosave_enabled <- function() {
      !demo_mode && isTRUE(input$enable_autosave)
    }
    
    # Log when the autosave checkbox is toggled (non-demo only)
    if (!demo_mode) {
      shiny::observeEvent(input$enable_autosave, {
        if (isFALSE(input$enable_autosave)) {
          message("Auto-save feature disabled by user")
        } else {
          message("Auto-save feature enabled by user")
        }
      }, ignoreInit = TRUE)
    }
    
    # ------------------------------------------------------------------------
    # Periodic autosave (non-demo) or static status (demo)
    # ------------------------------------------------------------------------
    if (!demo_mode) {
      auto_save_timer <- shiny::reactiveTimer(auto_save_interval * 1000)
      
      shiny::observe({
        # Always tick the timer in this observer
        auto_save_timer()
        
        # Check whether autosave is enabled
        if (!is_autosave_enabled()) {
          update_autosave_status("Auto-save feature is disabled.")
          return()
        }
        
        auto_save_file <- file.path(
          auto_save_path,
          paste0("autosave_", shared_reactives$user_title, ".rds")
        )
        
        # Collect data from all group sections
        # Take a snapshot of selected_genera as a plain list, then strip off reactives
        section_snapshot <- lapply(
          shiny::reactiveValuesToList(selected_genera),
          function(section_obj) {
            # Each entry is a reactiveVal(list(data = ...)), so dereference it here
            if (is.function(section_obj)) {
              # If the snapshot still contains wrapped reactiveVals, call them
              section_obj()
            } else {
              section_obj
            }
          }
        )
        
        combined_data <- build_autosave_df(section_snapshot, group_defs)
        
        if (is.null(combined_data) || nrow(combined_data) == 0L) {
          update_autosave_status("No data to auto-save.")
          return()
        }
        
        if (!is.null(combined_data) && nrow(combined_data) > 0L) {
          if (!is.null(shared_reactives$user_title) && nzchar(shared_reactives$user_title)) {
            combined_data$Title          <- shared_reactives$user_title
            combined_data$Date           <- as.character(shared_reactives$user_date)
            combined_data$schema_version <- "2.0.0"
            
            # Save taxa autosave
            saveRDS(combined_data, auto_save_file)
            
            # Save metric autosave
            metric_file <- file.path(
              metric_save_path,
              paste0("metrics_", shared_reactives$user_title, ".rds")
            )
            saveRDS(summarized_data(), metric_file)
            
            update_autosave_status("Auto-save completed.")
          } else {
            update_autosave_status("Please add a title to enable auto-save.")
          }
        } else {
          update_autosave_status("No data to auto-save.")
        }
      })
    } else {
      # Demo: no periodic autosaving, just a static status message
      shiny::observe({
        update_autosave_status("Auto-save feature is disabled in the demo version.")
      })
    }
    
    # ------------------------------------------------------------------------
    # Manage autosaves (load / delete)
    # ------------------------------------------------------------------------
    shiny::observeEvent(input$load_autosave, {
      # In demo mode, list packaged demo files. In non-demo, list user autosaves.
      files <- if (demo_mode) {
        if (nzchar(demo_auto_dir) && dir.exists(demo_auto_dir)) {
          list.files(demo_auto_dir, pattern = "\\.rds$", full.names = FALSE)
        } else {
          character(0)
        }
      } else {
        list.files(auto_save_path, pattern = "autosave_", full.names = FALSE)
      }
      
      if (length(files) > 0L) {
        # Demo: only load; Non-demo: load + delete
        dialog_title <- if (demo_mode) "Load Demo Dataset" else "Manage Auto-Saves"
        
        footer_buttons <- shiny::tagList(
          shiny::actionButton(
            ns("confirm_load"), 
            if (demo_mode) "Load Demo" else "Load Selected File"
          ),
          if (!demo_mode) {
            shiny::actionButton(
              ns("delete_file"),
              "Delete Selected File",
              class = "btn-danger"
            )
          }
        )
        
        shiny::showModal(
          shiny::modalDialog(
            title  = dialog_title,
            shiny::selectInput(ns("selected_file"), "Available Files", choices = files),
            footer = footer_buttons,
            easyClose = TRUE
          )
        )
      } else {
        # No files available
        msg <- if (demo_mode) {
          "There are no packaged demo datasets available."
        } else {
          "There are no auto-saved files to load or delete."
        }
        
        shiny::showModal(
          shiny::modalDialog(
            title = "No Files Available",
            msg,
            easyClose = TRUE,
            footer = shiny::modalButton("Close")
          )
        )
      }
    })
    
    shiny::observeEvent(input$confirm_load, {
      shiny::req(input$selected_file)
      
      selected_file_path <- if (demo_mode) {
        file.path(demo_auto_dir, input$selected_file)
      } else {
        file.path(auto_save_path, input$selected_file)
      }
      
      shared_reactives$server_update <- TRUE
      
      if (file.exists(selected_file_path)) {
        data <- readRDS(selected_file_path)
        
        # Recover title and date if present
        if ("Title" %in% colnames(data)) {
          shared_reactives$user_title <- data$Title[1]
        }
        if ("Date" %in% colnames(data)) {
          raw_date <- data$Date[1]
          shared_reactives$user_date <- tryCatch(
            as.Date(raw_date),
            error   = function(e) raw_date,
            warning = function(w) raw_date
          )
        }
        
        # Drop non-taxa columns before splitting
        other_data <- data[, !(colnames(data) %in% c("Title", "Date", "schema_version")), drop = FALSE]
        
        # Use pure helper to rebuild per-section data structure
        section_data <- tryCatch(
          build_sections_from_autosave(other_data, group_defs),
          error = function(e) {
            shiny::showNotification(
              paste("Auto-save file is missing expected columns:", e$message),
              type = "error",
              closeButton = TRUE
            )
            NULL
          }
        )
        
        if (is.null(section_data)) {
          shiny::removeModal()
          return(invisible(NULL))
        }
        
        message("Loaded sections from autosave: ", paste(names(section_data), collapse = ", "))

        # Clear ALL sections first so that taxa from a prior session do not
        # bleed into the freshly loaded state.
        all_section_ids <- names(shiny::isolate(shiny::reactiveValuesToList(selected_genera)))
        for (section_id in all_section_ids) {
          if (startsWith(section_id, "section_") && !is.null(selected_genera[[section_id]])) {
            shiny::isolate({
              current_group <- selected_genera[[section_id]]()
              if (inherits(current_group, "reactivevalues")) {
                current_group$data <- list()
              }
            })
          }
        }

        # Push loaded data back into selected_genera
        for (section_id in names(section_data)) {
          # Only update sections that actually exist in selected_genera
          if (!is.null(selected_genera[[section_id]])) {
            shiny::isolate({
              # This is a reactive() that returns a reactiveValues object
              current_group <- selected_genera[[section_id]]()

              # Sanity check: should be a reactivevalues
              if (!inherits(current_group, "reactivevalues")) {
                message("Warning: selected_genera[[", section_id, "]]() did not return a reactivevalues object.")
              } else {
                # Overwrite the data slot with the loaded rows
                current_group$data <- section_data[[section_id]]$data

                # IMPORTANT:
                # - We do NOT call selected_genera[[section_id]](current_group)
                #   because it's a reactive(), not a reactiveVal.
                # - We do NOT reassign selected_genera[[section_id]] <- ...
                #   because that would break the existing reactive wiring.
              }
            })
          }
        }
        
        shiny::removeModal()
        shiny::showNotification(
          if (demo_mode) "Demo dataset loaded successfully." else "Auto-save data loaded successfully.",
          type = "message"
        )
        
      } else {
        shiny::showNotification("The selected file no longer exists.", type = "error")
        shiny::removeModal()
      }
    })
    
    # ------------------------------------------------------------------------
    # Delete autosaves (non-demo only; there is nothing to delete in demo)
    # ------------------------------------------------------------------------
    if (!demo_mode) {
      shiny::observeEvent(input$delete_file, {
        shiny::req(input$selected_file)
        selected_file_path <- file.path(auto_save_path, input$selected_file)
        
        if (file.exists(selected_file_path)) {
          file.remove(selected_file_path)
          shiny::showNotification("Selected auto-save file has been deleted.", type = "warning")
          message(paste("Deleted file", selected_file_path))
          
          files <- list.files(auto_save_path, pattern = "autosave_", full.names = FALSE)
          if (length(files) > 0L) {
            shiny::updateSelectInput(session, "selected_file", choices = files)
          } else {
            shiny::showNotification("No auto-save files remain.", type = "message")
          }
        } else {
          shiny::showNotification("The selected file no longer exists.", type = "error")
          shiny::removeModal()
        }
      })
    }
  })
}
