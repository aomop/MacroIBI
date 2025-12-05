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
      rbind(
        metric_scores$data,
        data.frame(
          metric_name  = "IBI Score (0-50)",
          response     = "Decrease",
          min          = NA, 
          fifth        = NA, 
          ninety_fifth = NA, 
          max          = NA,
          metric_value = NA, 
          metric_score = NA,
          adj_score    = sum(metric_scores$data$adj_score, na.rm = TRUE)
        )
      )
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
        section_ids <- names(shiny::reactiveValuesToList(selected_genera))
        
        all_data <- lapply(section_ids, function(section_id) {
          if (!startsWith(section_id, "section_")) {
            return(NULL)
          }
          
          meta <- group_defs[group_defs$section_id == section_id, , drop = FALSE]
          if (nrow(meta) == 0L) {
            return(NULL)
          }
          
          group_id   <- meta$group_id[1]
          group_name <- meta$group_name[1]
          
          group_reactive <- selected_genera[[section_id]]
          group_data     <- shiny::isolate(group_reactive()$data)
          
          if (is.null(group_data) || length(group_data) == 0L) {
            return(NULL)
          }
          
          do.call(
            rbind,
            lapply(group_data, function(row) {
              data.frame(
                group_id   = group_id,
                group_name = group_name,
                section_id = section_id,
                Taxon      = row$taxon,
                Dipnet1    = row$dipnet1,
                Dipnet2    = row$dipnet2,
                tsn        = row$tsn,
                parentTsn  = row$parentTsn,
                stringsAsFactors = FALSE
              )
            })
          )
        })
        
        if (length(all_data) == 0L || all(vapply(all_data, is.null, logical(1)))) {
          update_autosave_status("No data to auto-save.")
          return()
        }
        
        combined_data <- do.call(rbind, all_data)
        
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
          shared_reactives$user_date <- data$Date[1]
        }
        
        # Drop non-taxa columns before splitting
        other_data <- data[, !(colnames(data) %in% c("Title", "Date", "schema_version")), drop = FALSE]
        
        # --------------------------------------------------------------------
        # New schema: group_id / section_id
        # --------------------------------------------------------------------
        if ("group_id" %in% colnames(other_data) && "section_id" %in% colnames(other_data)) {
          
          split_by_group <- split(other_data, other_data$group_id)
          
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
          
          # --------------------------------------------------------------------
          # Legacy autosave schema: Group column contains section IDs
          # --------------------------------------------------------------------
        } else if ("Group" %in% colnames(other_data)) {
          split_data <- split(other_data, other_data$Group)
          
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
            "Auto-save file is missing expected grouping columns.",
            type = "error",
            closeButton = TRUE
          )
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
