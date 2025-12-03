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
    auto_save_path = get_app_path("autosave_dir"),
    metric_save_path = get_app_path("metric_autosave_dir"),
    selected_genera,
    shared_reactives,
    metric_scores,
    auto_save_interval = 30,
    group_defs,
    demo_mode = FALSE) {

  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    if (demo_mode) {
      seed_demo_autosaves(auto_save_path, metric_save_path)
    }

    summarized_data <- shiny::reactive(
      rbind(
        metric_scores$data,
        data.frame(
          metric_name = "IBI Score (0-50)",
          response = "Decrease",
          min = NA, fifth = NA, ninety_fifth = NA, max = NA,
          metric_value = NA, metric_score = NA,
          adj_score = sum(metric_scores$data$adj_score, na.rm = TRUE)
        )
      )
    )
    
    if (!dir.exists(auto_save_path)) dir.create(auto_save_path, recursive = TRUE)
    if (!dir.exists(metric_save_path)) dir.create(metric_save_path, recursive = TRUE)
    
    update_autosave_status <- function(message) {
      output$autosave_status <- shiny::renderText({
        paste0(message, " (Last updated: ", Sys.time(), ")")
      })
    }

    is_autosave_enabled <- function() {
      !demo_mode && isTRUE(input$enable_autosave)
    }

    if (!demo_mode) {
      shiny::observeEvent(input$enable_autosave, {
        if (isFALSE(input$enable_autosave)) {
          message("Auto-save feature disabled by user")
        } else {
          message("Auto-save feature enabled by user")
        }
      }, ignoreInit = TRUE)
    }

    auto_save_timer <- shiny::reactiveTimer(auto_save_interval * 1000)

    ## --- Periodic autosave --------------------------------------------------
    shiny::observe({
      if (!is_autosave_enabled()) {
        if (demo_mode) {
          update_autosave_status("Auto-save feature is disabled in the demo version.")
        } else {
          update_autosave_status("Auto-save feature is disabled.")
        }
        return()
      }
      
      auto_save_file <- file.path(
        auto_save_path,
        paste0("autosave_", shared_reactives$user_title, ".rds")
      )
      
      auto_save_timer()
      
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
          
          saveRDS(combined_data, auto_save_file)
          
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
    
    ## --- Manage autosaves (load/delete) ------------------------------------
    shiny::observeEvent(input$load_autosave, {
      files <- list.files(auto_save_path, pattern = "autosave_", full.names = FALSE)
      
      if (length(files) > 0) {
        shiny::showModal(
          shiny::modalDialog(
            title = "Manage Auto-Saves",
            shiny::selectInput(ns("selected_file"), "Available Files", choices = files),
            footer = shiny::tagList(
              shiny::actionButton(ns("confirm_load"), "Load Selected File"),
              shiny::actionButton(ns("delete_file"), "Delete Selected File", class = "btn-danger")
            )
          )
        )
      } else {
        shiny::showModal(
          shiny::modalDialog(
            title = "No Auto-Saves Available",
            "There are no auto-saved files to load or delete.",
            easyClose = TRUE,
            footer = shiny::modalButton("Close")
          )
        )
      }
    })
    
    shiny::observeEvent(input$confirm_load, {
      shiny::req(input$selected_file)
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
        
        other_data <- data[, !(colnames(data) %in% c("Title", "Date", "schema_version")), drop = FALSE]
        
        # New schema: group_id / section_id
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
        } else if ("Group" %in% colnames(other_data)) {
          # Legacy autosave schema: Group column contains section IDs
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
        shiny::showNotification("Auto-save data loaded successfully.", type = "message")
        
      } else {
        shiny::showNotification("The selected file no longer exists.", type = "error")
        shiny::removeModal()
      }
    })
    
    shiny::observeEvent(input$confirm_delete, {
      shiny::req(input$selected_file)
      selected_file_path <- file.path(auto_save_path, input$selected_file)
      
      if (file.exists(selected_file_path)) {
        file.remove(selected_file_path)
        shiny::showNotification("Selected auto-save file has been deleted.", type = "warning")
        message(paste("Deleted file", selected_file_path))
        
        files <- list.files(auto_save_path, pattern = "\\.rds$", full.names = FALSE)
        if (length(files) > 0) {
          updateSelectInput(session, "selected_file", choices = files)
        } else {
          shiny::showNotification("No auto-save files remain.", type = "message")
        }
      } else {
        shiny::showNotification("The selected file no longer exists.", type = "error")
        shiny::removeModal()
      }
    })
  })
}

demo_autosave_templates <- function() {
  list(
    list(
      title = "Prairie Pothole Demo",
      date = "2023-06-15",
      taxa = data.frame(
        group_id = c(
          "dragonflies_mayflies_damselflies_and_caddisflies_eot_orders",
          "dragonflies_mayflies_damselflies_and_caddisflies_eot_orders",
          "beetles_order_coleoptera",
          "beetles_order_coleoptera",
          "flies_and_midges_order_diptera",
          "flies_and_midges_order_diptera"
        ),
        group_name = c(
          "Dragonflies, Mayflies, Damselflies and Caddisflies - EOT Orders",
          "Dragonflies, Mayflies, Damselflies and Caddisflies - EOT Orders",
          "Beetles - Order Coleoptera",
          "Beetles - Order Coleoptera",
          "Flies and Midges - Order Diptera",
          "Flies and Midges - Order Diptera"
        ),
        section_id = NA_character_,
        Taxon = c(
          "Brachycercus",
          "Brachycercus berneri",
          "Jambhala",
          "Cyclotrypema",
          "Proegmenomyia",
          "Panacris"
        ),
        Dipnet1 = c(8, 12, 5, 4, 18, 10),
        Dipnet2 = c(6, 9, 3, 2, 15, 9),
        tsn = c("101468", "609588", "838231", "719678", "625633", "625628"),
        parentTsn = c("776915", "101468", "837915", "719592", "130185", "130185"),
        stringsAsFactors = FALSE
      ),
      metrics = data.frame(
        metric_name = c(
          "EPT Taxa Richness",
          "% Ephemeroptera Individuals",
          "Taxa Evenness",
          "% Sensitive Taxa",
          "Clinger Taxa Richness",
          "IBI Score (0-50)"
        ),
        response = c("Increase", "Increase", "Increase", "Increase", "Increase", "Decrease"),
        min = c(2, 5, 0.2, 10, 3, NA),
        fifth = c(4, 12, 0.35, 18, 5, NA),
        ninety_fifth = c(18, 68, 0.88, 72, 16, NA),
        max = c(20, 80, 1.0, 85, 20, NA),
        metric_value = c(14, 54, 0.72, 61, 12, NA),
        metric_score = c(9, 8, 8, 9, 8, NA),
        adj_score = c(9, 8, 8, 9, 8, 42),
        stringsAsFactors = FALSE
      )
    ),
    list(
      title = "Floodplain Marsh Demo",
      date = "2024-07-02",
      taxa = data.frame(
        group_id = c(
          "true_bugs_order_hemiptera",
          "true_bugs_order_hemiptera",
          "other_aquatic_insects",
          "other_aquatic_insects",
          "beetles_order_coleoptera",
          "dragonflies_mayflies_damselflies_and_caddisflies_eot_orders"
        ),
        group_name = c(
          "True Bugs - Order Hemiptera",
          "True Bugs - Order Hemiptera",
          "Other Aquatic Insects",
          "Other Aquatic Insects",
          "Beetles - Order Coleoptera",
          "Dragonflies, Mayflies, Damselflies and Caddisflies - EOT Orders"
        ),
        section_id = NA_character_,
        Taxon = c(
          "Electrovelia",
          "Electrovelia baltica",
          "Chauliodes",
          "Chauliodes pectinicornis",
          "Jambhala nekula",
          "Brachycercus harrisella"
        ),
        Dipnet1 = c(7, 11, 5, 6, 9, 10),
        Dipnet2 = c(6, 9, 4, 5, 7, 8),
        tsn = c("1014630", "1015620", "115024", "115027", "844485", "101469"),
        parentTsn = c("721743", "1014630", "666126", "115024", "838231", "101468"),
        stringsAsFactors = FALSE
      ),
      metrics = data.frame(
        metric_name = c(
          "% Corixidae",
          "Functional Feeding Groups",
          "Taxa Evenness",
          "% Sensitive Taxa",
          "Clinger Taxa Richness",
          "IBI Score (0-50)"
        ),
        response = c("Decrease", "Increase", "Increase", "Increase", "Increase", "Decrease"),
        min = c(0, 2, 0.2, 12, 4, NA),
        fifth = c(2, 4, 0.4, 20, 6, NA),
        ninety_fifth = c(25, 12, 0.85, 70, 17, NA),
        max = c(35, 15, 1.0, 85, 20, NA),
        metric_value = c(6, 10, 0.69, 48, 13, NA),
        metric_score = c(9, 8, 7, 8, 8, NA),
        adj_score = c(9, 8, 7, 8, 8, 40),
        stringsAsFactors = FALSE
      )
    ),
    list(
      title = "Shallow Lake Demo",
      date = "2022-09-20",
      taxa = data.frame(
        group_id = c(
          "other_non_insect_invertebrates",
          "other_non_insect_invertebrates",
          "snails_class_gastropoda",
          "snails_class_gastropoda",
          "flies_and_midges_order_diptera",
          "crustaceans_subclass_eumalacostraca"
        ),
        group_name = c(
          "Other Non-Insect Invertebrates",
          "Other Non-Insect Invertebrates",
          "Snails - Class Gastropoda",
          "Snails - Class Gastropoda",
          "Flies and Midges - Order Diptera",
          "Crustaceans - Subclass Eumalacostraca"
        ),
        section_id = NA_character_,
        Taxon = c(
          "Valvata",
          "Valvata bicarinata",
          "Campeloma",
          "Campeloma decisum",
          "Proegmenomyia metallica",
          "Procambarus clarkii"
        ),
        Dipnet1 = c(6, 10, 4, 7, 16, 9),
        Dipnet2 = c(5, 8, 3, 6, 14, 7),
        tsn = c("70346", "70355", "81322", "81323", "627210", "6667"),
        parentTsn = c("70345", "70346", "81321", "81322", "625633", "6655"),
        stringsAsFactors = FALSE
      ),
      metrics = data.frame(
        metric_name = c(
          "Taxa Richness",
          "% Tolerant Taxa",
          "Taxa Evenness",
          "% Predators",
          "Clinger Taxa Richness",
          "IBI Score (0-50)"
        ),
        response = c("Increase", "Decrease", "Increase", "Increase", "Increase", "Decrease"),
        min = c(5, 10, 0.25, 8, 4, NA),
        fifth = c(8, 15, 0.45, 12, 6, NA),
        ninety_fifth = c(24, 65, 0.9, 38, 18, NA),
        max = c(28, 80, 1.0, 45, 20, NA),
        metric_value = c(19, 22, 0.76, 21, 15, NA),
        metric_score = c(8, 9, 8, 7, 8, NA),
        adj_score = c(8, 9, 8, 7, 8, 40),
        stringsAsFactors = FALSE
      )
    )
  )
}

seed_demo_autosaves <- function(auto_save_path, metric_save_path) {
  dir.create(auto_save_path, recursive = TRUE, showWarnings = FALSE)
  dir.create(metric_save_path, recursive = TRUE, showWarnings = FALSE)

  purrr::walk(demo_autosave_templates(), function(template) {
    autosave_file <- file.path(auto_save_path, paste0("autosave_", template$title, ".rds"))
    metric_file <- file.path(metric_save_path, paste0("metrics_", template$title, ".rds"))

    if (!file.exists(autosave_file)) {
      autosave_data <- template$taxa
      autosave_data$Title <- template$title
      autosave_data$Date <- template$date
      autosave_data$schema_version <- "2.0.0"
      saveRDS(autosave_data, autosave_file)
    }

    if (!file.exists(metric_file)) {
      saveRDS(template$metrics, metric_file)
    }
  })
}
