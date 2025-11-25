#' Autosave module UI
#'
#' @param id Module identifier.
#' @return A Shiny UI fragment.
#' @keywords internal
autosave_module_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::checkboxInput(ns("enable_autosave"), "Enable Auto-Save", value = FALSE),
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
#' @keywords internal
autosave_module_server <- function(
    id, auto_save_path = get_app_path("autosave_dir"), metric_save_path = get_app_path("metric_autosave_dir"), selected_genera,
    shared_reactives, metric_scores, auto_save_interval = 30) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

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
      ))

    if (!dir.exists(auto_save_path)) dir.create(auto_save_path, recursive = TRUE)
    if (!dir.exists(metric_save_path)) dir.create(metric_save_path, recursive = TRUE)

    update_autosave_status <- function(message) {
      output$autosave_status <- shiny::renderText({
        paste0(message, " (Last updated: ", Sys.time(), ")")
      })
    }

    shiny::observeEvent(input$enable_autosave, {
      if (isFALSE(input$enable_autosave)) {
        message("Auto-save feature disabled by user")
      } else {
        message("Auto-save feature enabled by user")
      }
    }, ignoreInit = TRUE)

    auto_save_timer <- shiny::reactiveTimer(auto_save_interval * 1000)

    shiny::observe({
      if (!input$enable_autosave) {
        update_autosave_status("Auto-save feature is disabled.")
        return()
      }

      auto_save_file <- file.path(auto_save_path, paste0("autosave_", shared_reactives$user_title, ".csv"))
      auto_save_timer()

      group_names <- names(shiny::reactiveValuesToList(selected_genera))
      all_data <- lapply(group_names, function(group_name) {
        if (startsWith(group_name, "section_")) {
          group_reactive <- selected_genera[[group_name]]
          group_data <- shiny::isolate(group_reactive()$data)
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
      if (!is.null(combined_data) && nrow(combined_data) > 0) {
        if (!is.null(shared_reactives$user_title) && shared_reactives$user_title != "") {
          auto_save_file <- file.path(auto_save_path, paste0("autosave_", shared_reactives$user_title, ".rds"))
          combined_data$Title <- shared_reactives$user_title
          combined_data$Date <- as.character(shared_reactives$user_date)
          saveRDS(combined_data, auto_save_file)

          metric_file <- file.path(metric_save_path, paste0("metrics_", shared_reactives$user_title, ".rds"))
          saveRDS(summarized_data(), metric_file)

          update_autosave_status("Auto-save completed.")
        } else {
          update_autosave_status("Please add a title to enable auto-save.")
        }
      } else {
        update_autosave_status("No data to auto-save.")
      }
    })

    shiny::observeEvent(input$load_autosave, {
      files <- list.files(auto_save_path, pattern = "autosave_", full.names = FALSE)

      if (length(files) > 0) {
        shiny::showModal(
          shiny::modalDialog(
            title = "Manage Auto-Saves",
            shiny::selectInput(ns("selected_file"), "Available Files", choices = files),
            footer = shiny::tagList(
              shiny::modalButton("Cancel"),
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

        other_data <- data[, !(colnames(data) %in% c("Title", "Date")), drop = FALSE]
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
                    id = i,
                    taxon = group_data$Taxon[i],
                    dipnet1 = group_data$Dipnet1[i],
                    dipnet2 = group_data$Dipnet2[i],
                    tsn = group_data$tsn[i],
                    parentTsn = group_data$parentTsn[i]
                  )
                })

                selected_genera[[group_name]] <- shiny::reactiveVal(current_group)
              })
            }
          }
        }

        shiny::removeModal()
        shiny::showNotification("Auto-save loaded successfully!", type = "message")
        message(paste("Loaded data from file:", input$selected_file))
      } else {
        shiny::showNotification("The selected file no longer exists.", type = "error")
      }
    })

    shiny::observeEvent(input$delete_file, {
      shiny::req(input$selected_file)
      selected_file_path <- file.path(auto_save_path, input$selected_file)

      shiny::showModal(
        shiny::modalDialog(
          title = "Confirm Deletion",
          paste("Are you sure you want to delete the file", input$selected_file, "?",
                "<strong>The data will be permanently deleted - <span style='color: red;'>there is no method for recovery.</strong></span>"),
          footer = shiny::tagList(
            shiny::modalButton("Cancel"),
            shiny::actionButton(ns("confirm_delete"), "Yes, Delete", class = "btn-danger")
          )
        )
      )
    })

    shiny::observeEvent(input$confirm_delete, {
      shiny::req(input$selected_file)
      selected_file_path <- file.path(auto_save_path, input$selected_file)

      if (file.exists(selected_file_path)) {
        file.remove(selected_file_path)
        shiny::showNotification("Selected auto-save file has been deleted.", type = "warning")
        message(paste("Deleted file", selected_file_path))

        files <- list.files(auto_save_path, pattern = "\\.rds$", full.names = FALSE)
        shiny::removeModal()

        if (length(files) > 0) {
          shiny::showModal(
            shiny::modalDialog(
              title = "Manage Auto-Saves",
              shiny::selectInput(ns("selected_file"), "Available Files", choices = files),
              footer = shiny::tagList(
                shiny::modalButton("Cancel"),
                shiny::actionButton(ns("confirm_load"), "Load Selected File"),
                shiny::actionButton(ns("delete_file"), "Delete Selected File", class = "btn-danger")
              )
            )
          )
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
