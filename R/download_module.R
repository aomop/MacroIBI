#' Download module UI
#'
#' @param id Module identifier.
#' @return A Shiny UI fragment.
#' @keywords internal
download_module_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::tags$div(
      shiny::tags$h4("Save Data", style = "font-size: 16px;"),
      shiny::downloadButton(ns("save_data"), "Download Data (CSV)")
    )
  )
}

#' Download module server
#'
#' @param id Module identifier.
#' @param selected_genera Reactive values of selected genera.
#' @param shared_reactives Shared reactive values.
#' @param group_defs Data frame with columns section_id, group_id, group_name
#'   mapping module IDs to stable group identifiers.
#' @param auto_save_interval Interval used for autosave (reserved for future use).
#' @param auto_save_path Path to autosave directory (reserved for future use).
#' @param expose_data_reactive Optional reactiveVal used to expose assembled data.
#' @keywords internal
download_module_server <- function(
    id,
    selected_genera,
    shared_reactives,
    group_defs,
    auto_save_interval = 30,
    auto_save_path = get_app_path("autosave_dir"),
    expose_data_reactive = NULL
) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Assemble a single data frame from all section_* modules,
    # using group_defs to map section_id -> (group_id, group_name).
    assemble_data <- function() {
      shiny::req(selected_genera)
      
      # Names in selected_genera are your section IDs: section_1, section_2, ...
      section_ids <- names(shiny::reactiveValuesToList(selected_genera))
      
      all_data <- lapply(section_ids, function(section_id) {
        # Only care about actual taxon sections
        if (!startsWith(section_id, "section_")) {
          return(NULL)
        }
        
        # Look up stable group info for this section
        meta <- group_defs[group_defs$section_id == section_id, , drop = FALSE]
        if (nrow(meta) == 0L) {
          # No mapping defined for this section; safer to skip than guess
          return(NULL)
        }
        
        group_id   <- meta$group_id[1]
        group_name <- meta$group_name[1]
        
        # Pull the data from this section
        group_reactive <- selected_genera[[section_id]]
        section_data   <- shiny::isolate(group_reactive()$data)
        
        if (is.null(section_data) || length(section_data) == 0L) {
          return(NULL)
        }
        
        # Convert list-of-rows structure to a data.frame with group metadata
        do.call(
          rbind,
          lapply(section_data, function(row) {
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
      
      combined_data <- do.call(rbind, all_data)
      
      if (is.null(combined_data) || nrow(combined_data) == 0L) {
        return(NULL)
      }
      
      combined_data$Title          <- shared_reactives$user_title
      combined_data$Date           <- as.character(shared_reactives$user_date)
      combined_data$schema_version <- "2.0.0"
      
      combined_data
    }
    
    # Optional: expose assembled data to other modules
    if (!is.null(expose_data_reactive)) {
      shiny::observe({
        data <- assemble_data()
        if (!is.null(data)) {
          expose_data_reactive(data)
        }
      })
    }
    
    # Manual download handler
    output$save_data <- shiny::downloadHandler(
      filename = function() {
        paste0(
          "IBI_",
          shared_reactives$user_title, "_",
          shared_reactives$user_date,
          ".csv"
        )
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
