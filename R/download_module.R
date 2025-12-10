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
    
    assemble_data <- function() {
      shiny::req(selected_genera)
      
      # Get current section IDs from reactiveValues
      section_ids <- names(shiny::reactiveValuesToList(selected_genera))
      
      # Build a plain list where each element is the *value* behind the reactive,
      # or the object itself if it's already a reactiveValues.
      selected_list <- shiny::isolate(
        stats::setNames(
          lapply(section_ids, function(section_id) {
            obj <- selected_genera[[section_id]]
            
            if (is.null(obj)) {
              return(NULL)
            }
            
            # If it's a reactive()/reactiveVal, call it; otherwise assume it's already
            # the underlying reactiveValues/list object like in the autosave module.
            if (is.function(obj)) {
              obj()
            } else {
              obj
            }
          }),
          section_ids
        )
      )
      
      assemble_download_data(
        selected_list    = selected_list,
        shared_reactives = shared_reactives,
        group_defs       = group_defs
      )
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
