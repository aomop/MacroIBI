#' Taxon section server
#'
#' @param id Module identifier.
#' @param group_name Display name for the group.
#' @param taxonomy_data Taxonomy data frame.
#' @param selected_taxon Reactive value representing the selected taxon.
#' @param total_unique_taxa Reactive expression with the total unique taxa count.
#' @return A list of reactive values for the section.
#' @keywords internal
server_taxon_section <- function(id, group_name, taxonomy_data, selected_taxon, total_unique_taxa) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns  # Namespace for the module

    # Initialize reactive values
    selected_genera <- shiny::reactiveValues(data = list())    # Stores selected taxa for this group
    uploaded_data <- shiny::reactiveVal(NULL)                  # Placeholder for uploaded data
    
    ## --- Add a Taxon to the Group Table ---
    shiny::observe({
      taxon_info <- selected_taxon()  # Retrieve selected taxon information

      if (!is.null(taxon_info$taxon) && taxon_info$section_id == id) {
        taxon_name <- taxon_info$taxon
        tsn <- taxon_info$tsn
        parentTsn <- taxon_info$parentTsn
        section_id <- taxon_info$section_id
        
        # Add the taxon only if it's not already in the table
        if (!(taxon_name %in% purrr::map_chr(selected_genera$data, "taxon"))) {
          new_id <- uuid::UUIDgenerate()  # Generate a unique ID for the new row
          
          # Add the new taxon with default dipnet counts
          selected_genera$data <- c(
            selected_genera$data,
            list(list(id = new_id, 
                      taxon = taxon_name, 
                      common = common,
                      dipnet1 = 1, 
                      dipnet2 = 0, 
                      tsn = tsn, 
                      parentTsn = parentTsn, 
                      section = section_id))
          )
          
          # Reset selected_taxon to NULL to prevent re-triggering
          selected_taxon(NULL)
        }
        
        message(paste0("Added ", taxon_name, " (TSN: ", tsn, ") to ", group_name))
      }
    })
    
    ## --- Manage Delete Button Observers ---
    shiny::observe({
      # Ensure observers are dynamically registered for each row in `selected_genera$data`
      lapply(selected_genera$data, function(row_data) {
        id <- paste0("delete_", row_data$id)  # Unique ID for the delete button
        
        # Use a unique observer for each delete button
        shiny::observeEvent(input[[id]], {
          # Remove the corresponding row from `selected_genera$data`
          selected_genera$data <- Filter(function(x) x$id != row_data$id, selected_genera$data)
          message(paste0("Deleted ", row_data$taxon))
        }, ignoreInit = TRUE, priority = 900)  # High priority to avoid conflicts
      })
    })
    
    # Initialize reactive value to control the visibility of the tree
    tree_visible <- shiny::reactiveVal(FALSE)
    
    # Observer to toggle the visibility on button click
    shiny::observeEvent(input$show_tree, {
      tree_visible(!tree_visible())  # Toggle the visibility state
      message(paste0("Taxonomic tree visibility set to ", tree_visible(), " in ", group_name))
    })
    
    output$tree_ui <- shiny::renderUI({
      if (tree_visible()) {
        taxonomic_tree_ui(ns("taxonomic_tree"))
      }
    })
    
    ## --- Render Dynamic Taxon Table UI ---
    output$group_table_ui <- shiny::renderUI({
      if (length(selected_genera$data) == 0) return(NULL) # No data, return NULL
      shiny::tagList(
        # Create rows for each taxon
        lapply(selected_genera$data, function(row_data) {
          row_id <- row_data$id
          # Look up common name from taxonomy at render time
          cn <- if ("common_names" %in% names(taxonomy_data)) {
            cn_val <- taxonomy_data$common_names[taxonomy_data$tsn == row_data$tsn]
            if (length(cn_val) > 0 && !is.na(cn_val[1]) && nzchar(cn_val[1])) cn_val[1] else NULL
          }
          shiny::fluidRow(
            shiny::column(3,
              shiny::h5(row_data$taxon),
              if (!is.null(cn)) shiny::tags$div(
                style = "color: rgba(0,0,0,0.5); font-size: 0.85em; margin-top: -5px;",
                cn
              )
            ),  # Taxon name + common name subtitle
            shiny::column(3, shiny::numericInput(ns(paste0("dipnet1_", row_id)), label = NULL, value = row_data$dipnet1)),  # Dipnet1 Input
            shiny::column(3, shiny::numericInput(ns(paste0("dipnet2_", row_id)), label = NULL, value = row_data$dipnet2)),  # Dipnet2 input
            shiny::column(2, shiny::verbatimTextOutput(ns(paste0("sum_count_", row_id)))),  # Row sum
            shiny::column(1, shiny::actionButton(
              ns(paste0("delete_", row_id)),
              "Delete",
              class = "delete-btn"
            ))  # Delete button
          )
        }),
        # Add footer with summary information
        shiny::fluidRow(
          shiny::column(4, shiny::h4("Total Taxa: ", shiny::textOutput(ns("unique_taxa_count")))),                  # Unique taxa count
          shiny::column(4, shiny::h4("Percent of Total Sample: ", shiny::textOutput(ns("percent_total_sample")))),  # Percent of total sample
          shiny::column(4, shiny::h4("Total Individuals: ", shiny::textOutput(ns("group_sum_count"))))              # Group sum count
        ),
        shiny::actionButton(ns("show_tree"), "Show/Hide Taxonomic Hierarchy"),
        shiny::uiOutput(ns("tree_ui"))  # Dynamically rendered tree UI
      )
    })
    
    ## --- Update Taxon Data Dynamically ---
    shiny::observe({
      lapply(selected_genera$data, function(row_data) {
        row_id <- row_data$id
        
        # Retrieve input values for Dipnet counts
        dipnet1 <- input[[paste0("dipnet1_", row_id)]]
        dipnet2 <- input[[paste0("dipnet2_", row_id)]]
        
        # Update selected_genera$data with input values
        selected_genera$data <- lapply(selected_genera$data, function(item) {
          if (item$id == row_id) {
            item$dipnet1 <- if (!is.null(dipnet1)) dipnet1 else item$dipnet1
            item$dipnet2 <- if (!is.null(dipnet2)) dipnet2 else item$dipnet2
          }
          return(item)
        })
        
        # Render the row-level sum of Dipnet1 and Dipnet2
        output[[paste0("sum_count_", row_id)]] <- shiny::renderText({
          if (!is.null(dipnet1) && !is.null(dipnet2)) dipnet1 + dipnet2 else NA
        })
      })
    })
    
    ## --- Calculate Group Summary Statistics ---
    group_sum_count <- shiny::reactive({
      if (length(selected_genera$data) == 0) return(0)  # No data, return 0
      sum(purrr::map_dbl(selected_genera$data, function(row_data) {
        dipnet1 <- input[[paste0("dipnet1_", row_data$id)]]
        dipnet2 <- input[[paste0("dipnet2_", row_data$id)]]
        if (!is.null(dipnet1) && !is.null(dipnet2)) dipnet1 + dipnet2 else 0
      }), na.rm = TRUE)
    })
    
    # Calculate unique taxa, considering hierarchical relationships
    calculate_unique_taxa <- shiny::reactive({
      selected_taxa <- purrr::map_chr(selected_genera$data, "taxon")
      compute_unique_taxa(selected_taxa, taxonomy_data)
    })
    
    ## --- Render Summary Outputs ---
    output$percent_total_sample <- shiny::renderText({
      total <- total_unique_taxa()
      if (is.null(total) || total == 0) "0%" else {
        percent <- round((calculate_unique_taxa() / total) * 100, 2)
        paste0(percent, "%")
      }
    })
    
    # Render unique taxa count and total individuals
    output$unique_taxa_count <- shiny::renderText({ calculate_unique_taxa() })
    output$group_sum_count <- shiny::renderText({ group_sum_count() })
    
    # Initialize the tree-building module
    taxonomic_tree_server(id = "taxonomic_tree", 
                          selected_genera = reactive(selected_genera), 
                          taxonomy_df = reactive(taxonomy_data))
    
    ## --- Return Reactives ---
    list(
      unique_taxa = reactive(calculate_unique_taxa()),
      group_sum_count = reactive(group_sum_count()),
      selected_genera = reactive(selected_genera)
    )
  })
}
