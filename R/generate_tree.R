#' Taxonomic tree UI
#'
#' @param id Module identifier.
#' @return A Shiny UI fragment.
#' @keywords internal
taxonomic_tree_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shinycssloaders::withSpinner(shiny::plotOutput(ns("taxonomic_tree"))),
    shinyjs::hidden(id =
    shiny::downloadButton(ns("download_tree"), "Download Tree Image")
    )
  )
}

#' Taxonomic tree server
#'
#' @param id Module identifier.
#' @param selected_genera Reactive for the current group's selected genera.
#'   This should be a reactive that returns an object with a `$data` field,
#'   where `$data` is a list of taxon rows (each row a list with `tsn`, etc.).
#' @param taxonomy_df Reactive taxonomy data frame.
#' @keywords internal
#' @import stats
#' @import graphics
taxonomic_tree_server <- function(id, selected_genera, taxonomy_df) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # --------------------------------------------------------------------
    # Reactive: build tree layout for the current selection
    # --------------------------------------------------------------------
    reactive_tree <- shiny::reactive({
      # Ensure we have both the selected genera and taxonomy
      shiny::req(selected_genera(), taxonomy_df())
      
      section_state <- selected_genera()
      section_data  <- section_state$data
      
      if (is.null(section_data) || length(section_data) == 0L) {
        return(list(
          tree    = NULL,
          message = "No taxa in this group yet.\nAdd at least two taxa to see a tree."
        ))
      }
      
      # Extract TSNs of selected taxa
      selected_tsns <- purrr::map_chr(section_data, ~ as.character(.x$tsn))
      
      if (length(selected_tsns) < 2L) {
        return(list(
          tree    = NULL,
          message = "Tree cannot be generated with fewer than two taxa."
        ))
      }
      
      # Build nodes table from taxonomy using helper
      nodes <- build_tree_nodes(selected_tsns, taxonomy_df())
      
      if (is.null(nodes) || nrow(nodes) == 0L) {
        return(list(
          tree    = NULL,
          message = "No matching nodes found in the taxonomy for the selected taxa."
        ))
      }
      
      if (sum(nodes$selected) <= 1L) {
        return(list(
          tree    = NULL,
          message = "Tree collapses to a single node.\nNeed at least two distinct taxa in this clade."
        ))
      }
      
      # Compute layout; if it errors, return a debug message
      tree_obj <- tryCatch(
        layout_tree(nodes),
        error = function(e) {
          warning("layout_tree() failed: ", conditionMessage(e))
          NULL
        }
      )
      
      if (is.null(tree_obj)) {
        return(list(
          tree    = NULL,
          message = "An error occurred while laying out the tree.\nCheck the logs for details."
        ))
      }
      
      # Success: wrap in our standard structure
      list(
        tree    = tree_obj,
        message = NULL
      )
    })
    
    # --------------------------------------------------------------------
    # Plot output
    # --------------------------------------------------------------------
    `%||%` <- function(x, y) if (is.null(x)) y else x
    
    output$taxonomic_tree <- shiny::renderPlot({
      res <- reactive_tree()
      
      # If the reactive was cancelled by req()
      if (is.null(res)) {
        plot.new()
        text(
          0.5, 0.5,
          "Tree not available yet.\n(reactive cancelled by req())",
          cex = 1.2
        )
        return()
      }
      
      if (is.null(res$tree)) {
        msg <- res$message %||% "Tree could not be generated for an unknown reason."
        plot.new()
        text(0.5, 0.5, msg, cex = 1.2)
      } else {
        draw_tree_base(res$tree)
      }
    })
    
    # --------------------------------------------------------------------
    # Download handler
    # --------------------------------------------------------------------
    output$download_tree <- shiny::downloadHandler(
      filename = function() {
        paste0(ns("section"), "_taxa_tree.png")
      },
      content = function(file) {
        res <- reactive_tree()
        
        grDevices::png(file, width = 1200, height = 800, res = 120)
        on.exit(grDevices::dev.off(), add = TRUE)
        
        if (is.null(res) || is.null(res$tree)) {
          plot.new()
          text(
            0.5, 0.5,
            "Tree cannot be generated for the selected taxa.",
            cex = 1.5
          )
        } else {
          draw_tree_base(res$tree)
        }
      }
    )
  })
}
