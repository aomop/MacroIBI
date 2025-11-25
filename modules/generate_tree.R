# --------------------------------------------------------------------
# UI Module for Taxonomic Tree Display
# --------------------------------------------------------------------
# Provides a UI placeholder for displaying a taxonomic tree visualization.
# Includes components for the tree display and a download button for the tree image.
taxonomic_tree_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::plotOutput(ns("taxonomic_tree")),
    shinyjs::hidden(id =
    shiny::downloadButton(ns("download_tree"), "Download Tree Image")
    )
  )
}

# --------------------------------------------------------------------
# Server Module for Constructing and Displaying the Taxonomic Tree
# --------------------------------------------------------------------
# Handles the server-side logic to construct and manage a taxonomic tree based on selected genera.
taxonomic_tree_server <- function(id, selected_genera, taxonomy_df) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns # Namespace function to manage IDs within this module

    # Reactive expression for constructing the tree using taxonomy data
    reactive_tree <- shiny::reactive({
      shiny::req(selected_genera()) # Ensure selected genera data is available
      
      # Extract the specific section data
      section_data <- selected_genera()$data
      shiny::req(section_data)  # Ensure section data is not NULL
      
      # Convert section_data to data.frame
      taxa_data <- data.frame(
        tsn = purrr::map_chr(section_data, "tsn"),
        stringsAsFactors = FALSE
      )
      
      # Generate the complete lineage up to the highest level for each TSN
      all_tsns <- unique(unlist(purrr::map(taxa_data$tsn, function(tsn) {
        current_tsn <- tsn
        tsns <- c(current_tsn)
        while(TRUE) {
          current_entry <- taxonomy_df()[taxonomy_df()$tsn == current_tsn, ]
          if(nrow(current_entry) == 0 || is.na(current_entry$parentTsn) || current_entry$parentTsn == "") {
            break
          }
          current_tsn <- current_entry$parentTsn
          tsns <- c(tsns, current_tsn)
        }
        tsns
      })))

      # Prepare full_taxonomy
      full_taxonomy <- taxonomy_df()[taxonomy_df()$tsn %in% all_tsns, ]
      full_taxonomy <- full_taxonomy[!duplicated(full_taxonomy$tsn), ]
      full_taxonomy <- full_taxonomy[order(match(full_taxonomy$tsn, all_tsns)), ]
      
      full_taxonomy$selected <- full_taxonomy$tsn %in% taxa_data$tsn
      
      # Step 1: Subset only the selected rows
      selected_rows <- full_taxonomy[full_taxonomy$selected == TRUE, ]
      
      # Return NULL if there is only one taxon
      if (nrow(selected_rows) <= 1) {
        warning("Tree cannot be generated with only one taxon.")
        return(NULL)
      }
      
      # Step 2: Define the taxonomic hierarchy
      taxonomic_hierarchy <- c("Species", "Subgenus", "Genus", "Subtribe", "Tribe", 
                               "Subfamily", "Family", "Superfamily", "Infraorder", "Suborder", 
                               "Order", "Superorder", "Infraclass", "Subclass", "Class", "Superclass", 
                               "Subphylum", "Phylum", "Superphylum", "Infrakingdom", "Subkingdom", "Kingdom")
      
      # Step 3: Find the MRCA level (lowest consistent rank)
      mrca_rank <- NA
      
      for (rank in taxonomic_hierarchy) {
        # Check if the column exists in selected_rows
        if (rank %in% colnames(selected_rows)) {
          values <- selected_rows[[rank]]  # Extract the column
          
          # Check for MRCA conditions: no NA and only one unique value
          if (all(!is.na(values)) && length(unique(values)) == 1) {
            mrca_rank <- rank
            break  # Stop at the first valid MRCA level
          }
        }
      }
      
      # Check if MRCA was found
      if (is.na(mrca_rank)) {
        stop("Failed to identify MRCA: no consistent taxonomic rank found.")
      }
      
      # Step 4: Retain rows at or below the MRCA level
      # Define valid levels as ranks equal to or more specific than mrca_rank
      valid_levels <- taxonomic_hierarchy[which(taxonomic_hierarchy == mrca_rank):1]
      
      full_taxonomy <- full_taxonomy[full_taxonomy$level %in% valid_levels, ]
   
      if (nrow(full_taxonomy) <= 1) {
        warning("Tree cannot be generated with only one taxon.")
        return(NULL)
      }

      # Attempt to create the tree if multiple rows are present
      edge_list <- as.matrix(full_taxonomy[, c("parentTsn", "tsn")])

      tree <- tryCatch({
        ape::as.phylo(edge_list, directed = TRUE)
      }, error = function(e) {
        warning(paste("Failed to create tree:", e$message))
        NULL
      })

      if (is.null(tree)) {
        return(NULL)
      }

      # Step 1: Create a mapping from TSNs to taxon names
      tsn_to_taxon <- setNames(full_taxonomy$taxon, full_taxonomy$tsn)

      # Step 2: Safely replace TSNs in tree with taxon names
      tree$tip.label <- purrr::map_chr(tree$tip.label, function(x) {
        if (!is.na(x) && as.character(x) %in% names(tsn_to_taxon)) {
          tsn_to_taxon[[as.character(x)]]
        } else {
          paste("Unknown:", x)  # Fallback: keep the TSN value if no match
        }
      })

      tree$node.label <- purrr::map_chr(tree$node.label, function(x) {
        if (!is.na(x) && as.character(x) %in% names(tsn_to_taxon)) {
          tsn_to_taxon[[as.character(x)]]
        } else {
          paste("")  # Fallback: keep the TSN value if no match
        }
      })

      list(tree = tree)
    })

    draw_tree <- function(tree_obj) {
      ape::plot(tree_obj, show.tip.label = TRUE, cex = 0.8)
      ape::nodelabels(tree_obj$node.label, frame = "n", cex = 0.7, adj = c(1.1, -0.1))
      title("Taxonomic Tree")
    }

    # Output the tree plot
    output$taxonomic_tree <- shiny::renderPlot({
      tree_plot <- reactive_tree()
      if (is.null(tree_plot)) {
        plot.new()
        text(0.5, 0.5, "Tree cannot be generated with only one taxon.", cex = 1.5)
      } else {
        draw_tree(tree_plot$tree)
      }
    })
    
    
    # Download handler for tree image
    output$download_tree <- shiny::downloadHandler(
      filename = function() {
        paste0(ns("section"), "_taxa_tree.png")
      },
      content = function(file) {
        tree_plot <- reactive_tree()
        grDevices::png(file, width = 1200, height = 800, res = 120)
        on.exit(grDevices::dev.off(), add = TRUE)

        if (is.null(tree_plot)) {
          plot.new()
          text(0.5, 0.5, "Tree cannot be generated with only one taxon.", cex = 1.5)
        } else {
          draw_tree(tree_plot$tree)
        }
      }
    )
  })
}
