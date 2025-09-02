# --------------------------------------------------------------------
# UI Module for Taxonomic Tree Display
# --------------------------------------------------------------------
# Provides a UI placeholder for displaying a taxonomic tree visualization.
# Includes components for the tree display and a download button for the tree image.
taxonomic_tree_ui <- function(id) {
  ns <- NS(id)
  tagList(
    plotOutput(ns("taxonomic_tree")),
    hidden(id = 
    downloadButton(ns("download_tree"), "Download Tree Image")
    )
  )
}

# --------------------------------------------------------------------
# Server Module for Constructing and Displaying the Taxonomic Tree
# --------------------------------------------------------------------
# Handles the server-side logic to construct and manage a taxonomic tree based on selected genera.
taxonomic_tree_server <- function(id, selected_genera, taxonomy_df) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns # Namespace function to manage IDs within this module
    
    # Reactive expression for constructing the tree using taxonomy data
    reactive_tree <- reactive({
      req(selected_genera()) # Ensure selected genera data is available
      
      # Extract the specific section data
      section_data <- selected_genera()$data
      req(section_data)  # Ensure section data is not NULL
      
      # Convert section_data to data.frame
      taxa_data <- data.frame(
        tsn = sapply(section_data, `[[`, "tsn"),
        stringsAsFactors = FALSE
      )
      
      # Generate the complete lineage up to the highest level for each TSN
      all_tsns <- unique(unlist(sapply(taxa_data$tsn, function(tsn) {
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
   
      # Attempt to create the tree if multiple rows are present
      if(nrow(full_taxonomy) > 1) {
        tryCatch({
          edge_list <- as.matrix(full_taxonomy[, c("parentTsn", "tsn")])
          tree <- as.phylo(edge_list, directed = TRUE)
          plot(tree)  # If tree creation is successful, plot it
        }, error = function(e) {
          print(paste("Failed to create tree:", e$message))
        })
      }
      
      # Step 1: Create a mapping from TSNs to taxon names
      tsn_to_taxon <- setNames(full_taxonomy$taxon, full_taxonomy$tsn)
      
      # Step 2: Safely replace TSNs in tree with taxon names
      tree$tip.label <- sapply(tree$tip.label, function(x) {
        if (!is.na(x) && as.character(x) %in% names(tsn_to_taxon)) {
          tsn_to_taxon[[as.character(x)]]
        } else {
          paste("Unknown:", x)  # Fallback: keep the TSN value if no match
        }
      })
      
      tree$node.label <- sapply(tree$node.label, function(x) {
        if (!is.na(x) && as.character(x) %in% names(tsn_to_taxon)) {
          tsn_to_taxon[[as.character(x)]]
        } else {
          paste("")  # Fallback: keep the TSN value if no match
        }
      })
      
      # Match `taxa_data$tsn` to `tree$tip.label` using the same conversion
      converted_tsn_labels <- sapply(taxa_data$tsn, function(tsn) {
        tsn_to_taxon[[as.character(tsn)]]
      })
      
      # Build the tree
      p <- ggtree(tree) +
        geom_tiplab(aes(label = label), fontface = 3) +
        geom_nodelab(aes(label = label), fontface = 3, hjust = 1.125, nudge_y = 0.125) +
        theme_tree2() +
        labs(title = "Taxonomic Tree")
      
      # Extract the x-axis range dynamically from the ggtree object
      tree_data <- ggplot_build(p)$data[[1]]  # Access tree's ggplot data
      xmax <- max(tree_data$x, na.rm = TRUE)  # Get the maximum x value for the tree
      
      # Rebuild the tree with dynamic x-axis limits
      p + xlim(0, xmax + 2)  # Add some buffer space
        
        #browser()
    })
    
    # Output the tree plot
    output$taxonomic_tree <- renderPlot({
      tree_plot <- reactive_tree()
      if (is.null(tree_plot)) {
        plot.new()
        text(0.5, 0.5, "Tree cannot be generated with only one taxon.", cex = 1.5)
      } else {
        print(tree_plot)
      }
    })
    
    
    # Download handler for tree image
    output$download_tree <- downloadHandler(
      filename = function() {
        paste0(ns("section"), "_taxa_tree.png")
      },
      content = function(file) {
        ggsave(file, plot = reactive_tree(), width = 10, height = 6)
      }
    )
  })
}