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
    ns <- session$ns
    
    # --------------------------------------------------------------------
    # Helper: collect lineage (tsn + all ancestors) for a single TSN
    # --------------------------------------------------------------------
    collect_lineage <- function(start_tsn, tax_tbl) {
      start_tsn <- as.character(start_tsn)
      tax_tbl$tsn       <- as.character(tax_tbl$tsn)
      tax_tbl$parentTsn <- as.character(tax_tbl$parentTsn)
      
      lineage <- character()
      visited <- character()
      current <- start_tsn
      
      while (!is.na(current) && nzchar(current) && !(current %in% visited)) {
        visited  <- c(visited, current)
        lineage  <- c(lineage, current)
        
        row <- tax_tbl[tax_tbl$tsn == current, ]
        if (nrow(row) == 0) break
        
        parent <- row$parentTsn[1]
        parent <- as.character(parent)
        
        if (is.na(parent) || parent == "") break
        
        current <- parent
      }
      
      unique(lineage)
    }
    
    # --------------------------------------------------------------------
    # Helper: build a "tree_nodes" table (tsn, parentTsn, taxon, level, selected)
    # --------------------------------------------------------------------
    build_tree_nodes <- function(selected_tsns, tax_tbl) {
      selected_tsns <- unique(as.character(selected_tsns))
      tax_tbl$tsn       <- as.character(tax_tbl$tsn)
      tax_tbl$parentTsn <- as.character(tax_tbl$parentTsn)
      
      # Get all tsns on the paths from leaves up to roots
      all_tsns <- unique(unlist(
        lapply(selected_tsns, collect_lineage, tax_tbl = tax_tbl)
      ))
      
      # Subset taxonomy to just those nodes
      nodes <- tax_tbl[tax_tbl$tsn %in% all_tsns,
                       c("tsn", "parentTsn", "taxon", "level")]
      nodes$selected <- nodes$tsn %in% selected_tsns
      
      nodes
    }
    
    # --------------------------------------------------------------------
    # Helper: compute layout (x,y) for each node
    # --------------------------------------------------------------------
    layout_tree <- function(nodes) {
      # Define a rank order from top (Kingdom) to bottom (Species)
      rank_levels <- c(
        "Kingdom", "Subkingdom", "Infrakingdom",
        "Superphylum", "Phylum", "Subphylum",
        "Superclass", "Class", "Subclass", "Infraclass",
        "Superorder", "Order", "Suborder", "Infraorder",
        "Superfamily", "Family", "Subfamily",
        "Tribe", "Subtribe",
        "Genus", "Subgenus",
        "Species"
      )
      
      # Map taxonomic level to a numeric rank index
      nodes$rank_index <- match(nodes$level, rank_levels)
      
      # In case some levels are missing, just drop NAs
      nodes <- nodes[!is.na(nodes$rank_index), ]
      
      # x coordinate: higher ranks higher on the plot
      max_rank <- max(nodes$rank_index, na.rm = TRUE)
      nodes$x  <- max_rank - nodes$rank_index + 1
      
      # y coordinate:
      #  1. assign leaves (selected taxa) equally spaced
      #  2. internal nodes get mean of their children
      leaf_tsns <- nodes$tsn[nodes$selected]
      leaf_tsns <- unique(leaf_tsns)
      
      # If we really only have 0 or 1 leaves, we can't build a meaningful tree
      if (length(leaf_tsns) <= 1) return(NULL)
      
      coords <- data.frame(
        tsn = nodes$tsn,
        y   = NA_real_,
        x   = nodes$x,
        stringsAsFactors = FALSE
      )
      
      coords$y[match(leaf_tsns, coords$tsn)] <- seq_along(leaf_tsns)
      coords$x <- max(coords$x, na.rm = TRUE) - coords$x + 1
      
      # Build an edge table for convenience
      edges <- nodes[!is.na(nodes$parentTsn) & nodes$parentTsn != "",
                     c("parentTsn", "tsn")]
      names(edges) <- c("parent", "child")
      
      # Propagate y positions upward: each parent gets mean(child y)
      changed <- TRUE
      while (changed) {
        changed <- FALSE
        
        for (p in unique(edges$parent)) {
          idx_p <- which(coords$tsn == p)
          if (length(idx_p) == 0) next
          
          if (is.na(coords$y[idx_p])) {
            child_tsns <- edges$child[edges$parent == p]
            child_x    <- coords$y[match(child_tsns, coords$tsn)]
            child_x    <- child_x[!is.na(child_x)]
            
            if (length(child_x) > 0) {
              coords$y[idx_p] <- mean(child_x)
              changed <- TRUE
            }
          }
        }
      }
      
      # Return both nodes and edges so we can draw the tree
      list(
        nodes = nodes,
        edges = edges,
        coords = coords,
        leaf_tsns = leaf_tsns
      )
    }
    
    # --------------------------------------------------------------------
    # Helper: draw the tree with base R graphics
    # --------------------------------------------------------------------
    draw_tree_base <- function(tree) {
      nodes     <- tree$nodes
      edges     <- tree$edges
      coords    <- tree$coords
      leaf_tsns <- tree$leaf_tsns
      
      # Save and restore par so we don't mess up other plots
      old_par <- par(no.readonly = TRUE)
      on.exit(par(old_par), add = TRUE)
      
      # Give extra right margin and let drawing go outside plot region
      # mar: bottom, left, top, right
      par(mar = c(2, 2, 2, 8), xpd = NA)
      
      # Basic plotting window (you already swapped x/y; this just respects that)
      y_range <- range(coords$y, na.rm = TRUE)
      x_range <- range(coords$x, na.rm = TRUE)
      
      # If you want x flipped, keep rev(x_range) here:
      plot(
        rev(x_range) + c(-1, 1),  # flip horizontally; drop rev() if you don't want that
        y_range + c(-1, 1),
        type = "n", axes = FALSE, xlab = "", ylab = ""
      )
      
      # Draw edges
      for (i in seq_len(nrow(edges))) {
        p <- edges$parent[i]
        c <- edges$child[i]
        
        ip <- match(p, coords$tsn)
        ic <- match(c, coords$tsn)
        
        if (is.na(coords$x[ip]) || is.na(coords$x[ic])) next
        
        segments(
          x0 = coords$x[ip], y0 = coords$y[ip],
          x1 = coords$x[ic], y1 = coords$y[ic]
        )
      }
      
      # Label leaves (selected taxa)
      leaf_idx <- match(leaf_tsns, coords$tsn)
      leaf_lab <- nodes$taxon[match(leaf_tsns, nodes$tsn)]
      
      # Dynamic label size based on number of leaves
      n_leaves <- length(leaf_tsns)
      label_cex <- max(0.4, min(1.2, 12 / n_leaves))
      # Rough rule:
      # - up to ~12 leaves → cex ≈ 1
      # - lots of leaves   → cex shrinks but not below 0.4
      
      text(
        x      = coords$x[leaf_idx],
        y      = coords$y[leaf_idx] - 0.1,
        labels = leaf_lab,
        srt    = 45,
        adj    = c(1, 0.5),
        cex    = label_cex
      )
      
      title("Taxonomic Relationships")
    }
    
    # --------------------------------------------------------------------
    # Reactive: build tree layout for the current selection
    # --------------------------------------------------------------------
    reactive_tree <- shiny::reactive({
      shiny::req(selected_genera(), taxonomy_df())
      
      section_data <- selected_genera()$data
      shiny::req(section_data)
      
      # Extract TSNs of selected taxa
      selected_tsns <- purrr::map_chr(section_data, ~ as.character(.x$tsn))
      
      # Build nodes table from taxonomy
      nodes <- build_tree_nodes(selected_tsns, taxonomy_df())
      
      # If we somehow only have 0 or 1 selected taxa, bail early
      if (sum(nodes$selected) <= 1) {
        warning("Tree cannot be generated with fewer than two taxa.")
        return(NULL)
      }
      
      # Compute layout
      layout_tree(nodes)
    })
    
    # --------------------------------------------------------------------
    # Plot output
    # --------------------------------------------------------------------
    output$taxonomic_tree <- shiny::renderPlot({
      tree <- reactive_tree()
      if (is.null(tree)) {
        plot.new()
        text(0.5, 0.5, "Tree cannot be generated for the selected taxa.", cex = 1.5)
      } else {
        draw_tree_base(tree)
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
        tree <- reactive_tree()
        grDevices::png(file, width = 1200, height = 800, res = 120)
        on.exit(grDevices::dev.off(), add = TRUE)
        
        if (is.null(tree)) {
          plot.new()
          text(0.5, 0.5, "Tree cannot be generated for the selected taxa.", cex = 1.5)
        } else {
          draw_tree_base(tree)
        }
      }
    )
  })
}
