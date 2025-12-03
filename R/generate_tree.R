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
#' @param selected_genera Reactive values of selected genera.
#' @param taxonomy_df Reactive taxonomy data frame.
#' @keywords internal
#' @import stats
#' @import graphics
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
    
    # --------------------------------------------------------------------
    # Helper: compute layout (x,y) for each node
    # --------------------------------------------------------------------
    layout_tree <- function(nodes) {
      # Map taxonomic level to a numeric rank index
      nodes$rank_index <- match(nodes$level, rank_levels)
      
      # Drop levels we don't know how to rank
      nodes <- nodes[!is.na(nodes$rank_index), , drop = FALSE]
      
      # --- If we have fewer than 2 selected taxa, no tree to draw ----------
      selected_tsns <- unique(nodes$tsn[nodes$selected])
      if (length(selected_tsns) <= 1) return(NULL)
      
      # --------------------------------------------------------------------
      # 1) Find MRCA of all selected taxa
      # --------------------------------------------------------------------
      parent_lookup <- setNames(nodes$parentTsn, nodes$tsn)
      
      get_ancestors <- function(tsn) {
        tsn <- as.character(tsn)
        out <- character()
        seen <- character()
        cur  <- tsn
        
        while (!is.na(cur) && nzchar(cur) && !(cur %in% seen)) {
          seen <- c(seen, cur)
          out  <- c(out, cur)
          
          p <- parent_lookup[[cur]]
          if (is.null(p) || is.na(p) || p == "") break
          
          cur <- as.character(p)
        }
        out
      }
      
      anc_list   <- lapply(selected_tsns, get_ancestors)
      common_anc <- Reduce(intersect, anc_list)
      
      if (length(common_anc) == 0) {
        # No common ancestor found in this subset (paranoid fallback)
        min_rank_to_keep <- min(nodes$rank_index, na.rm = TRUE)
      } else {
        # Pick the deepest common ancestor = MRCA (largest rank_index)
        mrca_idx  <- which(nodes$tsn %in% common_anc)
        mrca_rank <- max(nodes$rank_index[mrca_idx], na.rm = TRUE)
        
        # We want MRCA level + 1 "up" the tree:
        # e.g., if MRCA is Order, we keep from Superorder down.
        min_rank_to_keep <- max(1L, mrca_rank - 1L)
      }
      
      # --------------------------------------------------------------------
      # 2) Drop ranks above (MRCA parent), i.e., anything "more general"
      # --------------------------------------------------------------------
      nodes <- nodes[nodes$rank_index >= min_rank_to_keep, , drop = FALSE]
      
      nodes$x <- nodes$rank_index - min_rank_to_keep + 1
      
      # Selected taxa = “leaves” for our purposes
      selected_tsns <- unique(nodes$tsn[nodes$selected])
      if (length(selected_tsns) <= 1) return(NULL)
      
      # Build edge table for the trimmed tree
      edges <- nodes[!is.na(nodes$parentTsn) & nodes$parentTsn != "",
                     c("parentTsn", "tsn")]
      names(edges) <- c("parent", "child")
      
      # --- Build adjacency: children_by_parent ------------------------------
      children_by_parent <- split(edges$child, edges$parent)
      
      # Find roots: nodes whose parent is not in the node set (or is NA/"")
      parent_in_nodes <- nodes$parentTsn %in% nodes$tsn
      root_tsns <- unique(nodes$tsn[!parent_in_nodes | is.na(nodes$parentTsn) | nodes$parentTsn == ""])
      
      # Speed up membership tests
      selected_set <- unique(selected_tsns)
      
      # Depth-first traversal to get a topology-respecting leaf order
      leaf_order <- character()
      
      dfs <- function(tsn) {
        # Recurse into children first
        ch <- children_by_parent[[tsn]]
        if (!is.null(ch) && length(ch) > 0) {
          # Optional: sort children for a stable, deterministic order
          # You could sort by rank_index or by taxon name; simplest is:
          ch <- sort(ch)
          for (cc in ch) dfs(cc)
        }
        # If this node is one of the selected taxa, treat it as a leaf
        if (tsn %in% selected_set) {
          leaf_order <<- c(leaf_order, tsn)
        }
      }
      
      # Walk from each root (usually just one)
      for (r in root_tsns) {
        dfs(r)
      }
      
      # Safety: if something weird happens and we didn't visit all selected_tsns,
      # fall back to adding any missing ones at the end
      missing <- setdiff(selected_set, leaf_order)
      if (length(missing) > 0) {
        leaf_order <- c(leaf_order, missing)
      }
      
      # ----------------------------------------------------------------------
      # Build coords and assign y according to leaf_order
      # ----------------------------------------------------------------------
      coords <- data.frame(
        tsn = nodes$tsn,
        y   = NA_real_,
        x   = nodes$x,
        stringsAsFactors = FALSE
      )
      
      # Leaf spacing factor: tweak this to stretch/compress vertically
      spacing <- 1
      coords$y[match(leaf_order, coords$tsn)] <- seq_along(leaf_order) * spacing
      
      # Propagate y up the tree: parents at mean(child y)
      changed <- TRUE
      while (changed) {
        changed <- FALSE
        
        for (p in unique(edges$parent)) {
          idx_p <- which(coords$tsn == p)
          if (length(idx_p) == 0) next
          
          if (is.na(coords$y[idx_p])) {
            child_tsns <- edges$child[edges$parent == p]
            child_y    <- coords$y[match(child_tsns, coords$tsn)]
            child_y    <- child_y[!is.na(child_y)]
            
            if (length(child_y) > 0) {
              coords$y[idx_p] <- mean(child_y)
              changed <- TRUE
            }
          }
        }
      }
      
      list(
        nodes     = nodes,
        edges     = edges,
        coords    = coords,
        leaf_tsns = selected_tsns
      )
    }
      
    # --------------------------------------------------------------------
    # Helper: draw the tree with base R graphics
    # --------------------------------------------------------------------
    draw_tree_base <- function(tree,
                               axis_levels = rank_levels) {
      nodes     <- tree$nodes
      edges     <- tree$edges
      coords    <- tree$coords
      leaf_tsns <- tree$leaf_tsns   # selected taxa; name is a bit misleading if some are internal
      
      # Save and restore par so we don't mess up other plots
      old_par <- par(no.readonly = TRUE)
      on.exit(par(old_par), add = TRUE)
      
      # Give extra bottom margin for axis labels and let drawing go outside plot region
      # mar: bottom, left, top, right
      par(mar = c(5, 2, 2, 8), xpd = NA)
      
      # Basic plotting window
      y_range <- range(coords$y, na.rm = TRUE)
      x_range <- range(coords$x, na.rm = TRUE)
      
      plot(
        x_range + c(-1, 1),
        y_range + c(-1, 1),
        type = "n", axes = FALSE, xlab = "", ylab = ""
      )
      
      ## ---- derive clade positions from nodes -------------------------------
      level_df <- merge(
        nodes[, c("tsn", "level", "rank_index")],
        coords[, c("tsn", "x")],
        by = "tsn"
      )
      
      level_df <- level_df[!duplicated(level_df$level),
                           c("level", "rank_index", "x")]
      level_df <- level_df[level_df$level %in% axis_levels, , drop = FALSE]
      level_df <- level_df[order(level_df$rank_index), ]
      
      clade_positions <- setNames(level_df$x, level_df$level)
      
      if (length(clade_positions) > 0) {
        axis(
          side     = 1,
          at       = clade_positions,
          labels   = names(clade_positions),
          tick     = TRUE,
          las      = 2,
          cex.axis = 0.8
        )
        
        abline(v = clade_positions, lty = 3, col = "grey80")
      }
      ## ----------------------------------------------------------------------
      
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
      
      ## ---- draw node points -------------------------------------------
      # Align nodes$selected with coords rows
      node_idx_for_coords <- match(coords$tsn, nodes$tsn)
      node_selected       <- nodes$selected[node_idx_for_coords]
      
      # Simple filled circles: filled if selected, hollow otherwise
      points(
        x   = coords$x,
        y   = coords$y,
        pch = 21,
        bg  = ifelse(node_selected, "black", "white"),
        cex = 0.8
      )
      ## ---- legend for selected / non-selected nodes -------------------------
      # Get current user coordinates
      usr <- par("usr")  # c(xmin, xmax, ymin, ymax)
      
      # Place legend slightly to the right of the plot area and a bit below the top
      legend_x <- usr[2] 
      legend_y <- usr[4] 
      
      legend(
        x      = legend_x,
        y      = legend_y,
        legend = c("Selected taxon", "Non-selected\nparent taxon"),
        pch    = 21,
        pt.bg  = c("black", "white"),  # fill colors to match points()
        pt.cex = 0.9,
        bty    = "n",                  # no box
        xpd    = NA                    # allow drawing in margin
      )
      ## ----------------------------------------------------------------------
      
      
      # Label selected taxa (leaf_tsns may include internal nodes)
      leaf_idx <- match(leaf_tsns, coords$tsn)
      leaf_lab <- nodes$taxon[match(leaf_tsns, nodes$tsn)]
      
      # Dynamic label size based on number of labeled taxa
      n_leaves  <- length(leaf_tsns)
      label_cex <- max(0.4, min(1.2, 12 / n_leaves))
      
      # Base vertical offset for labels (in "character heights")
      base_offset    <- strheight("M", cex = label_cex) * 1.2
      x_label_offset <- strwidth("M",  cex = label_cex) * 0.8
      y_padding      <- strheight("M", cex = label_cex) * 0.6
      
      # Helper: is this tsn an internal node (has children) or a tip?
      has_children <- function(tsn) {
        any(edges$parent == tsn)
      }
      
      # Precompute a mean child y for each parent for directionality
      # (we only need this for nodes that *are* parents)
      child_mean_y <- tapply(
        coords$y[match(edges$child, coords$tsn)],
        edges$parent,
        mean,
        na.rm = TRUE
      )
      
      # Compute label positions taxon-by-taxon
      label_x <- numeric(n_leaves)
      label_y <- numeric(n_leaves)
      
      for (i in seq_along(leaf_tsns)) {
        tsn_i <- leaf_tsns[i]
        ci    <- leaf_idx[i]
        
        x_i <- coords$x[ci]
        y_i <- coords$y[ci]
        
        # Start with defaults
        x_raw <- x_i
        y_raw <- y_i
        
        if (!has_children(tsn_i)) {
          # --- LEAF: label to the right, same y -----------------------
          x_raw <- x_i + x_label_offset
          y_raw <- y_i
        } else {
          # --- INTERNAL SELECTED NODE: above or below -----------------
          cm_y <- child_mean_y[[as.character(tsn_i)]]
          
          if (is.null(cm_y) || is.na(cm_y)) {
            # Fallback: above the node
            y_raw <- y_i - base_offset
          } else {
            # Direction of children relative to this node
            direction <- sign(cm_y - y_i)
            
            if (direction >= 0) {
              # Children are below → put label above
              y_raw <- y_i - base_offset
            } else {
              # Children are above → put label below
              y_raw <- y_i + base_offset
            }
          }
        }
        
        # Clamp labels inside plotting region vertically
        y_min <- usr[3] + y_padding
        y_max <- usr[4] - y_padding
        
        label_x[i] <- x_raw
        label_y[i] <- max(y_min, min(y_raw, y_max))
      }
      
      text(
        x      = label_x,
        y      = label_y,
        labels = leaf_lab,
        srt    = 0,
        adj    = c(0, 0.5),
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
      if (is.null(section_data) || length(section_data) == 0) {
        return(list(
          tree    = NULL,
          message = "No taxa in this group yet.\nAdd at least two taxa to see a tree."
        ))
      }
      
      # Extract TSNs of selected taxa
      selected_tsns <- purrr::map_chr(section_data, ~ as.character(.x$tsn))
      
      if (length(selected_tsns) < 2) {
        return(list(
          tree    = NULL,
          message = "Tree cannot be generated with fewer than two taxa."
        ))
      }
      
      # Build nodes table from taxonomy
      nodes <- build_tree_nodes(selected_tsns, taxonomy_df())
      
      if (is.null(nodes) || nrow(nodes) == 0) {
        return(list(
          tree    = NULL,
          message = "No matching nodes found in the taxonomy for the selected taxa."
        ))
      }
      
      if (sum(nodes$selected) <= 1) {
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
        text(0.5, 0.5, "Tree not available yet.\n(reactive cancelled by req())", cex = 1.2)
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
