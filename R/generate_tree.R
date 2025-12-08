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
      # Normalize to character
      start_tsn <- as.character(start_tsn)
      
      tax_tbl <- tax_tbl %>%
        dplyr::mutate(
          tsn       = as.character(.data$tsn),
          parentTsn = as.character(.data$parentTsn)
        )
      
      lineage <- character()
      visited <- character()
      current <- start_tsn
      
      while (!is.na(current) && nzchar(current) && !(current %in% visited)) {
        visited <- c(visited, current)
        lineage <- c(lineage, current)
        
        # dplyr::filter instead of base [ tax_tbl$tsn == current, ]
        row <- tax_tbl %>%
          dplyr::filter(.data$tsn == current) %>%
          dplyr::slice(1L)  # in case of duplicates
        
        if (nrow(row) == 0L) break
        
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
      # Normalize input TSNs
      selected_tsns <- unique(as.character(selected_tsns))
      
      # Ensure character columns
      tax_tbl <- tax_tbl %>%
        dplyr::mutate(
          tsn       = as.character(.data$tsn),
          parentTsn = as.character(.data$parentTsn)
        )
      
      # Get all tsns on the paths from leaves up to roots
      all_tsns <- selected_tsns %>%
        lapply(collect_lineage, tax_tbl = tax_tbl) %>%
        unlist(use.names = FALSE) %>%
        unique()
      
      # Subset taxonomy to just those nodes & add selected flag
      nodes <- tax_tbl %>%
        dplyr::filter(.data$tsn %in% all_tsns) %>%
        dplyr::select(.data$tsn, .data$parentTsn, .data$taxon, .data$level) %>%
        dplyr::mutate(
          selected = .data$tsn %in% selected_tsns
        )
      
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
      # Map taxonomic level to a numeric rank index and drop unknown levels
      nodes <- nodes %>%
        dplyr::mutate(
          rank_index = match(.data$level, rank_levels)
        ) %>%
        dplyr::filter(!is.na(.data$rank_index))
      
      # If we have fewer than 2 selected taxa, no tree to draw
      selected_tsns <- nodes %>%
        dplyr::filter(.data$selected) %>%
        dplyr::pull(.data$tsn) %>%
        unique()
      
      if (length(selected_tsns) <= 1L) return(NULL)
      
      # --------------------------------------------------------------------
      # 1) Find LCA of all selected taxa
      # --------------------------------------------------------------------
      parent_lookup <- nodes$parentTsn
      names(parent_lookup) <- nodes$tsn
      
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
      
      if (length(common_anc) == 0L) {
        # Paranoid fallback
        min_rank_to_keep <- min(nodes$rank_index, na.rm = TRUE)
      } else {
        lca_idx  <- which(nodes$tsn %in% common_anc)
        lca_rank <- max(nodes$rank_index[lca_idx], na.rm = TRUE)
        
        # keep from LCA parent's level "down"
        min_rank_to_keep <- max(1L, lca_rank - 1L)
      }
      
      # --------------------------------------------------------------------
      # 2) Drop ranks above LCA parent & compute local x
      # --------------------------------------------------------------------
      nodes <- nodes %>%
        dplyr::filter(.data$rank_index >= min_rank_to_keep) %>%
        dplyr::mutate(
          x = .data$rank_index - min_rank_to_keep + 1L
        )
      
      # Recompute selected_tsns after trimming
      selected_tsns <- nodes %>%
        dplyr::filter(.data$selected) %>%
        dplyr::pull(.data$tsn) %>%
        unique()
      
      if (length(selected_tsns) <= 1L) return(NULL)
      
      # Build edge table
      edges <- nodes %>%
        dplyr::filter(!is.na(.data$parentTsn), .data$parentTsn != "") %>%
        dplyr::transmute(
          parent = .data$parentTsn,
          child  = .data$tsn
        )
      
      # children_by_parent adjacency
      children_by_parent <- split(edges$child, edges$parent)
      
      # Find roots: parentTsn not in nodes$tsn or NA/""
      parent_in_nodes <- nodes$parentTsn %in% nodes$tsn
      root_tsns <- nodes %>%
        dplyr::mutate(
          parent_in_nodes = parent_in_nodes
        ) %>%
        dplyr::filter(
          !.data$parent_in_nodes |
            is.na(.data$parentTsn) |
            .data$parentTsn == ""
        ) %>%
        dplyr::pull(.data$tsn) %>%
        unique()
      
      selected_set <- unique(selected_tsns)
      
      # DFS to get leaf order
      leaf_order <- character()
      
      dfs <- function(tsn) {
        ch <- children_by_parent[[tsn]]
        if (!is.null(ch) && length(ch) > 0L) {
          ch <- sort(ch)
          for (cc in ch) dfs(cc)
        }
        if (tsn %in% selected_set) {
          leaf_order <<- c(leaf_order, tsn)
        }
      }
      
      for (r in root_tsns) {
        dfs(r)
      }
      
      missing <- setdiff(selected_set, leaf_order)
      if (length(missing) > 0L) {
        leaf_order <- c(leaf_order, missing)
      }
      
      # --------------------------------------------------------------------
      # 3) Build coords and assign y
      # --------------------------------------------------------------------
      coords <- nodes %>%
        dplyr::select(tsn = .data$tsn, x = .data$x) %>%
        dplyr::mutate(
          y = NA_real_
        )
      
      spacing <- 1
      coords$y[match(leaf_order, coords$tsn)] <- seq_along(leaf_order) * spacing
      
      # Propagate y up: parents at mean(child y)
      changed <- TRUE
      while (changed) {
        changed <- FALSE
        
        for (p in unique(edges$parent)) {
          idx_p <- which(coords$tsn == p)
          if (length(idx_p) == 0L) next
          
          if (is.na(coords$y[idx_p])) {
            child_tsns <- edges$child[edges$parent == p]
            child_y    <- coords$y[match(child_tsns, coords$tsn)]
            child_y    <- child_y[!is.na(child_y)]
            
            if (length(child_y) > 0L) {
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
      leaf_tsns <- tree$leaf_tsns   # selected taxa
      
      # Save and restore par so we don't mess up other plots
      old_par <- par(no.readonly = TRUE)
      on.exit(par(old_par), add = TRUE)
      
      # Margins + allow drawing in margins
      par(mar = c(5, 2, 2, 8), xpd = NA)
      
      # Basic plotting window -------------------------------------------------
      y_range <- range(coords$y, na.rm = TRUE)
      x_range <- range(coords$x, na.rm = TRUE)
      
      plot(
        x_range + c(-1, 1),
        y_range + c(-1, 1),
        type = "n", axes = FALSE, xlab = "", ylab = ""
      )
      
      # -----------------------------------------------------------------------
      # 1) X-axis rank labels ("clade positions")
      # -----------------------------------------------------------------------
      level_df <- nodes %>%
        dplyr::select(.data$tsn, .data$level, .data$rank_index) %>%
        dplyr::left_join(
          coords %>%
            dplyr::select(.data$tsn, .data$x),
          by = "tsn"
        ) %>%
        dplyr::distinct(.data$level, .keep_all = TRUE) %>%
        dplyr::filter(.data$level %in% axis_levels) %>%
        dplyr::arrange(.data$rank_index)
      
      clade_positions <- level_df$x
      names(clade_positions) <- level_df$level
      
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
      
      # -----------------------------------------------------------------------
      # 2) Edges (tree skeleton)
      # -----------------------------------------------------------------------
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
      
      # -----------------------------------------------------------------------
      # 3) Node points: join coords + selected flag via dplyr
      # -----------------------------------------------------------------------
      coords_nodes <- coords %>%
        dplyr::left_join(
          nodes %>%
            dplyr::select(.data$tsn, .data$selected),
          by = "tsn"
        )
      
      node_selected <- !is.na(coords_nodes$selected) & coords_nodes$selected
      
      points(
        x   = coords_nodes$x,
        y   = coords_nodes$y,
        pch = 21,
        bg  = ifelse(node_selected, "black", "white"),
        cex = 0.8
      )
      
      # Legend ----------------------------------------------------------------
      usr <- par("usr")  # c(xmin, xmax, ymin, ymax)
      legend_x <- usr[2]
      legend_y <- usr[4]
      
      legend(
        x      = legend_x,
        y      = legend_y,
        legend = c("Selected taxon", "Non-selected\nparent taxon"),
        pch    = 21,
        pt.bg  = c("black", "white"),
        pt.cex = 0.9,
        bty    = "n",
        xpd    = NA
      )
      
      # -----------------------------------------------------------------------
      # 4) Label data prep with joins
      # -----------------------------------------------------------------------
      # Build a small data frame of the taxa to label
      leaf_df <- data.frame(tsn = leaf_tsns, stringsAsFactors = FALSE) %>%
        dplyr::left_join(coords, by = "tsn") %>%
        dplyr::left_join(
          nodes %>%
            dplyr::select(.data$tsn, .data$taxon),
          by = "tsn"
        )
      
      n_leaves  <- nrow(leaf_df)
      leaf_lab  <- leaf_df$taxon
      
      # Dynamic label size
      label_cex <- max(0.4, min(1.2, 12 / n_leaves))
      
      base_offset    <- strheight("M", cex = label_cex) * 1.2
      x_label_offset <- strwidth("M",  cex = label_cex) * 0.8
      y_padding      <- strheight("M", cex = label_cex) * 0.6
      
      # Precompute: does each tsn have children? ------------------------------
      parent_counts <- edges %>%
        dplyr::count(.data$parent, name = "n_children")
      
      leaf_df <- leaf_df %>%
        dplyr::left_join(
          parent_counts,
          by = c("tsn" = "parent")
        )
      
      has_children_vec <- !is.na(leaf_df$n_children) & leaf_df$n_children > 0
      
      # Precompute child mean y per parent (tidy version of tapply) ----------
      child_mean_tbl <- edges %>%
        dplyr::left_join(
          coords %>%
            dplyr::select(.data$tsn, .data$y),
          by = c("child" = "tsn")
        ) %>%
        dplyr::group_by(.data$parent) %>%
        dplyr::summarise(
          mean_y = mean(.data$y, na.rm = TRUE),
          .groups = "drop"
        )
      
      child_mean_y <- child_mean_tbl$mean_y
      names(child_mean_y) <- child_mean_tbl$parent
      
      # -----------------------------------------------------------------------
      # 5) Compute label positions
      # -----------------------------------------------------------------------
      label_x <- numeric(n_leaves)
      label_y <- numeric(n_leaves)
      
      y_min <- usr[3] + y_padding
      y_max <- usr[4] - y_padding
      
      for (i in seq_len(n_leaves)) {
        tsn_i        <- leaf_df$tsn[i]
        x_i          <- leaf_df$x[i]
        y_i          <- leaf_df$y[i]
        this_has_kid <- has_children_vec[i]
        
        x_raw <- x_i
        y_raw <- y_i
        
        if (!this_has_kid) {
          # Leaf: label to the right at same y
          x_raw <- x_i + x_label_offset
          y_raw <- y_i
        } else {
          # Internal selected node: above or below
          cm_y <- child_mean_y[tsn_i]
          
          if (is.na(cm_y)) {
            # Fallback: above
            y_raw <- y_i - base_offset
          } else {
            direction <- sign(cm_y - y_i)
            
            if (direction >= 0) {
              # Children are below → label above
              y_raw <- y_i - base_offset
            } else {
              # Children are above → label below
              y_raw <- y_i + base_offset
            }
          }
        }
        
        # Clamp vertically
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
