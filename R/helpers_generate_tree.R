# R/helpers_taxonomic_tree.R

#' Default taxonomic rank levels used for tree layout
#'
#' These are the ITIS-like rank labels used to order nodes from
#' root to tips when drawing taxonomic trees.
#'
#' @keywords internal
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

#' Collect lineage for a TSN
#'
#' Given a starting TSN and a taxonomy table, walk up the \code{parentTsn}
#' chain to the root, returning all TSNs on that path (including the start).
#'
#' Cycles are guarded against by tracking visited nodes.
#'
#' @param start_tsn A single TSN (numeric or character).
#' @param tax_tbl A data frame with at least \code{tsn} and \code{parentTsn}
#'   columns.
#'
#' @return A character vector of TSNs from \code{start_tsn} up to the root.
#' @keywords internal
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
    visited  <- c(visited, current)
    lineage  <- c(lineage, current)
    
    row <- tax_tbl %>%
      dplyr::filter(.data$tsn == current) %>%
      dplyr::slice(1L)  # in case of duplicates
    
    if (nrow(row) == 0L) break
    
    parent <- as.character(row$parentTsn[1])
    if (is.na(parent) || parent == "") break
    
    current <- parent
  }
  
  unique(lineage)
}

#' Build taxonomic tree nodes for selected TSNs
#'
#' Construct a node table (tsn, parentTsn, taxon, level, selected) containing
#' all selected taxa plus all of their ancestors down to the root.
#'
#' Only rows whose \code{level} appears in \code{rank_levels} are retained.
#'
#' @param selected_tsns Character or numeric vector of TSNs representing the
#'   selected taxa.
#' @param tax_tbl A taxonomy data frame with columns \code{tsn},
#'   \code{parentTsn}, \code{taxon}, and \code{level}.
#'
#' @return A data frame with columns \code{tsn}, \code{parentTsn},
#'   \code{taxon}, \code{level}, and \code{selected}.
#' @keywords internal
build_tree_nodes <- function(selected_tsns, tax_tbl) {
  # Normalize input TSNs
  selected_tsns <- unique(as.character(selected_tsns))
  
  # Ensure key columns are character
  tax_tbl <- tax_tbl %>%
    dplyr::mutate(
      tsn       = as.character(.data$tsn),
      parentTsn = as.character(.data$parentTsn)
    )
  
  # Get all TSNs on the paths from leaves up to roots
  all_tsns <- selected_tsns %>%
    lapply(collect_lineage, tax_tbl = tax_tbl) %>%
    unlist(use.names = FALSE) %>%
    unique()
  
  # Subset taxonomy to just those nodes & add selected flag
  nodes <- tax_tbl %>%
    dplyr::filter(.data$tsn %in% all_tsns) %>%
    dplyr::select(
      dplyr::all_of(c("tsn", "parentTsn", "taxon", "level"))
    ) %>%
    dplyr::mutate(
      selected = .data$tsn %in% selected_tsns
    ) %>%
    dplyr::filter(
      !is.na(.data$level),
      .data$level %in% rank_levels
    )
  
  nodes
}

#' Layout a taxonomic tree for plotting
#'
#' Compute x/y coordinates and edges for a taxonomic tree defined by
#' \code{nodes} (as returned by \code{build_tree_nodes()}).
#'
#' The layout:
#' \itemize{
#'   \item maps taxonomic ranks to x positions via \code{rank_levels}
#'   \item prunes ranks above the least common ancestor (LCA) of the
#'         selected taxa
#'   \item lays out leaves vertically in order of a DFS walk
#'   \item positions internal nodes at the mean y of their children
#' }
#'
#' @param nodes A data frame with columns \code{tsn}, \code{parentTsn},
#'   \code{taxon}, \code{level}, and \code{selected}.
#'
#' @return A list with elements:
#'   \describe{
#'     \item{nodes}{The input nodes (possibly filtered) with rank_index/x.}
#'     \item{edges}{A data frame of parent/child TSN pairs.}
#'     \item{coords}{A data frame giving x/y coordinates for each TSN.}
#'     \item{leaf_tsns}{The TSNs of selected taxa that act as leaves.}
#'   }
#'   or \code{NULL} if a layout cannot be constructed.
#' @keywords internal
layout_tree <- function(nodes) {
  # Keep a copy of the full nodes table for lineage calculation
  nodes_full <- nodes
  
  # Map taxonomic level to a numeric rank index and drop unknown levels
  nodes <- nodes %>%
    dplyr::mutate(
      rank_index = match(.data$level, rank_levels)
    ) %>%
    dplyr::filter(!is.na(.data$rank_index))
  
  if (nrow(nodes) == 0L) {
    return(NULL)
  }
  
  # If we have fewer than 2 selected taxa, no tree to draw
  selected_tsns <- nodes %>%
    dplyr::filter(.data$selected) %>%
    dplyr::pull(.data$tsn) %>%
    unique()
  
  if (length(selected_tsns) <= 1L) return(NULL)
  
  # 1) Find LCA of all selected taxa using collect_lineage() ---------------
  anc_list <- lapply(
    selected_tsns,
    function(tsn) collect_lineage(tsn, tax_tbl = nodes_full)
  )
  
  anc_list_nonempty <- anc_list[lengths(anc_list) > 0]
  
  if (length(anc_list_nonempty) == 0L) {
    min_rank_to_keep <- min(nodes$rank_index, na.rm = TRUE)
  } else {
    common_anc <- Reduce(intersect, anc_list_nonempty)
    
    if (length(common_anc) == 0L) {
      min_rank_to_keep <- min(nodes$rank_index, na.rm = TRUE)
    } else {
      lca_idx  <- which(nodes$tsn %in% common_anc)
      lca_rank <- max(nodes$rank_index[lca_idx], na.rm = TRUE)
      min_rank_to_keep <- max(1L, lca_rank - 1L)
    }
  }
  
  # 2) Drop ranks above LCA parent & compute local x -----------------------
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
  
  # 3) Build edges & adjacency ---------------------------------------------
  node_tsns <- nodes$tsn
  
  edges <- nodes %>%
    dplyr::filter(
      !is.na(.data$parentTsn),
      .data$parentTsn != "",
      .data$parentTsn %in% node_tsns
    ) %>%
    dplyr::transmute(
      parent = .data$parentTsn,
      child  = .data$tsn
    )
  
  children_by_parent <- split(edges$child, edges$parent)
  
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
  
  # 4) Build coords and assign y -------------------------------------------
  coords <- nodes %>%
    dplyr::transmute(
      tsn = .data$tsn,
      x   = .data$x,
      y   = NA_real_
    )
  
  spacing <- 1
  
  leaf_idx <- match(leaf_order, coords$tsn)
  valid    <- !is.na(leaf_idx)
  leaf_idx <- leaf_idx[valid]
  
  if (length(leaf_idx) == 0L) {
    return(NULL)
  }
  
  coords$y[leaf_idx] <- seq_along(leaf_idx) * spacing
  
  # Propagate y up: parents at mean(child y)
  changed <- TRUE
  iter    <- 0L
  while (changed) {
    iter    <- iter + 1L
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
    
    if (iter > 1000L) {
      # Safety valve to avoid infinite loops in pathological graphs
      break
    }
  }
  
  list(
    nodes     = nodes,
    edges     = edges,
    coords    = coords,
    leaf_tsns = selected_tsns
  )
}

#' Draw a taxonomic tree using base graphics
#'
#' Given a tree layout (as returned by \code{layout_tree()}), draw
#' a taxonomic tree using base R graphics. This is used by the Shiny module
#' for on-screen rendering and PNG downloads.
#'
#' @param tree A list with elements \code{nodes}, \code{edges}, \code{coords},
#'   and \code{leaf_tsns}, as returned by \code{layout_tree()}.
#' @param axis_levels Character vector of rank labels to show on the x axis.
#'
#' @return Invisibly returns \code{NULL} after plotting.
#' @keywords internal
draw_tree_base <- function(tree,
                           axis_levels = rank_levels) {
  nodes     <- tree$nodes
  edges     <- tree$edges
  coords    <- tree$coords
  leaf_tsns <- tree$leaf_tsns
  
  old_par <- par(no.readonly = TRUE)
  on.exit(par(old_par), add = TRUE)
  
  par(mar = c(5, 2, 2, 8), xpd = NA)
  
  y_range <- range(coords$y, na.rm = TRUE)
  x_range <- range(coords$x, na.rm = TRUE)
  
  plot(
    x_range + c(-1, 1),
    y_range + c(-1, 1),
    type = "n", axes = FALSE, xlab = "", ylab = ""
  )
  
  # 1) Axis / clade positions ----------------------------------------------
  level_df <- nodes %>%
    dplyr::transmute(
      tsn        = .data$tsn,
      level      = .data$level,
      rank_index = .data$rank_index
    ) %>%
    dplyr::left_join(
      coords %>%
        dplyr::select(dplyr::all_of(c("tsn", "x"))),
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
  
  # 2) Edges ---------------------------------------------------------------
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
  
  # 3) Node points --------------------------------------------------------
  coords_nodes <- coords %>%
    dplyr::left_join(
      nodes %>%
        dplyr::select(dplyr::all_of(c("tsn", "selected"))),
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
  
  usr <- par("usr")
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
  
  # 4) Labels -------------------------------------------------------------
  leaf_df <- data.frame(tsn = leaf_tsns, stringsAsFactors = FALSE) %>%
    dplyr::left_join(coords, by = "tsn") %>%
    dplyr::left_join(
      nodes %>%
        dplyr::select(dplyr::all_of(c("tsn", "taxon"))),
      by = "tsn"
    )
  
  n_leaves  <- nrow(leaf_df)
  leaf_lab  <- leaf_df$taxon
  label_cex <- max(0.4, min(1.2, 12 / n_leaves))
  
  base_offset    <- strheight("M", cex = label_cex) * 1.2
  x_label_offset <- strwidth("M",  cex = label_cex) * 0.8
  y_padding      <- strheight("M", cex = label_cex) * 0.6
  
  parent_counts <- edges %>%
    dplyr::count(.data$parent, name = "n_children")
  
  leaf_df <- leaf_df %>%
    dplyr::left_join(parent_counts, by = c("tsn" = "parent"))
  
  has_children_vec <- !is.na(leaf_df$n_children) & leaf_df$n_children > 0
  
  child_mean_tbl <- edges %>%
    dplyr::left_join(
      coords %>%
        dplyr::select(dplyr::all_of(c("tsn", "y"))),
      by = c("child" = "tsn")
    ) %>%
    dplyr::group_by(.data$parent) %>%
    dplyr::summarise(
      mean_y  = mean(.data$y, na.rm = TRUE),
      .groups = "drop"
    )
  
  child_mean_y <- child_mean_tbl$mean_y
  names(child_mean_y) <- child_mean_tbl$parent
  
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
      x_raw <- x_i + x_label_offset
      y_raw <- y_i
    } else {
      cm_y <- child_mean_y[tsn_i]
      
      if (is.na(cm_y)) {
        y_raw <- y_i - base_offset
      } else {
        direction <- sign(cm_y - y_i)
        if (direction >= 0) {
          y_raw <- y_i - base_offset
        } else {
          y_raw <- y_i + base_offset
        }
      }
    }
    
    label_x[i] <- x_raw
    label_y[i] <- max(y_min, min(y_raw, y_max))
  }
  
  text(
    x      = label_x,
    y      = label_y,
    labels = leaf_lab,
    adj    = c(0, 0.5),
    cex    = label_cex
  )
  
  title("Taxonomic Relationships")
  
  invisible(NULL)
}
