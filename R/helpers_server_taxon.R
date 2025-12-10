#' Compute count of unique taxa given hierarchical taxonomy
#'
#' Given a vector of selected taxon names and a taxonomy data frame, count how
#' many are "unique" in the sense that parent taxa are not counted when a more
#' specific descendant is also selected.
#'
#' A taxon is treated as a parent of another when all of its non-NA rank fields
#' (e.g. Order, Suborder, Superfamily, Family, Genus, Species) match the
#' corresponding fields of the other taxon. Parents that are ancestors of any
#' other selected taxon are not counted as unique.
#'
#' @param selected_taxa Character vector of taxon names.
#' @param taxonomy_data Data frame with at least columns:
#'   \code{taxon}, \code{Order}, \code{Suborder}, \code{Superfamily},
#'   \code{Family}, \code{Genus}, \code{Species}.
#'
#' @return Integer count of unique taxa.
#' @keywords internal
compute_unique_taxa <- function(selected_taxa, taxonomy_data) {
  selected_taxa <- unique(selected_taxa)
  
  if (length(selected_taxa) == 0L) {
    return(0L)
  }
  
  selected_df <- taxonomy_data[taxonomy_data$taxon %in% selected_taxa, , drop = FALSE]
  
  if (nrow(selected_df) == 0L) {
    return(0L)
  }
  
  # Start by assuming all selected taxa are unique
  selected_df$unique <- TRUE
  
  # For each taxon, see if it is a parent of any other taxon in the selection
  for (i in seq_len(nrow(selected_df))) {
    current_row <- selected_df[i, ]
    
    for (j in seq_len(nrow(selected_df))) {
      if (i == j) next
      
      other_row <- selected_df[j, ]
      
      # Check if current_row is a parent of other_row based on hierarchical ranks
      if ((!is.na(current_row$Order)       && current_row$Order       == other_row$Order)       &&
          (is.na(current_row$Suborder)     || (!is.na(other_row$Suborder)     && current_row$Suborder     == other_row$Suborder))     &&
          (is.na(current_row$Superfamily)  || (!is.na(other_row$Superfamily)  && current_row$Superfamily  == other_row$Superfamily))  &&
          (is.na(current_row$Family)       || (!is.na(other_row$Family)       && current_row$Family       == other_row$Family))       &&
          (is.na(current_row$Genus)        || (!is.na(other_row$Genus)        && current_row$Genus        == other_row$Genus))        &&
          (is.na(current_row$Species)      || (!is.na(other_row$Species)      && current_row$Species      == other_row$Species))) {
        
        # current_row is an ancestor of other_row → mark as not unique
        selected_df$unique[i] <- FALSE
        break
      }
    }
  }
  
  sum(selected_df$unique, na.rm = TRUE)
}
