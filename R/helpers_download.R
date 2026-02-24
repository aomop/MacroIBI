#' Assemble download data from selected genera
#'
#' @param selected_list Named list. Each element corresponds to a section
#'   and must have a $data field that is a list of row lists.
#' @param shared_reactives List-like object with user_title and user_date.
#' @param group_defs Data frame with columns section_id, group_id, group_name.
#' @return A data.frame or NULL if there are no rows.
#' @keywords internal
assemble_download_data <- function(selected_list, shared_reactives, group_defs, taxonomy_data) {
  # Names in selected_list are your section IDs: section_1, section_2, ...
  section_ids <- names(selected_list)
  
  all_data <- lapply(section_ids, function(section_id) {
    # Only care about actual taxon sections
    if (!startsWith(section_id, SECTION_PREFIX)) {
      return(NULL)
    }
    
    # Look up stable group info for this section
    meta <- group_defs[group_defs$section_id == section_id, , drop = FALSE]
    if (nrow(meta) == 0L) {
      # No mapping defined for this section; safer to skip than guess
      return(NULL)
    }
    
    group_id   <- meta$group_id[1]
    group_name <- meta$group_name[1]
    
    section_obj <- selected_list[[section_id]]
    if (is.null(section_obj)) {
      return(NULL)
    }
    
    section_data <- section_obj$data
    
    if (is.null(section_data) || length(section_data) == 0L) {
      return(NULL)
    }
    
    # Convert list-of-rows structure to a data.frame with group metadata
    do.call(
      rbind,
      lapply(section_data, function(row) {
        data.frame(
          group_id   = group_id,
          group_name = group_name,
          section_id = section_id,
          Taxon      = row$taxon,
          Common     = taxonomy_data$common_names[taxonomy_data$tsn == row$tsn],
          Level      = taxonomy_data$level[taxonomy_data$tsn == row$tsn],
          Dipnet1    = row$dipnet1,
          Dipnet2    = row$dipnet2,
          tsn        = row$tsn,
          parentTsn  = row$parentTsn,
          stringsAsFactors = FALSE
        )
      })
    )
  })
  
  # Drop NULL elements before rbind to avoid errors when everything is empty
  all_data <- all_data[!vapply(all_data, is.null, logical(1L))]
  if (length(all_data) == 0L) {
    return(NULL)
  }
  
  combined_data <- do.call(rbind, all_data)
  
  if (is.null(combined_data) || nrow(combined_data) == 0L) {
    return(NULL)
  }
  
  # --- Safe handling of user_title and user_date ---------------------------
  # Pull values out of shared_reactives, but be robust to NULL / length 0 / ""
  raw_title <- shared_reactives$user_title
  if (is.null(raw_title) || length(raw_title) == 0L || identical(raw_title, "")) {
    title_val <- NA_character_
  } else {
    # Use the first element, coerced to character
    title_val <- as.character(raw_title)[1]
  }
  
  raw_date <- shared_reactives$user_date
  if (is.null(raw_date) || length(raw_date) == 0L || identical(raw_date, "")) {
    date_val <- NA_character_
  } else {
    # Safely coerce Date/POSIXct/character to character
    date_val <- as.character(raw_date)[1]
  }
  
  # Recycle to match number of rows (this avoids length-mismatch errors)
  n_rows <- nrow(combined_data)
  combined_data$Title          <- rep(title_val, n_rows)
  combined_data$Date           <- rep(date_val,  n_rows)
  combined_data$schema_version <- "2.0.0"
  
  combined_data
}
