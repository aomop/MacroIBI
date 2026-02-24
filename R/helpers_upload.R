#' Extract metadata and taxa rows from uploaded CSV
#'
#' @param data Data frame read from the uploaded CSV.
#'
#' @return A list with:
#'   - meta: list(title = ..., date = ...)
#'   - taxa: data frame with non-metadata columns only.
#' @keywords internal
split_uploaded_results <- function(data) {
  title <- if ("Title" %in% names(data)) data$Title[1] else NA_character_
  date  <- if ("Date"  %in% names(data)) data$Date[1]  else NA_character_
  
  taxa <- data[, !(names(data) %in% METADATA_COLS), drop = FALSE]
  
  list(
    meta = list(
      title = title,
      date  = date
    ),
    taxa = taxa
  )
}

#' Normalize uploaded taxa into per-section row lists
#'
#' Handles both the new schema (group_id / section_id) and the legacy
#' schema (Group column with section_* IDs). Returns, for each section_id,
#' a list of taxon rows suitable for assigning into `current_group$data`.
#'
#' @param data Data frame of uploaded taxa (metadata already stripped).
#' @param group_defs Mapping data frame with columns:
#'   section_id, group_id, group_name.
#'
#' @return A named list. Each element is named by section_id and contains
#'   a list of row-lists with fields:
#'   id, taxon, dipnet1, dipnet2, tsn, parentTsn.
#' @keywords internal
normalize_uploaded_taxa <- function(data, group_defs) {
  make_row_list <- function(df) {
    lapply(seq_len(nrow(df)), function(i) {
      list(
        id        = i,
        taxon     = df$Taxon[i],
        dipnet1   = df$Dipnet1[i],
        dipnet2   = df$Dipnet2[i],
        tsn       = df$tsn[i],
        parentTsn = df$parentTsn[i]
      )
    })
  }
  
  result <- list()
  
  if ("group_id" %in% names(data) && "section_id" %in% names(data)) {
    # --- New schema path ----------------------------------------------------
    split_by_group <- split(data, data$group_id)
    
    for (gid in names(split_by_group)) {
      group_data <- split_by_group[[gid]]
      
      meta <- group_defs[group_defs$group_id == gid, , drop = FALSE]
      if (nrow(meta) == 0L) {
        next
      }
      
      section_id <- meta$section_id[1]
      
      result[[section_id]] <- make_row_list(group_data)
    }
    
  } else if ("Group" %in% names(data)) {
    # --- Legacy schema path -------------------------------------------------
    split_data <- split(data, data$Group)
    
    for (group_name in names(split_data)) {
      if (!startsWith(group_name, SECTION_PREFIX)) next
      
      group_data <- split_data[[group_name]]
      
      result[[group_name]] <- make_row_list(group_data)
    }
    
  } else {
    stop("Uploaded file does not contain expected grouping columns.")
  }
  
  result
}
