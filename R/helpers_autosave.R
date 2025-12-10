#' Build summarized metric table for autosave
#'
#' This takes the current metric scores and appends a final "IBI Score (0-50)"
#' row whose `adj_score` is the sum of all other adjusted scores.
#'
#' @param metric_scores_data A data.frame with at least an `adj_score` column.
#' @return A data.frame with the original rows plus a final IBI summary row.
#' @keywords internal
build_summarized_metrics <- function(metric_scores_data) {
  # Defensive check: require adj_score column
  if (!"adj_score" %in% names(metric_scores_data)) {
    stop("metric_scores_data must contain an 'adj_score' column.")
  }
  
  # Calculate the total adjusted score, ignoring missing values
  total_adj <- sum(metric_scores_data$adj_score, na.rm = TRUE)
  
  # Build the final IBI summary row
  ibi_row <- data.frame(
    metric_name  = "IBI Score (0-50)",
    response     = "Decrease",
    min          = NA_real_,
    fifth        = NA_real_,
    ninety_fifth = NA_real_,
    max          = NA_real_,
    metric_value = NA_real_,
    metric_score = NA_real_,
    adj_score    = total_adj,
    stringsAsFactors = FALSE
  )
  
  # rbind will recycle missing columns in metric_scores_data as needed
  out <- rbind(metric_scores_data, ibi_row)
  rownames(out) <- NULL
  out
}

#' Flatten selected genera into an autosave-friendly data.frame
#'
#' This mirrors the logic used inside autosave_module_server(), but works on
#' a plain snapshot of `selected_genera` instead of live reactives. It is
#' designed for unit testing and for use inside the server module.
#'
#' @param selected_snapshot A named list. Each element name is a section_id
#'   (e.g. "section_1"), and each element is a list with a `data` element.
#'   The `data` element should itself be a list of rows, where each row is a
#'   list containing at least: taxon, dipnet1, dipnet2, tsn, parentTsn.
#' @param group_defs A data.frame with columns:
#'   - section_id
#'   - group_id
#'   - group_name
#' @return A data.frame with one row per taxon to be auto-saved, or NULL if
#'   there is no data to save.
#' @keywords internal
build_autosave_df <- function(selected_snapshot, group_defs) {
  # If there is nothing at all, bail out early
  if (length(selected_snapshot) == 0L) {
    return(NULL)
  }
  
  section_ids <- names(selected_snapshot)
  
  # Build a list of data.frames, one per section with data
  all_rows <- lapply(section_ids, function(section_id) {
    # Only modules are named "section_*" in this app
    if (!startsWith(section_id, "section_")) {
      return(NULL)
    }
    
    # Look up metadata for this section in group_defs
    meta <- group_defs[group_defs$section_id == section_id, , drop = FALSE]
    if (nrow(meta) == 0L) {
      # If there is no mapping, we skip this section
      return(NULL)
    }
    
    group_id   <- meta$group_id[1]
    group_name <- meta$group_name[1]
    
    # Pull the structure for this section from the snapshot
    section_obj <- selected_snapshot[[section_id]]
    
    # Each section_obj should have a $data list (matching the app convention)
    group_data <- section_obj$data
    
    # If there is no data yet for this section, skip it
    if (is.null(group_data) || length(group_data) == 0L) {
      return(NULL)
    }
    
    # Turn each row list into a single-row data.frame and row-bind
    rows_df <- do.call(
      rbind,
      lapply(group_data, function(row) {
        data.frame(
          group_id   = group_id,
          group_name = group_name,
          section_id = section_id,
          Taxon      = row$taxon,
          Dipnet1    = row$dipnet1,
          Dipnet2    = row$dipnet2,
          tsn        = row$tsn,
          parentTsn  = row$parentTsn,
          stringsAsFactors = FALSE
        )
      })
    )
    
    rows_df
  })
  
  # Drop NULL entries (sections with no data or no mapping, etc.)
  all_rows <- Filter(Negate(is.null), all_rows)
  
  # If every section was empty, return NULL
  if (!length(all_rows)) {
    return(NULL)
  }
  
  out <- do.call(rbind, all_rows)
  rownames(out) <- NULL
  out
}

#' Convert a per-section data.frame into the section data list
#'
#' Internal helper: turns a data.frame of taxa into the `list(data = list(...))`
#' structure used by selected_genera.
#'
#' @param group_data A data.frame with columns: Taxon, Dipnet1, Dipnet2, tsn, parentTsn.
#' @return A list with a single element `data`, which is a list of row-lists.
#' @keywords internal
build_section_data_list <- function(group_data) {
  if (nrow(group_data) == 0L) {
    return(list(data = list()))
  }
  
  rows <- lapply(seq_len(nrow(group_data)), function(i) {
    list(
      id        = i,
      taxon     = group_data$Taxon[i],
      dipnet1   = group_data$Dipnet1[i],
      dipnet2   = group_data$Dipnet2[i],
      tsn       = group_data$tsn[i],
      parentTsn = group_data$parentTsn[i]
    )
  })
  
  list(data = rows)
}

#' Rebuild section data from an autosave data.frame
#'
#' This helper mirrors the logic in autosave_module_server() when loading an
#' auto-save file, but returns a plain R structure instead of touching
#' reactives. It supports both the "new" schema (group_id + section mapping)
#' and the legacy schema (Group column with section IDs).
#'
#' @param autosave_df A data.frame of taxa (i.e. with Title/Date/schema_version
#'   columns already removed). It should contain either:
#'   - columns `group_id` and `section_id` (new schema), or
#'   - column `Group` containing section IDs (legacy schema).
#' @param group_defs A data.frame with columns:
#'   - section_id
#'   - group_id
#'   - group_name
#'   used to map group_id back to section_id in the new schema.
#'
#' @return A named list. Each element name is a section_id (e.g. "section_1"),
#'   and each value is a list with component `data`, which is itself a list of
#'   rows (id, taxon, dipnet1, dipnet2, tsn, parentTsn). Returns an empty list
#'   if there is no usable data. Throws an error if required grouping columns
#'   are missing.
#' @keywords internal
build_sections_from_autosave <- function(autosave_df, group_defs) {
  # If the data.frame is empty or not a data.frame at all, nothing to do
  if (!is.data.frame(autosave_df) || nrow(autosave_df) == 0L) {
    return(list())
  }
  
  # We always expect the taxon columns to be present in either schema
  required_taxon_cols <- c("Taxon", "Dipnet1", "Dipnet2", "tsn", "parentTsn")
  missing_taxon_cols  <- setdiff(required_taxon_cols, names(autosave_df))
  if (length(missing_taxon_cols) > 0L) {
    stop(
      "autosave_df is missing required columns: ",
      paste(missing_taxon_cols, collapse = ", ")
    )
  }
  
  out <- list()
  
  # --------------------------------------------------------------------------
  # New schema: group_id + section mapping via group_defs
  # --------------------------------------------------------------------------
  if (all(c("group_id", "section_id") %in% names(autosave_df))) {
    split_by_group <- split(autosave_df, autosave_df$group_id)
    
    for (gid in names(split_by_group)) {
      group_data <- split_by_group[[gid]]
      
      # Look up section_id via group_defs (source of truth)
      meta <- group_defs[group_defs$group_id == gid, , drop = FALSE]
      if (nrow(meta) == 0L) {
        # If the group is unknown, we skip it
        next
      }
      
      section_id <- meta$section_id[1]
      
      # Build the section data structure from the rows
      out[[section_id]] <- build_section_data_list(group_data)
    }
    
    return(out)
  }
  
  # --------------------------------------------------------------------------
  # Legacy schema: Group column contains section IDs (e.g. "section_1")
  # --------------------------------------------------------------------------
  if ("Group" %in% names(autosave_df)) {
    split_by_group <- split(autosave_df, autosave_df$Group)
    
    for (group_name in names(split_by_group)) {
      # Only section_* entries are relevant for this app
      if (!startsWith(group_name, "section_")) {
        next
      }
      
      group_data <- split_by_group[[group_name]]
      
      out[[group_name]] <- build_section_data_list(group_data)
    }
    
    return(out)
  }
  
  # --------------------------------------------------------------------------
  # Fallback: no recognized grouping columns
  # --------------------------------------------------------------------------
  stop(
    "Auto-save data is missing expected grouping columns: ",
    "need either 'group_id'/'section_id' or 'Group'."
  )
}
