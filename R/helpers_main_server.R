#' Build group definitions from a list of group names
#' @keywords internal
build_group_defs <- function(group_list) {
  data.frame(
    section_id = paste0("section_", seq_along(group_list)),
    group_id   = sanitize_group_id(group_list),
    group_name = group_list,
    stringsAsFactors = FALSE
  )
}

#' Normalize in_region to a logical flag
#' @keywords internal
normalize_in_region_flag <- function(taxonomy) {
  taxonomy %>%
    dplyr::mutate(
      in_region_flag = dplyr::case_when(
        .data$in_region %in% c(TRUE, "TRUE", "True", "T", "1")    ~ TRUE,
        .data$in_region %in% c(FALSE, "FALSE", "False", "F", "0") ~ FALSE,
        TRUE                                                      ~ NA
      ),
      in_region_flag = dplyr::coalesce(.data$in_region_flag, TRUE)
    )
}

#' Build choices for the main taxon selectize input
#' @keywords internal
build_taxon_choices <- function(taxonomy, show_out_of_region = FALSE) {
  choices_df <- taxonomy %>%
    dplyr::filter(
      !is.na(.data$taxon),
      .data$taxon != "",
      !is.na(.data$Group)
    ) %>%
    normalize_in_region_flag()
  
  if (!show_out_of_region) {
    choices_df <- dplyr::filter(choices_df, .data$in_region_flag)
  }
  
  choices_df %>%
    dplyr::mutate(
      caution_flag = dplyr::if_else(
        !.data$in_region_flag,
        "<span style='color:#d9534f; font-weight:bold; margin-right:4px;' title='Outside region'>&#9888;</span>",
        ""
      ),
      name = paste0(
        .data$caution_flag,
        "<strong>", .data$taxon, "</strong> ",
        "<span style='color: rgba(0, 0, 0, 0.5); font-size: 0.9em;'>",
        .data$level,
        "</span>"
      ),
      value = .data$taxon
    ) %>%
    dplyr::select(dplyr::all_of(c("value", "name")))
}

#' Resolve selected taxon to section + IDs
#'
#' Given the taxonomy, group list, and a selected taxon name, return a list
#' with taxon, section_id, tsn, and parentTsn. If the taxon is missing or
#' ambiguous, returns NULLs.
#'
#' @keywords internal
resolve_selected_taxon <- function(taxonomy, group_list, selected_taxon_data) {
  # Handle empty / NULL selection.
  if (is.null(selected_taxon_data) || !nzchar(selected_taxon_data)) {
    return(list(taxon = NULL, section_id = NULL, tsn = NULL, parentTsn = NULL))
  }
  
  taxon_rows <- taxonomy %>%
    dplyr::filter(.data$taxon == .env$selected_taxon_data)
  
  # No match? Bail out.
  if (nrow(taxon_rows) == 0L) {
    return(list(taxon = NULL, section_id = NULL, tsn = NULL, parentTsn = NULL))
  }
  
  taxon_group <- taxon_rows %>%
    dplyr::pull(.data$Group) %>%
    unique()
  
  # Multiple groups for the same taxon? Treat as ambiguous → no selection.
  if (length(taxon_group) != 1L) {
    return(list(taxon = NULL, section_id = NULL, tsn = NULL, parentTsn = NULL))
  }
  
  section_id <- paste0("section_", which(group_list == taxon_group))
  
  taxon_info <- taxon_rows %>%
    dplyr::select(dplyr::all_of(c("taxon", "tsn", "parentTsn"))) %>%
    dplyr::distinct()
  
  list(
    taxon     = selected_taxon_data,
    section_id = section_id,
    tsn        = taxon_info$tsn[1],
    parentTsn  = taxon_info$parentTsn[1]
  )
}

#' Sum group totals with a minimum length requirement
#' @keywords internal
sum_group_totals <- function(counts, expected_groups) {
  # counts: named or unnamed list of values (or reactives handled by reactive_handler)
  # expected_groups: integer count of how many groups we expect
  
  if (length(counts) < expected_groups) {
    return(0)
  }
  
  sum(purrr::map_dbl(counts, reactive_handler), na.rm = TRUE)
}
