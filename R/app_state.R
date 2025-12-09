#' Internal application state
#'
#' Utilities to manage package paths and user data directories.
#' @keywords internal
"_PACKAGE"

.app_state <- new.env(parent = emptyenv())

#' Initialize application paths
#'
#' @return A list of paths used by the app.
#' @keywords internal
init_app_state <- function() {
  www_path <- system.file("app/www", package = "macroibi")
  extdata_path <- system.file("extdata", package = "macroibi")
  user_data_dir <- tools::R_user_dir("macroibi", which = "data")
  autosave_dir <- file.path(user_data_dir, "auto_saves")
  metric_autosave_dir <- file.path(user_data_dir, "metric_autosaves")

  dirs <- c(user_data_dir, autosave_dir, metric_autosave_dir)
  vapply(dirs, dir.create, FUN.VALUE = logical(1), recursive = TRUE, showWarnings = FALSE)

  .app_state$paths <- list(
    www_path = www_path,
    extdata_path = extdata_path,
    www_prefix = "macroibi",
    user_data_dir = user_data_dir,
    autosave_dir = autosave_dir,
    metric_autosave_dir = metric_autosave_dir
  )
}

#' Retrieve a stored path
#'
#' @param key Name of the path to retrieve.
#' @return A character path.
#' @keywords internal
get_app_path <- function(key) {
  if (is.null(.app_state$paths)) init_app_state()
  .app_state$paths[[key]]
}

#' Load bundled taxonomy data
#'
#' @return A taxonomy data frame.
#' @keywords internal
load_taxonomy <- function() {
  taxonomy_path <- file.path(get_app_path("extdata_path"), "taxonomy_20251208.rds")
  
  readRDS(taxonomy_path) %>%
    dplyr::mutate(
      # Replace "" with NA in all character columns
      dplyr::across(
        .cols = dplyr::where(is.character),
        .fns  = ~ dplyr::na_if(.x, "")
      ),
      # Re-level Group as a factor
      Group = factor(
        .data$Group,
        levels = c(
          "Dragonflies, Mayflies, Damselflies, and Caddisflies - EOT Orders",
          "Beetles - Order Coleoptera",
          "Flies and Midges - Order Diptera",
          "True Bugs - Order Hemiptera",
          "Other Aquatic Insects",
          "Snails - Class Gastropoda",
          "Leeches - Order Hirudinida",
          "Crustaceans - Subclass Eumalacostraca",
          "Other Non-Insect Invertebrates"
        )
      )
    )
}
