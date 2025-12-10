#' Seed demo autosaves from inst/
#'
#' Copy packaged demo autosaves into the user-writable autosave directories
#' when running in demo_mode.
#'
#' @param auto_save_path Character(1). Path to the user autosave directory for
#'   taxa data.
#' @param metric_save_path Character(1). Path to the user autosave directory for
#'   metric data.
#'
#' @return Invisibly returns NULL.
#' @keywords internal
seed_demo_autosaves_from_inst <- function(auto_save_path, metric_save_path) {
  pkg <- utils::packageName()
  
  demo_auto_dir   <- system.file("demo_autosaves",        package = pkg)
  demo_metric_dir <- system.file("demo_metric_autosaves", package = pkg)
  
  copy_demo_autosaves(
    demo_auto_dir   = demo_auto_dir,
    demo_metric_dir = demo_metric_dir,
    auto_save_path   = auto_save_path,
    metric_save_path = metric_save_path
  )
}

#' Copy demo autosaves from given directories
#'
#' Internal helper used by seed_demo_autosaves_from_inst(). This is split out
#' so we can unit test the file-copying behaviour without relying on an
#' installed package structure.
#'
#' @param demo_auto_dir   Path to demo autosaves (taxa) inside the package, or
#'   a temp directory in tests.
#' @param demo_metric_dir Path to demo metric autosaves inside the package, or
#'   a temp directory in tests.
#' @param auto_save_path   User-writable autosave directory for taxa.
#' @param metric_save_path User-writable autosave directory for metrics.
#'
#' @return Invisibly returns NULL.
#' @keywords internal
copy_demo_autosaves <- function(
    demo_auto_dir,
    demo_metric_dir,
    auto_save_path,
    metric_save_path
) {
  # If no demo autosaves are shipped, do nothing
  if (!nzchar(demo_auto_dir) || !dir.exists(demo_auto_dir)) {
    return(invisible(NULL))
  }
  
  # Make sure the target directories exist
  dir.create(auto_save_path,   recursive = TRUE, showWarnings = FALSE)
  dir.create(metric_save_path, recursive = TRUE, showWarnings = FALSE)
  
  # --- Copy taxa autosaves ---------------------------------------------------
  demo_auto_files <- list.files(demo_auto_dir, pattern = "\\.rds$", full.names = TRUE)
  
  if (length(demo_auto_files) > 0L) {
    file.copy(
      from      = demo_auto_files,
      to        = file.path(auto_save_path, basename(demo_auto_files)),
      overwrite = FALSE  # don't clobber user autosaves if they exist
    )
  }
  
  # --- Copy metric autosaves (optional) --------------------------------------
  if (nzchar(demo_metric_dir) && dir.exists(demo_metric_dir)) {
    demo_metric_files <- list.files(demo_metric_dir, pattern = "\\.rds$", full.names = TRUE)
    
    if (length(demo_metric_files) > 0L) {
      file.copy(
        from      = demo_metric_files,
        to        = file.path(metric_save_path, basename(demo_metric_files)),
        overwrite = FALSE
      )
    }
  }
  
  invisible(NULL)
}
