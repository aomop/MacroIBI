# Copy packaged demo autosaves into the user-writable autosave directories
# when running in demo_mode.
seed_demo_autosaves_from_inst <- function(auto_save_path, metric_save_path) {
  # Use the current package name (works inside your package code)
  pkg <- utils::packageName()
  
  # Directories inside the installed package (read-only)
  demo_auto_dir   <- system.file("demo_autosaves",        package = pkg)
  demo_metric_dir <- system.file("demo_metric_autosaves", package = pkg)
  
  # If no demo autosaves are shipped, do nothing
  if (!nzchar(demo_auto_dir) || !dir.exists(demo_auto_dir)) {
    return(invisible(NULL))
  }
  
  # Make sure the target directories exist and are writable
  dir.create(auto_save_path,   recursive = TRUE, showWarnings = FALSE)
  dir.create(metric_save_path, recursive = TRUE, showWarnings = FALSE)
  
  # --- Copy taxa autosaves -----------------------------------------------
  demo_auto_files <- list.files(demo_auto_dir, pattern = "\\.rds$", full.names = TRUE)
  
  if (length(demo_auto_files) > 0L) {
    file.copy(
      from      = demo_auto_files,
      to        = file.path(auto_save_path, basename(demo_auto_files)),
      overwrite = FALSE  # don't clobber user autosaves if they exist
    )
  }
  
  # --- Copy metric autosaves (optional) ----------------------------------
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
