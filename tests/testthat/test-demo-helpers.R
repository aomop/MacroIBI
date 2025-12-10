test_that("copy_demo_autosaves does nothing when demo directory is missing", {
  # Create fake paths that do not exist
  demo_auto_dir   <- tempfile("no_demo_auto_")
  demo_metric_dir <- tempfile("no_demo_metric_")
  
  auto_save_path   <- tempfile("auto_save_")
  metric_save_path <- tempfile("metric_save_")
  
  # Ensure demo dirs really don't exist
  if (dir.exists(demo_auto_dir))   unlink(demo_auto_dir, recursive = TRUE)
  if (dir.exists(demo_metric_dir)) unlink(demo_metric_dir, recursive = TRUE)
  
  # Should be silent and not create any files
  expect_silent(
    copy_demo_autosaves(
      demo_auto_dir   = demo_auto_dir,
      demo_metric_dir = demo_metric_dir,
      auto_save_path   = auto_save_path,
      metric_save_path = metric_save_path
    )
  )
  
  expect_false(dir.exists(auto_save_path))
  expect_false(dir.exists(metric_save_path))
})

test_that("copy_demo_autosaves copies .rds files and does not overwrite existing files", {
  # Temporary directories for this test
  demo_auto_dir   <- tempfile("demo_auto_")
  demo_metric_dir <- tempfile("demo_metric_")
  auto_save_path   <- tempfile("auto_save_")
  metric_save_path <- tempfile("metric_save_")
  
  dir.create(demo_auto_dir,   recursive = TRUE, showWarnings = FALSE)
  dir.create(demo_metric_dir, recursive = TRUE, showWarnings = FALSE)
  
  # Create some fake demo autosaves
  taxa_demo_file   <- file.path(demo_auto_dir, "autosave_example.rds")
  taxa_nonrds_file <- file.path(demo_auto_dir, "README.txt")
  metric_demo_file <- file.path(demo_metric_dir, "metrics_example.rds")
  
  saveRDS(list(x = 1), taxa_demo_file)
  writeLines("not an rds", taxa_nonrds_file)
  saveRDS(list(y = 2), metric_demo_file)
  
  # First run: should copy the .rds files into the target dirs
  copy_demo_autosaves(
    demo_auto_dir   = demo_auto_dir,
    demo_metric_dir = demo_metric_dir,
    auto_save_path   = auto_save_path,
    metric_save_path = metric_save_path
  )
  
  expect_true(dir.exists(auto_save_path))
  expect_true(dir.exists(metric_save_path))
  
  expect_setequal(list.files(auto_save_path),   "autosave_example.rds")
  expect_setequal(list.files(metric_save_path), "metrics_example.rds")
  
  # Overwrite the taxa autosave in the target directory with custom content
  user_taxa_file <- file.path(auto_save_path, "autosave_example.rds")
  saveRDS(list(x = 999), user_taxa_file)
  
  # Second run: because overwrite = FALSE, our user-edited file should remain unchanged
  copy_demo_autosaves(
    demo_auto_dir   = demo_auto_dir,
    demo_metric_dir = demo_metric_dir,
    auto_save_path   = auto_save_path,
    metric_save_path = metric_save_path
  )
  
  loaded <- readRDS(user_taxa_file)
  expect_equal(loaded$x, 999)
})
