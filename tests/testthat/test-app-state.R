testthat::test_that("init_app_state initializes and stores paths", {
  # Access the internal .app_state environment from the package namespace
  state_env <- get(".app_state", envir = asNamespace("macroibi"))
  
  # Save any existing paths and restore after the test
  old_paths <- state_env$paths
  on.exit({
    state_env$paths <- old_paths
  }, add = TRUE)
  
  # Start from a clean slate
  state_env$paths <- NULL
  
  paths <- init_app_state()
  
  # init_app_state should return a list of paths
  testthat::expect_type(paths, "list")
  
  # Required keys
  expected_names <- c(
    "www_path",
    "extdata_path",
    "www_prefix",
    "user_data_dir",
    "autosave_dir",
    "metric_autosave_dir"
  )
  testthat::expect_true(all(expected_names %in% names(paths)))
  
  # .app_state$paths should be the same object that was returned
  testthat::expect_identical(state_env$paths, paths)
  
  # Prefix should be fixed to the package name
  testthat::expect_identical(paths$www_prefix, "macroibi")
  
  # User directories should exist on disk
  testthat::expect_true(dir.exists(paths$user_data_dir))
  testthat::expect_true(dir.exists(paths$autosave_dir))
  testthat::expect_true(dir.exists(paths$metric_autosave_dir))
})

testthat::test_that("init_app_state is idempotent", {
  state_env <- get(".app_state", envir = asNamespace("macroibi"))
  
  old_paths <- state_env$paths
  on.exit({
    state_env$paths <- old_paths
  }, add = TRUE)
  
  state_env$paths <- NULL
  
  first <- init_app_state()
  second <- init_app_state()
  
  # Calling init twice should give the same paths (no randomization)
  testthat::expect_identical(first, second)
})

testthat::test_that("get_app_path lazily initializes state", {
  state_env <- get(".app_state", envir = asNamespace("macroibi"))
  
  old_paths <- state_env$paths
  on.exit({
    state_env$paths <- old_paths
  }, add = TRUE)
  
  # Force state to be NULL so get_app_path must call init_app_state()
  state_env$paths <- NULL
  
  user_path <- get_app_path("user_data_dir")
  
  # init_app_state should have populated state_env$paths
  testthat::expect_false(is.null(state_env$paths))
  
  # Returned value should match the stored path
  testthat::expect_identical(user_path, state_env$paths$user_data_dir)
  
  # Paths should be character vectors
  testthat::expect_type(user_path, "character")
})

testthat::test_that("get_app_path returns NULL for unknown keys", {
  state_env <- get(".app_state", envir = asNamespace("macroibi"))
  
  old_paths <- state_env$paths
  on.exit({
    state_env$paths <- old_paths
  }, add = TRUE)
  
  # Ensure state is initialized
  state_env$paths <- NULL
  init_app_state()
  
  # Unknown key is not an error, just NULL
  testthat::expect_null(get_app_path("not_a_real_key"))
})

testthat::test_that("load_taxonomy replaces empty strings and re-levels Group", {
  state_env <- get(".app_state", envir = asNamespace("macroibi"))
  
  old_paths <- state_env$paths
  on.exit({
    state_env$paths <- old_paths
  }, add = TRUE)
  
  # Build a minimal fake taxonomy with:
  # - Group as character
  # - Another character column with an empty string to test na_if
  fake_taxonomy <- data.frame(
    Group = "Beetles - Order Coleoptera",
    other_col = c("keep_me", ""),
    stringsAsFactors = FALSE
  )
  
  # Save to a temporary directory using the expected file name
  tmp_dir <- tempfile("taxonomy_extdata_")
  dir.create(tmp_dir, recursive = TRUE, showWarnings = FALSE)
  fake_path <- file.path(tmp_dir, "taxonomy_20251208.rds")
  saveRDS(fake_taxonomy, fake_path)
  
  # Point extdata_path at our temporary directory so load_taxonomy()
  # reads the fake file instead of the real one.
  state_env$paths <- list(extdata_path = tmp_dir)
  
  tax <- load_taxonomy()
  
  # Basic structure
  testthat::expect_s3_class(tax, "data.frame")
  testthat::expect_true(all(c("Group", "other_col") %in% names(tax)))
  
  # Group should be a factor with the specified levels
  expected_levels <- c(
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
  
  testthat::expect_s3_class(tax$Group, "factor")
  testthat::expect_identical(levels(tax$Group), expected_levels)
  
  # The value we supplied should be one of those levels
  testthat::expect_identical(
    as.character(tax$Group[1]),
    "Beetles - Order Coleoptera"
  )
  
  # Empty string should have been converted to NA in other_col
  testthat::expect_identical(tax$other_col, c("keep_me", NA_character_))
})
