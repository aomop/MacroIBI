# Regression cover for date-based taxonomy file selection.
#
# load_taxonomy() and refresh_taxonomy() used to pick the "latest" file by
# filesystem mtime. Installing a package or checking out a repo rewrites those
# timestamps, so the choice was effectively arbitrary -- a user could silently
# get an older taxonomy than the one they expected. Selection is now driven by
# the date in the filename, so each test below inverts mtime order to make sure
# the filename wins.

testthat::test_that("load_taxonomy picks the newest file by filename date, not mtime", {
  state_env <- get(".app_state", envir = asNamespace("macroibi"))
  old_paths <- state_env$paths

  tmp_dir <- tempfile("taxonomy_selection_")
  dir.create(tmp_dir, recursive = TRUE, showWarnings = FALSE)

  on.exit({
    state_env$paths <- old_paths
    unlink(tmp_dir, recursive = TRUE)
  }, add = TRUE)

  older <- file.path(tmp_dir, "taxonomy_2026-02-24.rds")
  newer <- file.path(tmp_dir, "taxonomy_2026-07-01.rds")

  saveRDS(
    data.frame(Group = "Beetles - Order Coleoptera", marker = "older",
               stringsAsFactors = FALSE),
    older
  )
  saveRDS(
    data.frame(Group = "Beetles - Order Coleoptera", marker = "newer",
               stringsAsFactors = FALSE),
    newer
  )

  # Invert mtime order: make the OLDER-dated file the most recently modified.
  Sys.setFileTime(newer, Sys.time() - 60 * 60 * 24)
  Sys.setFileTime(older, Sys.time())

  state_env$paths <- list(extdata_path = tmp_dir)

  tax <- load_taxonomy()

  testthat::expect_identical(tax$marker, "newer")
})


testthat::test_that("refresh_taxonomy selects by filename date and dates output to match", {
  input_dir  <- tempfile("taxonomy_input_")
  output_dir <- tempfile("taxonomy_output_")
  dir.create(input_dir,  recursive = TRUE, showWarnings = FALSE)
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

  on.exit({
    unlink(input_dir,  recursive = TRUE)
    unlink(output_dir, recursive = TRUE)
  }, add = TRUE)

  older <- file.path(input_dir, "taxonomy_20260224.csv")
  newer <- file.path(input_dir, "taxonomy_20260701.csv")

  utils::write.csv(data.frame(taxon = "Baetis", marker = "older"),
                   older, row.names = FALSE)
  utils::write.csv(data.frame(taxon = "Baetis", marker = "newer"),
                   newer, row.names = FALSE)

  Sys.setFileTime(newer, Sys.time() - 60 * 60 * 24)
  Sys.setFileTime(older, Sys.time())

  result <- refresh_taxonomy(input_dir = input_dir, output_path = output_dir)

  testthat::expect_identical(result$marker, "newer")

  # The output is named for the SOURCE csv's date, not today's, so rebuilding
  # from an older CSV cannot masquerade as a current snapshot.
  testthat::expect_true(
    file.exists(file.path(output_dir, "taxonomy_2026-07-01.rds"))
  )
})


testthat::test_that("refresh_taxonomy ignores CSVs that are not pipeline output", {
  input_dir  <- tempfile("taxonomy_input_")
  output_dir <- tempfile("taxonomy_output_")
  dir.create(input_dir,  recursive = TRUE, showWarnings = FALSE)
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

  on.exit({
    unlink(input_dir,  recursive = TRUE)
    unlink(output_dir, recursive = TRUE)
  }, add = TRUE)

  utils::write.csv(data.frame(x = 1),
                   file.path(input_dir, "field_notes.csv"),
                   row.names = FALSE)

  testthat::expect_error(
    refresh_taxonomy(input_dir = input_dir, output_path = output_dir),
    "No taxonomy_<YYYYMMDD>.csv files found",
    fixed = TRUE
  )
})


testthat::test_that("refresh_taxonomy still requires an explicit input_dir", {
  testthat::expect_error(
    refresh_taxonomy(),
    "No `input_dir` specified",
    fixed = TRUE
  )
})
