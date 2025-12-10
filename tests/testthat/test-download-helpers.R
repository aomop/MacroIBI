# tests/testthat/test-download-assemble-data.R

testthat::test_that("assemble_download_data combines sections and stamps metadata", {
  # Fake group_defs mapping section IDs to group meta
  group_defs <- data.frame(
    section_id = c("section_1", "section_2"),
    group_id   = c("G1", "G2"),
    group_name = c("Group One", "Group Two"),
    stringsAsFactors = FALSE
  )
  
  # Fake shared_reactives with title and date
  shared_reactives <- list(
    user_title = "Test Wetland",
    user_date  = as.Date("2025-12-01")
  )
  
  # Fake selected_list structure:
  # - list names are section IDs (section_1, section_2)
  # - each has a $data field that is a list of rows
  selected_list <- list(
    section_1 = list(
      data = list(
        list(
          taxon     = "Taxon A",
          dipnet1   = 5L,
          dipnet2   = 3L,
          tsn       = 1001L,
          parentTsn = 2001L
        ),
        list(
          taxon     = "Taxon B",
          dipnet1   = 0L,
          dipnet2   = 2L,
          tsn       = 1002L,
          parentTsn = 2001L
        )
      )
    ),
    section_2 = list(
      data = list(
        list(
          taxon     = "Taxon C",
          dipnet1   = 1L,
          dipnet2   = 1L,
          tsn       = 1003L,
          parentTsn = 2002L
        )
      )
    ),
    # This non-section entry should be ignored by assemble_download_data()
    other_info = list(
      data = list(
        list(
          taxon     = "Ignored Taxon",
          dipnet1   = 99L,
          dipnet2   = 99L,
          tsn       = 9999L,
          parentTsn = 9999L
        )
      )
    )
  )
  
  result <- assemble_download_data(
    selected_list    = selected_list,
    shared_reactives = shared_reactives,
    group_defs       = group_defs
  )
  
  # We expect 3 rows: 2 from section_1, 1 from section_2
  testthat::expect_s3_class(result, "data.frame")
  testthat::expect_equal(nrow(result), 3L)
  
  # Check required columns are present
  expected_cols <- c(
    "group_id", "group_name", "section_id",
    "Taxon", "Dipnet1", "Dipnet2",
    "tsn", "parentTsn",
    "Title", "Date", "schema_version"
  )
  testthat::expect_true(all(expected_cols %in% names(result)))
  
  # Check that group metadata and section_id are filled correctly
  # First two rows are section_1 / Group One
  testthat::expect_equal(result$group_id[1:2],   c("G1", "G1"))
  testthat::expect_equal(result$group_name[1:2], c("Group One", "Group One"))
  testthat::expect_equal(result$section_id[1:2], c("section_1", "section_1"))
  
  # Third row is section_2 / Group Two
  testthat::expect_equal(result$group_id[3],   "G2")
  testthat::expect_equal(result$group_name[3], "Group Two")
  testthat::expect_equal(result$section_id[3], "section_2")
  
  # Taxon and dipnet values preserved
  testthat::expect_equal(result$Taxon,   c("Taxon A", "Taxon B", "Taxon C"))
  testthat::expect_equal(result$Dipnet1, c(5L, 0L, 1L))
  testthat::expect_equal(result$Dipnet2, c(3L, 2L, 1L))
  
  # Shared metadata stamped correctly
  testthat::expect_equal(unique(result$Title),          "Test Wetland")
  testthat::expect_equal(unique(result$Date),           "2025-12-01")
  testthat::expect_equal(unique(result$schema_version), "2.0.0")
})

testthat::test_that("assemble_download_data skips sections without mapping or data", {
  group_defs <- data.frame(
    section_id = c("section_1"),
    group_id   = c("G1"),
    group_name = c("Group One"),
    stringsAsFactors = FALSE
  )
  
  shared_reactives <- list(
    user_title = "Empty Test",
    user_date  = as.Date("2025-12-02")
  )
  
  selected_list <- list(
    # Mapped section but empty data
    section_1 = list(
      data = list()
    ),
    # Unmapped section that would otherwise have data
    section_2 = list(
      data = list(
        list(
          taxon     = "Unmapped Taxon",
          dipnet1   = 1L,
          dipnet2   = 1L,
          tsn       = 123L,
          parentTsn = 456L
        )
      )
    )
  )
  
  result <- assemble_download_data(
    selected_list    = selected_list,
    shared_reactives = shared_reactives,
    group_defs       = group_defs
  )
  
  # Both sections should be effectively ignored, so we get NULL
  testthat::expect_null(result)
})

testthat::test_that("assemble_download_data returns NULL when selected_list is empty", {
  group_defs <- data.frame(
    section_id = character(),
    group_id   = character(),
    group_name = character(),
    stringsAsFactors = FALSE
  )
  
  shared_reactives <- list(
    user_title = "No Data Test",
    user_date  = as.Date("2025-12-03")
  )
  
  selected_list <- list()
  
  result <- assemble_download_data(
    selected_list    = selected_list,
    shared_reactives = shared_reactives,
    group_defs       = group_defs
  )
  
  testthat::expect_null(result)
})
