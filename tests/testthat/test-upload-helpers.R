# tests/testthat/test-upload-helpers.R

# Helpers under test:
# - split_uploaded_results(data)
# - normalize_uploaded_taxa(data, group_defs)

testthat::test_that("split_uploaded_results extracts meta and strips metadata columns", {
  df <- data.frame(
    Title          = "Wetland A",
    Date           = "2024-07-03",
    schema_version = "2.0.0",
    group_id       = "g1",
    section_id     = "section_1",
    Taxon          = "Taxon A",
    Dipnet1        = 3,
    Dipnet2        = 4,
    tsn            = "1001",
    parentTsn      = "1000",
    stringsAsFactors = FALSE
  )
  
  res <- split_uploaded_results(df)
  
  # Structure
  testthat::expect_true(is.list(res))
  testthat::expect_true(all(c("meta", "taxa") %in% names(res)))
  
  # Meta extraction
  testthat::expect_equal(res$meta$title, "Wetland A")
  testthat::expect_equal(res$meta$date,  "2024-07-03")
  
  # Taxa data should not include Title / Date / schema_version
  taxa <- res$taxa
  testthat::expect_s3_class(taxa, "data.frame")
  testthat::expect_false("Title"          %in% names(taxa))
  testthat::expect_false("Date"           %in% names(taxa))
  testthat::expect_false("schema_version" %in% names(taxa))
  
  # Non-meta columns should still be present
  testthat::expect_true(all(
    c("group_id", "section_id", "Taxon", "Dipnet1", "Dipnet2", "tsn", "parentTsn") %in% names(taxa)
  ))
})

testthat::test_that("split_uploaded_results handles missing Title/Date gracefully", {
  df <- data.frame(
    schema_version = "2.0.0",
    group_id       = "g1",
    section_id     = "section_1",
    Taxon          = "Taxon A",
    Dipnet1        = 1,
    Dipnet2        = 2,
    tsn            = "1001",
    parentTsn      = "1000",
    stringsAsFactors = FALSE
  )
  
  res <- split_uploaded_results(df)
  
  # Meta values should be NA when columns are absent
  testthat::expect_true(is.na(res$meta$title))
  testthat::expect_true(is.na(res$meta$date))
  
  # Taxa should still drop schema_version
  taxa <- res$taxa
  testthat::expect_false("schema_version" %in% names(taxa))
})



# ------------------------------------------------------------------------
# normalize_uploaded_taxa() tests
# ------------------------------------------------------------------------

testthat::test_that("normalize_uploaded_taxa handles new schema with group_id/section_id", {
  data <- data.frame(
    group_id   = c("g1", "g1", "g2"),
    section_id = c("section_1", "section_1", "section_2"),
    Taxon      = c("Taxon A1", "Taxon A2", "Taxon B1"),
    Dipnet1    = c(1, 2, 3),
    Dipnet2    = c(0, 1, 0),
    tsn        = c("1001", "1002", "2001"),
    parentTsn  = c("1000", "1000", "2000"),
    stringsAsFactors = FALSE
  )
  
  group_defs <- data.frame(
    section_id = c("section_1", "section_2"),
    group_id   = c("g1", "g2"),
    group_name = c("Group One", "Group Two"),
    stringsAsFactors = FALSE
  )
  
  res <- normalize_uploaded_taxa(data, group_defs)
  
  # Should be a named list keyed by section_id
  testthat::expect_true(is.list(res))
  testthat::expect_equal(sort(names(res)), sort(c("section_1", "section_2")))
  
  # section_1: two rows
  sec1 <- res[["section_1"]]
  testthat::expect_true(is.list(sec1))
  testthat::expect_equal(length(sec1), 2L)
  
  testthat::expect_equal(sec1[[1]]$taxon,   "Taxon A1")
  testthat::expect_equal(sec1[[1]]$dipnet1, 1)
  testthat::expect_equal(sec1[[1]]$dipnet2, 0)
  testthat::expect_equal(sec1[[1]]$tsn,     "1001")
  testthat::expect_equal(sec1[[1]]$parentTsn, "1000")
  
  testthat::expect_equal(sec1[[2]]$taxon,   "Taxon A2")
  testthat::expect_equal(sec1[[2]]$dipnet1, 2)
  testthat::expect_equal(sec1[[2]]$dipnet2, 1)
  
  # section_2: one row
  sec2 <- res[["section_2"]]
  testthat::expect_true(is.list(sec2))
  testthat::expect_equal(length(sec2), 1L)
  testthat::expect_equal(sec2[[1]]$taxon, "Taxon B1")
})

testthat::test_that("normalize_uploaded_taxa skips groups not in group_defs (new schema)", {
  data <- data.frame(
    group_id   = c("g1", "unknown"),
    section_id = c("section_1", "section_x"),
    Taxon      = c("Taxon A", "Taxon X"),
    Dipnet1    = c(1, 2),
    Dipnet2    = c(0, 1),
    tsn        = c("1001", "9999"),
    parentTsn  = c("1000", "9990"),
    stringsAsFactors = FALSE
  )
  
  group_defs <- data.frame(
    section_id = "section_1",
    group_id   = "g1",
    group_name = "Group One",
    stringsAsFactors = FALSE
  )
  
  res <- normalize_uploaded_taxa(data, group_defs)
  
  # Only section_1 should be present
  testthat::expect_equal(names(res), "section_1")
  testthat::expect_equal(res$section_1[[1]]$taxon, "Taxon A")
})

testthat::test_that("normalize_uploaded_taxa handles legacy schema with Group == section_*", {
  data <- data.frame(
    Group     = c("section_1", "section_1", "section_2", "OtherGroup"),
    Taxon     = c("Taxon A1", "Taxon A2", "Taxon B1", "Ignored Taxon"),
    Dipnet1   = c(1, 2, 3, 9),
    Dipnet2   = c(0, 1, 0, 9),
    tsn       = c("1001", "1002", "2001", "9999"),
    parentTsn = c("1000", "1000", "2000", "9990"),
    stringsAsFactors = FALSE
  )
  
  # group_defs isn't used in legacy path, but we pass it for signature compatibility
  group_defs <- data.frame(
    section_id = character(),
    group_id   = character(),
    group_name = character(),
    stringsAsFactors = FALSE
  )
  
  res <- normalize_uploaded_taxa(data, group_defs)
  
  # Should only have section_* keys
  testthat::expect_equal(sort(names(res)), sort(c("section_1", "section_2")))
  
  sec1 <- res[["section_1"]]
  testthat::expect_equal(length(sec1), 2L)
  testthat::expect_equal(sec1[[1]]$taxon, "Taxon A1")
  testthat::expect_equal(sec1[[2]]$taxon, "Taxon A2")
  
  sec2 <- res[["section_2"]]
  testthat::expect_equal(length(sec2), 1L)
  testthat::expect_equal(sec2[[1]]$taxon, "Taxon B1")
})

testthat::test_that("normalize_uploaded_taxa errors cleanly when no grouping columns present", {
  data <- data.frame(
    Taxon     = c("Taxon A"),
    Dipnet1   = 1,
    Dipnet2   = 0,
    tsn       = "1001",
    parentTsn = "1000",
    stringsAsFactors = FALSE
  )
  
  group_defs <- data.frame(
    section_id = character(),
    group_id   = character(),
    group_name = character(),
    stringsAsFactors = FALSE
  )
  
  testthat::expect_error(
    normalize_uploaded_taxa(data, group_defs),
    "grouping columns",
    ignore.case = TRUE
  )
})
