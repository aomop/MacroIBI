test_that("build_summarized_metrics appends an IBI summary row", {
  # Minimal fake metric data
  metrics <- data.frame(
    metric_name  = c("Metric A", "Metric B"),
    response     = c("Increase", "Decrease"),
    min          = c(0, 0),
    fifth        = c(1, 2),
    ninety_fifth = c(9, 8),
    max          = c(10, 10),
    metric_value = c(3.5, 6.0),
    metric_score = c(10, 15),
    adj_score    = c(10, 15),
    stringsAsFactors = FALSE
  )
  
  out <- build_summarized_metrics(metrics)
  
  # We should have original number of rows + 1
  expect_equal(nrow(out), nrow(metrics) + 1L)
  
  # Column names should be preserved
  expect_equal(names(out), names(metrics))
  
  # Last row should be the IBI summary
  ibi_row <- out[nrow(out), ]
  expect_equal(ibi_row$metric_name, "IBI Score (0-50)")
  expect_equal(ibi_row$response, "Decrease")
  expect_true(is.na(ibi_row$metric_value))
  expect_true(is.na(ibi_row$metric_score))
  
  # The IBI adj_score should be the sum of all previous adj_scores
  expected_total <- sum(metrics$adj_score, na.rm = TRUE)
  expect_equal(ibi_row$adj_score, expected_total)
})

test_that("build_summarized_metrics handles NA adj_scores", {
  metrics <- data.frame(
    metric_name  = c("A", "B"),
    response     = c("Inc", "Dec"),
    min          = NA_real_,
    fifth        = NA_real_,
    ninety_fifth = NA_real_,
    max          = NA_real_,
    metric_value = c(1, 2),
    metric_score = c(5, 10),
    adj_score    = c(NA_real_, 10),
    stringsAsFactors = FALSE
  )
  
  out <- build_summarized_metrics(metrics)
  ibi_row <- out[nrow(out), ]
  
  # Should ignore NA in the sum
  expect_equal(ibi_row$adj_score, 10)
})

test_that("build_autosave_df returns NULL when there is no data", {
  # Empty snapshot
  empty_snapshot <- list()
  group_defs <- data.frame(
    section_id = character(0),
    group_id   = character(0),
    group_name = character(0),
    stringsAsFactors = FALSE
  )
  
  out <- build_autosave_df(empty_snapshot, group_defs)
  expect_null(out)
  
  # Snapshot with sections but no data
  snapshot_no_data <- list(
    section_1 = list(data = list()),
    section_2 = list(data = NULL)
  )
  
  group_defs2 <- data.frame(
    section_id = c("section_1", "section_2"),
    group_id   = c("grp1", "grp2"),
    group_name = c("Group 1", "Group 2"),
    stringsAsFactors = FALSE
  )
  
  out2 <- build_autosave_df(snapshot_no_data, group_defs2)
  expect_null(out2)
})

test_that("build_autosave_df flattens selected genera correctly", {
  # Fake group definitions: two sections, one of which has no mapping
  group_defs <- data.frame(
    section_id = c("section_1", "section_2"),
    group_id   = c("grp1", "grp2"),
    group_name = c("Group 1", "Group 2"),
    stringsAsFactors = FALSE
  )
  
  # Snapshot mimicking selected_genera()$data structure
  snapshot <- list(
    section_1 = list(
      data = list(
        list(
          id        = 1,
          taxon     = "Taxon A",
          dipnet1   = 3L,
          dipnet2   = 5L,
          tsn       = 1001L,
          parentTsn = 500L
        ),
        list(
          id        = 2,
          taxon     = "Taxon B",
          dipnet1   = 0L,
          dipnet2   = 1L,
          tsn       = 1002L,
          parentTsn = 500L
        )
      )
    ),
    # A non-module entry that should be ignored because it doesn't start with "section_"
    something_else = list(
      data = list(
        list(
          id        = 99,
          taxon     = "Should Be Ignored",
          dipnet1   = 1L,
          dipnet2   = 1L,
          tsn       = 9999L,
          parentTsn = 8888L
        )
      )
    )
  )
  
  out <- build_autosave_df(snapshot, group_defs)
  
  # Should have one row per list element in section_1$data
  expect_s3_class(out, "data.frame")
  expect_equal(nrow(out), 2L)
  
  # Required columns
  expect_true(all(c(
    "group_id", "group_name", "section_id",
    "Taxon", "Dipnet1", "Dipnet2", "tsn", "parentTsn"
  ) %in% names(out)))
  
  # Check that metadata fields were filled correctly
  expect_true(all(out$group_id == "grp1"))
  expect_true(all(out$group_name == "Group 1"))
  expect_true(all(out$section_id == "section_1"))
  
  # Check that taxon values came through as expected
  expect_equal(sort(out$Taxon), sort(c("Taxon A", "Taxon B")))
})

test_that("build_autosave_df skips sections without group_defs mapping", {
  # group_defs only includes section_1
  group_defs <- data.frame(
    section_id = "section_1",
    group_id   = "grp1",
    group_name = "Group 1",
    stringsAsFactors = FALSE
  )
  
  snapshot <- list(
    section_1 = list(
      data = list(
        list(
          id        = 1,
          taxon     = "Taxon A",
          dipnet1   = 1L,
          dipnet2   = 2L,
          tsn       = 1001L,
          parentTsn = 500L
        )
      )
    ),
    section_2 = list(
      data = list(
        list(
          id        = 2,
          taxon     = "Taxon B",
          dipnet1   = 3L,
          dipnet2   = 4L,
          tsn       = 1002L,
          parentTsn = 600L
        )
      )
    )
  )
  
  out <- build_autosave_df(snapshot, group_defs)
  
  # Only section_1 should be represented
  expect_equal(nrow(out), 1L)
  expect_equal(out$section_id, "section_1")
  expect_equal(out$Taxon, "Taxon A")
})

test_that("build_section_data_list creates the expected structure", {
  group_data <- data.frame(
    Taxon      = c("Taxon A", "Taxon B"),
    Dipnet1    = c(1L, 3L),
    Dipnet2    = c(2L, 4L),
    tsn        = c(1001L, 1002L),
    parentTsn  = c(500L, 500L),
    stringsAsFactors = FALSE
  )
  
  section_list <- build_section_data_list(group_data)
  
  # Top-level structure
  expect_type(section_list, "list")
  expect_true("data" %in% names(section_list))
  
  data_list <- section_list$data
  expect_type(data_list, "list")
  expect_length(data_list, 2L)
  
  # First row
  r1 <- data_list[[1]]
  expect_equal(r1$id, 1L)
  expect_equal(r1$taxon, "Taxon A")
  expect_equal(r1$dipnet1, 1L)
  expect_equal(r1$dipnet2, 2L)
  expect_equal(r1$tsn, 1001L)
  expect_equal(r1$parentTsn, 500L)
  
  # Second row
  r2 <- data_list[[2]]
  expect_equal(r2$id, 2L)
  expect_equal(r2$taxon, "Taxon B")
})

test_that("build_section_data_list handles empty data.frames", {
  empty_df <- data.frame(
    Taxon      = character(0),
    Dipnet1    = integer(0),
    Dipnet2    = integer(0),
    tsn        = integer(0),
    parentTsn  = integer(0),
    stringsAsFactors = FALSE
  )
  
  section_list <- build_section_data_list(empty_df)
  expect_type(section_list, "list")
  expect_true("data" %in% names(section_list))
  expect_length(section_list$data, 0L)
})

test_that("build_sections_from_autosave returns empty list for empty input", {
  autosave_df <- data.frame(
    Taxon      = character(0),
    Dipnet1    = integer(0),
    Dipnet2    = integer(0),
    tsn        = integer(0),
    parentTsn  = integer(0),
    stringsAsFactors = FALSE
  )
  
  group_defs <- data.frame(
    section_id = character(0),
    group_id   = character(0),
    group_name = character(0),
    stringsAsFactors = FALSE
  )
  
  out <- build_sections_from_autosave(autosave_df, group_defs)
  expect_type(out, "list")
  expect_length(out, 0L)
})

test_that("build_sections_from_autosave errors if taxon columns are missing", {
  autosave_df <- data.frame(
    group_id = "g1",
    section_id = "section_1",
    stringsAsFactors = FALSE
  )
  
  group_defs <- data.frame(
    section_id = "section_1",
    group_id   = "g1",
    group_name = "Group 1",
    stringsAsFactors = FALSE
  )
  
  expect_error(
    build_sections_from_autosave(autosave_df, group_defs),
    "missing required columns"
  )
})

test_that("build_sections_from_autosave (new schema) builds section data via group_defs", {
  autosave_df <- data.frame(
    group_id   = c("g1", "g1", "g2"),
    section_id = c("ignored_a", "ignored_b", "ignored_c"), # these are ignored
    Taxon      = c("Taxon A", "Taxon B", "Taxon C"),
    Dipnet1    = c(1L, 3L, 5L),
    Dipnet2    = c(2L, 4L, 6L),
    tsn        = c(101L, 102L, 103L),
    parentTsn  = c(10L, 10L, 20L),
    stringsAsFactors = FALSE
  )
  
  group_defs <- data.frame(
    section_id = c("section_1", "section_2"),
    group_id   = c("g1", "g2"),
    group_name = c("Group 1", "Group 2"),
    stringsAsFactors = FALSE
  )
  
  out <- build_sections_from_autosave(autosave_df, group_defs)
  
  # Should have one entry for section_1 (g1) and one for section_2 (g2)
  expect_type(out, "list")
  expect_setequal(names(out), c("section_1", "section_2"))
  
  # section_1: two rows
  sec1 <- out[["section_1"]]
  expect_true("data" %in% names(sec1))
  expect_length(sec1$data, 2L)
  expect_equal(
    sort(vapply(sec1$data, function(x) x$taxon, character(1))),
    sort(c("Taxon A", "Taxon B"))
  )
  
  # section_2: one row
  sec2 <- out[["section_2"]]
  expect_length(sec2$data, 1L)
  expect_equal(sec2$data[[1]]$taxon, "Taxon C")
})

test_that("build_sections_from_autosave (new schema) skips unknown group_ids", {
  autosave_df <- data.frame(
    group_id   = c("g1", "unknown"),
    section_id = c("ignored_1", "ignored_2"),
    Taxon      = c("Taxon A", "Taxon B"),
    Dipnet1    = c(1L, 2L),
    Dipnet2    = c(3L, 4L),
    tsn        = c(101L, 102L),
    parentTsn  = c(10L, 20L),
    stringsAsFactors = FALSE
  )
  
  group_defs <- data.frame(
    section_id = "section_1",
    group_id   = "g1",
    group_name = "Group 1",
    stringsAsFactors = FALSE
  )
  
  out <- build_sections_from_autosave(autosave_df, group_defs)
  
  # Only g1 should be represented
  expect_setequal(names(out), "section_1")
  expect_length(out[["section_1"]]$data, 1L)
  expect_equal(out[["section_1"]]$data[[1]]$taxon, "Taxon A")
})

test_that("build_sections_from_autosave (legacy schema) builds section data from Group column", {
  autosave_df <- data.frame(
    Group      = c("section_1", "section_1", "other_group"),
    Taxon      = c("Taxon A", "Taxon B", "Ignored Taxon"),
    Dipnet1    = c(1L, 3L, 9L),
    Dipnet2    = c(2L, 4L, 10L),
    tsn        = c(101L, 102L, 999L),
    parentTsn  = c(10L, 10L, 99L),
    stringsAsFactors = FALSE
  )
  
  group_defs <- data.frame(
    section_id = character(0),
    group_id   = character(0),
    group_name = character(0),
    stringsAsFactors = FALSE
  )
  
  out <- build_sections_from_autosave(autosave_df, group_defs)
  
  # Only section_1 should be present (other_group is not a section_* id)
  expect_setequal(names(out), "section_1")
  
  sec1 <- out[["section_1"]]
  expect_length(sec1$data, 2L)
  expect_equal(
    sort(vapply(sec1$data, function(x) x$taxon, character(1))),
    sort(c("Taxon A", "Taxon B"))
  )
})

test_that("build_sections_from_autosave errors when no grouping columns are present", {
  autosave_df <- data.frame(
    Taxon      = "Taxon A",
    Dipnet1    = 1L,
    Dipnet2    = 2L,
    tsn        = 101L,
    parentTsn  = 10L,
    stringsAsFactors = FALSE
  )
  
  group_defs <- data.frame(
    section_id = character(0),
    group_id   = character(0),
    group_name = character(0),
    stringsAsFactors = FALSE
  )
  
  expect_error(
    build_sections_from_autosave(autosave_df, group_defs),
    "missing expected grouping columns"
  )
})

test_that("selected_genera snapshot survives autosave round-trip", {
  # Fake group_defs for mapping
  group_defs <- data.frame(
    section_id = c("section_1", "section_2"),
    group_id   = c("g1", "g2"),
    group_name = c("Group 1", "Group 2"),
    stringsAsFactors = FALSE
  )
  
  # Fake selected_genera snapshot (what you'd get from reactiveValuesToList)
  original_snapshot <- list(
    section_1 = list(
      data = list(
        list(
          id        = 1,
          taxon     = "Taxon A",
          dipnet1   = 3L,
          dipnet2   = 0L,
          tsn       = 101L,
          parentTsn = 10L
        ),
        list(
          id        = 2,
          taxon     = "Taxon B",
          dipnet1   = 1L,
          dipnet2   = 2L,
          tsn       = 102L,
          parentTsn = 10L
        )
      )
    ),
    section_2 = list(
      data = list(
        list(
          id        = 1,
          taxon     = "Taxon C",
          dipnet1   = 5L,
          dipnet2   = 0L,
          tsn       = 201L,
          parentTsn = 20L
        )
      )
    )
  )
  
  # 1) Save side: flatten snapshot
  autosave_df <- build_autosave_df(original_snapshot, group_defs)
  expect_s3_class(autosave_df, "data.frame")
  expect_gt(nrow(autosave_df), 0L)
  
  # 2) Load side: rebuild section data from autosave
  reloaded_sections <- build_sections_from_autosave(autosave_df, group_defs)
  
  # We should have the same set of sections
  expect_setequal(names(reloaded_sections), names(original_snapshot))
  
  # For each section, taxa + counts should match
  for (sec in names(original_snapshot)) {
    orig_taxa <- vapply(original_snapshot[[sec]]$data, function(x) x$taxon, character(1))
    new_taxa  <- vapply(reloaded_sections[[sec]]$data, function(x) x$taxon, character(1))
    
    expect_setequal(orig_taxa, new_taxa)
    
    orig_counts <- vapply(original_snapshot[[sec]]$data, function(x) x$dipnet1, integer(1))
    new_counts  <- vapply(reloaded_sections[[sec]]$data, function(x) x$dipnet1, integer(1))
    
    expect_equal(orig_counts[order(orig_taxa)], new_counts[order(new_taxa)])
  }
})
