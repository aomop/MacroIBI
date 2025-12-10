test_that("calculate_metric_score computes scaled scores correctly", {
  values <- c(0, 5, 10)
  mins   <- c(0, 0, 0)
  maxs   <- c(10, 10, 10)
  
  # Forward scaling
  scores <- calculate_metric_score(values, mins, maxs, scale_factor = 10, inverse = FALSE)
  expect_equal(scores, c(0, 5, 10))
  
  # Inverse scaling
  inv_scores <- calculate_metric_score(values, mins, maxs, scale_factor = 10, inverse = TRUE)
  expect_equal(inv_scores, c(10, 5, 0))
})

test_that("calculate_metric_score returns NA for NA or zero-range", {
  values <- c(1, NA, 5)
  mins   <- c(0, 0, 5)
  maxs   <- c(0, 10, 5)  # zero range for 1st and 3rd
  
  scores <- calculate_metric_score(values, mins, maxs, scale_factor = 10, inverse = FALSE)
  expect_true(all(is.na(scores)))
})

test_that("calculate_corixids_ratio computes ratio of Corixidae counts", {
  # Taxonomy with Corixidae and another family
  taxonomy <- data.frame(
    Family = c("Corixidae", "Corixidae", "NotCorixidae"),
    taxon  = c("Corixa1", "Corixa2", "Other"),
    stringsAsFactors = FALSE
  )
  
  # group_totals: simple functions as stand-ins for reactives
  group_totals <- list(
    section_2 = function() 20,  # beetles
    section_4 = function() 30   # bugs
  )
  
  # selected_genera for section_4: list of rows with dipnet counts
  selected_genera <- list(
    section_4 = function() {
      list(
        data = list(
          list(taxon = "Corixa1", dipnet1 = 1, dipnet2 = 1),   # 2
          list(taxon = "Corixa2", dipnet1 = 2, dipnet2 = 0),   # 2
          list(taxon = "Other",   dipnet1 = 100, dipnet2 = 100) # ignored
        )
      )
    }
  )
  
  # Numerator = 4; denominator = 20 + 30 = 50 → 0.08
  ratio <- calculate_corixids_ratio(selected_genera, group_totals, taxonomy)
  expect_equal(ratio, 4 / 50)
})

test_that("calculate_corixids_ratio returns 0 when no beetles/bugs", {
  taxonomy <- data.frame(
    Family = "Corixidae",
    taxon  = "Corixa1",
    stringsAsFactors = FALSE
  )
  
  group_totals <- list(
    section_2 = function() 0,
    section_4 = function() 0
  )
  
  selected_genera <- list(
    section_4 = function() {
      list(data = list())
    }
  )
  
  ratio <- calculate_corixids_ratio(selected_genera, group_totals, taxonomy)
  expect_equal(ratio, 0)
})

test_that("calculate_corixids_ratio is robust to errors in selected data", {
  taxonomy <- data.frame(
    Family = "Corixidae",
    taxon  = "Corixa1",
    stringsAsFactors = FALSE
  )
  
  group_totals <- list(
    section_2 = function() 10,
    section_4 = function() 10
  )
  
  # selected_genera[[\"section_4\"]] throws when called
  selected_genera <- list(
    section_4 = function() stop("boom")
  )
  
  ratio <- calculate_corixids_ratio(selected_genera, group_totals, taxonomy)
  expect_equal(ratio, 0)
})
