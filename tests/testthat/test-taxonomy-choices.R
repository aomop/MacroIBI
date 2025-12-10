test_that("build_taxon_choices hides out-of-region taxa by default", {
  taxonomy <- test_taxonomy_two_groups()
  taxonomy$in_region <- c("TRUE", "FALSE")  # mark second taxon as out-of-region
  
  choices <- build_taxon_choices(taxonomy, show_out_of_region = FALSE)
  
  # Should only include the in-region taxon
  expect_equal(choices$value, "Aeshna")
  expect_equal(length(choices$name), 1L)
  expect_false(grepl("&#9888;", choices$name))  # no warning icon
})

test_that("build_taxon_choices can show out-of-region taxa with warnings", {
  taxonomy <- test_taxonomy_two_groups()
  taxonomy$in_region <- c("TRUE", "FALSE")
  
  choices <- build_taxon_choices(taxonomy, show_out_of_region = TRUE)
  
  # Both taxa present
  expect_equal(sort(choices$value), c("Aeshna", "Bithynia"))
  
  # At least one name includes the warning icon (for out-of-region)
  expect_true(any(grepl("&#9888;", choices$name)))
})
