# tests/testthat/test-helpers-taxon-section.R

testthat::test_that("compute_unique_taxa returns 0 for empty or unmatched input", {
  taxonomy_data <- data.frame(
    taxon       = c("OrderA", "GenusA", "SpeciesA"),
    Order       = c("O1", "O1", "O1"),
    Suborder    = c(NA, NA, NA),
    Superfamily = c(NA, NA, NA),
    Family      = c(NA, "F1", "F1"),
    Genus       = c(NA, "GenusA", "GenusA"),
    Species     = c(NA, NA, "SpeciesA"),
    stringsAsFactors = FALSE
  )
  
  # No taxa
  res_empty <- compute_unique_taxa(character(), taxonomy_data)
  testthat::expect_identical(res_empty, 0L)
  
  # Selected taxon not in taxonomy_data
  res_unmatched <- compute_unique_taxa("NotInTaxonomy", taxonomy_data)
  testthat::expect_identical(res_unmatched, 0L)
})

testthat::test_that("compute_unique_taxa counts a single selected taxon as 1", {
  taxonomy_data <- data.frame(
    taxon       = c("OrderA", "GenusA", "SpeciesA"),
    Order       = c("O1", "O1", "O1"),
    Suborder    = c(NA, NA, NA),
    Superfamily = c(NA, NA, NA),
    Family      = c(NA, "F1", "F1"),
    Genus       = c(NA, "GenusA", "GenusA"),
    Species     = c(NA, NA, "SpeciesA"),
    stringsAsFactors = FALSE
  )
  
  res <- compute_unique_taxa("GenusA", taxonomy_data)
  testthat::expect_identical(res, 1L)
})

testthat::test_that("compute_unique_taxa drops parent when child is also selected", {
  taxonomy_data <- data.frame(
    taxon       = c("OrderA", "GenusA", "SpeciesA"),
    Order       = c("O1", "O1", "O1"),
    Suborder    = c(NA, NA, NA),
    Superfamily = c(NA, NA, NA),
    Family      = c(NA, "F1", "F1"),
    Genus       = c(NA, "GenusA", "GenusA"),
    Species     = c(NA, NA, "SpeciesA"),
    stringsAsFactors = FALSE
  )
  
  # Order + Species selected → only Species should be counted as unique
  res_order_species <- compute_unique_taxa(
    c("OrderA", "SpeciesA"),
    taxonomy_data
  )
  testthat::expect_identical(res_order_species, 1L)
  
  # Genus + Species selected → only Species counted
  res_genus_species <- compute_unique_taxa(
    c("GenusA", "SpeciesA"),
    taxonomy_data
  )
  testthat::expect_identical(res_genus_species, 1L)
  
  # Order + Genus + Species selected → still only 1 unique (Species)
  res_all <- compute_unique_taxa(
    c("OrderA", "GenusA", "SpeciesA"),
    taxonomy_data
  )
  testthat::expect_identical(res_all, 1L)
})

testthat::test_that("compute_unique_taxa counts independent lineages separately", {
  taxonomy_data <- data.frame(
    taxon       = c("OrderA", "GenusA1", "SpeciesA1",
                    "OrderB", "GenusB1", "SpeciesB1"),
    Order       = c("O1", "O1", "O1", "O2", "O2", "O2"),
    Suborder    = c(NA, NA, NA, NA, NA, NA),
    Superfamily = c(NA, NA, NA, NA, NA, NA),
    Family      = c(NA, "F1", "F1", NA, "F2", "F2"),
    Genus       = c(NA, "GenusA1", "GenusA1", NA, "GenusB1", "GenusB1"),
    Species     = c(NA, NA, "SpeciesA1", NA, NA, "SpeciesB1"),
    stringsAsFactors = FALSE
  )
  
  # One species from each order → 2 unique taxa
  res <- compute_unique_taxa(
    c("SpeciesA1", "SpeciesB1"),
    taxonomy_data
  )
  testthat::expect_identical(res, 2L)
  
  # OrderA + SpeciesB1 → both are unique (different lineages)
  res_mixed <- compute_unique_taxa(
    c("OrderA", "SpeciesB1"),
    taxonomy_data
  )
  testthat::expect_identical(res_mixed, 2L)
})

testthat::test_that("compute_unique_taxa ignores duplicates in selected_taxa", {
  taxonomy_data <- data.frame(
    taxon       = c("GenusA", "SpeciesA"),
    Order       = c("O1", "O1"),
    Suborder    = c(NA, NA),
    Superfamily = c(NA, NA),
    Family      = c("F1", "F1"),
    Genus       = c("GenusA", "GenusA"),
    Species     = c(NA, "SpeciesA"),
    stringsAsFactors = FALSE
  )
  
  # Duplicate selections should not change the unique count
  res <- compute_unique_taxa(
    c("SpeciesA", "SpeciesA", "GenusA"),
    taxonomy_data
  )
  testthat::expect_identical(res, 1L)
})
