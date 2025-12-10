test_that("resolve_selected_taxon returns correct mapping for a valid taxon", {
  taxonomy   <- test_taxonomy_two_groups()
  group_list <- unique(taxonomy$Group)
  
  res <- resolve_selected_taxon(taxonomy, group_list, "Aeshna")
  
  expect_equal(res$taxon, "Aeshna")
  expect_equal(res$section_id, "section_1")  # first group in group_list
  expect_equal(res$tsn, 1001L)
  expect_equal(res$parentTsn, 10L)
})

test_that("resolve_selected_taxon returns NULLs for unknown taxon", {
  taxonomy   <- test_taxonomy_two_groups()
  group_list <- unique(taxonomy$Group)
  
  res <- resolve_selected_taxon(taxonomy, group_list, "NotATaxon")
  
  expect_null(res$taxon)
  expect_null(res$section_id)
  expect_null(res$tsn)
  expect_null(res$parentTsn)
})

test_that("resolve_selected_taxon returns NULLs for ambiguous group", {
  taxonomy   <- test_taxonomy_ambiguous_group()
  group_list <- unique(taxonomy$Group)
  
  res <- resolve_selected_taxon(taxonomy, group_list, "Aeshna")
  
  expect_null(res$taxon)
  expect_null(res$section_id)
  expect_null(res$tsn)
  expect_null(res$parentTsn)
})
