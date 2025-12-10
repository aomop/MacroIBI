# tests/testthat/test-taxonomic-tree-helpers.R

testthat::test_that("collect_lineage walks up parent chain and stops safely", {
  tax_tbl <- data.frame(
    tsn       = c("1", "2", "3", "4"),
    parentTsn = c(NA, "1", "2", "2"),
    stringsAsFactors = FALSE
  )
  
  # Simple linear chain 3 -> 2 -> 1
  lin <- collect_lineage("3", tax_tbl)
  testthat::expect_equal(lin, c("3", "2", "1"))
  
  # Node with sibling: 4 -> 2 -> 1
  lin2 <- collect_lineage("4", tax_tbl)
  testthat::expect_equal(lin2, c("4", "2", "1"))
  
  # Missing start TSN: returns just the start (no parents found)
  lin_missing <- collect_lineage("999", tax_tbl)
  testthat::expect_equal(lin_missing, "999")
  
  # Cycle should not infinite-loop
  tax_cycle <- data.frame(
    tsn       = c("10", "20"),
    parentTsn = c("20", "10"),
    stringsAsFactors = FALSE
  )
  lin_cycle <- collect_lineage("10", tax_cycle)
  # Order might be 10,20 or 10,20,10 before unique(), but unique() trims it
  testthat::expect_true(all(lin_cycle %in% c("10", "20")))
})

testthat::test_that("build_tree_nodes returns selected nodes and ancestors", {
  tax_tbl <- data.frame(
    tsn       = c("1", "2", "3", "4", "5"),
    parentTsn = c(NA, "1", "2", "2", "3"),
    taxon     = c("Root", "Child A", "Child B", "Leaf 1", "Leaf 2"),
    level     = c("Order", "Family", "Genus", "Species", "Species"),
    stringsAsFactors = FALSE
  )
  
  sel <- c("4", "5")  # two leaves
  
  nodes <- build_tree_nodes(sel, tax_tbl)
  
  # Should include selected leaves and ancestors up to the root
  testthat::expect_true(all(sel %in% nodes$tsn))
  testthat::expect_true(all(c("1", "2", "3") %in% nodes$tsn))
  
  # selected flag correct
  sel_flag <- nodes$selected[nodes$tsn %in% sel]
  testthat::expect_true(all(sel_flag))
  
  # Non-selected ancestors are not marked selected
  anc_flag <- nodes$selected[nodes$tsn %in% c("1", "2", "3")]
  testthat::expect_true(all(!anc_flag))
  
  # Levels should be a subset of rank_levels
  testthat::expect_true(all(nodes$level %in% rank_levels))
})

testthat::test_that("layout_tree returns NULL when insufficient data", {
  # No nodes
  nodes_empty <- data.frame(
    tsn       = character(),
    parentTsn = character(),
    taxon     = character(),
    level     = character(),
    selected  = logical(),
    stringsAsFactors = FALSE
  )
  
  lay_empty <- layout_tree(nodes_empty)
  testthat::expect_null(lay_empty)
  
  # One selected taxon only
  nodes_one <- data.frame(
    tsn        = "1",
    parentTsn  = NA_character_,
    taxon      = "Root",
    level      = "Order",
    selected   = TRUE,
    stringsAsFactors = FALSE
  )
  
  lay_one <- layout_tree(nodes_one)
  testthat::expect_null(lay_one)
})

testthat::test_that("layout_tree computes coordinates and edges for a simple tree", {
  tax_tbl <- data.frame(
    tsn       = c("1",  "2",   "3"),
    parentTsn = c(NA,   "1",   "2"),
    taxon     = c("Root", "Mid", "Leaf"),
    level     = c("Order", "Family", "Species"),
    stringsAsFactors = FALSE
  )
  
  nodes <- build_tree_nodes("3", tax_tbl)
  # Ensure we mark at least two selected taxa for layout to succeed
  nodes$selected[nodes$tsn %in% c("2", "3")] <- TRUE
  
  lay <- layout_tree(nodes)
  
  testthat::expect_type(lay, "list")
  testthat::expect_true(all(c("nodes", "edges", "coords", "leaf_tsns") %in% names(lay)))
  
  # Coords must have x/y and same TSNs as nodes
  testthat::expect_true(all(c("tsn", "x", "y") %in% names(lay$coords)))
  testthat::expect_equal(sort(lay$coords$tsn), sort(lay$nodes$tsn))
  
  # y for the selected leaves should not be NA
  testthat::expect_true(all(!is.na(lay$coords$y[lay$coords$tsn %in% lay$leaf_tsns])))
  
  # Edges parent/child are present and consistent with nodes
  testthat::expect_true(all(lay$edges$parent %in% lay$nodes$tsn))
  testthat::expect_true(all(lay$edges$child  %in% lay$nodes$tsn))
})

testthat::test_that("draw_tree_base runs without error given a valid layout", {
  tax_tbl <- data.frame(
    tsn       = c("1",  "2",   "3"),
    parentTsn = c(NA,   "1",   "2"),
    taxon     = c("Root", "Mid", "Leaf"),
    level     = c("Order", "Family", "Species"),
    stringsAsFactors = FALSE
  )
  
  nodes <- build_tree_nodes(c("2", "3"), tax_tbl)
  lay   <- layout_tree(nodes)
  
  tmp <- tempfile(fileext = ".png")
  
  grDevices::png(tmp, width = 800, height = 600, res = 96)
  testthat::expect_silent(
    draw_tree_base(lay)
  )
  grDevices::dev.off()
  
  testthat::expect_true(file.exists(tmp))
})
