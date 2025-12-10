# tests/testthat/helper-testdata.R
# This file is automatically sourced by testthat before running tests.

# Minimal 2-taxon taxonomy for testing
test_taxonomy_two_groups <- function() {
  data.frame(
    taxon     = c("Aeshna", "Bithynia"),
    Group     = c(
      "Dragonflies, Mayflies, Damselflies, and Caddisflies - EOT Orders",
      "Snails - Class Gastropoda"
    ),
    in_region = c("True", "True"),
    level     = c("Genus", "Genus"),
    tsn       = c(1001L, 2002L),
    parentTsn = c(10L, 20L),
    stringsAsFactors = FALSE
  )
}

# Ambiguous-group taxonomy for resolving test
test_taxonomy_ambiguous_group <- function() {
  data.frame(
    taxon     = c("Aeshna", "Aeshna"),
    Group     = c("Group A", "Group B"),
    in_region = c("True", "True"),
    level     = c("Genus", "Genus"),
    tsn       = c(1001L, 1001L),
    parentTsn = c(10L, 10L),
    stringsAsFactors = FALSE
  )
}
