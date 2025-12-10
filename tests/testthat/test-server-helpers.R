test_that("sanitize_group_id normalizes group names predictably", {
  input <- c(
    "Dragonflies, Mayflies, Damselflies, and Caddisflies - EOT Orders",
    " Snails - Class Gastropoda ",
    "Other  Non-Insect   Invertebrates!!!"
  )
  
  out <- sanitize_group_id(input)
  
  expect_equal(
    out,
    c(
      "dragonflies_mayflies_damselflies_and_caddisflies_eot_orders",
      "snails_class_gastropoda",
      "other_non_insect_invertebrates"
    )
  )
})

test_that("build_group_defs produces a stable mapping", {
  groups <- c("Group A", "Group B")
  defs <- build_group_defs(groups)
  
  expect_equal(defs$section_id, c("section_1", "section_2"))
  expect_equal(defs$group_id, c("group_a", "group_b"))
  expect_equal(defs$group_name, groups)
})
