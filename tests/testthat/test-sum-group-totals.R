test_that("sum_group_totals returns 0 when not all groups present", {
  counts <- list(
    section_1 = 10,
    section_2 = 5
  )
  
  # Expecting 3 groups but only 2 present
  res <- sum_group_totals(counts, expected_groups = 3L)
  expect_equal(res, 0)
})

test_that("sum_group_totals sums values when all groups present", {
  counts <- list(
    section_1 = 10,
    section_2 = 5,
    section_3 = 2
  )
  
  res <- sum_group_totals(counts, expected_groups = 3L)
  expect_equal(res, 17)
})
