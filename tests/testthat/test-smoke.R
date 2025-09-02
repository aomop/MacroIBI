test_that("app file exists and is readable", {
expect_true(file.exists("app.R"))
})
test_that("taxonomy data file exists", {
expect_true(file.exists("taxonomy.rds"))
})
test_that("Shiny app loads (headless)", {
skip_if_not_installed("shinytest2")
app <- shinytest2::AppDriver$new(".", load_timeout = 10000)
app$stop()
})
