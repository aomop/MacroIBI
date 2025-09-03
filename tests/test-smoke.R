# tests/testthat/test-smoke.R
test_that("app file exists and is readable", {
  root <- {
    cands <- unique(c(".", "..", "../..", Sys.getenv("GITHUB_WORKSPACE", "")))
    hit <- which(vapply(cands, function(p) file.exists(file.path(p, "app.R")), logical(1)))
    if (length(hit)) normalizePath(cands[hit[1]], winslash = "/", mustWork = FALSE) else getwd()
  }
  expect_true(file.exists(file.path(root, "app.R")))
})

test_that("taxonomy data file exists", {
  root <- {
    cands <- unique(c(".", "..", "../..", Sys.getenv("GITHUB_WORKSPACE", "")))
    hit <- which(vapply(cands, function(p) file.exists(file.path(p, "taxonomy.rds")), logical(1)))
    if (length(hit)) normalizePath(cands[hit[1]], winslash = "/", mustWork = FALSE) else getwd()
  }
  expect_true(file.exists(file.path(root, "taxonomy.rds")))
})

test_that("Shiny app loads (headless)", {
  skip_if_not_installed("shinytest2")
  root <- {
    cands <- unique(c(".", "..", "../..", Sys.getenv("GITHUB_WORKSPACE", "")))
    hit <- which(vapply(cands, function(p) file.exists(file.path(p, "app.R")), logical(1)))
    if (length(hit)) normalizePath(cands[hit[1]], winslash = "/", mustWork = FALSE) else getwd()
  }
  app <- shinytest2::AppDriver$new(root, load_timeout = 10000)
  app$stop()
})
