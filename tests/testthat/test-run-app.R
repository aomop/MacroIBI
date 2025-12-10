test_that("run_macroibi returns a shiny app (demo mode)", {
  app <- run_macroibi(demo_mode = TRUE)
  expect_s3_class(app, "shiny.appobj")
})

test_that("user data directories are initialized", {
  macroibi:::init_app_state()
  data_dir <- tools::R_user_dir("macroibi", which = "data")
  expect_true(dir.exists(file.path(data_dir, "auto_saves")))
  expect_true(dir.exists(file.path(data_dir, "metric_autosaves")))
})
