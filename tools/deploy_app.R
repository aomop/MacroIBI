options(renv.config.auto.activate = FALSE)
Sys.unsetenv("RENV_PROJECT")

options(repos = c(CRAN = "https://cran.r-project.org"))

getOption("repos")

rsconnect::deployApp(
  appDir        = ".",
  appName       = "MacroIBI",
  account       = "smsc2",
  server        = "shinyapps.io",
  appFiles      = "app.R"
)
