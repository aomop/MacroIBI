#' Launch the MacroIBI Shiny application
#'
#' This function initializes package paths, loads bundled data, and returns the
#' MacroIBI Shiny application object.
#'
#' @return A `shiny.appobj` that can be passed to `shiny::runApp()`.
#' @param demo_mode Logical; if `TRUE`, runs the app in demo mode with sample data.
#' @examples
#' if (interactive()) {
#'   run_macroibi()
#' }
#' @import shiny
#' @importFrom bslib bs_theme
#' @importFrom viridis viridis
#' @export
run_macroibi <- function(demo_mode = FALSE) {
  init_app_state()

  shiny::addResourcePath(get_app_path("www_prefix"), get_app_path("www_path"))

  taxonomy <- load_taxonomy()
  group_list <- levels(taxonomy$Group)
  group_colors <- viridis::viridis(length(group_list), alpha = 0.5)
  theme <- bslib::bs_theme(
    version = 5,
    bootswatch = "sandstone"
  )

  shiny::shinyApp(
    ui = macroibi_ui(theme, group_list, group_colors, get_app_path("www_prefix"), demo_mode),
    server = macroibi_server(taxonomy, group_list, demo_mode)
  )
}
