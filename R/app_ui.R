#' Internal UI builder
#' @keywords internal
macroibi_ui <- function(theme, group_list, group_colors, www_prefix) {
  bslib::page_navbar(
    theme = theme,
    title = "Wetland IBI Dashboard",
    id = "macroibi_app",
    bslib::nav_panel(
      tags$head(tags$link(rel = "stylesheet", type = "text/css", href = file.path(www_prefix, "styles.css"))),
      tags$head(
        tags$style(HTML(
          "    .shiny-notification {
      position: fixed;
      top: 50% !important;
      left: 50% !important;
      transform: translate(-50%, -50%) !important;
      font-size: 18px;
      padding: 15px;
      width: 300px;
      background: white;
      border-radius: 10px;
      box-shadow: 0px 0px 10px rgba(0, 0, 0, 0.5);
    }
  "
        ))
      ),
      title = "Data",
      value = "data_tab",
      bslib::layout_sidebar(
        sidebar = bslib::sidebar(
          width = "300px",
          selectizeInput(
            "main_taxon_select", "Select Taxon:", choices = NULL,
            options = list(
              placeholder = "Type to search for taxa...",
              create = FALSE
            )
          ),
          bslib::card(
            title = "Upload/Download",
            class = "mt-3",
            download_module_ui("download_module"),
            upload_module_ui("upload_module")
          ),
          bslib::card(
            title = "Autosave Settings",
            class = "mt-3",
            autosave_module_ui("reload_autosave")
          ),
          actionButton("clear_all", "Clear All Data", icon = icon("trash"), class = "btn-danger")
        ),
        mainPanel(
          fluidRow(
            column(12, uiOutput("display_info"))
          ),
          lapply(seq_along(group_list), function(i) {
            ui_taxon_section(
              paste0("section_", i), group_list[i], group_colors[i]
            )
          })
        )
      )
    ),
    bslib::nav_panel(
      title = "Results",
      value = "results_tab",
      h4("Total Individuals: ", textOutput("grand_total_output")),
      div(
        ui_metric_scores("metrics"),
        div(
          style = "position: relative; display: inline-block;",
          tags$span(
            "How are these calculated?",
            style = "cursor: pointer; text-decoration: underline; color: blue;"
          ),
          div(
            style = paste0(
              "display: none; position: absolute; background-color: #ffffff;",
              " border: 1px solid #ccc; padding: 10px; z-index: 100; width: 600px;"
            ),
            p(tags$em("*The corixid metric is calculated as the absolute abundance of Corixids divided by the sum
                    of the absolute abundance of all true bugs and beetles")),
            tags$img(src = file.path(www_prefix, "Picture1.png"), alt = "Metric Adjustment Equations", style = "width: 100%; max-width: 600px;")
          ),
          onmouseover = "this.querySelector('div').style.display = 'block';",
          onmouseout = "this.querySelector('div').style.display = 'none';"
        )
      ),
      div(results_download_ui("results_download"))
    )
  )
}
