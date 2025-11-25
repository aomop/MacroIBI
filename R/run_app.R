#' Launch the MacroIBI Shiny application
#'
#' This function initializes package paths, loads bundled data, and returns the
#' MacroIBI Shiny application object.
#'
#' @return A `shiny.appobj` that can be passed to `shiny::runApp()`.
#' @examples
#' if (interactive()) {
#'   run_macroibi()
#' }
#' @import shiny
#' @importFrom bslib bs_theme
#' @importFrom viridis viridis
#' @export
run_macroibi <- function() {
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
    ui = macroibi_ui(theme, group_list, group_colors, get_app_path("www_prefix")),
    server = macroibi_server(taxonomy, group_list)
  )
}

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

#' Internal server
#' @keywords internal
macroibi_server <- function(taxonomy, group_list) {
  function(input, output, session) {
    group_totals <- reactiveValues()
    unique_taxa_counts <- reactiveValues()
    selected_taxon <- reactiveVal(list(taxon = NULL, section_id = NULL, tsn = NULL, parentTsn = NULL))
    selected_genera <- reactiveValues(data = list())
    toggle_state <- reactiveVal(TRUE)
    shared_reactives <- reactiveValues(user_title = NULL, user_date = NULL, server_update = TRUE)

    total_unique_taxa <- shiny::reactive({
      counts <- shiny::reactiveValuesToList(unique_taxa_counts)
      sum(purrr::map_dbl(counts, reactive_handler), na.rm = TRUE)
    })

    grand_total_observations <- shiny::reactive({
      counts <- shiny::reactiveValuesToList(group_totals)
      if (length(counts) < length(group_list)) return(0)
      sum(purrr::map_dbl(counts, reactive_handler), na.rm = TRUE)
    })

    modal_shown <- reactiveVal(FALSE)

    shiny::observe({
      if (!modal_shown()) {
        showModal(
          modalDialog(
            HTML(
              "<h2>Welcome to the Macroinvertebrate IBI Calculator!</h2>
            <p>This Shiny app calculates the Macroinvertebrate Index of Biotic Integrity (IBI) using protocols developed in collaboration with the Shakopee Mdewakanton Sioux Community.</p>
            <p><strong>Before You Begin:</strong></p>
            <ul>
            <li><strong>Title and Date:</strong> Please provide a valid title (wetland name) and sampling date to generate meaningful filenames. These details can be updated later.</li>
            <li><strong>Autosave:</strong> The autosave feature is turned off by default. Enable it by clicking the checkbox on the left if you wish to use it.</li>
            </ul>"
            ),
            fluidRow(
              column(5, textInput("user_title", label = "Wetland Name:", value = shared_reactives$user_title)),
              column(5, textInput("user_date", label = "Date of Sampling:", value = shared_reactives$user_date)),
              column(2, div(
                style = "margin-top: 28px;"
              ))
            ),
            HTML("<p><i>*To enable data saving, please enter a valid title and date. If you choose to continue without it, data saving will be disabled
          </i></p>"),
            footer = tagList(
              modalButton("Continue without Metadata"),
              actionButton("submit_btn", "Let's go!", class = "btn btn-primary")
            )
          )
        )
        modal_shown(TRUE)
      }
    })

    observe({
      if (!is.null(shared_reactives$user_title)) {
        updateTextInput(session, "user_title", value = shared_reactives$user_title)
      }
    })

    observe({
      if (!is.null(shared_reactives$user_date)) {
        updateTextInput(session, "user_date", value = shared_reactives$user_date)
      }
    })

    observeEvent(input$clear_all, {
      showModal(modalDialog(
        title = strong("Confirm Reset"),
        HTML(
          "
      <p>Are you sure you wish to clear all data? This will completely re-load the app state.
      <p><strong>All unsaved data will be lost.
      <span style='color: red;'>This cannot be undone.</span> </strong> </p>
      <p><em>Autosaved files will remain cached.</em></p>
    "
        ),
        footer = tagList(
          modalButton("Cancel"),
          actionButton("confirm_clear", "Yes, clear all data", class = "btn-danger")
        )
      ))
    })

    observeEvent(input$confirm_clear, {
      removeModal()
      session$reload()
    })

    observe({
      all_choices <- taxonomy |>
        dplyr::filter(!is.na(.data$taxon), .data$taxon != "") |>
        dplyr::mutate(
          name = paste0(
            "<strong>", .data$taxon, "</strong> <span style='color: rgba(0, 0, 0, 0.5); font-size: 0.9em;'>", .data$level, "</span>"
          ),
          value = .data$taxon
        ) |>
        dplyr::select(dplyr::all_of(c("value", "name")))

      shiny::updateSelectizeInput(
        session, "main_taxon_select",
        choices = c(NA, setNames(all_choices$value, all_choices$name)),
        server = TRUE,
        options = list(
          render = I('{
          option: function(item, escape) {
            return "<div>" + item.label + "</div>";
          },
          item: function(item, escape) {
            return "<div>" + escape(item.value) + "</div>";
          }
        }')
        )
      )
    })

    shiny::observeEvent(input$main_taxon_select, {
      selected_taxon_data <- input$main_taxon_select

      taxon_group <- taxonomy |>
        dplyr::filter(.data$taxon == .env$selected_taxon_data) |>
        dplyr::pull(.data$Group) |>
        unique()

      taxon_info <- taxonomy |>
        dplyr::filter(.data$taxon == .env$selected_taxon_data) |>
        dplyr::select(dplyr::all_of(c("taxon", "tsn", "parentTsn"))) |>
        unique()

      if (length(taxon_group) == 1) {
        section_id <- paste0("section_", which(group_list == taxon_group))
        selected_taxon(list(taxon = selected_taxon_data,
          section_id = section_id,
          tsn = taxon_info$tsn,
          parentTsn = taxon_info$parentTsn))
      }
    })

    observeEvent(input$submit_btn, {
      req(input$user_title, input$user_date)

      validate(
        need(nzchar(input$user_title), message = "Please enter a valid title."),
        need(nzchar(input$user_date), message = "Please enter a valid date.")
      )

      shared_reactives$server_update <- FALSE
      shared_reactives$user_title <- input$user_title
      shared_reactives$user_date <- input$user_date

      toggle_state(FALSE)

      removeModal()
    })

    observeEvent(input$edit_btn, {
      toggle_state(TRUE)
    })

    observe({
      if (shared_reactives$server_update &&
        !is.null(shared_reactives$user_title) &&
          !is.null(shared_reactives$user_date)) {
        message(paste("Auto-updated title to", shared_reactives$user_title))
        message(paste("Auto-updated date to", shared_reactives$user_date))
      }
    })

    output$display_info <- renderUI({
      if (!toggle_state()) {
        fluidPage(
          fluidRow(
            column(10, tags$h3(style = "font-weight: bold;", paste("Wetland Name:", shared_reactives$user_title))),
            column(2, actionButton("edit_btn", "Edit", class = "btn btn-primary"))
          ),
          fluidRow(
            column(10, tags$h5(style = "color: gray;", paste("Sampled:", shared_reactives$user_date)))
          )
        )
      } else {
        fluidRow(
          column(5, textInput("user_title", label = "Wetland Name:", value = shared_reactives$user_title)),
          column(5, textInput("user_date", label = "Date of Sampling:", value = shared_reactives$user_date)),
          column(2, div(
            style = "margin-top: 28px;",
            actionButton("submit_btn", "Submit", class = "btn btn-primary")
          ))
        )
      }
    })

    output$grand_total_output <- renderText({ grand_total_observations() })

    lapply(seq_along(group_list), function(i) {
      module_id <- paste0("section_", i)
      group_results <- server_taxon_section(
        id = module_id,
        group_name = group_list[i],
        taxonomy_data = taxonomy,
        selected_taxon = selected_taxon,
        total_unique_taxa = total_unique_taxa
      )
      selected_genera[[module_id]] <- group_results$selected_genera
      unique_taxa_counts[[module_id]] <- group_results$unique_taxa
      group_totals[[module_id]] <- group_results$group_sum_count
    })

    metric_scores <- server_metrics(
      id = "metrics",
      selected_genera = selected_genera,
      taxonomy = taxonomy,
      unique_taxa_counts = unique_taxa_counts,
      group_totals = group_totals,
      grand_total_observations = grand_total_observations
    )

    download_module_server(
      "download_module",
      selected_genera,
      shared_reactives
    )

    autosave_module_server(
      id = "reload_autosave",
      auto_save_path = get_app_path("autosave_dir"),
      metric_save_path = get_app_path("metric_autosave_dir"),
      selected_genera = selected_genera,
      shared_reactives = shared_reactives,
      metric_scores = metric_scores,
      auto_save_interval = 30
    )

    upload_module_server(
      id = "upload_module",
      taxonomy = taxonomy,
      selected_genera = selected_genera,
      shared_reactives = shared_reactives,
      toggle_state = toggle_state
    )

    results_download_server(
      id = "results_download",
      metric_scores = metric_scores,
      shared_reactives = shared_reactives,
      selected_genera = selected_genera,
      taxonomy = taxonomy,
      metric_save_path = get_app_path("metric_autosave_dir")
    )
  }
}
