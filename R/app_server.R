#' Internal server
#' @param taxonomy Taxonomy data frame.
#' @param group_list List of taxonomic groups.
#' @keywords internal
#' @import rlang
#' @import shinycssloaders
macroibi_server <- function(taxonomy, group_list, demo_mode = FALSE) {
  function(input, output, session) {
    group_totals <- reactiveValues()
    unique_taxa_counts <- reactiveValues()
    selected_taxon <- reactiveVal(list(taxon = NULL, section_id = NULL, tsn = NULL, parentTsn = NULL))
    selected_genera <- reactiveValues(data = list())
    toggle_state <- reactiveVal(TRUE)
    shared_reactives <- reactiveValues(user_title = NULL, user_date = NULL, server_update = TRUE)
    
    # --------------------------------------------------------------------
    # Stable mapping from section IDs to group IDs/names
    # --------------------------------------------------------------------
    sanitize_group_id <- function(x) {
      x <- tolower(x)
      x <- gsub("[^a-z0-9]+", "_", x)
      x <- gsub("^_+|_+$", "", x)
      x
    }
    
    group_defs <- data.frame(
      section_id = paste0("section_", seq_along(group_list)),
      group_id   = sanitize_group_id(group_list),
      group_name = group_list,
      stringsAsFactors = FALSE
    )
    
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
        demo_banner <- ""
        if (isTRUE(demo_mode)) {
          demo_banner <- "<p><strong>Demo Mode:</strong> Auto-save is disabled for this hosted preview. Use the provided auto-saved examples to explore the workflow.</p>"
        }
        autosave_instruction <- "<li><strong>Autosave:</strong> The autosave feature is turned off by default. Enable it by clicking the checkbox on the left if you wish to use it.</li>"
        if (isTRUE(demo_mode)) {
          autosave_instruction <- "<li><strong>Autosave:</strong> Auto-save is disabled in this demo. Use the Load Auto-Save button to explore sample datasets.</li>"
        }

        showModal(
          modalDialog(
            HTML(paste0(
              "<h2>Welcome to the Macroinvertebrate IBI Calculator!</h2>",
              demo_banner,
              "<p>This Shiny app calculates the Macroinvertebrate Index of Biotic Integrity (IBI) using protocols developed in collaboration with the Shakopee Mdewakanton Sioux Community.</p>",
              "<p><strong>Before You Begin:</strong></p>",
              "<ul>",
              "<li><strong>Title and Date:</strong> Please provide a valid title (wetland name) and sampling date to generate meaningful filenames. These details can be updated later.</li>",
              autosave_instruction,
              "</ul>"
            )),
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
      selected_genera = selected_genera,
      shared_reactives = shared_reactives,
      group_defs = group_defs
    )
    
    autosave_module_server(
      id = "reload_autosave",
      auto_save_path = get_app_path("autosave_dir"),
      metric_save_path = get_app_path("metric_autosave_dir"),
      selected_genera = selected_genera,
      shared_reactives = shared_reactives,
      metric_scores = metric_scores,
      auto_save_interval = 30,
      group_defs = group_defs,
      demo_mode = demo_mode
    )
    
    upload_module_server(
      id = "upload_module",
      taxonomy = taxonomy,
      selected_genera = selected_genera,
      shared_reactives = shared_reactives,
      toggle_state = toggle_state,
      group_defs = group_defs
    )
    
    results_download_server(
      id = "results_download",
      metric_scores = metric_scores,
      shared_reactives = shared_reactives,
      selected_genera = selected_genera,
      taxonomy = taxonomy,
      metric_save_path = get_app_path("metric_autosave_dir"),
      group_defs = group_defs
    )
  }
}
