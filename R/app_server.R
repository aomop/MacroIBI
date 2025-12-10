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
    
    group_defs <- build_group_defs(group_list)
    
    total_unique_taxa <- shiny::reactive({
      counts <- shiny::reactiveValuesToList(unique_taxa_counts)
      sum(purrr::map_dbl(counts, reactive_handler), na.rm = TRUE)
    })
    
    grand_total_observations <- shiny::reactive({
      counts <- shiny::reactiveValuesToList(group_totals)
      sum_group_totals(counts, expected_groups = length(group_list))
    })
    
    modal_shown <- reactiveVal(FALSE)

    shiny::observe({
      if (!modal_shown()) {
        
        # --- Optional demo banner ------------------------------------------------
        demo_banner <- NULL
        if (isTRUE(demo_mode)) {
          demo_banner <- shiny::tagList(
            shiny::p(
              shiny::strong("Demo Mode:"),
              " You're viewing a hosted demo with reduced features. ",
              "Auto-save is disabled and uploads are limited, but you can explore the workflow ",
              "using the bundled example files."
            ),
            shiny::p(
              "Install the R package from ",
              shiny::a(
                href   = "https://github.com/aomop/MacroIBI",
                target = "_blank",
                "github.com/aomop/MacroIBI"
              ),
              " to access all features."
            )
          )
        }
        
        # --- Shared intro content -----------------------------------------------
        intro_block <- shiny::tagList(
          shiny::HTML("<h2>Welcome to the Macroinvertebrate IBI Calculator!</h2>"),
          shiny::HTML(
            "<p>This Shiny app calculates the Macroinvertebrate Index of Biotic Integrity (IBI) 
        using protocols developed in collaboration with the Shakopee Mdewakanton Sioux Community.</p>"
          ),
          demo_banner
        )
        
        # --- Non-demo: full instructions + inputs -------------------------------
        autosave_instruction <- shiny::HTML(
          "<li><strong>Autosave:</strong> The autosave feature is turned off by default. 
      Enable it by clicking the checkbox on the left if you wish to use it.</li>"
        )
        
        full_body <- shiny::tagList(
          intro_block,
          shiny::HTML("<p><strong>Before You Begin:</strong></p>"),
          shiny::HTML("<ul>"),
          shiny::HTML(
            "<li><strong>Title and Date:</strong> Please provide a valid title (wetland name) and 
        sampling date to generate meaningful filenames. These details can be updated later.</li>"
          ),
          autosave_instruction,
          shiny::HTML("</ul>"),
          shiny::fluidRow(
            shiny::column(
              5,
              shiny::textInput(
                "user_title",
                label = "Wetland Name:",
                value = shared_reactives$user_title
              )
            ),
            shiny::column(
              5,
              shiny::textInput(
                "user_date",
                label = "Date of Sampling:",
                value = shared_reactives$user_date
              )
            ),
            shiny::column(
              2,
              shiny::div(style = "margin-top: 28px;")
            )
          ),
          shiny::HTML(
            "<p><i>*To enable data saving, please enter a valid title and date. 
        If you choose to continue without it, data saving will be disabled.</i></p>"
          )
        )
        
        # --- Demo-only body: intro + demo banner only ---------------------------
        demo_body <- intro_block
        
        # --- Footer depends on demo_mode ----------------------------------------
        modal_footer <- if (isTRUE(demo_mode)) {
          # Simple OK button for demo
          shiny::tagList(
            shiny::modalButton("OK")
          )
        } else {
          shiny::tagList(
            shiny::modalButton("Continue without Metadata"),
            shiny::actionButton("submit_btn", "Let's go!", class = "btn btn-primary")
          )
        }
        
        # --- Choose which body to show ------------------------------------------
        body_content <- if (isTRUE(demo_mode)) demo_body else full_body
        
        shiny::showModal(
          shiny::modalDialog(
            body_content,
            footer    = modal_footer,
            easyClose = FALSE
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
      show_out <- isTRUE(input$show_out_of_region)
      choices_df <- build_taxon_choices(taxonomy, show_out_of_region = show_out)
      
      shiny::updateSelectizeInput(
        session, "main_taxon_select",
        choices = c(NA, stats::setNames(choices_df$value, choices_df$name)),
        server  = TRUE,
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
      res <- resolve_selected_taxon(
        taxonomy          = taxonomy,
        group_list        = group_list,
        selected_taxon_data = input$main_taxon_select
      )
      
      # Only update if we got a valid result
      if (!is.null(res$taxon)) {
        selected_taxon(res)
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
      grand_total_observations = grand_total_observations,
      group_defs = group_defs
    )
    
    quality_class <- shiny::reactive({
      p <- summarize_metric_scores(metric_scores$data) %>%
        dplyr::pull(adj_score)
      
      t <- p[length(p)]
      
      dplyr::case_when(
        t >= 38 ~ "4 (Excellent)",
        t >= 28 ~ "3 (Good)",
        t >= 20 ~ "2 (Fair)",
        t >= 10 ~ "1 (Poor)",
        TRUE   ~ NA
      )
    })
    
    output$ram_quality_class <- renderText({ quality_class() })
    output$grand_total_output <- renderText({ grand_total_observations() })
    
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
