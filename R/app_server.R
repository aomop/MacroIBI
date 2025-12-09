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
      # Whether to include out-of-region taxa
      show_out <- isTRUE(input$show_out_of_region)
      
      choices_df <- taxonomy %>%
        dplyr::filter(
          !is.na(.data$taxon),
          .data$taxon != "",
          !is.na(.data$Group)
        ) %>%
        # Normalize in_region (which is currently character) to a logical
        dplyr::mutate(
          # Treat strings like "TRUE"/"True"/"T"/"1" as TRUE,
          # "FALSE"/"False"/"F"/"0" as FALSE, everything else as NA
          in_region_flag = dplyr::case_when(
            .data$in_region %in% c(TRUE, "TRUE", "True", "T", "1")   ~ TRUE,
            .data$in_region %in% c(FALSE, "FALSE", "False", "F", "0") ~ FALSE,
            TRUE ~ NA
          ),
          # For NA or weird values, default to TRUE (unknown → allowed)
          in_region_flag = dplyr::coalesce(.data$in_region_flag, TRUE)
        )
      
      # (a) Hide out-of-region taxa by default
      if (!show_out) {
        choices_df <- choices_df %>%
          dplyr::filter(.data$in_region_flag)
      }
      
      # (b) Add red caution flag for out-of-region taxa when they are shown
      choices_df <- choices_df %>%
        dplyr::mutate(
          # Red warning icon for out-of-region taxa
          caution_flag = dplyr::if_else(
            !.data$in_region_flag,
            "<span style='color:#d9534f; font-weight:bold; margin-right:4px;' title='Outside region'>&#9888;</span>",
            ""
          ),
          name = paste0(
            .data$caution_flag,
            "<strong>", .data$taxon, "</strong> ",
            "<span style='color: rgba(0, 0, 0, 0.5); font-size: 0.9em;'>",
            .data$level,
            "</span>"
          ),
          value = .data$taxon
        ) %>%
        dplyr::select(.data$value, .data$name)
      
      shiny::updateSelectizeInput(
        session, "main_taxon_select",
        choices = c(NA, stats::setNames(choices_df$value, choices_df$name)),
        server  = TRUE,
        options = list(
          render = I('{
        option: function(item, escape) {
          // item.label is the HTML label we constructed in R
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
      
      taxon_group <- taxonomy %>%
        dplyr::filter(.data$taxon == .env$selected_taxon_data) %>%
        dplyr::pull(.data$Group) %>%
        unique()
      
      taxon_info <- taxonomy %>%
        dplyr::filter(.data$taxon == .env$selected_taxon_data) %>%
        dplyr::select(dplyr::all_of(c("taxon", "tsn", "parentTsn"))) %>%
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
      grand_total_observations = grand_total_observations,
      group_defs = group_defs
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
