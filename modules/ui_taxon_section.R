# --------------------------------------------------------------------
# UI Module for the Taxon Section
# --------------------------------------------------------------------
# This is a paired module with server_taxon_section; sometimes it is helpful
# to separate module functionalities when one becomes too complex.
# Generates a styled container for displaying the individual group tables.
ui_taxon_section <- function(id, group_name, color) {
  # Namespace function to ensure unique IDs within the module
  ns <- shiny::NS(id)

  # Return the UI elements for the taxon section
  shiny::tagList(
    shiny::tags$div(
      # Apply custom styling to the section container
      style = paste(
        "background-color:", color,           # Background color for the section
        "; padding: 5px; margin-bottom: 5px;",# Padding and margin for spacing
        "border-radius: 5px;"                 # Rounded corners for aesthetics
      ),
      
      # Group name displayed as a header
      shiny::h3(group_name),
      
      # Header row for the taxon table
      shiny::fluidRow(
        shiny::column(3, shiny::strong("Taxon")),           # Column for taxon name
        shiny::column(3, shiny::strong("Dipnet 1 Count")),  # Column for first dipnet count
        shiny::column(3, shiny::strong("Dipnet 2 Count")),  # Column for second dipnet count
        shiny::column(2, shiny::strong("Sum Count")),       # Column for the sum of counts
        shiny::column(1, shiny::strong("Actions"))          # Column for actions (e.g., delete button)
      ),

      # Placeholder for dynamically generated rows in the taxon table
      shiny::uiOutput(ns("group_table_ui"))
      # Button to toggle the display of the taxonomic tree
    )
  )
}
