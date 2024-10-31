library(shiny)
library(dplyr)
library(base64enc)
library(reactable)

final_data <- readRDS("data-raw//data_2.rds")
unique_species_german <- unique(final_data$species_german)

ui <- fluidPage(
  titlePanel("Mushroom Learning App"),
  sidebarLayout(
    sidebarPanel(
      reactableOutput("species_table"),  # Table for selecting species
      br(),
      textOutput("feedback")
    ),
    mainPanel(
      uiOutput("mushroom_images")  # Display images in tabs for each species
    )
  )
)

server <- function(input, output, session) {
  # Load data
  # final_data <- readRDS("raw-data/data_2.rds")
  # unique_species_german <- unique(final_data$species_german)
  #
  # Convert the species list to a data frame for reactable
  species_data <- data.frame(Species = unique_species_german)

  # Initialize reactive values for tracking progress and feedback
  values <- reactiveValues(index = 1, feedback = "", selected_species = NULL)

  # Render the species selection table
  output$species_table <- renderReactable({
    reactable(species_data, onClick = "select", selection = "single", columns = list(
      Species = colDef(name = "Species", cell = function(value) {
        htmltools::tags$div(style = "cursor: pointer;", value)
      })
    ))
  })

  # Observe table selection for species
  observeEvent(input$species_table_selection, {
    selected_index <- input$species_table_selection
    if (!is.null(selected_index)) {
      values$selected_species <- species_data$Species[selected_index]

      # Compare selected species to the current species
      current_species <- species_data$Species[values$index]
      if (values$selected_species == current_species) {
        values$feedback <- "Richtig! Weiter zum nächsten Bild."
        if (values$index < nrow(species_data)) {
          values$index <- values$index + 1
        } else {
          values$feedback <- "Herzlichen Glückwunsch! Sie haben alle Bilder abgeschlossen."
        }
      } else {
        values$feedback <- "Falsch. Bitte erneut versuchen."
      }
    }
  }, ignoreInit = TRUE)

  # Display feedback message
  output$feedback <- renderText({ values$feedback })

  # Render images for the current species in separate tabs
  output$mushroom_images <- renderUI({
    current_species_german <- species_data$Species[values$index]
    current_images <- final_data %>%
      filter(species_german == current_species_german) %>%
      pull(local_path)

    encoded_images <- lapply(current_images, function(img_path) {
      paste0("data:image/jpeg;base64,", base64encode(img_path))
    })

    image_tabs <- lapply(seq_along(encoded_images), function(i) {
      tabPanel(paste("Bild", i), tags$img(src = encoded_images[[i]], height = "400px", width = "auto", style = "margin: 10px; border-radius: 10px;"))
    })
    do.call(tabsetPanel, image_tabs)
  })
}

# Run the Shiny app
shinyApp(ui = ui, server = server)
