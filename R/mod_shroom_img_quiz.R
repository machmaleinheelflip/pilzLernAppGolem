#' shroom_img_quiz UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#' @import dplyr
#' @import base64enc
mod_shroom_img_quiz_ui <- function(id) {
  ns <- NS(id)
  tagList(
    fluidPage(
      titlePanel("Mushroom Learning App"),
      sidebarLayout(
        sidebarPanel(
          # Generating buttons directly in the UI
          do.call(fluidRow, lapply(my_dataset$species_german, function(species, i) {
            actionButton(
              inputId = ns(paste0("species_button_", i)),
              label = species,
              style = "margin: 5px;"
            )
          })),
          br(),
          textOutput(ns("feedback"))
        ),
        mainPanel(
          uiOutput(ns("mushroom_images"))  # Display images in tabs for each species
        )
      )
    )
  )
}


#' shroom_img_quiz Server Functions
#'
#' @noRd
mod_shroom_img_quiz_server <- function(id){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    # browser()
    unique_species_german <- unique(my_dataset$species_german)

    # Initialize reactive values for tracking progress
    values <- reactiveValues(index = 1, feedback = "", button_choices = NULL)

    # Generate new species button choices each round
    observe({
      # Get the correct species German name
      correct_species_german <- unique_species_german[values$index]
      # Select two random incorrect species
      other_species <- sample(unique_species_german[unique_species_german != correct_species_german], 2)
      # Combine the correct and incorrect species, then shuffle
      values$button_choices <- sample(c(correct_species_german, other_species))
    })

    # Render the species buttons
    output$species_buttons <- renderUI({
      # Dynamically create 3 buttons with randomized species options
      fluidRow(
        lapply(seq_along(values$button_choices), function(i) {
          actionButton(
            inputId = ns(paste0("species_button_", i)),  # Ensuring proper namespacing
            label = values$button_choices[i],
            style = "margin: 5px;"
          )
        })
      )
    })


    # Render images for the current species in separate tabs, encoding them to base64
    output$mushroom_images <- renderUI({
      # Get the current species German name
      current_species_german <- unique_species_german[values$index]
      # Filter images for the current species
      current_images <- my_dataset %>%
        filter(species_german == current_species_german) %>%
        pull(local_path)
      # Encode each image to base64, handle if there are fewer than 3 images
      encoded_images <- lapply(current_images, function(img_path) {
        paste0("data:image/jpeg;base64,", base64encode(img_path))
      })


      # Dynamically create a tabPanel for each available image
      image_tabs <- lapply(seq_along(encoded_images), function(i) {
        tabPanel(paste("Bild", i), tags$img(src = encoded_images[[i]], height = "400px", width = "auto", style = "margin: 10px; border-radius: 10px;"))
      })

      # Render the tabsetPanel with the dynamically created tabPanels
      do.call(tabsetPanel, image_tabs)
    })


    # Observe clicks on species buttons and check if the selected species is correct
    for (i in 1:3) {
      observeEvent(input[[ns(paste0("species_button_", i))]], {
        selected_species_german <- values$button_choices[i]
        correct_species_german <- unique_species_german[values$index]

        if (selected_species_german == correct_species_german) {
          values$feedback <- "Richtig! Weiter zum nächsten Bild."
          if (values$index < length(unique_species_german)) {
            values$index <- values$index + 1  # Move to the next species
          } else {
            values$feedback <- "Herzlichen Glückwunsch! Sie haben alle Bilder abgeschlossen."
          }
        } else {
          values$feedback <- "Falsch. Bitte erneut versuchen."
        }
        # Force Shiny to update the output for feedback and potentially other UI components
        output$feedback <- renderText({
          values$feedback
        })
      }, ignoreNULL = FALSE)
    }


    # Display feedback message
    output$feedback <- renderText({
      values$feedback
    })
  })
}

## To be copied in the UI
# mod_shroom_img_quiz_ui("shroom_img_quiz_1")

## To be copied in the server
# mod_shroom_img_quiz_server("shroom_img_quiz_1")
