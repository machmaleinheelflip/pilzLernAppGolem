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
#' @import reactable
mod_shroom_img_quiz_ui <- function(id) {
  ns <- NS(id)
  tagList(
    fluidPage(
      titlePanel("Mushroom Learning App"),
      sidebarLayout(
        sidebarPanel(
          reactableOutput(ns("species_table")),  # Table for selecting species
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
  mod_shroom_img_quiz_server <- function(id) {
    moduleServer(id, function(input, output, session) {
      ns <- session$ns

      # browser()
      unique_species_german <- unique(my_dataset$species)  # Assuming this is your species list
      values <- reactiveValues(current_species = unique_species_german[1])

      # Setup reactable table
      output$species_table <- renderReactable({
        # browser()
        reactable(my_dataset %>%
                    select(species) %>%
                    unique(),
                  onClick = "select",
                  selection = "single",
                  striped = TRUE,
                  resizable = TRUE,
                  highlight = TRUE,
                  borderless = TRUE,
                  theme = reactableTheme(
                    rowSelectedStyle = list(backgroundColor = "#eee", boxShadow = "inset 2px 0 0 0 #ffa62d")
                  ),
                  defaultSelected = NULL,
                  columns = list(
                    species = colDef(name = "Species",
                                            cell = function(value) {
                                              htmltools::tags$div(style = "cursor: pointer;", value)
                                              })
        ))
      })

      selected_row_index <-reactive(reactable::getReactableState("species_table", "selected", session))

      observeEvent(selected_row_index(), {
        # browser()
        selected_species <- unique_species_german[selected_row_index()]
        if (selected_species == values$current_species) {
          values$feedback <- "Richtig! Weiter zum nächsten Bild."
        } else {
          values$feedback <- "Falsch. Bitte erneut versuchen."
        }
      })

      # Observe selections in the reactable table
      # observeEvent(input[[ns("species_table_selection")]], {
      #   selected_index <- input[[ns("species_table_selection")]]
      #   selected_species <- unique_species_german[selected_index]
      #
      #   if (selected_species == values$current_species) {
      #     values$feedback <- "Richtig! Weiter zum nächsten Bild."
      #   } else {
      #     values$feedback <- "Falsch. Bitte erneut versuchen."
      #   }
      # })

      # Update feedback
      output$feedback <- renderText({
        values$feedback
      })

      # Suppose you have a method to change the current species when correct
      observeEvent(values$feedback, {
        if (values$feedback == "Richtig! Weiter zum nächsten Bild.") {
          # Rotate to the next species in a cyclic manner
          next_index <- match(values$current_species, unique_species_german) %% length(unique_species_german) + 1
          values$current_species <- unique_species_german[next_index]
        }
      })


      # Render images for the current species in separate tabs, encoding them to base64
      output$mushroom_images <- renderUI({
        # browser()
        # Get the current species German name
        current_species_german <- values$current_species

        # Filter images for the current species
        current_images <- my_dataset %>%
          filter(species == current_species_german) %>%
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



    })
  }


## To be copied in the UI
# mod_shroom_img_quiz_ui("shroom_img_quiz_1")

## To be copied in the server
# mod_shroom_img_quiz_server("shroom_img_quiz_1")
