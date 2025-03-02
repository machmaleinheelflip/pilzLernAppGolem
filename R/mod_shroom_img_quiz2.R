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
#' @import shinyalert
mod_shroom_img_quiz2_ui <- function(id) {
  ns <- NS(id)
  tagList(
    layout_columns(
      col_widths = c(4, 8),
      card(
        uiOutput(ns("shroom_images"))  # Display images in tabs for each species
      ),
      card(
        max_height = "375px",
        reactableOutput(ns("species_table")),  # Table for selecting species
        # textOutput(ns("feedback")),
        actionButton(ns("show_solution"), "Ich weis es nicht.")
      )
    )
  )
}


#' shroom_img_quiz Server Functions
#'
#' @noRd
  mod_shroom_img_quiz2_server <- function(id) {
    moduleServer(id, function(input, output, session) {
      ns <- session$ns

      shrooms <- shrooms %>%
        filter(!is.na(species_german))

      # generate randomized integer between 1 and length(unique(shrooms$species_german)
      random_species_number <- sample(1:length(unique(shrooms$species_german)), 1)

      # browser()
      # unique_species_german <- unique(shrooms$species_german) # Assuming this is your species list
      values <- reactiveValues(current_species = shrooms$species_german[random_species_number], species= shrooms$species[random_species_number])
      random_specs <- reactiveValues(data=NULL)

      observe({
        req(values$current_species)
        # browser()
        random_specs$data <- shrooms %>%
          select(species_german, species) %>%
          unique() %>%
          slice_sample(n=3) %>%
          bind_rows(data.frame(species_german= values$current_species, species=values$species)) %>%
          unique() %>%
          mutate(random_sorting= sample(1:nrow(.), nrow(.))) %>%
          arrange(random_sorting) %>%
          select(-random_sorting)
      })

      # Setup reactable table
      output$species_table <- renderReactable({
        req(values$current_species)
        # browser()

        reactable(
          data= random_specs$data,
          onClick = "select",
          selection = "single",
          resizable = TRUE,
          highlight = TRUE,
          theme = reactableTheme(
            rowSelectedStyle = list(backgroundColor = "#eee", boxShadow = "inset 2px 0 0 0 #ffa62d")
          ),
          compact = TRUE,
          striped = TRUE,
          borderless = TRUE,
          pagination = FALSE,
          sortable = TRUE,
          showSortable = TRUE,
          defaultColDef = colDef(
            defaultSortOrder = "desc",
            align = "left",
            cell = reactablefmtr::data_bars(
              data = .,
              fill_color = viridis::mako(1000),
              background = "#ffffff",
              text_position = "outside-end",
              number_fmt = scales::comma
            )
          ),
          defaultSelected = NULL,
          columns = list(
            .selection = colDef(show = FALSE),
            species_german = colDef(name = "Art (Deutsch)",
                                    # cell = function(value) {
                                    #   htmltools::tags$div(style = "cursor: pointer;", value)
                                    #   }
            ),
            species = colDef(name = "Scientific Name")
          ))
      })


      # show_solution_react <- reactiveVal(0)
      observeEvent(input$show_solution, {
        values$feedback <- paste("Lösung: ", values$current_species)

        # Deselect selected
        updateReactable(outputId = "species_table",
                        data = random_specs$data,
                        selected = NA)
      })


      selected_row_index <- reactive(reactable::getReactableState("species_table", "selected", session))

      observeEvent(selected_row_index(), {
        # browser()
        selected_species <- random_specs$data[selected_row_index(), "species_german"]

        if (selected_species == values$current_species) {
          values$feedback <- "Richtig! Weiter zum nächsten Bild."

          # Deselect selected
          updateReactable(outputId = "species_table",
                          data = random_specs$data,
                          selected = NA)
        } else {
          values$feedback <- "Falsch. Bitte erneut versuchen."
          # Deselect selected
          updateReactable(outputId = "species_table",
                          selected = NA)
        }
      })

      # Update feedback
      output$feedback <- renderText({
        values$feedback
      })

      # Suppose you have a method to change the current species when correct
      observeEvent(values$feedback, {
        if (values$feedback == "Richtig! Weiter zum nächsten Bild." || values$feedback == paste("Lösung: ", values$current_species)) {
          # Rotate to the next species in a cyclic manner
          # next_index <- match(values$current_species, unique_species_german) %% length(unique_species_german) + 1

          if (values$feedback == paste("Lösung: ", values$current_species)) {
            # browser()
            shinyalert(
              title = paste("Lösung: ", values$current_species),
              size = "xs",
              closeOnClickOutside = TRUE,
              html = FALSE,
              type = "success",
              showConfirmButton = TRUE,
              confirmButtonText = "OK",
              confirmButtonCol = "#AEDEF4",
              animation = TRUE
            )
          }

          if (values$feedback == "Richtig! Weiter zum nächsten Bild.") {
            # browser()
            shinyalert(
              title = "Richtig! :)",
              size = "xs",
              closeOnClickOutside = TRUE,
              html = FALSE,
              type = "success",
              showConfirmButton = TRUE,
              confirmButtonText = "OK",
              confirmButtonCol = "#AEDEF4",
              animation = TRUE
            )
          }

          # next one
          next_index <- sample(1:length(unique(shrooms$species_german)), 1)
          values$current_species <- shrooms$species_german[next_index]
          values$species <- shrooms$species[next_index]

          # Vakue ressten
          values$feedback <- ""
        }

        if (values$feedback == "Falsch. Bitte erneut versuchen.") {
          shinyalert(
            title = "Leider falsch :(",
            size = "xs",
            closeOnClickOutside = TRUE,
            html = FALSE,
            type = "error",
            showConfirmButton = TRUE,
            confirmButtonText = "OK",
            confirmButtonCol = "#AEDEF4",
            animation = TRUE
          )
        }

        # Deselect selected
        updateReactable(outputId = "species_table",
                        data = random_specs$data,
                        selected = NA)
      })


      # Render images for the current species in separate tabs, encoding them to base64
      output$shroom_images <- renderUI({
        # browser()
        # Get the current species German name
        current_species_german <- values$current_species

        # Filter images for the current species
        current_images <- shrooms %>%
          filter(species_german == current_species_german) %>%
          # pull(local_path)
          pull(media_url)

        # # Encode each image to base64, handle if there are fewer than 3 images
        # encoded_images <- lapply(current_images, function(img_path) {
        #   paste0("data:image/jpeg;base64,", base64encode(img_path))
        # })

        # Encode and resize each image
        encoded_images <- lapply(current_images, encode_resized_image_from_url)

        # Dynamically create a tabPanel for each available image
        image_tabs <- lapply(seq_along(encoded_images), function(i) {
          # tabPanel(paste("Bild", i), tags$img(src = encoded_images[[i]], height = "auto", width = "100%", style = "border-radius: 10px;"))
          tabPanel(paste("Bild", i), tags$img(src = encoded_images[[i]], height = "auto", width = "100%", style = "border-radius: 10px;"))
        })

        # Render the tabsetPanel with the dynamically created tabPanels
        do.call(tabsetPanel, image_tabs)
      })

    })
  }


## To be copied in the UI
# mod_shroom_img_quiz2_ui("shroom_img_quiz_1")

## To be copied in the server
# mod_shroom_img_quiz2_server("shroom_img_quiz_1")
