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
mod_shroom_img_quiz_level_1_ui <- function(id) {
  ns <- NS(id)
  tagList(
    layout_columns(
      col_widths = c(4, 8),
      card(
        uiOutput(ns("shroom_images")) # Display images in tabs for each species
      ),
      card(
        # max_height = "375px",
        reactableOutput(ns("species_table")), # Table for selecting species
        # textOutput(ns("feedback")),
        actionButton(ns("show_solution"), "Ich weis es nicht.")
      )
    )
  )
}


#' shroom_img_quiz Server Functions
#'
#' @noRd
mod_shroom_img_quiz_level_1_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    shrooms <- shrooms %>%
      filter(!is.na(species_german)) %>%
      left_join(shroomGroups, relationship = "many-to-one") %>%
      filter(!is.na(key1)) %>%
      # make equaliy sized samples of each key1 group
      group_by(key1) %>%
      slice_sample(n = 50) %>%
      ungroup()

    random_species_number <- sample(1:length(unique(shrooms$species_german)), 1)
    values <- reactiveValues(
      current_species = shrooms$species_german[random_species_number],
      species = shrooms$species[random_species_number],
      key1 = shrooms$key1[random_species_number]
    )
    random_specs <- reactiveValues(data = NULL)

    observe({
      req(values$current_species)
      random_specs$data <- shrooms %>%
        select(key1) %>%
        unique() %>%
        arrange(key1)
    })

    output$species_table <- renderReactable({
      req(values$current_species)

      reactable(
        data = random_specs$data,
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
          align = "left"
        ),
        defaultSelected = NULL,
        columns = list(
          key1 = colDef(name = "Gruppe")
        )
      )
    })

    observeEvent(input$show_solution, {
      values$feedback <- paste("Lösung: ", values$key1)
      updateReactable(
        outputId = "species_table",
        data = random_specs$data,
        selected = NA
      )
    })

    selected_row_index <- reactive(reactable::getReactableState("species_table", "selected", session))

    observeEvent(selected_row_index(), {
      selected_key <- random_specs$data[selected_row_index(), "key1"]

      if (selected_key == values$key1) {
        values$feedback <- "Richtig! Weiter zum nächsten Bild."
        updateReactable(
          outputId = "species_table",
          data = random_specs$data,
          selected = NA
        )
      } else {
        values$feedback <- "Falsch. Bitte erneut versuchen."
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
        # Reset feedback after showing the alert
        values$feedback <- ""
        updateReactable(
          outputId = "species_table",
          selected = NA
        )
      }
    })

    output$feedback <- renderText({
      values$feedback
    })

    observeEvent(values$feedback, {
      if (values$feedback == "Richtig! Weiter zum nächsten Bild." || values$feedback == paste("Lösung: ", values$key1)) {
        if (values$feedback == paste("Lösung: ", values$key1)) {
          shinyalert(
            title = paste("Lösung: ", values$key1),
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

        next_index <- sample(1:length(unique(shrooms$species_german)), 1)
        values$current_species <- shrooms$species_german[next_index]
        values$species <- shrooms$species[next_index]
        values$key1 <- shrooms %>%
          filter(species_german == values$current_species) %>%
          pull(key1) %>%
          first()
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

      updateReactable(
        outputId = "species_table",
        data = random_specs$data,
        selected = NA
      )
    })

    output$shroom_images <- renderUI({
      current_species_german <- values$current_species
      current_images <- shrooms %>%
        filter(species_german == current_species_german) %>%
        pull(media_url)

      encoded_images <- lapply(current_images, encode_resized_image_from_url)

      image_tabs <- lapply(seq_along(encoded_images), function(i) {
        tabPanel(paste("Bild", i), tags$img(src = encoded_images[[i]], height = "auto", width = "100%", style = "border-radius: 10px;"))
      })

      do.call(tabsetPanel, image_tabs)
    })
  })
}


## To be copied in the UI
# mod_shroom_img_quiz_level_1_ui("shroom_img_quiz_level_1_1")

## To be copied in the server
# mod_shroom_img_quiz_level_1_server("shroom_img_quiz_level_1_1")
