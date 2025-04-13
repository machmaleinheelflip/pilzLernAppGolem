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
mod_shroom_img_quiz_level_2_ui <- function(id) {
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
mod_shroom_img_quiz_level_2_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # browser()
    shrooms <- shrooms %>%
      filter(!is.na(species_german)) %>%
      left_join(shroomGroups, relationship = "many-to-one") %>%
      filter(!is.na(key1))

    # Add current column tracking
    key_columns <- c("key2", "key3", "key4", "key5", "key6")

    random_species_number <- sample(1:length(unique(shrooms$species_german)), 1)
    values <- reactiveValues(
      current_species = shrooms$species_german[random_species_number],
      species = shrooms$species[random_species_number],
      current_key_column = "key2",
      key1 = shrooms$key1[random_species_number], # Keep this for filtering
      key2 = shrooms$key2[random_species_number],
      key3 = shrooms$key3[random_species_number],
      key4 = shrooms$key4[random_species_number],
      key5 = shrooms$key5[random_species_number],
      key6 = shrooms$key6[random_species_number]
    )
    random_specs <- reactiveValues(data = NULL)

    observe({
      req(values$current_species)
      current_key <- values$current_key_column

      # Get current species data
      species_data <- shrooms %>%
        filter(species_german == values$current_species)

      # Initialize with filtering by key1
      filtered_data <- shrooms %>%
        filter(key1 == values$key1)

      # Add filters for all previous keys up to current
      current_idx <- match(current_key, key_columns)
      if (current_idx > 1) {
        for (i in 1:(current_idx - 1)) {
          prev_key <- key_columns[i]
          filtered_data <- filtered_data %>%
            filter(!!sym(prev_key) == values[[prev_key]])
        }
      }

      # Check if we're at the final key (next key has NA values)
      next_idx <- current_idx + 1
      is_final_key <- next_idx > length(key_columns) ||
        all(is.na(species_data[[key_columns[next_idx]]]))

      # Get unique values for current key from filtered data
      if (is_final_key) {
        random_specs$data <- filtered_data %>%
          select(all_of(current_key), genus) %>%
          unique() %>%
          arrange(across(everything()))
      } else {
        random_specs$data <- filtered_data %>%
          select(all_of(current_key)) %>%
          unique() %>%
          arrange(across(everything()))
      }
    })

    # Update species_table render
    output$species_table <- renderReactable({
      req(values$current_species)
      req(random_specs$data)

      current_key <- values$current_key_column

      # Define columns dynamically based on whether genus is present
      cols <- if ("genus" %in% names(random_specs$data)) {
        list(
          key2 = colDef(name = "Untergruppe"),
          key3 = colDef(name = "Detail 1"),
          key4 = colDef(name = "Detail 2"),
          key5 = colDef(name = "Detail 3"),
          key6 = colDef(name = "Art"),
          genus = colDef(name = "Wissenschaftlicher Name")
        )
      } else {
        list(
          key2 = colDef(name = "Untergruppe"),
          key3 = colDef(name = "Detail 1"),
          key4 = colDef(name = "Detail 2"),
          key5 = colDef(name = "Detail 3"),
          key6 = colDef(name = "Art")
        )
      }

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
        columns = cols
      )
    })

    # Update selection handler
    selected_row_index <- reactive(reactable::getReactableState("species_table", "selected", session))

    observeEvent(selected_row_index(), {
      current_key <- values$current_key_column
      selected_value <- random_specs$data[selected_row_index(), current_key]

      if (selected_value == values[[current_key]]) {
        # Get current species data
        species_data <- shrooms %>%
          filter(species_german == values$current_species)

        # Get index of next column
        current_idx <- match(current_key, key_columns)
        next_idx <- current_idx + 1

        # Check if next column exists and has non-NA values
        if (next_idx <= length(key_columns) &&
          !all(is.na(species_data[[key_columns[next_idx]]]))) {
          values$current_key_column <- key_columns[next_idx]
          updateReactable(
            outputId = "species_table",
            selected = NA
          )
        } else {
          values$feedback <- "Richtig! Weiter zum nächsten Bild."
        }
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

    observeEvent(input$show_solution, {
      # Get current species data including genus
      current_species_data <- shrooms %>%
        filter(species_german == values$current_species) %>%
        select(species_german, genus) %>%
        slice(1)

      shinyalert(
        title = paste(
          "Lösung:", current_species_data$species_german,
          "\nWissenschaftlicher Name:", current_species_data$genus
        ),
        size = "xs",
        closeOnClickOutside = TRUE,
        html = FALSE,
        type = "info",
        showConfirmButton = TRUE,
        confirmButtonText = "OK",
        confirmButtonCol = "#AEDEF4",
        animation = TRUE
      )

      # Reset to a new species after showing solution
      next_index <- sample(1:length(unique(shrooms$species_german)), 1)
      values$current_species <- shrooms$species_german[next_index]
      values$species <- shrooms$species[next_index]
      values$current_key_column <- "key2" # Start with key2 instead of key1

      # Update all keys for new species
      species_data <- shrooms %>%
        filter(species_german == values$current_species) %>%
        slice(1)

      # Update key1 first
      values$key1 <- species_data$key1

      # Then update the rest of the keys
      for (key in key_columns) {
        values[[key]] <- species_data[[key]]
      }
    })

    observeEvent(values$feedback, {
      if (values$feedback == "Richtig! Weiter zum nächsten Bild." ||
        values$feedback == paste("Lösung: ", values[[values$current_key_column]])) {
        if (values$feedback == paste("Lösung: ", values$key1)) {
          # Get the scientific genus for the current species
          current_genus <- shrooms %>%
            filter(species_german == values$current_species) %>%
            pull(genus) %>%
            first()

          shinyalert(
            title = paste("Lösung: ", values$key1, "\nWissenschaftlicher Name: ", current_genus),
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
          # Get the scientific genus for the current species
          current_genus <- shrooms %>%
            filter(species_german == values$current_species) %>%
            pull(genus) %>%
            first()

          shinyalert(
            title = paste("Richtig! :)\nWissenschaftlicher Name: ", current_genus),
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
        values$current_key_column <- "key2" # Start with key2 instead of key1

        # Update all keys for new species
        species_data <- shrooms %>%
          filter(species_german == values$current_species) %>%
          slice(1)

        # Update key1 first
        values$key1 <- species_data$key1

        # Then update the rest of the keys
        for (key in key_columns) {
          values[[key]] <- species_data[[key]]
        }

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
# mod_shroom_img_quiz_level_2_ui("shroom_img_quiz_level_2_1")

## To be copied in the server
# mod_shroom_img_quiz_level_2_server("shroom_img_quiz_level_2_1")
