#' Image Gallery UI Function
#'
#' @description A shiny Module for displaying a species lexicon with images.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#' @import dplyr
#' @import base64enc
#' @import magick
mod_species_gallery_ui <- function(id) {
  ns <- NS(id)
  tagList(
    fluidPage(
      titlePanel("Species Image Gallery"),
      mainPanel(
        uiOutput(ns("species_images_gallery"))  # Display images with species titles
      )
    )
  )
}

#' Image Gallery Server Function
#'
#' @noRd
mod_species_gallery_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    output$species_images_gallery <- renderUI({
      # Initialize progress bar
      progress <- shiny::Progress$new(session)
      progress$set(message = "Loading images...")

      tryCatch({
        # Assuming 'my_dataset' contains 'species' and 'local_path' columns
        species_images <- my_dataset %>%
          group_by(species) %>%
          summarise(images = list(local_path), .groups = "drop") %>%
          slice(1:10)

        # Initialize the number of images processed
        images_processed <- 0
        total_images <- length(unlist(species_images$images))

        # Create a UI element for each species with all corresponding images
        gallery_ui <- lapply(seq_along(species_images$species), function(i) {
          species_name <- species_images$species[i]
          images <- species_images$images[[i]]

          # Encode images to base64
          encoded_images <- lapply(images, function(img_path) {
            progress$set(detail = sprintf("Processing image %d of %d", images_processed + 1, total_images))
            images_processed <<- images_processed + 1  # Increment images processed count
            progress$inc(1 / total_images)  # Increment progress by the fraction of total images
            paste0("data:image/jpeg;base64,", base64encode(img_path))
          })

          # Construct UI for this species
          img_tags <- lapply(encoded_images, function(img) {
            tags$img(src = img, height = "100px", width = "auto", style = "margin: 10px; border-radius: 5px;")
          })

          # Combine image tags and add a title
          do.call(tagList, c(tags$h5(species_name, style = "text-align: center;"), img_tags))
        })

        do.call(tagList, gallery_ui)
      }, finally = {
        # Ensure the progress bar is closed even if an error occurs
        progress$close()
      })
    })
  })
}



# In the UI part of your main app
# mod_species_gallery_ui("species_gallery_1")

# In the server part of your main app
# mod_species_gallery_server("species_gallery_1")
