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
mod_shroom_overview_ui <- function(id) {
  ns <- NS(id)
  tagList(
    shiny::selectInput(
      ns("level_selector"),
      label = "Choose a mode:",
      choices = c("Anfänger", "Fortgeschritten (Gattungsnivau)", "Experte (Artniveau)", "Classic"),
      selected = "Anfänger"
    ),
    uiOutput(ns("quiz_ui"))
  )
}

#' shroom_img_quiz Server Functions
#'
#' @noRd
mod_shroom_overview_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    output$quiz_ui <- renderUI({
      switch(input$level_selector,
        "Classic" = mod_shroom_img_quiz_ui("shroom_img_quiz_1"),
        "Anfänger" = mod_shroom_img_quiz_level_1_ui("shroom_img_quiz_level_1_1"),
        "Fortgeschritten (Gattungsnivau)" = mod_shroom_img_quiz_level_2_ui("shroom_img_quiz_level_2_1"),
        "Experte (Artniveau)" = mod_shroom_img_quiz_level_3_ui("shroom_img_quiz_level_3_1")
        # "Level 4" = mod_shroom_img_quiz_ui("shroom_img_quiz_4")
      )
    })
  })
}

## To be copied in the UI
# mod_shroom_overview_ui("shroom_img_quiz_1")

## To be copied in the server
# mod_shroom_overview_server("shroom_img_quiz_1")
