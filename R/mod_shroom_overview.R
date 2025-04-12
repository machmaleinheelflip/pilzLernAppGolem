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
      choices = c("Classic", "Level 1", "Level 2", "Level 3", "Level 4"),
      selected = "Classic"
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
        "Level 1" = mod_shroom_img_quiz_level_1_ui("shroom_img_quiz_level_1_1"),
        "Level 2" = mod_shroom_img_quiz_level_2_ui("shroom_img_quiz_level_2_1")
        # "Level 3" = mod_shroom_img_quiz_ui("shroom_img_quiz_3"),
        # "Level 4" = mod_shroom_img_quiz_ui("shroom_img_quiz_4")
      )
    })
  })
}

## To be copied in the UI
# mod_shroom_overview_ui("shroom_img_quiz_1")

## To be copied in the server
# mod_shroom_overview_server("shroom_img_quiz_1")
