#' licence_n_faq UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_licence_n_faq_ui <- function(id) {
  ns <- NS(id)
  tagList(
    actionButton(ns("show_licence"), "Data source, licence and citation"),
    # htmlOutput(ns("licence"))
   )
}

#' licence_n_faq Server Functions
#'
#' @noRd
mod_licence_n_faq_server <- function(id){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    text <- div(
      HTML("<span><b> Dataset source: </b>  </span><a href='https://www.gbif.org/dataset/50c9509d-22c7-4a22-a47d-8c48425ef4a7#'>iNaturalist Research-grade Observations</a>"),
      br(),
      HTML("<span><b> Licence: </b></span><a href='https://creativecommons.org/licenses/by-nc/4.0/legalcode'>CC BY-NC 4.0</a>"),
      br(),
      HTML("<span><b>  Citation:  </b> iNaturalist contributors, iNaturalist (2024). iNaturalist Research-grade Observations. iNaturalist.org. Occurrence dataset https://doi.org/10.15468/ab3s5x accessed via GBIF.org on 2025-01-04. </span>")
    )

    # output$licence <- renderUI({
    #   text
    # })

    observeEvent(input$show_licence, {
      shinyalert(
        title = "Data source, licence and citation",
        text = text,
        size = "xs",
        closeOnClickOutside = TRUE,
        html = TRUE,
        type = "info",
        showConfirmButton = TRUE,
        confirmButtonText = "OK",
        confirmButtonCol = "#AEDEF4",
        animation = TRUE
      )
    })

  })
}

## To be copied in the UI
# mod_licence_n_faq_ui("licence_n_faq_1")

## To be copied in the server
# mod_licence_n_faq_server("licence_n_faq_1")
