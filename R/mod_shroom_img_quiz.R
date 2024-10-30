#' shroom_img_quiz UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_shroom_img_quiz_ui <- function(id) {
  ns <- NS(id)
  tagList(
 
  )
}
    
#' shroom_img_quiz Server Functions
#'
#' @noRd 
mod_shroom_img_quiz_server <- function(id){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
 
  })
}
    
## To be copied in the UI
# mod_shroom_img_quiz_ui("shroom_img_quiz_1")
    
## To be copied in the server
# mod_shroom_img_quiz_server("shroom_img_quiz_1")
