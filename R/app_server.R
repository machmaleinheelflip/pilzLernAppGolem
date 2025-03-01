#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  # Your application server logic
  mod_welcome_server("welcome_1")

  mod_licence_n_faq_server("licence_n_faq_1")

  # mod_shroom_img_quiz_server("shroom_img_quiz_1")
  mod_shroom_img_quiz2_server("shroom_img_quiz_1")
  # mod_species_gallery_server("species_gallery_1")

  mod_plants_img_quiz_server("plants_img_quiz_1")

  mod_hymenoptera_img_quiz_server("hymenoptera_img_quiz_1")
}
