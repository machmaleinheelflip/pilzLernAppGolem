#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @import bslib
#' @noRd
app_ui <- function(request) {
  tagList(
    # Add the app version text here
    tags$div(style = "position: fixed; bottom: 10px; left: 10px; font-size: 12px; font-style: italic;",
             paste("version", utils::packageVersion("pilzLernAppGolem"))),
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # Your application UI logic
    page_navbar(
      title = "Mushroom Learning",
      # Adding tabsetPanel to switch between different modules
      nav_panel(
          "Mushroom Quiz",
            # mod_shroom_img_quiz_ui("shroom_img_quiz_1")  # Tab for the quiz
            mod_shroom_img_quiz2_ui("shroom_img_quiz_1")
          ),
      nav_panel(
        "Species Gallery",
        mod_species_gallery_ui("species_gallery_1") # Tab for the image gallery
        ),
      nav_panel(
        "German Spec Quiz",
        mod_plants_img_quiz_ui("plants_img_quiz_1")
      )
    )
  )
}


#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path(
    "www",
    app_sys("app/www")
  )

  tags$head(
    favicon(ext = 'png'),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "pilzLernAppGolem"
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
