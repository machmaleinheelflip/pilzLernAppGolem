#' welcome UI Function
#'
#' @description A shiny Module that serves as the landing page.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList fluidPage fluidRow column actionButton
#' @importFrom bslib bs_theme
#' @importFrom shinyLP jumbotron
#' @import shiny
#' @import shinyjs
mod_welcome_ui <- function(id){
  ns <- NS(id)

  tagList(
    shinyjs::useShinyjs(),

    # Meta tags for SEO
    tags$head(
      tags$title("Species Identification Quiz - Test Your Knowledge!"),
      tags$meta(name = "description",
                content = "Test your species identification skills with this free online quiz! Identify mushrooms, plants, and bees in a fun, interactive way."),
      tags$meta(name = "keywords",
                content = "species quiz, mushroom identification, plant ID, bee species test, nature quiz, fungi quiz, botany quiz"),
      tags$link(rel = "stylesheet", href = "styles.css")
    ),

    # layout_columns(
    #   col_widths = c(4, 8),
    #   card(

    fluidPage(
        # Hero Section
        jumbotron(
          header = "Welcome to the Species Identification Quiz!",
          content = "Improve your species identification skills for mushrooms, plants, and bees. Test yourself and learn new species with our fun and interactive quiz.",
          button = FALSE
        ),

        # Feature Section
        fluidRow(
          column(
            width = 4,
            div(class = "feature-box",
                # tags$h3("Identify Mushrooms"),
                actionLink(ns("navPanelShrooms"), "Identify Mushrooms",
                           style = "font-size: 24px; font-weight: bold; text-decoration: none; cursor: pointer;"),

                tags$p("Can you recognize different fungi? Improve your knowledge with this interactive quiz.")
            )
          ),
          column(
            width = 4,
            div(class = "feature-box",
                # tags$h3("Learn About Plants"),
                actionLink(ns("navPanelPlants"), "Learn About Plants",
                           style = "font-size: 24px; font-weight: bold; text-decoration: none; cursor: pointer;"),
                tags$p("Test your botany skills by identifying different plant species.")
            )
          ),
          column(
            width = 4,
            div(class = "feature-box",
                # tags$h3("Recognize Bees"),
                actionLink(ns("navPanelBees"), "Recognize Bees",
                           style = "font-size: 24px; font-weight: bold; text-decoration: none; cursor: pointer;"),
                tags$p("Discover how to differentiate various bee species.") # and understand their roles in nature.
            )
          )
        ),

        # FAQ Section
        fluidRow(
          column(
            width = 12,
            tags$h2("Frequently Asked Questions"),
            tags$details(
              tags$summary("How does the quiz work?"),
              tags$p("You will be shown images of species and need to choose the correct name from multiple choices.")
            ),
            tags$details(
              tags$summary("Is this quiz free?"),
              tags$p("Yes! It’s completely free and open to all nature enthusiasts.")
            ),
            tags$details(
              tags$summary("Can I use this for educational purposes?"),
              tags$p("Yes, the quiz is perfect for schools, universities, and nature lovers to improve species knowledge.")
            )
          )
        ),

        br(),

        mod_licence_n_faq_ui("licence_n_faq_1"),

        br(), br(),

        # Footer
        fluidRow(
          column(
            width = 12,
            tags$footer("2025 Species Quiz - Made with ❤️ by machmaleinheelflip", class = "footer")
          )
        )

      )
    # )
  )
}

#' welcome Server Function
#'
#' @noRd
mod_welcome_server <- function(id){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    observeEvent(input$navPanelShrooms, {
      # browser()
      shinyjs::runjs("$('li.nav-item:nth-child(2) > a:nth-child(1)').click();")
    })

    observeEvent(input$navPanelPlants, {
      # browser()
      shinyjs::runjs("$('li.nav-item:nth-child(3) > a:nth-child(1)').click();")
    })

    observeEvent(input$navPanelBees, {
      # browser()
      shinyjs::runjs("$('li.nav-item:nth-child(4) > a:nth-child(1)').click();")
    })


  })
}


## To be copied in the UI
# mod_welcome_ui("welcome_1")

## To be copied in the server
# mod_welcome_server("welcome_1")
