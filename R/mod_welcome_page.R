#' welcome_page UI Function
#'
#' @description welcome page for shinyapp
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_welcome_page_ui <- function(id){
  ns <- NS(id)
  tagList(

    fluidRow(
      shinydashboard::box(
        title = "Welcome to SARforecastDLM,",
        width = 12,
        solidHeader = TRUE,
        status = "primary",
        HTML("<em> a Shiny App to explore one-year forecast of Chinook salmon survival based on
             <a href='https://onlinelibrary.wiley.com/doi/full/10.1111/j.1365-2419.2005.00346.x'>Scheurell and Williams (2005)</a>
             </em>")
      )
    ),

    fluidRow(
      shinydashboard::box(
        title = "What does this application do?",
        width = 12,
        solidHeader = TRUE,
        status = "primary",
        column(
          width = 8,
          shiny::includeHTML(system.file("app/www/mod_welcome_application_intro_text.html", package = "SARforecastDLM"))
        ),
        column(
          width = 4,
          tags$img(src="www/mod_welcome_image_AdobeStock_75046304.jpeg",  style = "width: 100%; height: auto")#; max-height: 500px; object-fit: contain;
        )
        )
      )
    )
}


#' welcome_page Server Functions
#'
#' @noRd
mod_welcome_page_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

  })
}

## To be copied in the UI
# mod_welcome_page_ui("welcome_page_1")

## To be copied in the server
# mod_welcome_page_server("welcome_page_1")
