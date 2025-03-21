#' about_page UI Function
#'
#' @description about page for shinyapp
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_about_page_ui <- function(id){
  ns <- NS(id)
  tagList(

    fluidRow(
      shinydashboard::box(
        title = HTML("Welcome to SARforecastDLM,<small style ='font-size:0.6em; color: white;'>BetaVersion.Mar25</small>"),
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
          width = 7,
          shiny::includeHTML(system.file("app/www/mod_about_application_intro_text.html", package = "SARforecastDLM"))
        ),
        column(
          width = 5,
          tags$img(src="www/mod_about_image_AdobeStock_75046304.jpeg",  style = "width: 100%; height: auto")#; max-height: 500px; object-fit: contain;
        )
        )
      ),

    fluidRow(
      shinydashboard::box(
        title = "Overview",
        width = 12,
        solidHeader = FALSE,
        collapsible = TRUE,
        collapsed = FALSE,
        status = "primary",
        shiny::includeHTML(system.file("app/www/mod_about_overview_text.html", package = "SARforecastDLM"))
      )
    )
    )
}


#' about_page Server Functions
#'
#' @noRd
mod_about_page_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

  })
}

## To be copied in the UI
# mod_about_page_ui("about_page_1")

## To be copied in the server
# mod_about_page_server("about_page_1")
