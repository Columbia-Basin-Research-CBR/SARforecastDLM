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
    #add leaflet map
    mod_welcome_submodule_leaflet_map_ui("leaflet_map_1"),

    fluidRow(
      shinydashboard::box(
        title = "What does this application do?",
        width = 6,
        solidHeader = TRUE,
        status = "primary",
        div(
          HTML("<p>This application can be used to explore one-year ahead forecasted spring/summer Chinook salmon, <em>Oncorhynchus tshawytscha</em>, survival based on changes in ocean survival from indices of coastal ocean upwelling (CUI)(Figure 1).</p>
                 <p>Please note that this Shiny App is dependent on data availability and serves as an exploratory tool.
                 </p>")
        )
      ),
      shinydashboard::box(
        title = "How to use this application?",
        width = 6,
        solidHeader = TRUE,
        status = "primary",
        HTML("
                 <div style='text-align: left;'>
                  <p>This <b>SARforecastDLM</b> is an interactive query tool to explore patterns of spring/summer-run Chinook Salmon....:
                 </p>
                    <div style='text-align: center; margin: auto;'>
                    <ul style='display: inline-block; text-align: left;'>
                     <li><b>Smolt- to- adult ratio (SAR):</b> ..</li>
                     <li><b>Upwelling Indices:</b> ...</li>
                     </li>
                 </ul>
                 </div>
                 </div>")
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

    mod_welcome_submodule_leaflet_map_server("leaflet_map_1")


  })
}

## To be copied in the UI
# mod_welcome_page_ui("welcome_page_1")

## To be copied in the server
# mod_welcome_page_server("welcome_page_1")
