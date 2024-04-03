#' mainpage UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_mainpage_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(

      shinydashboard::box(
        width = 12,
        solidHeader = TRUE,
        status = "primary",
        collapsible = TRUE,
        collapsed = FALSE,
        title = "One-year ahead forecast of Chinook salmon survival",
        "Dynamic Linear Modelling (DLM) to explore changes in ocean survival from coastal ocean upwelling index (CUI)",
      ),

      shinydashboard::box(
        width = 12,
        status = "info",
        collapsible = TRUE,
        collapsed = FALSE,
        title = "Coastal Upwelling Index: 1964 to present",
        fluidRow(
          column(
            width = 10,
            offset = 1, # Centering the column
            plotly::plotlyOutput(outputId = ns("plot_CUI"),height = "50%")
          )
        )
      ),

      shinydashboard::box(width = 12,
                          title = "Add data selection in future",
                          status = "info",
                          collapsible = TRUE,
                          collapsed = TRUE),

      shinydashboard::box(
        width = 12,
        status = "info",
        collapsible = TRUE,
        collapsed = FALSE,
        title = "One year ahead forecast:",
        fluidRow(
          column(
            width = 12,
           plotly::plotlyOutput(outputId = ns("plot_forecast"))
          )
        )
      )
    )
  )
}

#' mainpage Server Functions
#'
#' @noRd
mod_mainpage_server <- function(id, data){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    output$plot_CUI <- plotly::renderPlotly({

      fct_CUI_plot(data = data)
    })


     output$plot_forecast <- plotly::renderPlotly({

      fct_forecast_plot(data = data)
    })

  })
}

## To be copied in the UI
# mod_mainpage_ui("mainpage_1")

## To be copied in the server
# mod_mainpage_server("mainpage_1")
