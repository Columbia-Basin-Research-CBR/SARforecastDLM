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
        "Dynamic Linear Modelling (DLM) to explore changes in ocean survival from upwelling indices",
      ),


      shinydashboard::box(width = 12,
                          title = "Select factors of interest as you explore one-year ahead forecast for SAR:",
                          status = "info",
                          collapsible = TRUE,
                          collapsed = FALSE,
                          mod_mainpage_submodule_dataselection_ui("submodule_dataselection_1")
                          ),

      shinydashboard::box(
        width = 12,
        status = "info",
        collapsible = TRUE,
        collapsed = FALSE,
        title = shiny::uiOutput(ns("dynamic_index_title")),
        fluidRow(
          column(
            width = 10,
            offset = 1, # Centering the column
            plotly::plotlyOutput(outputId = ns("plot_index"),height = "50%")
          )
        )
      ),

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
      ),

      shinydashboard::box(
        width = 12,
        status = "info",
        collapsible = TRUE,
        collapsed = FALSE,
        title = "Model predictive accuracy based on year(s) of input data:",
        fluidRow(
          column(
            width = 12,
            shinyWidgets::sliderTextInput(
              inputId = "year_select",
              label = "Select year(s):",
              choices = 1964:2005,
              selected = 1964:2005,
              from_min = 1974,
              from_max = 2005
            ),
            plotly::plotlyOutput(outputId = ns("plot_forecast_1"))
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

    # Generate dynamic index title
    output$dynamic_index_title <- shiny::renderUI({
      selected_index <- data()$index[1]  # Get selected index

      # Generate title based on selected index
      title <- switch(selected_index,
                      "CUI" = paste("Coastal Upwelling Index: 1964 to 2023"),
                      "CUTI" = paste("Coastal Upwelling Transport Index: 1988 to 2023"),
                      "Pick an index")

      shiny::HTML(title)  # Return title as HTML
    })

    #generate reactive plots
    output$plot_index <- plotly::renderPlotly({
      fct_index_plot(data = data())
    })


     output$plot_forecast <- plotly::renderPlotly({
      fct_forecast_plot(data = data())
    })


     output$plot_forecast_1 <- plotly::renderPlotly({
       fct_forecast_plot(data = data())
     })

  })
}

## To be copied in the UI
# mod_mainpage_ui("mainpage_1")

## To be copied in the server
# mod_mainpage_server("mainpage_1")
