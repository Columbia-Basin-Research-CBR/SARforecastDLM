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
                          title = "Coastal upwelling as a predictor of salmon survival",
                          status = "info",
                          collapsible = TRUE,
                          collapsed = FALSE,

                          HTML("<p>Scheurell and Williams (2005) hypothesized that upwelling indices, specifically upwelling events in april prior to juveniles entering the ocean in May and June, could be used as a method to forecast salmon survival.
                               Since publication, a new <a href = 'https://oceanview.pfeg.noaa.gov/products/upwelling/intro'> coastal upwelling transport index (CUTI)</a> has been developed that includes upwelling driven by changes in alongshore wind,
                               a feature not measured in the CUI, or Bakun Index.</p>
                               <p>Select a upwelling indice from the drop down below to explore differences in the indices and any changes to the forecast of salmon survival using the smolt-to-adult (SAR) and methods outlined in Scheurell and Williams (2005).</p>
                               "),
                          br(),
                          mod_mainpage_submodule_dataselection_ui("submodule_dataselection_1")
                          ),
      shinydashboard::box(
        width = 12,
        status = "info",
        collapsible = TRUE,
        collapsed = FALSE,
        title = shiny::uiOutput(ns("dynamic_index_title")),
                          column(
                            width = 10,
                            offset = 1, # Centering the column
                            plotly::plotlyOutput(outputId = ns("plot_index"),height = "50%")
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
        title = "Model forecast accuracy based on year(s) of input data:",
        fluidRow(
            column(
              width = 3,
              tags$style(HTML(".js-irs-0 .irs-single, .js-irs-0 .irs-bar-edge, .js-irs-0 .irs-bar {background:  #024c63}")), #change color of slider to match CBRtheme
              # Add a slider input for year selection
              uiOutput(ns("year_slider")),
              br(),
              # Add an action button to run the model
              actionButton(inputId = ns("run_model"),
                           label = "Run Model"),
              br()
              ),
          column(
            width = 9,
            br(),
            htmlOutput(ns("selected_range")),
            #plot comparison forecasts
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

     #reactive to update slider minimum value
     min_year <- reactive({
       selected_index <- data()$index[1]  # Get selected index
       if (selected_index == "CUI") {
         return(1964)
       } else if (selected_index == "CUTI") {
         return(1988)
       } else {
         return(NA)
       }
     })

     #reactive slider
     output$year_slider<- renderUI({
       sliderInput(inputId = ns("years_select"),
                   label = "Select Year Range:",
                   min = min_year(),
                   max = 2005,
                   value =  2000,
                   step = 1,
                   sep = "",
                   ticks = FALSE)
     })

     # reactive value to track whether the "Run Model" button has been clicked
     model_run_clicked <- reactiveVal(FALSE)

     observeEvent(input$run_model, {
       # When the "Run Model" button is clicked, set model_run_clicked to TRUE
       model_run_clicked(TRUE)
     })


     #reactive text output for slider once used
     output$selected_range <- renderUI({

       if (!model_run_clicked()) {
         HTML("Use the slider to select a year range to forecast SAR one-year ahead compared to all data years. Adjust the coastal index in the dropdown menu above.
              <br>
              Select 'Run Model' when ready. It may take a moment to load.")
       } else {
         HTML(paste("<p>Current model selected: ",min_year(), "to", input$years_select), "SAR via Scheurell and Williams (2005) and ", data()$index[1], " indice")
       }
     })


    model_run<- eventReactive(input$run_model, {
      # Ensure that input$years_select is set
      req(input$years_select)

       # Get the selected year range
       selected_years <- input$years_select
       selected_index <- data()$index[1]

       print(selected_index)
       print(selected_years)


       # # Run the model
       df_forecast<-fct_model_forecast(data = sar_raw_data, years_selected = selected_years, index_selected = selected_index)

       # Return df_forecast and selected_years
       list(df_forecast = df_forecast,
            selected_years = selected_years)
       })

    output$plot_forecast_1 <- plotly::renderPlotly({
      # Check if model_run is NULL (i.e., the model hasn't been run yet)
      if (is.null(model_run())) {
        return(NULL)
      }

      # If model_run is not NULL, plot the results
      fct_forecast_compare_plot(data_base = data() , data_select = model_run()$df_forecast, years_selected = model_run()$selected_years)
     })

  })
}

## To be copied in the UI
# mod_mainpage_ui("mainpage_1")

## To be copied in the server
# mod_mainpage_server("mainpage_1")
