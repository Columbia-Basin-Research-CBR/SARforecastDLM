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
      shinyWidgets::chooseSliderSkin(
        skin = "Flat", #"Shiny" "Modern"
        color = "#024c63"
      ),
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
                          shiny::includeHTML(system.file("app/www/mod_mainpage_upwelling_text.html", package = "SARforecastDLM")),
                          br(),
                          mod_mainpage_submodule_dataselection_ui("submodule_dataselection_1")
                          ),
      shinydashboard::box(
        width = 12,
        status = "info",
        collapsible = TRUE,
        collapsed = FALSE,
        title = shiny::uiOutput(ns("dynamic_index_title_1")),
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
        title = shiny::uiOutput(ns("dynamic_index_title_2")),
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
        title = "Model forecast accuracy based on years of input data:",
        fluidRow(
            column(
              width = 3,
              # Add a slider input for year selection
              uiOutput(ns("year_slider")),
              br(),
              # Add an action button to run the model
              actionButton(inputId = ns("run_model"),
                           label = "Run Model"),
              htmlOutput(ns("notification_text")),
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

    # Reactive values to track whether the "Run Model" button has been clicked
    model_run_clicked <- reactiveVal(FALSE)
    model_run_once <- reactiveVal(FALSE)
    model_run_text <- reactiveVal("")

    # Observe changes in the "Run Model" button
    observeEvent(input$run_model, {
      # When the "Run Model" button is clicked, set model_run_clicked to TRUE
      model_run_clicked(TRUE)
      model_run_once(TRUE)

      # Update the text to be displayed in output$selected_range
      model_run_text(HTML(paste("<p>Model results for ",min_year(), "to", input$years_select, data()$index[1], " indice and SAR via Scheurell and Williams (2005).")))
    })

    # Observe changes in the slider and/or the coastal index select input
    observeEvent(list(input$years_select, data()$index[1]), {
      # When the slider or the coastal index select input is changed, set model_run_clicked to FALSE
      model_run_clicked(FALSE)
    }, ignoreInit = TRUE)

    output$notification_text <- renderUI({
      if (model_run_once() && !model_run_clicked()) {
        return(HTML(
          paste("<br>
                <b>Model adjustments:</b> ",
                "<ul>
                <li> Year range:", min_year(), "-", input$years_select, "</li>
                <li> Indice:", data()$index[1], "</li>
                </ul>
                Please click 'Run Model' again to see adjustments.")
          )
        )
      } else {
        return(NULL)
      }
    })

    # Reactive text output for slider once used
    output$selected_range <- renderUI({
      if (!model_run_once()) {
        HTML("Use the slider to select a year range to forecast SAR one-year ahead compared to all data years. Adjust the coastal index in the dropdown menu above.
             <br>
             Select 'Run Model' when ready. It may take a moment to load.")
      } else {
        model_run_text()
      }
    })

    # Generate dynamic index title
    output$dynamic_index_title_1 <- shiny::renderUI({
      selected_index <- data()$index[1]  # Get selected index

      # Generate title based on selected index
      title <- switch(selected_index,
                      "CUI" = paste("Coastal Upwelling Index: 1964 to 2007"),
                      "CUTI" = paste("Coastal Upwelling Transport Index: 1988 to 2007"),
                      "Pick an index")

      shiny::HTML(title)
    })

    output$dynamic_index_title_2 <- shiny::renderUI({
      selected_index <- data()$index[1]  # Get selected index

      # Generate title based on selected index
      title <- switch(selected_index,
                      "CUI" = paste("One year ahead forecast with CUI: 1964 to 2007"),
                      "CUTI" = paste("One year ahead forecast with CUTI: 1988 to 2007"),
                      "Pick an index")

      shiny::HTML(title)
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
                   label = "Select year range:",
                   min = min_year(),
                   max = 2005,
                   value =  2000,
                   step = 1,
                   sep = "",
                   ticks = FALSE)
     })

     # Reactive value to store the data to be used in the forecast_1 plot
     data_base <- reactiveVal()

    #reactive to generate model and return plot
    model_run<- eventReactive(input$run_model, {
      # Ensure that input$years_select is set
      req(input$years_select)

       # Get the selected year range
       selected_years <- input$years_select
       selected_index <- data()$index[1]

       # Select the data based on the selected index--used to prevent data_base to update without hitting run model first (remove if want to compare CUI and CUTI results)
       if (selected_index == "CUI") {
         data_base(data())
       } else if (selected_index == "CUTI") {
         data_base(data())
       }


       # # Run the model
       df_forecast<-fct_forecast_model(data = sar_raw_data, years_selected = selected_years, index_selected = selected_index)

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
      fct_forecast_compare_plot(data_base = data_base() , data_select = model_run()$df_forecast, years_selected = model_run()$selected_years)
     })

  })
}

## To be copied in the UI
# mod_mainpage_ui("mainpage_1")

## To be copied in the server
# mod_mainpage_server("mainpage_1")
