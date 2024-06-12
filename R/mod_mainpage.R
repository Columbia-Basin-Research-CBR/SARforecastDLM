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
        "Dynamic Linear Modelling (DLM) to explore changes in ocean survival from marine indices",
      ),


      shinydashboard::box(
        width = 12,
        title = "Marine indices as a predictor of salmon survival",
        status = "info",
        collapsible = TRUE,
        collapsed = FALSE,
        shiny::includeHTML(system.file("app/www/mod_mainpage_upwelling_text.html", package = "SARforecastDLM")),
        br(),
        mod_mainpage_submodule_dataselection_ui("submodule_dataselection_1")
                          ),
      # Model forecast plot

      shinydashboard::box(
        width = 12,
        status = "info",
        collapsible = TRUE,
        collapsed = FALSE,
        title = "Model forecast",

            br(),
            htmlOutput(ns("selected_range")),
            #plot comparison forecasts
            plotly::plotlyOutput(outputId = ns("plot_forecast_1")),
            br(),
          h5("To compare model accuracy based on years of input data, select a year range below:"),
          br(),
          column(
            width = 4,
            # Add a slider input for year selection
            uiOutput(ns("year_slider"))
            ),
          column(
            width = 2,
            br(),
            br(),
            # Add an action button to run the model
            actionButton(inputId = ns("run_model"),
                         label = "Run Model")
            ),
          column(
            width = 6,
            htmlOutput(ns("notification_text")), #future addition look into tool-tip hover for this shinyBS::tooltip
          )
          ),
      #Index plot
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

      # Disable the "Run Model" button - if nothing has been changed since last run
      updateActionButton(session, "run_model", label = "Run Model", disabled = TRUE)

      # Update the text to be displayed in output$selected_range
      model_run_text(HTML(paste("<p><b>Model results for ",min_year(), "to", input$years_select, data()$index[1], " indice and ",  data()$sar.method[1], " SAR method.</b>
                                <br>To highlight specific areas of the plot, click on legend items to toggle on/off.")))
    })

    # Observe changes in the slider and/or the coastal index select input
    observeEvent(list(input$years_select, data()$index[1], data()$sar.method[1]), {
      # When the slider, index, or sar.method select input is changed, set model_run_clicked to FALSE
      model_run_clicked(FALSE)

      # Enable the "Run Model" button once an input has changed
      updateActionButton(session, "run_model", label = "Run Model", disabled = FALSE)
    }, ignoreInit = TRUE)


    output$notification_text <- renderUI({
      HTML(paste("<div style='color: #b47747;'>",
                 if (model_run_once() && !model_run_clicked()) {
                   paste("<br>
            <b>Model adjustments:</b> ",
                         "<ul>
            <li> Year range:", min_year(), "-", input$years_select, "</li>
            <li> Indice:", data()$index[1], "</li>
            <li> SAR method:", data()$sar.method[1], "</li>
            </ul>
            Please click 'Run Model' again to see adjustments.")
                 } else if (model_run_clicked()) {
                   "<br>Model has not changed, adjust parameters to re-run model."
                 } else {
                   NULL
                 },
                 "</div>")
      )
    })


    # Reactive text output for slider once used
    output$selected_range <- renderUI({
      if (!model_run_once()) {
        HTML("<ul>
              <li>Based on the select inputs above, use the slider to select a year range to forecast SAR one-year ahead compared to all data years.</li>
              <li>Select 'Run Model' when ready. It may take a moment to load.</li>
             </ul>")
      } else {
        model_run_text()
      }
    })


    # Reactive function to extract common variables for dynamic titles and slider
    common_vars <- reactive({
      selected_index <- data()$index[1]  # Get selected index
      selected_sar <- data()$sar.method[1]  # Get selected method

      # Filter data based on selected index and method
      filtered_data <- data()[data()$index == selected_index & data()$sar.method == selected_sar, ]

      # Calculate min and max year
      min_year <- min(filtered_data$year, na.rm = TRUE)
      max_year <- max(filtered_data$year, na.rm = TRUE)- 1

      # Return a list of the extracted variables
      list(selected_index = selected_index, selected_sar = selected_sar, min_year = min_year, max_year = max_year)
    })

    # Reactive function for min_year
    min_year <- reactive({
      vars <- common_vars()  # Get the common variables
      vars$min_year  # Return min_year
    })

    # Generate dynamic index title
    output$dynamic_index_title_2 <- shiny::renderUI({
      vars <- common_vars()  # Get the common variables

      # Generate title based on selected index, method, and year range
      title <- paste("One year ahead forecast with", vars$selected_index, "and", vars$selected_sar, ":", vars$min_year, "to", vars$max_year)

      shiny::HTML(title)
    })

    # Generate dynamic index title
    output$dynamic_index_title_1 <- shiny::renderUI({
      vars <- common_vars()  # Get the common variables

      # Generate title based on selected index
      title <- switch(vars$selected_index,
                      "CUI" = paste("Coastal Upwelling Index:", vars$min_year, "to", vars$max_year),
                      "CUTI" = paste("Coastal Upwelling Transport Index:", vars$min_year, "to", vars$max_year),
                      "ICPB" = paste("Index of Coastal Prey Biomass:", vars$min_year, "to", vars$max_year),
                      "Pick an index")

      shiny::HTML(title)
    })

    # Reactive slider
    output$year_slider <- renderUI({
      vars <- common_vars()  # Get the common variables

      sliderInput(inputId = ns("years_select"),
                  label = "Select year range:",
                  min = vars$min_year,
                  max = vars$max_year,
                  value =  vars$max_year, #round((vars$min_year + vars$max_year) / 2),#select middle year between min/max
                  step = 1,
                  sep = "",
                  ticks = FALSE)
    })

    # Reactive plots
     # Reactive value to store the data to be used in the forecast_1 plot
     data_base <- reactiveVal()

    #reactive to generate model and return plot
    model_run<- eventReactive(input$run_model, {
      # Ensure that input$years_select is set
      req(input$years_select)

       # Get the selected year range
       selected_years <- input$years_select
       selected_index <- data()$index[1]
       selected_sar   <- data()$sar.method[1]

       # Select the data based on the selected index--used to prevent data_base to update without hitting run model first (remove if want to compare CUI and CUTI results)
       if (selected_index == "CUI") {
         data_base(data())
       } else if (selected_index == "CUTI") {
         data_base(data())
       } else if (selected_index == "NCBI") {
         data_base(data())
       }


       # # Run the model
       df_forecast<-fct_forecast_model(data = sar_raw_data_updated, years_selected = selected_years, index_selected = selected_index, sar_method_selected = selected_sar)

       # Return df_forecast and selected_years
       list(df_forecast = df_forecast,
            selected_years = selected_years)
       })



      output$plot_forecast_1 <- plotly::renderPlotly({
      # Check if model_run is NULL (i.e., the model hasn't been run yet)
      if (is.null(model_run())) {
        # return(NULL)
        # If model_run is NULL, plot the results
        # fct_forecast_compare_plot(data_base = data_base() , data_select = model_run()$df_forecast, years_selected = vars$max_year )

        filtered_data <- data() %>%
          dplyr::filter(
            index == data()$index[1],
            sar.method == data()$sar.method[1],
            dataset == "base_forecast",
          )

        fct_forecast_plot(data = filtered_data())
      }

      # If model_run is not NULL, plot the results
      fct_forecast_compare_plot(data_base = data_base() , data_select = model_run()$df_forecast, years_selected = model_run()$selected_years)
     })
      print(model_run()$df_forecast)

      #index plot
      output$plot_index <- plotly::renderPlotly({
        fct_index_plot(data = data())
      })


  })
}

## To be copied in the UI
# mod_mainpage_ui("mainpage_1")

## To be copied in the server
# mod_mainpage_server("mainpage_1")
