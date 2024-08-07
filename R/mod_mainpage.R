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
        h5("To compare model accuracy based on years of input data, select a year range below and select `Run Model` to update the forecast plot:"),
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
                       label = "Run Model"),
          # Add an action button to reset the model
          actionButton(inputId = ns("reset_model"),
                       label = "Reset")
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

    # Reactive values to track the state of the model run and reset
    model_run_clicked <- reactiveVal(FALSE)
    model_run_once <- reactiveVal(FALSE)
    model_run_text <- reactiveVal("")
    model_reset_clicked <- reactiveVal(FALSE)
    input_changed_after_run <- reactiveVal(FALSE)


    # Observe changes in the "Run Model" button
    observeEvent(input$run_model, {
      # When the "Run Model" button is clicked, set model_run_clicked to TRUE
      model_run_clicked(TRUE)
      model_run_once(TRUE)
      model_reset_clicked(FALSE)  # Reset the reset state
      input_changed_after_run(FALSE)

      # Disable the "Run Model" button - if nothing has been changed since last run
      updateActionButton(session, "run_model", label = "Run Model", disabled = TRUE)

      # Update the text to be displayed in output$selected_range
      model_run_text(HTML(paste("<p><b>Model results for ",min_year(), "to", input$years_select, data()$index[1], " indice and ",  data()$sar.method[1], " SAR method.</b>
                                <br>To highlight specific areas of the plot, click on legend items to toggle on/off.")))
    })

    # Observe changes in the "Reset Model" button
    observeEvent(input$reset_model, {
      # When the "Reset Model" button is clicked, set model_run_clicked to FALSE
      model_run_clicked(FALSE)
      model_reset_clicked(TRUE)  # Set the reset state to TRUE


      # Enable the "Run Model" button once the model has been reset
      updateActionButton(session, "run_model", label = "Run Model", disabled = FALSE)

      # Update the text to be displayed in output$selected_range
      model_run_text("")

      #reset slider to max year
      updateSliderInput(session, "years_select", value = max_year())
    })

    # Observe changes in the slider and/or the coastal index select input
    observeEvent(list(input$years_select, data()$index[1], data()$sar.method[1]), {
      if ( model_run_once()) {
        # Only reset the plot if the model has not been run or has been reset
        model_reset_clicked(FALSE)
        input_changed_after_run(TRUE)


        # Enable the "Run Model" button once an input has changed
        updateActionButton(session, "run_model", label = "Run Model", disabled = FALSE)
      }
    }, ignoreInit = TRUE)

    output$notification_text <- renderUI({
      HTML(paste("<div style='color: #b47747;'>",
                 if (model_run_once() && input_changed_after_run()){
                   paste("<br>
            <b>Model adjustments:</b> ",
                         "<ul>
            <li> Year range:", min_year(), "-", input$years_select, "</li>
            <li> Indice:", data()$index[1], "</li>
            <li> SAR method:", data()$sar.method[1], "</li>
            </ul>
            Click 'Run Model' to see adjustments.")
                 } else if (model_run_clicked() && !input_changed_after_run()) {
                   "<br>Model has not changed, adjust parameters to re-run model."
                 } else {
                   NULL
                 },
                 "</div>")
      )
    })

    # Reactive text output for slider once used
    output$selected_range <- renderUI({
      if (!model_run_once() || model_reset_clicked()) {
        HTML(paste("<p><b>Model results for ",min_year(), "to", max_year(), data()$index[1], " indice and ",  data()$sar.method[1], " SAR method.</b>
        <br>To highlight specific areas of the plot, click on legend items to toggle on/off."))
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

    # Reactive function for max_year
    max_year <- reactive({
      vars <- common_vars()  # Get the common variables
      vars$max_year  # Return max_year
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
                      # "ICPB" = paste("Index of Coastal Prey Biomass:", vars$min_year, "to", vars$max_year),
                      "NCBI" = paste("Northern Copepod Biomass Index:", vars$min_year, "to", vars$max_year),
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
        # } else if (selected_index == "ICPB") {
        #   data_base(data())
      } else if (selected_index == "NCBI") {
        data_base(data())
      }


      # # Run the model
      df_forecast<-fct_forecast_model(data = sar_raw_data, years_selected = selected_years, index_selected = selected_index, sar_method_selected = selected_sar)

      # Return df_forecast and selected_years
      list(df_forecast = df_forecast,
           selected_years = selected_years)
    })

    #plot forecast
    output$plot_forecast_1 <- plotly::renderPlotly({
      # Check if model_run_clicked and model_run_once is FALSE (i.e., model has not been clicked or been run once)
      if (!model_run_clicked() || !model_run_once() || model_reset_clicked()) {
        # If model_run_clicked and model_run_once is FALSE, plot the base forecast and allow user to adjust plot based on inputs(index & sar.method)
        filtered_data <- data() %>%
          dplyr::filter(
            index == data()$index[1],
            sar.method == data()$sar.method[1],
            dataset == "base_forecast"
          )
        # base plot
        fct_forecast_plot(data = filtered_data)
        #Check if model_run is not NULL (i.e., model has been run)
      } else if (model_run_clicked() && !is.null(model_run())) {
        # If model_run has been run, plot the results of the select years in the model run compare plot
        fct_forecast_compare_plot(data_base = data_base() , data_select = model_run()$df_forecast, years_selected = model_run()$selected_years)
      }

    })

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
