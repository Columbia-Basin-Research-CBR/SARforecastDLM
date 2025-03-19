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
        title = "One-year ahead forecast of wild Snake River spring/summer Chinook salmon survival",
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
        # div(
        #   style = "text-align: right;",
        #   shinyWidgets::prettySwitch(
        #     inputId = ns("prediction_type"),
        #     label = "View prediction using all data",
        #     inline = TRUE,
        #     status = "default",
        #     fill = TRUE
        #   )
        # ),
        htmlOutput(ns("selected_range")),
        #plot comparison forecasts
        plotly::plotlyOutput(outputId = ns("plot_forecast_1")),
        div(
          style = "background-color: white; text-align: right; padding: 0 20px 10px 0; margin-top: -1px;",
        uiOutput(ns("data_caption"))
        ), #used for SW reach caption
        br(),
        h5("To compare model accuracy based on years of input data, select a year range below and select `Run Model` to update the forecast plot:"),
        br(),
        column(
          width = 4,
          # Add a slider input for year selection
          uiOutput(ns("year_slider")),
          uiOutput(ns("warning_message")) #warning to select atleast 7 years
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
          div(
            id = ns("model_adjustments_box"),
            style = "background-color: #f5f5f5; border: 1px solid #ddd; border-radius: 4px; padding: 10px; margin-top: 10px; font-family: monospace;",
            verbatimTextOutput(ns("model_adjustments"))
          )
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
    convergence_status <- reactiveVal("Not run yet")


    # Observe changes in the "Run Model" button
    observeEvent(input$run_model, {
      # When the "Run Model" button is clicked, set model_run_clicked to TRUE
      model_run_clicked(TRUE)
      model_run_once(TRUE)
      model_reset_clicked(FALSE)  # Reset the reset state
      input_changed_after_run(FALSE)

      # Disable the "Run Model" button - if nothing has been changed since last run
      updateActionButton(session, "run_model", label = "Run Model", disabled = TRUE)

      index_val<-data()$index[1]
      sar_val<- data()$sar.method[1]
      reach_val<-if(data()$sar.method[1] == "Scheuerell and Williams (2005)") NULL else paste0( "(", data()$reach[1], ")")

      # Update the text to be displayed in output$selected_range
      model_run_text(HTML(paste("<p><b>Model results for ",min_year(), "to", input$years_select, index_val, " indice and ",  sar_val, " SAR", reach_val, "method.</b>
                                <br>To highlight specific areas of the plot, click on legend items to toggle on/off.")))
    })

    # Observe changes in the "Reset Model" button
    observeEvent(input$reset_model, {
      # When the "Reset Model" button is clicked, set model_run_clicked to FALSE
      model_run_clicked(FALSE)
      model_reset_clicked(TRUE)  # Set the reset state to TRUE
      convergence_status("Not run yet")


      # Enable the "Run Model" button once the model has been reset
      updateActionButton(session, "run_model", label = "Run Model", disabled = FALSE)

      # Update the text to be displayed in output$selected_range
      model_run_text("")

      #reset slider to max year
      updateSliderInput(session, "years_select", value = max_year())
    })

    # Observe changes in the slider and/or the coastal index select input
    observeEvent(list(input$years_select, data()$index[1], data()$sar.method[1], data()$reach[1]), {
      if ( model_run_once()) {
        # Only reset the plot if the model has not been run or has been reset
        model_reset_clicked(FALSE)
        input_changed_after_run(TRUE)


        # Enable the "Run Model" button once an input has changed
        updateActionButton(session, "run_model", label = "Run Model", disabled = FALSE)
      }
    }, ignoreInit = TRUE)

    #show when model has been adjusted--and status of run:
    output$model_adjustments <- renderText({
      status_color <- if (!is.null(convergence_status())) {
        if (convergence_status() == "Success") {
          "✓ Model converged."
        } else if (convergence_status() == "Not run yet") {
          "Not run yet"
        } else if (convergence_status() == "Warning") {
          "❗Model failed to converge. Returning predictions including past and present data versus to show model overfitting."  } else {
       NULL
          }
      }
        if (model_run_clicked() && !input_changed_after_run()) {
          paste0(
            "Model run settings:\n",
            "Year range: ", min_year(), "-", input$years_select, "\n",
            "Indice:     ", data()$index[1], "\n",
            "SAR method: ", data()$sar.method[1], if(data()$sar.method[1] != "Scheuerell and Williams (2005)") paste0(" (", data()$reach[1], ")") else "",  "\n\n",
            "Model convergence:", status_color, "\n\n",
            "Adjust parameters to re-run model."
          )
        } else if (model_run_once() && input_changed_after_run()) {
        paste0(
          "Current settings:\n",
          "Year range: ", min_year(), "-", input$years_select, "\n",
          "Indice:     ", data()$index[1], "\n",
          "SAR method: ", data()$sar.method[1], if(data()$sar.method[1] != "Scheuerell and Williams (2005)") paste0(" (", data()$reach[1], ")") else "", "\n\n",
          "Click 'Run Model' to apply settings"
        )
      } else if (model_run_clicked() && !input_changed_after_run()) {
        paste0(
          "Model settings:\n",
          "Year range: ", min_year(), "-", input$years_select, "\n",
          "Indice:     ", data()$index[1], "\n",
          "SAR method: ", data()$sar.method[1], if(data()$sar.method[1] != "Scheuerell and Williams (2005)") paste0(" (", data()$reach[1], ")") else "",  "\n\n",
          "Model convergence:", status_color, "\n\n",
          "Adjust parameters to re-run model."
        )
      } else {
        "Select parameters\nThen click 'Run Model'"
      }
    })

    # Reactive text output for slider once used
    output$selected_range <- renderUI({
      req(data())
      req(input$years_select)

      if (!model_run_once() || model_reset_clicked()) {

        index_val<-data()$index[1]
        sar_val<- data()$sar.method[1]
        reach_val<-if(data()$sar.method[1] == "Scheuerell and Williams (2005)") NULL else paste0( "(", data()$reach[1], ")")

        # Update the text to be displayed in output$selected_range
        HTML(paste("<p><b>Model results for ",min_year(), "to", input$years_select, index_val, " indice and ",  sar_val, " SAR", reach_val, "method.</b>
                                <br>To highlight specific areas of the plot, click on legend items to toggle on/off."))
      } else {
        model_run_text()
      }
    })


    # Reactive function to extract common variables for dynamic titles and slider
    common_vars <- reactive({
      req(data())
      req(nrow(data()>0))
      req(data()$index[1])
      req(data()$sar.method[1])
      req(data()$reach[1])

      selected_index <- data()$index[1]  # Get selected index
      selected_sar <- data()$sar.method[1]  # Get selected method
      selected_reach <- data()$reach[1]  # Get selected reach

      # Validate selected values
      req(selected_index)
      req(selected_sar)
      req(selected_reach)

      # Filter data based on selected index and method
      filtered_data <- data()[data()$index == selected_index & data()$sar.method == selected_sar & data()$reach == selected_reach, ]

      req(filtered_data)
      req(nrow(filtered_data) > 0)

      # Calculate min and max year
      min_year <- min(filtered_data$year, na.rm = TRUE)
      max_year <- max(filtered_data$year, na.rm = TRUE)- 1

      # Return a list of the extracted variables
      list(selected_index = selected_index, selected_sar = selected_sar, selected_reach = selected_reach, min_year = min_year, max_year = max_year)
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
      title <- paste("One year ahead forecast with", vars$selected_index, "and", vars$selected_sar, paste0("(", vars$selected_reach, ")"),":", vars$min_year, "to", vars$max_year)

      shiny::HTML(title)
    })

    output$data_caption <- renderUI({
      vars <- common_vars()
      if(vars$selected_sar == "Scheuerell and Williams (2005)"){
        HTML("Scheuerell and Williams (2005) SAR is based on the uppermost dam on the Snake River (Snake River Upper-Upper), adjusting as dams were built upstream from 1964 to 2005.")
      } else {
       NULL
      }
    })

    # Generate dynamic index title
    output$dynamic_index_title_1 <- shiny::renderUI({
      vars <- common_vars()  # Get the common variables

      # Generate title based on selected index
      title <- switch(vars$selected_index,
                      "CUI" = paste("Coastal Upwelling Index:", vars$min_year, "to", vars$max_year),
                      "CUTI" = paste("Coastal Upwelling Transport Index:", vars$min_year, "to", vars$max_year),
                      "ICPB" = paste("Index of Coastal Prey Biomass:", vars$min_year, "to", vars$max_year),
                      "NCBI" = paste("Northern Copepod Biomass Index:", vars$min_year, "to", vars$max_year),
                      "Pick an index")

      shiny::HTML(title)
    })

    # Reactive slider
    output$year_slider <- renderUI({
      vars <- common_vars()  # Get the common variables
    req(vars$selected_sar)

      sliderInput(inputId = ns("years_select"),
                  label = "Select year range:",
                  min = vars$min_year,
                  max = vars$max_year,
                  value = vars$max_year,
                  step = 1,
                  sep = "",
                  ticks = FALSE)
    })

    # Separate observer to check the selected value after the slider exists
    observe({
      req(input$years_select)  # This ensures the input exists before proceeding
      vars <- common_vars()

      # Check if selected year is at least 7 years greater than min year
      if(input$years_select < (vars$min_year +6)) {
        output$warning_message <- renderUI({
          div(
            style = "color: red; font-weight: normal; margin-top: 15px;",
            paste0("Warning: Please select a span of at least 7 years, no earlier than ",
                  (vars$min_year + 6), ".")
          )
        })
      } else {
        output$warning_message <- renderUI({
          NULL  # Clear the warning when condition is met
        })
      }
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
      selected_reach <- data()$reach[1]
      print(selected_reach)
      # Select the data based on the selected index--used to prevent data_base to update without hitting run model first (remove if want to compare CUI and CUTI results)
      if (selected_index == "CUI") {
        data_base(data())
      } else if (selected_index == "CUTI") {
        data_base(data())
      } else if (selected_index == "ICPB") {
        data_base(data())
      } else if (selected_index == "NCBI") {
        data_base(data())
      }

      # # Run the model
      model_results<-fct_forecast_model(data = sar_raw_data,paramlist = paramlist, years_selected = selected_years, index_selected = selected_index, sar_method_selected = selected_sar, reach_selected = selected_reach)

      convergence_status(model_results$convergence_status)


      # Return df_forecast and selected_years
      list(df_forecast = model_results$df_forecast,
           selected_years = selected_years)
    })

    #plot forecast
    output$plot_forecast_1 <- plotly::renderPlotly({
      req(data())

      # Check if model_run_clicked and model_run_once is FALSE (i.e., model has not been clicked or been run once)
      if (!model_run_clicked() || !model_run_once() || model_reset_clicked()) {
        # If model_run_clicked and model_run_once is FALSE, plot the base forecast and allow user to adjust plot based on inputs(index & sar.method)
        filtered_data <- data() %>%
          dplyr::filter(
            index == data()$index[1],
            sar.method == data()$sar.method[1],
            reach == data()$reach[1],
            dataset == "base_forecast"
          )
        print(data()$reach[1])
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
      req(data())
      fct_index_plot(data = data())
    })
  })
}

## To be copied in the UI
# mod_mainpage_ui("mainpage_1")

## To be copied in the server
# mod_mainpage_server("mainpage_1")
