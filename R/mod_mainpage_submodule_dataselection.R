#' mainpage_submodule_dataselection UI Function
#'
#' @description shiny widget data select inputs for main page
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_mainpage_submodule_dataselection_ui <- function(id){
  ns <- NS(id)
  tagList(

    fluidRow(
      # select index (one only)
      column(
        width = 3,
        selectInput(
          inputId = ns("select_index"),
          label = "Select marine index",
          choices = unique(sar_raw_data$index),
          selected = "CUI",
          multiple = FALSE
        )
      ),
      # select sar.method
      column(
        width = 3,
        selectInput(
          inputId = ns("select_sar"),
          label = "Select SAR method",
          choices = unique(sar_raw_data$sar.method),
          selected = "Scheuerell and Williams (2005)",
          multiple = FALSE
        )
      ),
      #select reach
      column(
        width = 3,
        selectInput(
          inputId = ns("select_reach"),
          label = "Select Reach",
          choices = unique(sar_raw_data$reach),
          selected = "Snake River Upper-Upper",
          multiple = FALSE
        )
      )

      # #select rear_type
      # column(
      #   width = 3,
      #   selectInput(
      #     inputId = ns("select_rear"),
      #     label = "Includes rear type:",#"Select rear type(s)",
      #     choices ="Natural-origin", #c("Both", "Hatchery-origin", "Natural-origin"),
      #     selected = "Natural-origin",
      #     multiple = TRUE
      #   )
      # ),
      #
      # #select pass_type
      # column(
      #   width = 3,
      #   selectInput(
      #     inputId = ns("select_pass"),
      #     label = "Includes passage types:", #"Select passage type(s)",
      #     choices = "All", #c("All", "Transported", "In-river"),
      #     selected = "All",
      #     multiple = TRUE
      #   )
      # )
    )
  )
}

#' submodule_dataselection Server Functions
#'
#' @noRd
mod_mainpage_submodule_dataselection_server <- function(id, forecast_output){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    # Valid combos of models
    valid_combinations <- unique(sar_raw_data[, c("index", "sar.method", "reach")]) %>% tidyr::drop_na()

    # Reactive function to compute valid choices
    valid_options <- reactive({
      list(
        sar_methods = unique(valid_combinations$sar.method[valid_combinations$index == input$select_index]),
        reaches = unique(valid_combinations$reach[
          valid_combinations$index == input$select_index &
            valid_combinations$sar.method == input$select_sar
        ]),
        indices = unique(valid_combinations$index[
          valid_combinations$sar.method == input$select_sar &
            valid_combinations$reach == input$select_reach
        ])
      )
    })

    # Observe all inputs and update them in the correct order-- avoids errors
    observe({
      options <- valid_options()

      updateSelectInput(session, "select_sar", choices = options$sar_methods,
                        selected = if (input$select_sar %in% options$sar_methods) input$select_sar else options$sar_methods[1])

      updateSelectInput(session, "select_reach", choices =  options$reaches,
                        selected = if (input$select_reach %in% options$reaches) input$select_reach else options$reaches[1])

      updateSelectInput(session, "select_index", choices = options$indices,
                        selected = if (input$select_index %in% options$indices) input$select_index else options$indices[1])
    })


    # Filter to get listed dataframe based on selection
    filtered_data <- reactive({
      formatted_reach <- if (input$select_sar == "Scheuerell and Williams (2005)"){"uppupp"} else { gsub("-", "", tolower(input$select_reach))} #used for model_output listed names lgrlga, bonboa, and uppupp
      formatted_sar <- if(input$select_sar == "Scheuerell and Williams (2005)"){
        "sw"
      } else {
        tolower(input$select_sar)
      }

      # Construct the key to access the correct forecast dataframe
      forecast_list_key <- paste0(
        formatted_sar, "_",
        tolower(input$select_index), "_",
        formatted_reach
      )

      # Retrieve the dataframe from the forecasts list if it exists, otherwise return NULL
      if (forecast_list_key %in% names(forecast_output)) {
        return(forecast_output[[forecast_list_key]])
      } else {
        return(NULL)  # Return NULL if no matching key exists
      }
    })

    # Return the reactive expression(s)
    return(list(
      filtered_data = reactive(filtered_data)
      ))
  })
}

## To be copied in the UI
# mod_mainpage_submodule_dataselection_ui("submodule_dataselection_1")

## To be copied in the server
# mod_mainpage_submodule_dataselection_server("submodule_dataselection_1")



