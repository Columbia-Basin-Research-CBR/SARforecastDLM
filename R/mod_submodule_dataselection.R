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
          label = "Select coastal index",
          choices = unique(plot_data$index),
          selected = "CUI",
          multiple = FALSE
        )
      ),

      #select rear_type
      column(
        width = 3,
        selectInput(
          inputId = ns("select_rear"),
          label = "Select rear type(s)",
          choices = unique(plot_data$rear_type),
          selected = "Both",
          multiple = TRUE
        )
      ),

      #select pass_type
      column(
        width = 3,
        selectInput(
          inputId = ns("select_pass"),
          label = "Select passage type(s)",
          choices = unique(plot_data$pass_type),
          selected = "Both",
          multiple = TRUE
        )
      )
    )
  )
}

#' submodule_dataselection Server Functions
#'
#' @noRd
mod_mainpage_submodule_dataselection_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    # reactive for index, reartype, and pass type selection to filter data used in plots
    filtered_data<- reactive({

      plot_data %>%
        dplyr::filter(
          index == input$select_index,
          rear_type %in% c(input$select_rear),
          pass_type %in% c(input$select_pass)
        )
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
