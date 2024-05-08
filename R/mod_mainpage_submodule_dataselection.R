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
          label = "Select an upwelling index",
          choices = unique(base_plot_data$index),
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
          choices = unique(base_plot_data$sar.method),
          selected = "Scheuerell and Williams (2005)",
          multiple = TRUE
        )
      ),

      #select rear_type
      column(
        width = 3,
        selectInput(
          inputId = ns("select_rear"),
          label = "Includes rear type:",#"Select rear type(s)",
          choices ="Natural-origin", #c("Both", "Hatchery-origin", "Natural-origin"),
          selected = "Natural-origin",
          multiple = TRUE
        )
      ),

      #select pass_type
      column(
        width = 3,
        selectInput(
          inputId = ns("select_pass"),
          label = "Includes passage types:", #"Select passage type(s)",
          choices = "All", #c("All", "Transported", "In-river"),
          selected = "All",
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

      base_plot_data %>%
        dplyr::filter(
          index == input$select_index,
          sar.method %in% c(input$select_sar),
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



