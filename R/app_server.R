#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  # Your application server logic

  load(system.file("data/forecast_output.rda", package = "SARforecastDLM"))
  get("forecast_output")

  # Debugging observer to track tab changes
  observe({
    print(paste("Current tab:", input$tabs))
  })

  ## About page
  mod_about_page_server("about_page_1")

  #main page
  dataselect_reactives<- mod_mainpage_submodule_dataselection_server("submodule_dataselection_1", forecast_output = forecast_output)

  observe({
    filtered_data <- dataselect_reactives$filtered_data
    mod_mainpage_server("mainpage_1", data = filtered_data())
  })

  #supplementary page
  mod_supplementary_page_server("supplementary_page_1")

}
