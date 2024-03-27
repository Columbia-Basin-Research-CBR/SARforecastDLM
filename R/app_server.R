#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  # Your application server logic

  ## Welcome page
  mod_welcome_page_server("welcome_page_1")
  mod_welcome_submodule_leaflet_map_server("leaflet_map_1")




}
