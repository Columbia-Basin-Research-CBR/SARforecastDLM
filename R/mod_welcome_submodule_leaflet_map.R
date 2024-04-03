#' welcome_submodule_leaflet_map UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_welcome_submodule_leaflet_map_ui <- function(id){
  ns <- NS(id)
  tagList(

    fluidRow(
      column(width = 3),  # Empty column to center map
      column(
        width = 6,
        shinydashboard::box(
          width = NULL,
          solidHeader = FALSE,
          status = "primary",
          title = "Pacific Northwest Map with Columbia River and Dams",
          # shiny::img(src = "www/map.png", style = "max-width:100%; height:auto;"),
          leaflet::leafletOutput(ns("map")),
          br(),
          "Figure 1: Map of the Columbia and Snake River, Pacific Northwest, USA, with major hydroelectric dams denoted (dark circles) along Spring/Summer Chinook salmon and Steelhead migratory routes."
        )
      ),
      column(width = 3),  # Empty column to center map
    )
  )
}

#' welcome_page_submodule_leaflet_map Server Functions
#'
#' @noRd
mod_welcome_submodule_leaflet_map_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns


    # Mock data for Columbia and Snake rivers//add as needed
    rivers_data <- tibble::tibble(
      Name = c("Lower Granite Dam (LGR)","Lower Granite Dam (LGA)", "Bonneville Dam (BON)", "Bonneville Dam (BON)", "Coastal Upwelling Index (CUI)"),
      label =  c("<b>Lower Granite Dam (LGR)</b>
                 <br> add text about outmigration and
                 <br>which years of data this includes",
                 "",
                 "<b>Bonneville Dam (BON)</b>
                 <br>mmm even keep this dam? or
                 <br>add a diff set of icons for all dams within
                 <br>and highlight which were pulling historical data",
                 "",
                 "<b>Coastal Upwelling Index (CUI)</b>
                 <br>explain CUI and how it's used for forecasting SAR"),
      Lon = c(-117.43626,-117.43323, -121.94624, -121.93653, -125),
      Lat = c(46.66039, 46.65776, 45.64142,45.64979, 45),
      icon_type = c("damJ", "damA","damJ", "damA", "water")
    )


    output$map <- leaflet::renderLeaflet({

      leaflet::leaflet() %>%
        leaflet::setView(lng = -120, lat = 45, zoom = 5) %>%
        leaflet::addTiles(urlTemplate = "https://{s}.basemaps.cartocdn.com/light_all/{z}/{x}/{y}{r}.png",
                          attribution = 'Tiles &copy; <a href="https://www.carto.com/">Carto</a>') %>%
        #add icon for CUI
        leaflet::addAwesomeMarkers(
          data = dplyr::filter(rivers_data, icon_type == "water"),
          lng = ~Lon,
          lat = ~Lat,
          label = ~HTML(label),
          labelOptions = leaflet::labelOptions(noHide = FALSE, direction = "auto", html = TRUE),
          icon = leaflet::awesomeIcons(
            library = "fa",
            icon = "retweet",
            iconColor = "black",
            markerColor = 'lightgray'
          )) %>%
        #add icon for outgoing dams
        leaflet::addAwesomeMarkers(
          data = dplyr::filter(rivers_data, icon_type == "damJ"),
          lng = ~Lon,
          lat = ~Lat,
          label = ~HTML(label), #label is not designated by icontype--look into so not all labels show up per icon
          icon = leaflet::awesomeIcons(
            library = "fa",
            icon = "arrows-h", # change icons to one that works!
            iconColor = "black",
            markerColor = 'lightgray'
          )) %>%
        # #add icon for incoming dams
        # leaflet::addAwesomeMarkers(
        #   data = dplyr::filter(rivers_data, icon_type == "damA"),
        #   lng = ~Lon,
        #   lat = ~Lat,
        #   label = ~Name,
        #   icon = leaflet::awesomeIcons(
        #     library = "fa",
        #     icon = "fa-caret-right",
        #     iconColor = "black",
        #     markerColor = 'lightgray'
        #   )) %>%
        #add crosshairs to reset to setView position
        leaflet::addEasyButton(leaflet::easyButton(
          icon="fa-crosshairs", title="Locate",  onClick = leaflet::JS("function(btn, map) { map.setView([45, -120], 5); }")))

    })

  })
}

## To be copied in the UI
# mod_welcome_submodule_leaflet_map_ui("welcome_page_submodule_leaflet_map_1")

## To be copied in the server
# mod_welcome_submodule_leaflet_map_server("welcome_page_submodule_leaflet_map_1")
