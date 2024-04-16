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


    #dam sites and transportation filter
    rivers_data <-data.frame(
      Name = c("Lower Granite\n(LGR)","Adult returns","Transport", "Little Goose", "Lower Monumental", "Ice Harbor", "McNary", "John Day", "The Dalles", "Bonneville\n(BON)", "Outmigrating detections", "Transport", "adult return"),
      Lat = c(46.66053259552463,46.66053259552463,46.67080357578456, 46.587531484954575, 46.56343049721032, 46.249736092987355, 45.93405510635421,45.71499679473759, 45.614135364912876, 45.64441868254798, 45.64441868254798, 45.652536297369814, 45.64441868254798),
      Lon = c(-117.42737011713677,-117.42737011713677,-117.42019252542396 , -118.0260163871114,-118.54315348896209, -118.87984211781144,  -119.29757721597544,-120.69368083132734,-121.13417428900306,  -121.94060471598787, -121.94060471598787, -121.94060202780263, -121.94060471598787),
      passtype = c(0,0,1,0,0,0,0,0,0,0,0,1,0),
      direction = c(0,2,0,0,0,0,0,0,0,0,1,1,2),
      detection = c(0,1,1,0,0,0,0,0,0,0,0,0,0)
    )



    output$map <- leaflet::renderLeaflet({

      leaflet::leaflet() %>%
        leaflet::setView(lng = -119, lat = 46, zoom = 7) %>%
        leaflet::addTiles(urlTemplate = "https://{s}.basemaps.cartocdn.com/light_all/{z}/{x}/{y}{r}.png",
                          attribution = 'Tiles &copy; <a href="https://www.carto.com/">Carto</a>') %>%
        leaflet::addCircles(
          lat = 45,
          lng = -125,
          fillColor = "#024c63",
          # stroke = FALSE,
          radius = 100000,
          popup = "Upwelling Indices: CUI/CUTI"
        ) %>%

        #outgoing- in-river with
        #highlight detection sites for outgoing
        leaflet::addCircleMarkers(
          data = dplyr::filter(rivers_data, direction %in% c("1", "0") & detection == 1),
          lng = ~Lon,
          lat = ~Lat+.1,
          label = ~Name,
          color = 'darkred',
          fill = TRUE,
          fillOpacity = 1,
          radius = 5
        ) %>%
        # #outgoing - in-river
        leaflet::addPolylines(data = dplyr::filter(rivers_data, passtype == 0 & direction %in% c("1", "0")), lng = ~Lon, lat = ~Lat+.1, color = "grey") %>%
        # outgoing - transport
        leaflet::addPolylines(data = dplyr::filter(rivers_data, passtype == 1), lng = ~Lon, lat = ~Lat+.1, color = "grey") %>%
        #add specific labels for icons (outgoing)
        ##LGR
        leaflet::addAwesomeMarkers(
          lng = -117.42737011713677,
          lat = 46.66053259552463+.1,
          label = HTML("<div style='background-color: lightgrey;width: 200px; white-space: normal;'><b>Juveniles entering the hydrosystem</b><br>
                 All fish (Steelhead and spring/summer Chinook) originated upstream of LGR,
                 with a portion transported via barges from LGR to a release site below BON,
                expediting travel time through the hydrosystem, whereas the remaining travel in-river."),
          labelOptions = leaflet::labelOptions(noHide = FALSE, direction = "auto", html = TRUE),
          icon = leaflet::awesomeIcons(
            library = "fa",
            icon = "arrow-left", # change icons to one that works!
            iconColor = "black",
            markerColor = 'lightgray'
          )) %>%
        ##BON
        leaflet::addAwesomeMarkers(
          lng = -121.94060471598787,
          lat = 45.64441868254798+.1,
          label = HTML("<div style='background-color: lightgrey; width: 200px; white-space: normal;'><b>Juvenile entering the estuary and ocean</b><br>
                 Day-of-year (DOY) and river temperature are assigned based on outmigration detection at BON,
                 representing migration timing and seasonal changes experienced during outmigration, possible indicators of juveniles surviving to maturity. </div>"
          ),
          labelOptions = leaflet::labelOptions(noHide = FALSE, direction = "auto", html = TRUE, backgroundColor = "lightgrey"),
          icon = leaflet::awesomeIcons(
            library = "fa",
            icon = "arrow-left", # change icons to one that works!
            iconColor = "black",
            markerColor = 'lightgray'
          )) %>%
        #base dam sites
        leaflet::addCircleMarkers(
          data = dplyr::filter(rivers_data, passtype == 0 & direction %in% c("1", "0") & detection == 0),
          lng = ~Lon,
          lat = ~Lat,
          label = ~Name,
          color = 'darkgrey',
          fill = TRUE,
          fillColor = "black",
          fillOpacity = 1,
          radius = 4
        ) %>%
        #highlight detection sites
        leaflet::addCircleMarkers(
          data = dplyr::filter(rivers_data, passtype == 0 & direction == 2 & detection == 1),
          lng = ~Lon,
          lat = ~Lat-.1,
          label = ~Name,
          color = 'darkred',
          fill = TRUE,
          fillOpacity = 1,
          radius = 5
        ) %>%
        #add specific labels for icons (outgoing)
        ##LGR
        leaflet::addAwesomeMarkers(
          lng = -117.42737011713677,
          lat = 46.66053259552463-.1,
          label = HTML("<div style='width: 200px; white-space: normal;'><b>S Survival-to-Adult (SAR) & Transported to Bypassed fish survival ratio (T:B)</b><br>
                 To assess the effectiveness of the transportation program,  TIE into earlier points.</div>"
          ),
          labelOptions = leaflet::labelOptions(noHide = FALSE, direction = "auto", html = TRUE),
          icon = leaflet::awesomeIcons(
            library = "fa",
            icon = "arrow-right", # change icons to one that works!
            iconColor = "black",
            markerColor = 'white'
          )) %>%
        ##BON
        leaflet::addAwesomeMarkers(
          lng = -121.94060471598787,
          lat = 45.64441868254798-.1,
          label = HTML("<div style='width: 200px; white-space: normal;'><b>Adult returns</b><br>
                 IDK what goes here? talk about gosselin paper and PDO effects"),
          labelOptions = leaflet::labelOptions(noHide = FALSE, direction = "auto", html = TRUE),
          icon = leaflet::awesomeIcons(
            library = "fa",
            icon = "arrow-right", # change icons to one that works!
            iconColor = "black",
            markerColor = 'white'
          )) %>%
        leaflet::addPolylines(data = dplyr::filter(rivers_data, passtype == 0 & direction %in% c("2", "0")), lng = ~Lon, lat = ~Lat-.10, color = "black") %>%
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
