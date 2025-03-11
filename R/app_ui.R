#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # Your application UI logic
    fluidPage(

      ## Page content
      shinydashboardPlus::dashboardPage(
        header = shinydashboard::dashboardHeader(
          title = HTML("SARforecastDLM <small style ='font-size:0.6em;'>BetaVersion.Mar25</small>"),
          # Add a logo to the header
          tags$li(
            class = "dropdown header-img",
            tags$style(HTML("
              .header-img {
                float: right;
                padding-right: 10px;
              }
            ")),
            tags$img(src = "www/cbr_logo_horiz.png", height = "50px")
          )
        ),

        ## Sidebar content - used as a navigation menu to each tab
        sidebar = shinydashboard::dashboardSidebar(

          #override theme for sidepanel selectInput color
          shiny::includeCSS(system.file("app/www/theme.css", package = "SARforecastDLM")),

          shinydashboard::sidebarMenu(
            # Setting id makes input$tabs give the tabName of currently-selected tab
            id = "tabs",
            shinydashboard::menuItem("About", tabName = "about", icon = icon("house")),
            shinydashboard::menuItem("Forecast Survival", tabName = "figs", icon = icon("chart-line"), selected = TRUE),
            shinydashboard::menuItem("Supplementary Information", tabName = "bkg", icon = icon("book"))
          )
        ),

        ## Body content
        body = shinydashboard::dashboardBody(

          #add CSS CBR global theme
          fresh::use_theme(CBRtheme),

          #set tab naming with associated modules
          shinydashboard::tabItems(
            shinydashboard::tabItem(tabName = "about",mod_about_page_ui("about_page_ui_1")),
            shinydashboard::tabItem(tabName = "figs", mod_mainpage_ui("mainpage_1")),
            shinydashboard::tabItem(tabName = "bkg", mod_supplementary_page_ui("supplementary_page_1"))
          )
        ),
        footer = shinydashboardPlus::dashboardFooter(
          left = HTML(
            '<div style="color: #024c63;">
       <a href="https://cbr.washington.edu/" target="_blank" style="color: #024c63; text-decoration: none;" onmouseover="this.style.textDecoration=\'underline\';" onmouseout="this.style.textDecoration=\'none\';">Columbia Basin Research</a> •
       <a href="https://safs.uw.edu/" target="_blank" style="color: #024c63; text-decoration: none;" onmouseover="this.style.textDecoration=\'underline\';" onmouseout="this.style.textDecoration=\'none\';">School of Aquatic and Fishery Sciences</a> •
       <a href="https://environment.uw.edu/" target="_blank" style="color: #024c63; text-decoration: none;" onmouseover="this.style.textDecoration=\'underline\';" onmouseout="this.style.textDecoration=\'none\';">College of the Environment</a> •
       <a href="https://www.washington.edu/" target="_blank" style="color: #024c63; text-decoration: none;" onmouseover="this.style.textDecoration=\'underline\';" onmouseout="this.style.textDecoration=\'none\';">University of Washington</a>
     </div>'
          ),
          right = HTML(
            '<span class="footer-contact" style="color: #024c63;">
       <a href="mailto:web@cbr.washington.edu" target="_blank" style="color: #024c63; text-decoration: none;" onmouseover="this.style.textDecoration=\'underline\';" onmouseout="this.style.textDecoration=\'none\';">
         <i class="fa fa-envelope" style="color: #024c63;"></i> web@cbr.washington.edu
       </a>
       &nbsp;&nbsp;
       <a href="https://github.com/Columbia-Basin-Research-CBR/SARforecastDLM.git"
          target="_blank" style="color: #024c63; text-decoration: none;" onmouseover="this.style.textDecoration=\'underline\';" onmouseout="this.style.textDecoration=\'none\';">
         <i class="fa fa-github" style="color: #024c63;"></i> github.com/Columbia-Basin-Research-CBR
       </a>
     </span>'
          )
        )
      )
    )
  )
}


#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path(
    "www",
    app_sys("app/www")
  )

  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "SARforecastDLM"
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
