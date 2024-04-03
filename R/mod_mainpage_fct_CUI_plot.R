#' CUI_plot
#'
#' @description NOAA upwelling index (CUI) plot
#'
#' @return  plotly figure of CUI from 1964 to present
#'
#' @noRd
#'


fct_CUI_plot <- function(data) {



  plot <- plotly::plot_ly(data = data,
                        x = ~years,
                        y = ~CUI,
                        type = 'scatter',
                        mode = 'lines+markers',
                        marker = list(color = '#024c63'),
                        line = list(color = '#024c63')) %>%
    # Set the axis labels
    plotly::layout( xaxis = list(title = "Year",
                         tickmode = "array",
                         tickvals = seq(1964, 2024, 5)),
                    yaxis = list(title = "Coastal upwelling index\n(m^3/seawater/100 m of coastline)"))


  # plot<-plotly::ggplotly(
  #   ggplot2::ggplot(data = data, ggplot2::aes(x = years, y = CUI)) +
  #     ggplot2::geom_point(color = "#024c63") +
  #     ggplot2::geom_line(color = "#024c63" ) +
  #     ggplot2::labs(
  #          x = "Year",
  #          y = "Coastal upwelling index\n(m^3/seawater/100 m of coastline)")+
  #     ggplot2::scale_x_continuous(breaks = seq(1964, 2024, 5)) +
  #     ggplot2::theme_light() +
  #     ggplot2::theme(
  #                    panel.grid = ggplot2::element_blank(),
  #                    axis.line.x.top = ggplot2::element_blank(),
  #                    axis.line.y.right = ggplot2::element_blank(),
  #                   )
  # )

  plot<- plotly::config(plot,
                             displayModeBar = TRUE,
                             modeBarButtonsToRemove = list("zoom2d",
                                                           "autoScale2d",
                                                           "lasso2d",
                                                           "select2d",
                                                           "hoverClosestCartesian",
                                                           "hoverCompareCartesian"
                                                           )
                             )


  return(plot)
}

