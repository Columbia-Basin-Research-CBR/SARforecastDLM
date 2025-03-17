#' index_plot
#'
#' @description Upwelling index plot, either CUI or CUTI
#'
#' @return  plotly figure of index from X to present
#'
#' @noRd
#'


fct_index_plot <- function(data) {
  title <- switch(data$index[1],
    "CUI" = "Coastal Upwelling Index (CUI),\n45N 125W, monthly[April]\n(m^3/seawater/100 m of coastline)",
    "CUTI" = "Coastal Upwelling Transport Index (CUTI),\n45N, monthly[April]\n(vertical volume transport m2 s-1)",
    "ICPB" = "Index of Coastal Prey Biomass (ICPB),\n44.65N, monthly[January, February, March]\n(log(mg C per 1000 m^3))",
    "NCBI" = "Northern Copepod Biomass Index (NCBI),\n44.65N, 124.18W, monthly[April]\n(log(mg C per 1000 m^3))",
  )

  # data_mean <- data %>%
  #   dplyr::group_by(year) %>%
  #   dplyr::summarise(value = mean(value))


  plot <- plotly::plot_ly(
    data = data,
    x = ~year,
    y = ~value,
    type = "scatter",
    mode = "lines+markers",
    marker = list(color = "#024c63"),
    line = list(color = "#024c63")
  ) %>%
    # Set the axis labels
    plotly::layout(
      xaxis = list(title = "Year"),
      yaxis = list(title = title),
      margin = list(l = 100, r = 50, b = 50, t = 50, pad = 4)
    )


  plot <- plotly::config(plot,
    displayModeBar = TRUE,
    modeBarButtonsToRemove = list(
      "zoom2d",
      "autoScale2d",
      "lasso2d",
      "hoverClosestCartesian",
      "hoverCompareCartesian"
    )
  )
  return(plot)
}


# p<-plot_data_1 %>%
#   filter(sar.method == "DART",
#          index == "CUTI"
#           )
# fct_index_plot(p)
