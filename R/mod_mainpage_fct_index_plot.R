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
    "CUTI" = "Coastal Upwelling Transport Index (CUTI),\n45N, monthly[April]\n(vertical volume transport m2 s-1)"
  )


  plot <- plotly::plot_ly(
    data = data,
    x = ~year,
    y = ~upwelling_index,
    type = "scatter",
    mode = "lines+markers",
    marker = list(color = "#024c63"),
    line = list(color = "#024c63")
  ) %>%
    # Set the axis labels
    plotly::layout(
      xaxis = list(title = "Year"),
      yaxis = list(title = title)
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
