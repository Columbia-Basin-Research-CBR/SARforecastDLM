#' forecast_plot
#'
#' @description using DLM via MARSS package, plot the forecasted values and CI for the SAR in the next year
#'
#' @return plotly figure with highlighted portion to show forecasted values versus out-of-sample.
#'
#' @noRd


fct_forecast_plot <- function(data) {
  # Create a plotly plot
  p_plotly <- plotly::plot_ly() %>%
    # Add "forecasted" rectangle
    plotly::add_trace(
      x = c(2021, 2024, 2024, 2021),
      y = c(0, 0, .13, .13), # look into making adjustable based on index selected
      fill = "toself",
      fillcolor = "grey",
      line = list(width = 0),
      opacity = 0.2,
      # showlegend = FALSE,
      name = "Forecasted,\nout-of-sample",
      legendgroup = "forecasted2",
      legendrank = 3,
      type = "scatter",
      mode = "none",
      hoverinfo = "none"
    ) %>%
    # add observed and predicted SAR lines and markers
    plotly::add_markers(
      data = data,
      x = ~years,
      y = ~sar.obs,
      name = "Observed SAR",
      legendgroup = "observed",
      legendrank = 1,
      text = ~ paste("Year of ocean entry:", years, "<br>Observed SAR:", custom_round(sar.obs)),
      hoverinfo = "text",
      marker = list(color = "#b47747")
    ) %>%
    plotly::add_lines(
      data = data,
      x = ~years,
      y = ~fore_mean_raw,
      name = "Forecasted SAR",
      legendgroup = "forecasted",
      legendrank = 2,
      text = ~ paste("Year of ocean entry:", years, "<br>Forecasted SAR:", custom_round(fore_mean_raw)),
      hoverinfo = "text",
      line = list(color = "black"),
      hoverinfo = "text"
    ) %>%
    plotly::add_markers(
      data = data,
      x = ~years,
      y = ~fore_mean_raw,
      name = "Forecasted SAR",
      legendgroup = "forecasted",
      legendrank = 2,
      text = ~ paste("Year of ocean entry:", years, "<br>Forecasted SAR:", custom_round(fore_mean_raw)),
      hoverinfo = "text",
      marker = list(size = 6, color = "black"),
      showlegend = FALSE
    ) %>%
    plotly::add_lines(
      data = data,
      x = ~years,
      y = ~fore_var_upper,
      name = "Upper 95% CI",
      legendgroup = "forecasted",
      legendrank = 2,
      text = ~ paste("Year of ocean entry:", years, "<br>Forecasted SAR upper 95% CI:", custom_round(fore_var_upper)),
      line = list(
        dash = "dash",
        color = "black"
      ),
      hoverinfo = "text"
    ) %>%
    plotly::add_lines(
      data = data,
      x = ~years,
      y = ~fore_var_lower,
      name = "Lower 95% CI",
      legendgroup = "forecasted",
      legendrank = 2,
      text = ~ paste("Year of ocean entry:", years, "<br>Forecasted SAR lower 95% CI:", custom_round(fore_var_lower)),
      line = list(
        dash = "dash",
        color = "black"
      ),
      hoverinfo = "text"
    ) %>%
    plotly::layout(
      xaxis = list(title = "Year of ocean entry"),
      yaxis = list(title = "Smolt-to-adult survival \n(%)"),
      hovermode = "closest"
    )

  p_plotly <- plotly::config(p_plotly,
    displayModeBar = TRUE,
    modeBarButtonsToRemove = list(
      "zoom2d",
      "autoScale2d",
      "lasso2d",
      "select2d"
    )
  )

  # Return the plot
  return(p_plotly)
}
