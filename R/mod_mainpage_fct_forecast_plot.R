#' forecast_plot
#'
#' @description using DLM via MARSS package, plot the forecasted values and CI for the SAR in the next year
#'
#' @return plotly figure with highlighted portion to show forecasted values versus out-of-sample.
#'
#' @noRd


fct_forecast_plot <- function(data) {
  # Create a plotly plot
  p_plotly <- plotly::plot_ly()

  # Define color for Scheuerell and Williams (2005) method
  color <- "#024c63"

  # Define rectangle parameters for Scheuerell and Williams (2005) method
  # rectangle_params <- list(x = c(2005, 2006, 2006, 2005), y = c(0, 0, 6.5, 6.5))

  # # Add "forecasted" rectangle
  # p_plotly <- p_plotly %>%
  #   plotly::add_trace(
  #     x = rectangle_params$x,
  #     y = rectangle_params$y,
  #     fill = "toself",
  #     fillcolor = color,
  #     line = list(width = 0),
  #     opacity = 0.2,
  #     name ="Forecasted SAR,\nout-of-sample",
  #     legendgroup = "forecasted2",
  #     legendrank = 3,
  #     type = "scatter",
  #     mode = "none",
  #     hoverinfo = "none"
  #   )

  # Add observed and predicted SAR lines and markers
  p_plotly <- p_plotly %>%
    plotly::add_markers(
      data = data,
      x = ~year,
      y = ~y,
      name = "Observed SAR,\nScheuerell & Williams (2005)",
      legendgroup = "observed",
      legendrank = 1,
      text = ~ paste("Year of ocean entry:", year, "<br>Observed SAR:", custom_round(y)),
      hoverinfo = "text",
      marker = list(color = color, symbol = "circle-open")
    ) %>%
    plotly::add_lines(
      data = data,
      x = ~year,
      y = ~estimate,
      name = "Forecasted SAR",
      legendgroup = "forecasted",
      legendrank = 2,
      text = ~ paste("Year of ocean entry:", year, "<br>Forecasted SAR (%):", custom_round(estimate)),
      hoverinfo = "text",
      line = list(color = color),
      hoverinfo = "text"
    ) %>%
    plotly::add_markers(
      data = data,
      x = ~year,
      y = ~estimate,
      name = "Forecasted SAR",
      legendgroup = "forecasted",
      legendrank = 2,
      text = ~ paste("Year of ocean entry:", year, "<br>Forecasted SAR (%):", custom_round(estimate)),
      hoverinfo = "text",
      marker = list(size = 6, color = color),
      showlegend = FALSE
    ) %>%
    plotly::add_lines(
      data = data,
      x = ~year,
      y = ~fore_CI_95_upper,
      name = "Upper 95% CI",
      legendgroup = "forecasted",
      legendrank = 2,
      text = ~ paste("Year of ocean entry:", year, "<br>Forecasted SAR upper 95% CI:", custom_round(fore_CI_95_upper)),
      line = list(
        dash = "dash",
        color = color
      ),
      hoverinfo = "text"
    ) %>%
    plotly::add_lines(
      data = data,
      x = ~year,
      y = ~fore_CI_95_lower,
      name = "Lower 95% CI",
      legendgroup = "forecasted",
      legendrank = 2,
      text = ~ paste("Year of ocean entry:", year, "<br>Forecasted SAR lower 95% CI:", custom_round(fore_CI_95_lower)),
      line = list(
        dash = "dash",
        color = color
      ),
      hoverinfo = "text"
    )

  p_plotly <- p_plotly %>%
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
                               "lasso2d"
                             )
  )

  # Return the plot
  return(p_plotly)
}
