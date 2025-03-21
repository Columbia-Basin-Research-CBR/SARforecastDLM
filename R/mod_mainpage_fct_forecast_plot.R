#' forecast_plot
#'
#' @description using DLM via MARSS package, plot the forecasted values and CI for the SAR in the next year
#'
#' @return plotly figure with highlighted portion to show forecasted values versus one-step ahead.
#'
#' @noRd


fct_forecast_plot <- function(data) {
  # Create a plotly plot
  p_plotly <- plotly::plot_ly()

  # Define color for Scheuerell and Williams (2005) method
  color <- "#024c63"

  reach_value <- if(!is.null(data) && "reach" %in% names(data) && nrow(data) > 0 ) {
    if(unique(data$sar.method)[1] != "Scheuerell and Williams (2005)") {
      paste(",",unique(data$reach)[1])  # Get first unique value
    } else { NULL }
  }

  print(reach_value)

  # Add observed and predicted SAR lines and markers
  p_plotly <- p_plotly %>%
    plotly::add_markers(
      data = data,
      x = ~year,
      y = ~y,
      name = "Observed SAR",
      legendgroup = "observed",
      legendrank = 1,
      text = ~ paste("Year of ocean entry:", year, "<br>Observed SAR", paste0("(",data$reach,"):"),custom_round(y),"%"),
      hoverinfo = "text",
      marker = list(color = color, symbol = "circle-open")
    ) %>%
    plotly::add_lines(
      data = data,
      x = ~year,
      y = ~estimate,
      name = paste("Forecasted SAR,\ninc.", min(data$year), "to", max(data$year)-1, "SAR"),
      legendgroup = "forecasted",
      legendrank = 2,
      text = ~ paste("Year of ocean entry:", year, "<br>Forecasted SAR", paste0("(",data$reach,"):"),custom_round(estimate),"%"),
      hoverinfo = "text",
      line = list(color = color),
      hoverinfo = "text"
    ) %>%
    plotly::add_markers(
      data = data,
      x = ~year,
      y = ~estimate,
      name = paste("Forecasted SAR,\ninc.", min(data$year), "to", max(data$year)-1, "SAR"),
      legendgroup = "forecasted",
      legendrank = 2,
      text = ~ paste("Year of ocean entry:", year, "<br>Forecasted SAR", paste0("(",data$reach,"):"),custom_round(estimate),"%"),
      hoverinfo = "text",
      marker = list(size = 6, color = color),
      showlegend = FALSE
    ) %>%
    plotly::add_lines(
      data = data,
      x = ~year,
      y = ~hi_95,
      name = "Upper 95% CI",
      legendgroup = "forecasted",
      legendrank = 2,
      text = ~ paste("Year of ocean entry:", year, "<br>Forecasted SAR", paste0("(",data$reach,")"), "upper 95% CI:", custom_round(hi_95)),
      line = list(
        dash = "dash",
        color = color
      ),
      hoverinfo = "text"
    ) %>%
    plotly::add_lines(
      data = data,
      x = ~year,
      y = ~lo_95,
      name = "Lower 95% CI",
      legendgroup = "forecasted",
      legendrank = 2,
      text = ~ paste("Year of ocean entry:", year, "<br>Forecasted SAR", paste0("(",data$reach,")"), "lower 95% CI:", custom_round(lo_95)),
      line = list(
        dash = "dash",
        color = color
      ),
      hoverinfo = "text"
    )

  # Filter the last year of data
  last_year_data <- dplyr::filter(data, year == max(year))

  p_plotly <- p_plotly %>%
    # Add point range for the last year of data
    plotly::add_trace(
      data = last_year_data,
      x = ~year,
      y = ~estimate,
      type = "scatter",
      # error_y = list(
      #   arrayminus = ~estimate - lo_95,
      #   array = ~hi_95 - estimate,
      #   width = 1,
      #   thickness = 1.5,
      #   color = color
      # ),
      mode = "lines+markers",
      marker = list(
        size = 10,
        color = color,
        symbol = "star-square"  # Change shape of point to cross
      ),
      line = list(color = color),
      name = "Forecasted SAR,\none-step ahead",
      # legendgroup = "forecasted2",
      legendrank = 2,
      mode = "none",
      hoverinfo = "none"
    )


  p_plotly <- p_plotly %>%
    plotly::layout(
      xaxis = list(title = "Year of ocean entry"),
      yaxis = list(title = paste0("Smolt-to-adult survival", reach_value ,"\n(%)")),
      hovermode = "closest",
      margin = list(b = 80)
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
