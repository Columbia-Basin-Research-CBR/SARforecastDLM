#' forecast_plot
#'
#' @description using DLM via MARSS package, plot the forecasted values and CI for the SAR in the next year
#'
#' @return plotly figure with highlighted portion to show forecasted values versus out-of-sample.
#'
#' @noRd


fct_forecast_compare_plot <- function(data, data_1, years_selected =2000) {
  # Create a plotly plot
  p_plotly <- plotly::plot_ly()

  # Split data by sar.method
  data<-rbind(data, data_1)

  data_list <- split(data, data$dataset)

  # Define colors for different sar.methods
  colors <- c("base_data" = "#024c63", "select_forecast" = "#b47747")

  # Define rectangle parameters for different sar.methods
  rectangle_params <- list(
    "select_forecast" = list(x = c(years_selected, years_selected+3, years_selected+3, years_selected), y = c(0, 0, 6.5, 6.5)),
    "base_data" = list(x = c(2005, 2007, 2007, 2005), y = c(0, 0, 6.5, 6.5)) # Replace "sar.method2" with the actual second sar.method
  )

  # Add traces for each sar.method
  for(i in seq_along(data_list)) {

    # In the loop, use the sar.method name to get the color
    color <- colors[names(data_list[i])]

    p_plotly <- p_plotly %>%
      # Add "forecasted" rectangle
      plotly::add_trace(
        x = rectangle_params[[names(data_list[i])]]$x,
        y = rectangle_params[[names(data_list[i])]]$y,
        fill = "toself",
        fillcolor = color,
        line = list(width = 0),
        opacity = 0.2,
        name = paste("Forecasted,\nout-of-sample -", names(data_list[i])),
        legendgroup = paste("forecasted2", i),
        legendrank = 3,
        type = "scatter",
        mode = "none",
        hoverinfo = "none"
      ) %>%
      # add observed and predicted SAR lines and markers
      plotly::add_markers(
        data = data_list[[i]],
        x = ~year,
        y = ~y,
        name = paste("Observed SAR -", names(data_list[i])),
        legendgroup = paste("observed", i),
        legendrank = 1,
        text = ~ paste("Year of ocean entry:", year, "<br>Observed SAR:", custom_round(y)),
        hoverinfo = "text",
        marker = list(color = "lightgrey", symbol = "circle-open")
      ) %>%
      plotly::add_lines(
        data = data_list[[i]],
        x = ~year,
        y = ~estimate,
        name = paste("Forecasted SAR -", names(data_list[i])),
        legendgroup = paste("forecasted", i),
        legendrank = 2,
        text = ~ paste("Year of ocean entry:", year, "<br>Forecasted SAR:", custom_round(estimate)),
        hoverinfo = "text",
        line = list(color = color),
        hoverinfo = "text"
      ) %>%
      plotly::add_markers(
        data = data_list[[i]],
        x = ~year,
        y = ~estimate,
        name = paste("Forecasted SAR -", names(data_list[i])),
        legendgroup = paste("forecasted", i),
        legendrank = 2,
        text = ~ paste("Year of ocean entry:", year, "<br>Forecasted SAR:", custom_round(estimate)),
        hoverinfo = "text",
        marker = list(size = 6, color = color),
        showlegend = FALSE
      ) %>%
      plotly::add_lines(
        data = data_list[[i]],
        x = ~year,
        y = ~fore_CI_95_upper,
        name = "Upper 95% CI",
        legendgroup = paste("forecasted", i),
        legendrank = 2,
        text = ~ paste("Year of ocean entry:", year, "<br>Forecasted SAR upper 95% CI:", custom_round(fore_CI_95_upper)),
        line = list(
          dash = "dash",
          color = color
        ),
        hoverinfo = "text"
      ) %>%
      plotly::add_lines(
        data = data_list[[i]],
        x = ~year,
        y = ~fore_CI_95_lower,
        name = "Lower 95% CI",
        legendgroup = paste("forecasted", i),
        legendrank = 2,
        text = ~ paste("Year of ocean entry:", year, "<br>Forecasted SAR lower 95% CI:", custom_round(fore_CI_95_lower)),
        line = list(
          dash = "dash",
          color = color
        ),
        hoverinfo = "text"
      )
  }

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
