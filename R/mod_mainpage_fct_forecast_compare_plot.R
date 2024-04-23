#' forecast_plot
#'
#' @description using DLM via MARSS package, plot the forecasted values and CI for the SAR in the next year
#'
#' @return plotly figure with highlighted portion to show forecasted values versus out-of-sample.
#'
#' @noRd


fct_forecast_compare_plot <- function(data_base, data_select, years_selected) {
  # Create a plotly plot
  p_plotly <- plotly::plot_ly()

  data_base <-data_base %>%
    dplyr::filter(between(year, min(year), 2005))

  # Split data by sar.method
  data<-rbind(data_base, data_select)

  data_list <- split(data, data$dataset)

  # Define colors for base vs forecast selected
  colors <- c("base_forecast" = grDevices::rgb(2/255, 76/255, 99/255, 0.25), "select_forecast" = "#b47747") #setting alpha = .5 for color "#024c63"-RGB=(2, 76, 99), convert to scale 0-1 by /255 to set alpha to .25.

  # Define rectangle parameters
  rectangle_params <- list(
    "select_forecast" = list(x = c(years_selected, years_selected+3, years_selected+3, years_selected), y = c(0, 0, 6.5, 6.5)) #adjust 3 to whatever # is predicted
    # "base_forecast" = list(x = c(2005, 2007, 2007, 2005), y = c(0, 0, 6.5, 6.5)) # Replace "sar.method2" with the actual second sar.method
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
        data = data_base, #data_list[[i]],
        x = ~year,
        y = ~y,
        name = "Observed SAR",
        legendgroup = "observed",
        legendrank = 1,
        text = ~ paste("Year of ocean entry:", year, "<br>Observed SAR:", custom_round(y)),
        hoverinfo = "text",
        marker = list(color = "darkgrey", symbol = "circle-open")
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
