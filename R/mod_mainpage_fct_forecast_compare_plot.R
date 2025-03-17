#' forecast_plot
#'@param data_base data frame generated via reactive in mod_mainpage_server with observed SAR data
#'@param data_select data frame generated from model run with forecasted SAR data
#'@param years_selected years of interest used to forecast SAR compared to the data_base (all years)
#' @description using DLM via MARSS package, plot the forecasted values and CI for the SAR in the next year
#'
#' @return plotly figure with highlighted portion to show forecasted values versus out-of-sample.
#'
#' @noRd


fct_forecast_compare_plot <- function(data_base, data_select, years_selected) {
 #next step look into removing last year of data_base for first plot
  data_base <-data_base %>%
    dplyr::filter(dplyr::between(year, min(year), max(year)))

  # Split data by sar.method
  data<-rbind(data_base, data_select)

  data_list <- split(data, data$dataset)

  # Define colors for base vs forecast selected
  colors <- c("base_forecast" = grDevices::rgb(2/255, 76/255, 99/255, 0.25), "select_forecast" = "#b47747") #setting alpha = .5 for color "#024c63"-RGB=(2, 76, 99), convert to scale 0-1 by /255 to set alpha to .25.

  # Create a plotly plot
  p_plotly <- plotly::plot_ly()


  # Add "Observed SAR" plot
  p_plotly <- p_plotly %>%
    plotly::add_markers(
      data = data_list[["base_forecast"]],
      x = ~year,
      y = ~y,
      name = "Observed SAR",
      legendgroup = "observed",
      legendrank = 1,
      text = ~ paste("Year of ocean entry:", year, "<br>Observed SAR (%):", custom_round(y)),
      hoverinfo = "text",
      marker = list(color = "#024c63", symbol = "circle-open")
    )
  if (max(years_selected) != max(data_base$year)-1){

    #define color outside loop for rectangle
    color <- colors[names(data_list[2])]

  # Add traces for each sar.method
  for(i in seq_along(data_list)) {

    #In the loop, regenerate color for each forecasted SAR (base and selected)
    color <- colors[names(data_list[i])]

      p_plotly<- p_plotly %>%
        plotly::add_lines(
        data = data_list[[i]],
        x = ~year,
        y = ~estimate,
        name = ifelse(names(data_list[i]) == "base_forecast", paste("Predicted SAR,\ninc.", min(data_base$year),":",max(data_base$year)-1), paste("Predicted SAR,\ninc.", min(data_select$year),":",max(data_select$year-1))),
        legendgroup = paste("forecasted", i),
        legendrank = 2,
        text = ~ paste("Year of ocean entry:", year, "<br>Predicted SAR (%):", custom_round(estimate)),
        hoverinfo = "text",
        line = list(color = color),
        hoverinfo = "text"
      ) %>%
      plotly::add_markers(
        data = data_list[[i]],
        x = ~year,
        y = ~estimate,
        name = ifelse(names(data_list[i]) == "base_forecast", paste("Predicted SAR,\ninc.", min(data_base$year),":",max(data_base$year)-1),  paste("Predicted SAR,\ninc.", min(data_select$year),":",max(data_select$year-1))),
        legendgroup = paste("forecasted", i),
        legendrank = 2,
        text = ~ paste("Year of ocean entry:", year, "<br>Predicted SAR (%):", custom_round(estimate)),
        hoverinfo = "text",
        marker = list(size = 6, color = color),
        showlegend = FALSE
      ) %>%
      plotly::add_lines(
        data = data_list[[i]],
        x = ~year,
        y = ~hi_95,
        name = "Upper 95% CI",
        legendgroup = paste("forecasted", i),
        legendrank = 2,
        text = ~ paste("Year of ocean entry:", year, "<br>Predicted SAR upper 95% CI:", custom_round(hi_95)),
        line = list(
          dash = "dash",
          color = color
        ),
        hoverinfo = "text"
      ) %>%
      plotly::add_lines(
        data = data_list[[i]],
        x = ~year,
        y = ~lo_95,
        name = "Lower 95% CI",
        legendgroup = paste("forecasted", i),
        legendrank = 2,
        text = ~ paste("Year of ocean entry:", year, "<br>Predicted SAR lower 95% CI:", custom_round(lo_95)),
        line = list(
          dash = "dash",
          color = color
        ),
        hoverinfo = "text"
      )

      # Filter the last year of data
      last_year_data <- dplyr::filter(data_list[[i]], year == max(year))

      p_plotly <- p_plotly %>%
        # Add point range for the last year of data
        plotly::add_trace(
          data = last_year_data,
          x = ~year,
          y = ~estimate,
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
          name = "Forecasted SAR,\nout-of-sample",
          legendgroup = paste("forecasted", i),
          legendrank = 3,
          mode = "none",
          hoverinfo = "none"
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

  # Return the plot comparing selected forecast with base forecast
  return(p_plotly)

  } else {

    color<-"#024c63"

    p_plotly <- p_plotly %>%
      plotly::add_lines(
        data = data_list[["base_forecast"]],
        x = ~year,
        y = ~estimate,
        name = paste("Predicted SAR,\ninc.", min(data_base$year), "to", max(data_base$year)-1, "SAR"),
        legendgroup = "base_forecast",
        legendgroup = "base_forecast",
        legendrank = 2,
        text = ~ paste("Year of ocean entry:", year, "<br>Predicted SAR (%):", custom_round(estimate)),
        hoverinfo = "text",
        line = list(color = color),
        hoverinfo = "text"
      ) %>%
      plotly::add_markers(
        data = data_list[["base_forecast"]],
        x = ~year,
        y = ~estimate,
        name = paste("Predicted SAR,\ninc.", min(data_base$year), "to", max(data_base$year)-1, "SAR"),
        legendgroup = "base_forecast",
        legendgroup = "base_forecast",
        legendrank = 2,
        text = ~ paste("Year of ocean entry:", year, "<br>Predicted SAR (%):", custom_round(estimate)),
        hoverinfo = "text",
        marker = list(size = 6, color = color),
        showlegend = FALSE
      ) %>%
      plotly::add_lines(
        data = data_list[["base_forecast"]],
        x = ~year,
        y = ~hi_95,
        name = "Upper 95% CI",
        legendrank = 2,
        legendgroup = "base_forecast",
        text = ~ paste("Year of ocean entry:", year, "<br>Predicted SAR upper 95% CI:", custom_round(hi_95)),
        line = list(
          dash = "dash",
          color = color
        ),
        hoverinfo = "text"
      ) %>%
      plotly::add_lines(
        data = data_list[["base_forecast"]],
        x = ~year,
        y = ~lo_95,
        name = "Lower 95% CI",
        legendrank = 2,
        legendgroup = "base_forecast",
        text = ~ paste("Year of ocean entry:", year, "<br>Predicted SAR lower 95% CI:", custom_round(lo_95)),
        line = list(
          dash = "dash",
          color = color
        ),
        hoverinfo = "text"
      )

      # Filter the last year of data
      last_year_data <- dplyr::filter(data_list[["base_forecast"]], year == max(year))

      p_plotly <- p_plotly %>%
        # Add point range for the last year of data
        plotly::add_trace(
          data = last_year_data,
          x = ~year,
          y = ~estimate,
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
          name = "Forecasted SAR,\nout-of-sample",
          legendgroup = "forecasted",
          legendrank = 3,
          mode = "none",
          hoverinfo = "none"
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
    #return base forecasted plot without any transparency in color.
    return(p_plotly)
  }
}
