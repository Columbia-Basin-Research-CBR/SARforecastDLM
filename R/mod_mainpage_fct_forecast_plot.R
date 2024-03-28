#' forecast_plot
#'
#' @description using DLM via MARSS package, plot the forecasted values and CI for the SAR in the next year
#'
#' @return plotly figure with highlighted portion to show forecasted values versus out-of-sample.
#'
#' @noRd


fct_forecast_plot <-function(data){


# Create the plot

p<-ggplot2::ggplot(data, ggplot2::aes(x = years)) +
  ggplot2::geom_point(ggplot2::aes(y = sar.obs), color = "#b47747") +
  ggplot2::geom_line(ggplot2::aes(y = fore_mean)) +
  ggplot2::geom_line(ggplot2::aes(y = fore_mean + 2 * sqrt(fore_var)), linetype = "dashed") +
  ggplot2::geom_line(ggplot2::aes(y = fore_mean - 2 * sqrt(fore_var)), linetype = "dashed") +
  ggplot2::scale_x_continuous(breaks = seq(1964, 2024, 10)) +
  ggplot2::scale_linetype_manual(values = c("solid", "dashed"),
                        breaks = c("Forecasted logit.sar", "95% CI")) +
  ggplot2::labs(x = "Year of ocean entry",
       y = "Logit(SAR)") +
  # annotate("rect", xmin = 2021, xmax = 2023, ymin = -Inf, ymax = Inf, fill = "grey", alpha = 0.2) +
  ggplot2::theme_classic() +
  ggplot2::theme(axis.line = ggplot2::element_line(color = "black"))

# Convert to plotly
p_plotly<-plotly::ggplotly(p)

# Add rectangle
p_plotly <- p_plotly %>%
  plotly::add_trace(
    x = c(2021, 2024, 2024, 2021),
    y = c(-8.5, -8.5, -1.8, -1.8),
    fill = "toself",
    fillcolor = "grey",
    line = list(width = 0),
    opacity = 0.2,
    showlegend = FALSE,
    type = "scatter",
    mode = "none"
  )

# Return the plot
return(p_plotly)

}
