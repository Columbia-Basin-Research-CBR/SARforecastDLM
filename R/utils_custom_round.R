#' custom_round
#'
#' @description A utils function used to adjust rounding of SAR values lower than 0.00 in plotly figures.
#'
#' @return If the rounded value is 0.00, the function will increase the number of digits to 4 decimals.
#'
#' @noRd


custom_round <- function(x, digits1 = 2, digits2 = 4) {
  #round further if the number is 0
  rounded <- ifelse(round(x, digits1) == 0, round(x, digits2), round(x, digits1))
  #added to prevent plotly from using scientific notation in tooltip--uses sprintf to format the number as a string
  ifelse(rounded == round(x, digits2), sprintf(paste0("%.", digits2, "f"), rounded), sprintf(paste0("%.", digits1, "f"), rounded))
}
