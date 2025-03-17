#' mainpage_fct_model_forecast
#'
#' @description A fct that runs the DLM with selected years
#' @param data pre-generated data frame with all SAR and covariate data `data/sar_raw_data.rda`. Filtered to specific selection in function.
#' @param years_selected maximum year selected via UI slider input to re-run model from min year to selected max year. Single value.
#' @param index_selected user selected index from UI dropdown. Single value.
#' @param sar_method_selected user selected SAR method from UI dropdown. Single value.
#' @return returns a dataframe with forecasted values (+3) using the input years
#'
#' @noRd
fct_forecast_model<-function(data, paramlist, years_selected, index_selected, sar_method_selected, reach_selected){
 print(years_selected)

  paramlist <- paramlist %>%
    dplyr::filter(
                  index == index_selected,
                  sar.method == sar_method_selected,
                  reach == reach_selected
                  )

  all_data<- data %>%
    dplyr::filter(
                  index == index_selected,
                  sar.method == sar_method_selected,
                  reach == reach_selected
                  )

  train_data<- all_data %>%
    dplyr::filter(dplyr::between(year, min(year),max(years_selected)),
                  index == index_selected,
                  sar.method == sar_method_selected,
                  reach == reach_selected)

  test_data<- all_data %>%
    dplyr::filter(dplyr::between(year, min(years_selected+1),max(years_selected+1)),
                  index == index_selected,
                  sar.method == sar_method_selected,
                  reach == reach_selected)

  ## get time indices
  years <- train_data$year
  ## number of years of data
  TT <- length(years)
  ## get response variable: logit(survival)
  dat <- matrix(train_data$logit.s, nrow = 1)

  ## get predictor variable
  index <- train_data$value

  ## ## ## z-score the upwelling index
  index_z <- matrix((index - mean(index)) / sqrt(var(index)), nrow = 1) #currently standardizinbg based on years on input data not all data years -- change?

  ## z-score normalize the test data using the training data's mean and variance
  index_z_test <- (test_data$value - mean(index)) / sqrt(var(index))

  ## number of regr params (slope + intercept) = 2
  m <- dim(index_z)[1] + 1


  ### build DLM

  #get values from paramlist
  inputB <- paramlist$Binput
  inputU <- paramlist$Uinput
  inputQ <- paramlist$Qinput
  inputR <- paramlist$Rinput

  #reassign values for MARSS equations
  B <- if (is.character(inputB) && inputB == "diag") diag(m) else inputB
  U <- if (is.character(inputU) && inputU == "0") matrix(0, nrow = m, ncol = 1) else if (inputU == "u") matrix("u", nrow = m, ncol = 1) else inputU
  Q <- if (is.character(inputQ) && inputQ == "diagonal and unequal") "diagonal and unequal" else inputQ
  R <- if (is.character(inputR) && inputR == "r") matrix("r") else matrix(as.numeric(inputR))

  ## set remaining parameters
  Z <- array(NA, c(1, m, TT))  ## NxMxT; empty for now
  Z[1,1,] <- rep(1, TT)        ## Nx1; 1's for intercept
  Z[1,2,] <- index_z             ## Nx1; predictor variable
  A <- matrix(0)               ## 1x1; scalar = 0

  inits_list <- list(x0 = matrix(c(0, 0), nrow = m))
  mod_list <- list(B = B, U = U, Q = Q, Z = Z, A = A, R = R)
  dlm <- MARSS::MARSS(dat, inits = inits_list, model = mod_list)
  convergence_status <- ifelse(dlm$convergence == 0, "Success", "Warning")

  forecast_ytt1<-MARSS::forecast(dlm, h= 1, newdata = list(z = index_z_test, y = train_data[nrow(train_data),2]), type = "ytt1", interval = "confidence", fun.kf = "MARSSkfss")

  # adjust years for sar's with missing years
  if (unique(all_data$sar.method) == "DART" && unique(all_data$reach) == "BON_BOA") {
    year_vector <- c(2000:2004, 2006:2023)
  } else {
    year_vector <- min(years):(max(years) + 1)
  }

  df_forecast <- forecast_ytt1$pred %>%
    dplyr::mutate(dplyr::across(c(3:4, 6:9),~stats::plogis(.x)*100)) %>%
    janitor::clean_names() %>%
    dplyr::mutate(year = year_vector,
           index = index_selected,
           sar.method = sar_method_selected,
           reach = reach_selected,
           dataset = "select_forecast",
           rear_type = "Natural-origin",
           pass_type = "All",
           convergence = convergence_status) %>%
    dplyr::inner_join(dplyr::select(all_data, value, year), by = "year") %>% ##join index value
    dplyr::select(year, value, y, estimate, lo_95, hi_95,sar.method,index, reach, rear_type, pass_type, dataset, convergence)


    return(df_forecast)
}


#
# fct_forecast_model(data = sar_raw_data, paramlist = paramlist, years_selected = 2005,index_selected = "CUI", sar_method_selected = "Scheuerell and Williams (2005)", reach_selected = "LGR_LGA")
