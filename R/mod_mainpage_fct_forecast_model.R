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
fct_forecast_model<-function(data, years_selected, index_selected, sar_method_selected){
 print(years_selected)
  all_data<- data %>%
    dplyr::filter(#dplyr::between(year, min(year), max(year)),
                  index == index_selected,
                  sar.method == sar_method_selected)

  train_data<- all_data %>%
    dplyr::filter(dplyr::between(year, min(year),max(years_selected)),
                  index == index_selected,
                  sar.method == sar_method_selected)

  test_data<- all_data %>%
    dplyr::filter(dplyr::between(year, min(years_selected+1),max(years_selected+1)),
                  index == index_selected,
                  sar.method == sar_method_selected)

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


  ## number of regr params (slope + intercept) = 2
  m <- dim(index_z)[1] + 1


  ### build DLM

  ## for process eqn
  B <- diag(m)                        ## 2x2; Identity
  U <- matrix(0, nrow = m, ncol = 1)  ## 2x1; both elements = 0
  Q <- matrix(list(0), m, m)          ## 2x2; all 0 for now
  diag(Q) <- c("q.alpha", "q.beta")   ## 2x2; diag = (q1,q2)

  ## for observation eqn
  Z <- array(NA, c(1, m, TT))  ## NxMxT; empty for now
  Z[1,1,] <- rep(1, TT)        ## Nx1; 1's for intercept
  Z[1,2,] <- index_z             ## Nx1; predictor variable
  A <- matrix(0)               ## 1x1; scalar = 0
  R <- matrix("r")             ## 1x1; scalar = r

  ## only need starting values for regr parameters
  inits_list <- list(x0 = matrix(c(0, 0), nrow = m))

  ## list of model matrices & vectors
  mod_list <- list(B = B, U = U, Q = Q, Z = Z, A = A, R = R)


  ### fit univariate DLM
  # set.seed(1234)
  dlm <- MARSS::MARSS(dat, inits = inits_list, model = mod_list)

  ## forecast
  ## get list of Kalman filter output
  kf_out <- MARSS::MARSSkfss(dlm)


  ## forecasts of regr parameters; 2xT matrix - 2 in this case represents slope and intercept (2 regression parameters)
  eta <- kf_out$xtt1 #one step ahead forecast of regr parameters calc with kalman filter, represented as xtt1 in marss notation


  ## variance of regr parameters (slope and intercept); 1x2xT array
  Phi <- kf_out$Vtt1

  #Since DLM is time-varying only the last values in the training data are used to forecast the next year
  # Get the last forecasted regression parameters
  last_eta <- eta[, ncol(eta)]

  # Get the last forecasted variance
  last_Phi <- Phi[, , ncol(Phi)]

  #get index value for next year and standardize
  test_value<-test_data$value


  #get mean and standard deviation of index used in model
  mean_index <- mean(index, na.rm = TRUE)
  sd_index <- sd(index, na.rm = TRUE)
  test_z_value <- (test_value - mean_index) / sd_index  #standardize using the train dataset mean and standard deviation



  #forecast logit.s using last predicted regression parameters and z-value for next year
  forecasted_logit_s <- last_eta[1] + last_eta[2] * test_z_value


  # Calculate the forecasted standard error
  forecasted_se <- sqrt(last_Phi[1, 1] + 2 * last_eta[2] * last_Phi[1, 2] + last_eta[2]^2 * last_Phi[2, 2])

  #forecasted_s <- exp(forecasted_logit_s) / (1 + exp(forecasted_logit_s))*100
  forecasted_s_plogis <- plogis(forecasted_logit_s)*100

  # Calculate the prediction interval
  z_value<-qnorm(.975) ## Z-value for 95% confidence interval ~1.96 (change as needed)
  lower <- forecasted_s_plogis - z_value * forecasted_se
  upper <- forecasted_s_plogis + z_value * forecasted_se

  #create dataframe for forecast
  df_nextyear<- data.frame(
    y = NA,
    estimate = forecasted_s_plogis,
    fore_CI_95_lower = lower,
    fore_CI_95_upper = upper
  )

  #extract predictions for all years --currently using forecast but should be using predict--doesn't seem to work by calling MARSS::predict()--could try manually extracting and calculating predictions
  forecast_df<-MARSS::forecast(dlm, h= 1, type = "ytT", interval = "confidence")


  # Check if 'logit.s' and 'value' are empty--use MARSS step-ahead forecast without a index value if it is, if not empty, use test_data index value to forecast nahead
  if (is.na(test_data$logit.s) && is.na(test_data$value)) {
    # If both are empty, forecast with h=1 and keep last estimate
    df_pred_raw<- forecast_df$pred

    #wrangle for plot
    df_pred<-df_pred_raw%>%
      dplyr::mutate(dplyr::across(c(,3:9), ~stats::plogis(.) * 100)) %>%
      dplyr::select(y, estimate, "fore_CI_95_lower" = `Lo 95`,"fore_CI_95_upper" = `Hi 95`)

    df_forecast<-  df_pred %>%
      dplyr::mutate(year = (min(years):max(years+1)),
                    index = index_selected,
                    sar.method = sar_method_selected,
                    dataset = "select_forecast",
                    rear_type = "Natural-origin",
                    pass_type = "All") %>%
      dplyr::inner_join(dplyr::select(all_data, value, year), by = "year") %>% ##join index value
      dplyr::select(year, value, y, estimate, fore_CI_95_lower, fore_CI_95_upper,sar.method,index, rear_type, pass_type, dataset)

    return(df_forecast)
  } else {
    #remove last row (incorrect forecast for next year)
    df_pred_raw<- forecast_df$pred  %>% head(-1)


    #wrangle for plot
    df_pred<-df_pred_raw%>%
      dplyr::mutate(dplyr::across(c(,3:9), ~stats::plogis(.) * 100)) %>%
      dplyr::select(y, estimate, "fore_CI_95_lower" = `Lo 95`,"fore_CI_95_upper" = `Hi 95`)

    df_forecast<-  df_pred %>%
      dplyr::bind_rows(df_nextyear) %>%
      dplyr::mutate(year = (min(years):max(years+1)),
                    index = index_selected,
                    sar.method = sar_method_selected,
                    dataset = "select_forecast",
                    rear_type = "Natural-origin",
                    pass_type = "All") %>%
      dplyr::inner_join(dplyr::select(all_data, value, year), by = "year") %>% ##join index value
      dplyr::select(year, value, y, estimate, fore_CI_95_lower, fore_CI_95_upper,sar.method,index, rear_type, pass_type, dataset)


    return(df_forecast)
  }

}



# fct_forecast_model(data = sar_raw_data, years_selected = 2005,index_selected = "CUI", sar_method_selected = "Scheurell and Williams (2005)")
