
# Salmon Survival Forecasting

index_selected<- "ICPB"
sar_method_selected <- "DART"
years_selected<-2019

# newyear<-data.frame(
#   year = 2006,
#   logit.s = NA,
#   value = 1
#   )
#
#  all.data <- SalmonSurvCUI %>%
#    rename(value = CUI.apr) %>%
#    bind_rows(newyear) %>%
#    mutate(index = "CUI",
#           sar.method = "Scheuerell and Williams (2005)")

all.data<-sar_raw_data_updated %>%
  dplyr::filter(  index == index_selected,
                  sar.method == sar_method_selected) %>%
  bind_rows(data.frame(year = 2020, logit.s = NA, value = NA, index = "ICPB", sar.method = "DART"))

  train.data <- all.data %>%
    dplyr::filter(dplyr::between(year, min(year),max(years_selected)),
                  index == index_selected,
                  sar.method == sar_method_selected)
  #for baseplots
  test.data <-all.data %>%
    dplyr::filter(index == index_selected,
                  sar.method == sar_method_selected,
                  is.na(logit.s))

 #for  comparison plot
  test.data <-all.data %>%
    dplyr::filter(dplyr::between(year, min(years_selected+1),max(years_selected+1)),
                                 index == index_selected,
                                 sar.method == sar_method_selected)

#run model on train data
  years <- train.data$year
  ## number of years of data
  TT <- length(years)
  ## get response variable: logit(survival)
  dat <- matrix(train.data$logit.s, nrow = 1)

  ## get predictor variable
 index <- train.data$value

  ## ## ## z-score the upwelling index
  index_z <- matrix((index - mean(index)) / sqrt(var(index)), nrow = 1)

  # index_z_train <- index_z[1:TT]

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


  ### fit DLM
  # set.seed(1234)
  ## fit univariate DLM
  dlm <- MARSS::MARSS(dat, inits = inits_list, model = mod_list)


  ## forecast
  ## get list of Kalman filter output
  kf_out <- MARSSkfss(dlm)


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
  test_value<-test.data$value


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


  # add in ifelse statement here that if test.data is empty for y and value then keep nahead, but if has value then drop nahead and append-- include wrangling to prevent overriding forecast year
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
    dplyr::inner_join(dplyr::select(all.data, value, year), by = "year") %>% ##join index value
    dplyr::select(year, value, y, estimate, fore_CI_95_lower, fore_CI_95_upper,sar.method,index, rear_type, pass_type, dataset)


  return(df_forecast)

