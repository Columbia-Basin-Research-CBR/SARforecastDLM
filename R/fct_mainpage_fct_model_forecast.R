#' mainpage_fct_model_forecast
#'
#' @description A fct that runs the DLM with selected years
#' @return returns a dataframe with forecasted values (+3) using the input years
#'
#' @noRd
fct_model_forecast<-function(data, years_selected, index_selected){

  full_data<- data %>%
    dplyr::filter(dplyr::between(year, min(year), 2005),
                  index == index_selected)

  train_data<- full_data %>%
    dplyr::filter(dplyr::between(year, min(year),max(years_selected)),
                  index == index_selected)

  # test_data<- full_data %>%
  # dplyr::filter(between(year,2000 +1, 2005),#max(years_selected)
  #               index == "CUI")#index_selected)

  ## get time indices
  years <- train_data$year
  ## number of years of data
  TT <- length(years)
  ## get response variable: logit(survival)
  dat <- matrix(train_data$logit.s, nrow = 1)

  ## get predictor variable
  index <- full_data$value

  ## ## ## z-score the upwelling index
  index_z <- matrix((index - mean(index)) / sqrt(var(index)), nrow = 1)

  index_z_train <- index_z[1:TT]

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
  Z[1,2,] <- index_z_train             ## Nx1; predictor variable
  A <- matrix(0)               ## 1x1; scalar = 0
  R <- matrix("r")             ## 1x1; scalar = r

  ## only need starting values for regr parameters
  inits_list <- list(x0 = matrix(c(0, 0), nrow = m))

  ## list of model matrices & vectors
  mod_list <- list(B = B, U = U, Q = Q, Z = Z, A = A, R = R)


  ### fit DLM
  set.seed(1234)
  ## fit univariate DLM
  dlm_train <- MARSS::MARSS(dat, inits = inits_list, model = mod_list)


  #pull indice with full data z score
  index_z_test <- index_z[(TT+1):(TT+3)] #get 3 CUI ahead

  #newdata z array
  Z_test <- array(NA, c(1, m, 3))  ## NxMxT; empty for now
  Z_test[1,1,] <- rep(1, 3)        ## Nx1; 1's for intercept
  Z_test[1,2,] <- index_z_test

  ## forecast
  forecast_df<-MARSS::forecast(dlm_train, h = 3, type = "ytT", interval = "confidence")

  #extract forecast
  forecast_df_pred<- forecast_df$pred


  #wrangle for plot
  forecast_df_raw<-forecast_df_pred%>%
    dplyr::mutate(dplyr::across(c(,3:9), ~stats::plogis(.) * 100)) %>%
    dplyr::rename("fore_CI_95_upper" = `Hi 95`,
           "fore_CI_95_lower" = `Lo 95`) %>%
    dplyr::mutate(year = (min(years):max(years+3)),
           index = index_selected,
           sar.method = "Scheuerell and Williams (2005)",
           dataset = "select_forecast",
           rear_type = "Natural-origin",
           pass_type = "All") %>%
    dplyr::inner_join(dplyr::select(full_data, value, year), by = "year") %>% ##join index value
    dplyr::select(year, value, y, estimate, se, fore_CI_95_lower, fore_CI_95_upper,sar.method,index, rear_type, pass_type, dataset)


  return(forecast_df_raw)
}
