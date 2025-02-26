#' DLM -- only DART
#' This script looks at changing Q and R to different varcov structurs to troubleshoot
#' the poor convergence of the DART dataset from 2000 to 2021 using a DLM MARSS model.
#' The first fit shows all years (2000-2021) of data including an outlier year 2001,
#' the second fit removes 2001, and the third adds a covariate (CUI) to the model.
#' Fit 1-3 use a diagonal and unequal Q and R. Removing 2001 allows the model to converge, but Q(2,2) goes to 0.
#' The fourth fit allows Q to be unconstrained which allows the model to converge and Q does not go to 0.
#' CUI seems to do little to explain the overall variability in the model, but is not 0.


library(MARSS)
library(tidyverse)
library(here)

load(here("data", "sar_raw_data.rda"))


######---- include all years ----######
sar_dart<- sar_raw_data %>%
  filter(index == "CUI",sar.method == "DART") %>% #select only dart/CUI data
  drop_na() #remove 2022 used to forecast

#data for DLM
years <- sar_dart$year
TT <- length(years)
dat <- matrix(sar_dart$logit.s, nrow = 1)

### build DLM
m <- 1    ## state variables

## for process eqn
B <- diag(m)                        ## default is "identity" no interactions among regression parameters
U <- matrix(0, nrow = m, ncol = 1)  ## set to 0 for no drift in regression parameters over time
Q <- "diagonal and unequal"         ## allow for different variance with no covariance

## for observation eqn
A <- matrix(0)               ## 1x1; scalar = 0
R <- matrix("r")

inits_list <- list(x0 = matrix(c(0), nrow = m)
                   # Q = matrix(0.1, nrow = m, ncol = m), tried setting to small values to help with convergence
                   # R = matrix(.5)
                   )   ## only need starting values for regr parameters
mod_list <- list(B = B, U = U, Q = Q, A = A, R = R)
control_list <- list(maxit = 1000) #increased max iterations to help with convergence
fit1 <- MARSS::MARSS(dat, inits = inits_list, model = mod_list, control = control_list)

plot(fit1, silent = TRUE, plot.type = c("fitted.ytT", "xtT"))

#changing R or Q to different varcov structures still results in poor (if any convergence) and Q to be degenerate (go to 0).
# Issue seems to be the added noise from including outlier years is too great for the DLM to model

######---- removing outlier year (2001) ----######

sar_dart<- sar_raw_data %>%
  filter(
    year != 2022, #was included for forecasting
    index == "CUI",
    sar.method == "DART"
  ) %>%
  mutate( logit.s = ifelse(year == 2001, NA, logit.s)) #only remove 2001 from the data, leave NA

#data for DLM
years <- sar_dart$year
TT <- length(years)
dat <- matrix(sar_dart$logit.s, nrow = 1)

# keep same inits_list and model_list as above
fit2 <- MARSS::MARSS(dat, inits = inits_list, model = mod_list)

plot(fit2, silent = TRUE, plot.type = c("fitted.ytT", "xtT"))

######---- removing outlier year (2001), add CUI covariate ----######

#data for DLM
m<-2  #add in CUI covariate
#add in CUI covariate
index <- sar_dart$value
index_z <- matrix((index - mean(index)) / sqrt(var(index)), nrow = 1)

### build DLM
#update structure to include CUI covariate
## for process eqn
B <- diag(m)
U <- matrix(0, nrow = m, ncol = 1)

## for observation eqn
Z <- array(NA, c(1, m, TT))  ## NxMxT; empty for now
Z[1,1,] <- rep(1, TT)        ## Nx1; 1's for intercept
Z[1,2,] <- index_z             ## Nx1; predictor variable

#update inits_list and mod_list to include CUI
inits_list <- list(x0 = matrix(c(0), nrow = m))
mod_list <- list(B = B, U = U, Q = Q, Z = Z, A = A, R = R)
fit3 <- MARSS::MARSS(dat, inits = inits_list, model = mod_list, control = list(allow.degen = TRUE, maxit = 1000 ))
# model does not converge if allow.degen = FALSE-- meaning Q(2,2) process error for X2 goes to 0 (degenerate) and X2 is not being included in process error
plot(fit3, silent = TRUE, plot.type = c("fitted.ytT", "xtT"))
MARSS::MARSSparamCIs(fit3)

######---- removing outlier year (2001), add CUI covariate, and change Q to unconstrained ----######

## note, attempted setting Q(2,2) to small value first but did not converge, allowing Q to be unconstrained did.
Q <- "unconstrained" #matrix(c("alpha", 0, 0, "beta"), nrow = m, ncol = m)  side note, if trying to set MARSS to estimate a mix of a scalar and numbers use matrix(list()) not matrix(c())
#use same inits_list as above and update mod_list to "unconstrained" Q
mod_list <- list(B = B, U = U, Q = Q, Z = Z, A = A, R = R)
fit4 <- MARSS::MARSS(dat, inits = inits_list, model = mod_list)

plot(fit4, silent = TRUE, plot.type = c("fitted.ytT", "xtT"))

#'suggests Q(1,1) or process variance for the first state (intercept, survival process noise) explains most variability,
#'with Q(2,2), or process variance for the second state (CUI), is close to 0 but still has some dynamic effect on the model.
#'And Q(1,2) or Q(2,1) is a small negative covariance between the two states indicating a small trade-off between the two states over time.
#'I think this means that as the intercept shifts (survival) (X1),
#'the effect of CUI covariate (X2) changes such that if survival increases, the effect of CUI covariate decreases?
#'
#'
#####---- add 2001 back in ----#####

sar_dart<- sar_raw_data %>%
  filter(
    year != 2022, #was included for forecasting
    index == "CUI",
    sar.method == "DART"
  )

#update data for DLM
years <- sar_dart$year
TT <- length(years)
dat <- matrix(sar_dart$logit.s, nrow = 1)

## for observation eqn
Z <- array(NA, c(1, m, TT))  ## NxMxT; empty for now
Z[1,1,] <- rep(1, TT)        ## Nx1; 1's for intercept
Z[1,2,] <- index_z             ## Nx1; predictor variable

#update mod_list
mod_list <- list(B = B, U = U, Q = Q, Z = Z, A = A, R = R)
fit5<- MARSS::MARSS(dat, inits = inits_list, model = mod_list)
# no convergence for any Q structure, even unconstrained Q
