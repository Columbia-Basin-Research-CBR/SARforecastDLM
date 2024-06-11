
library(here)
library(tidyverse)

library(rstan)
options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)

library(shinystan)


# data is currently sourcing aspects of JG shared CJS code files.
#It uses the same data shared for sizepred code and then follows similar code extraction found in the CJS code Survival Results 2022-12-03.R
#but removes covariates by only including intercept for the covariate matrix and setting NX = 0

# Data conditioned on juvenile detection at LGR, and with length observed at LGR
# (note acronym change from LGR to LWG in manuscript)
# Sites with juvenile detections: 1) LGR, 2) BON, 3) TWX,
# Sites with adult detections: 4) BON, 5) LGR, 6) TRIBS



# 1. years for random effects, passage type as covariate
load(here("data-raw/SAR", "DATAXrivocadm_RT1FW_1994_2019_years_221123.RData") )
DATAXm <- DATAXrivocadm_RT1FW_years
CH <- DATAXm[,1:6]
reaches <- 5


# covariates
# covariates <- model.matrix(~ transport, data=DATAXm)
# names(covariates) <- c("intercept", "otransport")
#
#
# fw.down.cov <- 0   # which includes direct effect to juveniles and carryover effect to ocean survival
# fw.carry.cov <- 1  # for carryover effect of covariate(s) not tested in juvenile stage
# o.cov <- 0
# fw.up.cov <- 0
# NX <- fw.down.cov + o.cov +fw.carry.cov + fw.up.cov
#
#
# # no ocean only effects #
# xphi.counter <- matrix( c(rep(1,reaches), rep(c(1,0,1,0,0), fw.down.cov ), rep(c(0,1,1,0,0), fw.carry.cov), rep(c(0,0,0,1,0), fw.up.cov ) ), NX+1, reaches, byrow=TRUE)
# xp.counter <- matrix( c(rep(1,reaches+1), rep(c(0,0,0,0,0,0), NX)), NX+1, reaches+1, byrow=TRUE)

#run without covariates
covariates <- model.matrix(~ 1, data=DATAXm) #only intercept included
NX<-0
xphi.counter <- matrix( c(rep(1,reaches) ), NX+1, reaches, byrow=TRUE)
xp.counter <- matrix( c(rep(1,reaches+1), rep(c(0,0,0,0,0,0), NX)), NX+1, reaches+1, byrow=TRUE)


release_year <- as.numeric(factor(DATAXm$year))
return_year <- ifelse(!is.na(DATAXm$adu_year),
                      as.numeric(factor(DATAXm$adu_year)),
                      0) # code NA as zero
# phi: downstream, SAR grouped by release year; upstream grouped by return year
yr.group.phi <- cbind(matrix(rep(release_year, 3), ncol = 3), matrix(rep(return_year, 2), ncol = 2))
# p: downstream grouped by release year; upstream grouped by return year
yr.group.p <- cbind(matrix(rep(release_year, 3), ncol = 3), matrix(rep(return_year, 3), ncol = 3))


cjs_no_covariates<- stan(file = here("data-raw/SAR", "CJS-marray-phiXRE-pXRE-fixNA.stan"),
                                      data = list(T = reaches+1, M = nrow(DATAXm), K = NX+1,
                                                  X = covariates,
                                                  indX_phi = xphi.counter,
                                                  group_phi = yr.group.phi,
                                                  indX_p = xp.counter,
                                                  group_p = yr.group.p,
                                                  y = CH,
                                                  n = DATAXm$tagid ),
                                      pars = c("beta","sigma","epsilon_z","b","s","e_z","LL"),
                                      chains = 3, cores = 3, iter = 1500, warmup = 500,
                                      control = list(adapt_delta = 0.99, max_treedepth = 15), refresh=75)

saveRDS(cjs_no_covariates, here("data-raw/SAR", "cjs_no_covariates.RDS") )

print(cjs_no_covariates, pars = "beta", include = TRUE, prob = c(0.025,0.5,0.975))

plot(cjs_no_covariates)

pairs(cjs_no_covariates, pars = c("beta", "sigma") )

# extract based on JG code

mymodel<-cjs_no_covariates

# LGR-LGA reach
reach=5 # LGR-LGA reach

year.i <- DATAXm$year-min(DATAXm$year)+1

#beta
b <- as.matrix(mymodel,"b")
b0 <- b[,paste0("b[1,", 1:6, "]")]

beta <- as.matrix(mymodel,"beta")

beta0 <- beta[, paste0("beta[1,", 1:5, "]")]

#sigma
s <- as.matrix(mymodel,"s")

sigma <- as.matrix(mymodel,"sigma")

#epsilon_z  # random effects by year and by reach
epsilon_z_raw <- as.matrix(mymodel,"epsilon_z")
epsilon_z <- matrix(get_posterior_mean(mymodel,"epsilon_z")[,"mean-all chains"],
                    ncol = 5, byrow = T)

# for results
dum.surv.mat <- matrix(NA,3000,length(unique(year.i))) ##22 years updated loop from 1:19 to 1:22 to reflect years?
# 3000 estimates from iterations
for (j in 1:22) {
  # intercept for that year with random effect incorporated
  dum.intcpt.vec <- beta0[,reach] + as.matrix(sigma[,reach]) * epsilon_z[j,reach]


  # # 3000 interations for each row of m-array; therefore need to take *weighted average* across the m-array rows for each iteration
  # dum.cov.mat <- as.matrix(beta_up) %*% t(as.matrix(covariates[year.i==j,c("transport", "aTEMP","aFLOW","aSPILL")]))
  # weight by number of individuals
  # dum.w <- DATAXm$tagid[year.i==j]/sum(DATAXm$tagid[year.i==j])
  # parameters * covariates , weighted averaged across m-array rows for year j
  # dum.cov.vec <- logit(plogis(dum.cov.mat) %*% dum.w)


  # add into matrix of detection histories for the relevant year
  dum.surv.mat[,j] <- dum.intcpt.vec #+ dum.cov.vec
}

#view estimate
dum.surv.mat

fct_med_CI<- function(x) quantile(x,probs=c(0.025,0.5,0.975))  # median (50%) and  95% credible interval (2.25 to 97.5%)

# convert to survival probability, median + 95% CI for each year and transform to probability
# LGR_LGA_surv <- data.frame(plogis(apply(dum.surv.mat, MARGIN=2, FUN=fct_med_CI))) # seems stan is already transforming to probability
LGR_LGA_surv <- data.frame(apply(dum.surv.mat, MARGIN=2, FUN=fct_med_CI))
#set years
colnames(LGR_LGA_surv) <- 1998:2019

# convert to data.frame and transpose
df <- LGR_LGA_surv %>%
  as.data.frame() %>%
  rownames_to_column("probability") %>%
  gather(year, value, -probability) %>%
  spread(probability, value) %>%
  rename( median = `50%`, lower = `2.5%`,upper = `97.5%`) %>%
  mutate(year = as.numeric(year))


write.csv(df, file=here("data-raw/SAR", "LGR_LGA_surv.csv"), row.names = FALSE)

save(dum.surv.mat, file=here("Analysis", "results", paste0("WRTCh.reach_230728_",reach,".RData")))


