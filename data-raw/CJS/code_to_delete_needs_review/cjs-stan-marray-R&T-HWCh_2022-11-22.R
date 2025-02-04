
library(here)

library(rstan)
options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)

library(shinystan)


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
covariates <- model.matrix(~ transport, data=DATAXm)
names(covariates) <- c("intercept", "otransport")


fw.down.cov <- 0   # which includes direct effect to juveniles and carryover effect to ocean survival
fw.carry.cov <- 1  # for carryover effect of covariate(s) not tested in juvenile stage
o.cov <- 0
fw.up.cov <- 0
NX <- fw.down.cov + o.cov +fw.carry.cov + fw.up.cov

# no ocean only effects #
xphi.counter <- matrix( c(rep(1,reaches), rep(c(1,0,1,0,0), fw.down.cov ), rep(c(0,1,1,0,0), fw.carry.cov), rep(c(0,0,0,1,0), fw.up.cov ) ), NX+1, reaches, byrow=TRUE)
xp.counter <- matrix( c(rep(1,reaches+1), rep(c(0,0,0,0,0,0), NX)), NX+1, reaches+1, byrow=TRUE)


release_year <- as.numeric(factor(DATAXm$year))
return_year <- ifelse(!is.na(DATAXm$adu_year),
                      as.numeric(factor(DATAXm$adu_year)),
                      0) # code NA as zero
# phi: downstream, SAR grouped by release year; upstream grouped by return year
yr.group.phi <- cbind(matrix(rep(release_year, 3), ncol = 3), matrix(rep(return_year, 2), ncol = 2))
# p: downstream grouped by release year; upstream grouped by return year
yr.group.p <- cbind(matrix(rep(release_year, 3), ncol = 3), matrix(rep(return_year, 3), ncol = 3))


cjs_results <- stan(file = here("data-raw/SAR", "CJS-marray-phiXRE-pXRE-fixNA.stan"),
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
                                      control = list(adapt_delta = 0.99, max_treedepth = 15), refresh=75 )

save(cjs_results, file=here("data-raw/SAR", "cjs_results.RData") )
load(here("data-raw/SAR", "cjs_results"))

print(cjs_results, pars = "beta", include = TRUE, prob = c(0.025,0.5,0.975))







