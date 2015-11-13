# Load libraries
library(depmixS4)
library(flexmix)

# Load data
earthquakes <- read.csv("../data/earthquakes.csv", 
                        stringsAsFactors = FALSE)

# Table 1.2
model_earthquakes <- function(.m, ...) {
  .model <- depmix(response = count ~ 1, data = earthquakes, 
                   nstates = .m, trstart = runif(.m ** 2), ...)
  return(fit(.model))
}

set.seed(1)
fits <- lapply(1:4, model_earthquakes)


# Chapter 3
pois_HMM_mle(x = earthquakes$count, m = 1, 
             lambda0 =  c(19), gamma0 = r_gamma(1)) # Exact results
pois_HMM_mle(x = earthquakes$count, m = 2, 
             lambda0 =  c(15, 25), gamma0 = r_gamma(2)) # Exact results
pois_HMM_mle(x = earthquakes$count, m = 3, 
             lambda0 =  c(13, 19, 29), gamma0 = r_gamma(3)) # Exact results
pois_HMM_mle(x = earthquakes$count, m = 4, 
            lambda0 =  c(11.283, 13.853, 19.695, 29.700), 
            gamma0 = r_gamma(4)) # Trouble reproducing textbook results
