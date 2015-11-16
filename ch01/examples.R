# Load libraries
library(depmixS4)
library(flexmix)
library(msm)
library(mhsmm)

# Load data
earthquakes <- read.csv("../data/earthquakes.csv", 
                        stringsAsFactors = FALSE)

# Load functions
source("../appendix/A.1.R")
source("../appendix/A.2.R")
source("../appendix/A.3.R")
source("../appendix/A.4.R")

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

# Example
m <- 2
x <- sample(c(rpois(100, 5), rpois(100, 30)), replace = FALSE)
l0 <- runif(m)
g0 <- matrix(runif(m ** 2), nrow = m, ncol = m, byrow = TRUE)

pois_HMM_mle(x = x, m = m, lambda0 = l0, gamma0 = g0)

g <- r_gamma(m)

pois_HMM_generate_sample(n = 100, m = 2, lambda = c(2, 500), gamma = g)

# Using msm
hmmPois(5)
