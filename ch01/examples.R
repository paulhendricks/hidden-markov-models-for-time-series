# Load libraries
library(depmixS4)

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



