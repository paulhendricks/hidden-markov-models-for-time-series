# Changed to adhere to a better style

# A.1.1 Transform natural parameters to working
pois_HMM_pn2pw <- function(m, lambda, gamma) {
  tlambda <- log(lambda)
  tgamma <- NULL
  if(m > 1) {
    foo <- log(gamma / diag(gamma))
    tgamma <- as.vector(foo[!diag(m)])
  }
  parvect <- c(tlambda, tgamma)
  return(parvect)
}

# A.1.2 Transform working parameters to natural parameters
pois_HMM_pw2pn <- function(m, parvect) {
  epar <- exp(parvect)
  lambda <- epar[1:m]
  gamma <- diag(m)
  if(m > 1) {
    gamma[!gamma] <- epar[(m+1):(m*m)]
    gamma <- gamma / apply(gamma, 1, sum)
}
  delta <- solve(t(diag(m) - gamma + 1), rep(1, m))
  return(list(lambda = lambda, gamma = gamma, delta = delta))
}

# A.1.3 Log-likelihood of a stationary Poisson-HMM
pois_HMM_mllk <- function(parvect, x, m, ...) {
  if (m == 1) return(-sum(dpois(x, exp(parvect), log = TRUE)))
  n <- length(x)
  pn <- pois_HMM_pw2pn(m, parvect)
  allprobs <- outer(x, pn$lambda, dpois)
  allprobs <- ifelse(!is.na(allprobs), allprobs, 1)
  lscale <- 0
  foo <- pn$delta
  for (i in 1:n) {
    foo <- foo %*% pn$gamma * allprobs[i, ]
    sumfoo <- sum(foo)
    lscale <- lscale + log(sumfoo)
    foo <- foo / sumfoo
  }
  mllk <- -lscale
  return(mllk)
}

g <- 
  matrix(c(1/3, 1/3, 1/3, 
           2/3, 0/3, 1/3, 
           1/2, 1/2, 0/3), 
         nrow = 3, ncol = 3, byrow = TRUE)
pois_HMM_pn2pw(m = 3, lambda = c(3, 6, 9), gamma = g)
pois_HMM_pn2pw(m = 3, )