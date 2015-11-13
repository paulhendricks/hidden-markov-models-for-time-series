# Changed to adhere to a better style

# A.1.1 Transform natural parameters to working parameters
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

# A.1.4 ML estimation of a stationary Poisson-HMM
pois_HMM_mle <- function(x, m, lambda0, gamma0, ...) {
  parvect0 <- pois_HMM_pn2pw(m, lambda0, gamma0)
  mod <- nlm(pois_HMM_mllk, parvect0, x = x, m = m)
  pn <- pois_HMM_pw2pn(m, mod$estimate)
  mllk <- mod$minimum
  np <- length(parvect0)
  AIC <- 2 * (mllk + np)
  n <- sum(!is.na(x))
  BIC <- 2 * mllk + np * log(n)
  return(list(lambda = pn$lambda, gamma = pn$gamma, delta = pn$delta, 
              code = pn$code, mllk = mllk, AIC = AIC, BIC = BIC))
}

# A.2.1 Generate a realixation of Poisson-HMM
pois_HMM_generate_sample <- function(n, m, lambda, gamma, delta = NULL) {
  if (is.null(delta)) 
    delta <- solve(t(diag(m) - gamma + 1), rep(1, m))
  mvect <- 1:m
  state <- numeric(n)
  state[1] <- sample(mvect, 1, prob = delta)
  for (i in 2:n) 
    state[i] <- sample(mvect, 1, prob = gamma[state[i-1], ])
  x <- rpois(n, lambda = lambda[state])
  return(x)
}


r_gamma <- function(m) {
  gamma_raw <- matrix(runif(m ** 2), nrow = m, ncol = m, byrow = TRUE)
  gamma_normalized <- gamma_raw / apply(gamma_raw, MARGIN = 1, FUN = sum)
  return(gamma_normalized)
}

g <- r_gamma(m)

pois_HMM_generate_sample(n = 100, m = 2, lambda = c(2, 500), gamma = g)

# A.2.2 Forward and backward probabilities
pois_HMM_lalphabeta <- function(n, m, lambda, gamma, delta = NULL) {
  if (is.null(delta)) 
    delta <- solve(t(diag(m) - gamma + 1), rep(1, m))
  n <- length(x)
  laplha <- lbeta <- as.matrix(NA, m, n)
  allprobs <- outer(x, lambda, pois)
  foo <- delta * allprobs[1, ]
  sumfoo <- sum(foo)
  lscale <- log(sumfoo)
  foo <- foo / sumfoo
  laplha[, 1] <- log(foo) + lscale
  for (i in 2:n) {
    foo <- foo %*% gamma * allprobs[i, ]
    sumfoo <- sum(foo)
    lscale <- lscale  + log(sumfoo)
    foo <- foo / sumfoo
    laplha[, 1] <- log(foo) + lscale
  }
  lbeta[, n] <- rep(0, m)
  foo <- rep(1 / m, m)
  lscale <- log(m) 
  for (i in (n - 1):1) {
    foo <- gamma %*% (allprobs[i + 1, ] * foo)
    lbeta[, i] <- log(foo) + lscale
    sumfoo <- sum(foo)
    lscale <- lscale  + log(sumfoo)
    foo <- foo / sumfoo
  }
  return(list(la = lalpha, lbeta = lbeta))
}

# Example
m <- 2
x <- sample(c(rpois(100, 5), rpois(100, 30)), replace = FALSE)
l0 <- runif(m)
g0 <- matrix(runif(m ** 2), nrow = m, ncol = m, byrow = TRUE)

pois_HMM_mle(x = x, m = m, lambda0 = l0, gamma0 = g0)
