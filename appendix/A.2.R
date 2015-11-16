# Changed to adhere to a better style

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

# A.2.3 EM estimation of a Poisson-HMM
pois_HMM_em <- function(x, m, lambda, gamma, delta, 
                        maxiter = 1000, tol = 1e-6, ...) {
  lambda_next <- lambda
  gamma_next <- gamma
  delta_next <- delta
  
  
  
  return()
}

# A.2.4 Viterbi algorithm
pois_HMM_viterbi <- function(x, m, lambda, gamma, delta = NULL) {
  if (is.null(delta)) 
    delta <- solve(t(diag(m) - gamma + 1), rep(1, m))
  n <- length(x)
  poisprobs <- outer(x, lambda, dpois)
  xi <- matrix(0, n, m)
  foo <- delta * poisprobs[1, ]
  xi[1, ] <- foo / sum(foo)
  for (i in 2:n) {
    foo <- apply(xi[i - 1, ] * gamma, 2, max) * poisprobs[i, ]
    xi[i, ] <- foo / sum(foo)
  }
  iv <- numeric(n)
  iv[n] <- which.max(xi[n, ])
  for (i in (n-1):1) {
    iv[i] <- which.max(gamma[, iv[i + 1] * xi[i, ]])
  }
  return(iv)
}

# A.2.5 Conditional state probabilities
pois_HMM_state_probs <- function() {
  
  return()
}

# A.2.6 Local decoding
pois_HMM_local_decoding <- function() {
  
  return()
}

# A.2.7 State prediction
pois_HMM_state_prediction <- function() {
  if (is.null(delta)) 
    delta <- solve(t(diag(m) - gamma + 1), rep(1, m))
  return()
}

# A.2.8 Forecast distributions
pois_HMM_local_decoding <- function() {
  
  return()
}

# A.2.9 Conditional distribution of one observation given the rest
pois_HMM_local_decoding <- function() {
  
  return()
}

# A.2.10 Ordinary pseudo-residuals
pois_HMM_local_decoding <- function() {
  
  return()
}
