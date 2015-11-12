# Changed to adhere to a better style
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

