nls_grad_fn <- function(x) {
  eps_v <- rep(1, nrow(master))
  eta_v <- rep(1, nrow(master))
  t05_v <- rep(1, nrow(master))
  t10_v <- rep(1, nrow(master))
  t00_v <- rep(1, nrow(master))
  
  for (i in 1:nrow(master)) {
    ig <<- i # setting global to use with function
    master$NLS_Errors[ig] <<- master$agspend[ig] - nls_inverse(x)
    fprime <- 1/(1/nls_inverse(x) + x[1]/(1-nls_inverse(x)))
    eps_v[ig] <- 2*master$NLS_Errors[ig]*(-(1-x[2])*master$log_EPa[ig])*fprime
    eta_v[ig] <- 2*master$NLS_Errors[ig]*((x[1] - 1)*master$log_EPa[ig] + master$log_relprice[ig])*fprime
    t05_v[ig] <- 2*master$NLS_Errors[ig]*(-master$FE_2005[ig])*fprime
    t10_v[ig] <- 2*master$NLS_Errors[ig]*(-master$FE_2010[ig])*fprime
    t00_v[ig] <- 2*master$NLS_Errors[ig]*(-1)*fprime
  }
  
  eps_val <- sum(eps_v)
  eta_val <- sum(eta_v)
  t05_v <- sum(t05_v)
  t10_v <- sum(t10_v)
  t00_v <- sum(t00_v)
  
  return(c(eps_val, eta_val, t05_v, t10_v, t00_v))
}
