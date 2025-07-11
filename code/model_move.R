model {
  ## prior for the intercept
  b[1] ~ dnorm(0, pow(2.5, -2))
  
  ## prior for coefficients
  for (k in 2:K) {
    b[k] ~ dnorm(0, pow(1, -2))
  }
  
  p ~ dnorm(0.5, pow(1, -2))T(0, 1)
  
  for (i in 1:Nsample) {
    ## observation model for recaptured or not
    z[i] <- step(430 - X1[i]) + step(X1[i])
    phi[i] <- step(z[i] - 1.5)
    Y[i] ~ dbern(phi[i] * p)
    
    ## movement model
    D[i] <- abs(X1[i] - X0[i]) / Intv[i]
    X1[i] ~ dt(X0[i], tau[i], 3)
    tau[i] <- pow(sd_m[i], -2)
    
    log(sd_m[i]) <- inprod(b[], X[i,]) + log(Intv[i])
  }
  
}