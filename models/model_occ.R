#Occupancy model example

model {
  
  #Likelihood
  
  for (i in 1:n){
    
    logit(psi[i]) <- alpha + b.forest*forest[i]
    
    z[i] ~ dbern(psi[i])
    
    ptemp[i] <- z[i]*p
    
    obs[i] ~ dbinom(ptemp[i],nobs)
    
  }
  
  #Priors
  
  alpha ~ dunif(-100,100)
  b.forest ~ dnorm(0,0.01)
  p ~ dunif(0,1)
  
  #Derived parameters
  
  prob.occ <- mean(z[])
  
}