#Occupancy model example

model {
  
  #Likelihood
  
  for (i in 1:n){
    
    #True occupancy at site i
    logit(psi[i]) <- alpha + b.forest*forest[i]
    
    z[i] ~ dbern(psi[i])
    
    #Generate p - has to be 0 if site is unoccupied
    ptemp[i] <- z[i]*p
    
    #Model actual data
    obs[i] ~ dbinom(ptemp[i],nobs)
    
  }
  
  #Priors
  
  alpha ~ dunif(-100,100)
  b.forest ~ dnorm(0,0.01)
  p ~ dunif(0,1)
  
  #Derived parameters
  
  #Probability of occupancy
  prob.occ <- mean(z[])
  
}