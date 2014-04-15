#Simple N-mixture model

model {
  
  #Likelihood
  
  for (i in 1:nsites){
    
    lambda[i] <- exp(alpha + b.forest*forest[i])
    
    N[i] ~ dpois(lambda[i])
    
    for (j in 1:nobs){
      
      obs[i,j] ~ dbinom(p,N[i])
         
    }
  }
  
  #Priors
  
  alpha ~ dunif(-100,100)
  b.forest ~ dnorm(0,0.01)
  p ~ dunif(0,1)
   
}