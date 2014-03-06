#Poisson regression - effect of April temp and species on acorn production

model {
  
  #Likelihood
  for (i in 1:ntrees){
    
    #Calculate linear predictor
    log(lambda[i]) <- alpha + b.mintemp*mintemp[i] + b.species2*species2[i] + b.species3*species3[i]
    
    #Model observed data
    counts[i] ~ dpois(lambda[i])

  }
  
  #Priors (non-informative)
  alpha ~ dunif(-10,10)
  b.mintemp ~ dnorm(0,0.01)
  b.species2 ~ dnorm(0,0.01)
  b.species3 ~ dnorm(0,0.01)
  
  #Calculations for derived parameters
  #Difference between species 3 and 2
  diff.sp3.sp2 <- b.species3 - b.species2
  

}