model {
  
  # Likelihood
  #Iterate through each observation
  for(i in 1:nobs){
    
    #The deterministic part: the expected value for observation i
    #That is, the linear predictor alpha + betas*data
    
    pop.mean[i] <- alpha + beta.sex*sex[i] + beta.mast*mast[i]
    
    #The stochastic part - modeling the residual variation
    #Sq.mass is the actual data, modeled as a normal random variable
    #with mean equal to the linear predictor calculated above and with
    #a certain precision
    
    sq.mass[i] ~ dnorm(pop.mean[i], pop.prec)
    
  }
  
  # Priors
  
  #Convert standard deviation to precision like in last example
  pop.sd ~ dunif(0,100)
  pop.var <- pop.sd * pop.sd    
  pop.prec <- 1 / pop.var # Precision = 1/variance
  
  #Priors on intercept and slopes (betas)
  #Non-informative
  alpha ~ dunif(-500,500)
  beta.sex ~ dnorm(0,0.01)
  beta.mast ~ dnorm(0,0.01)
  
}