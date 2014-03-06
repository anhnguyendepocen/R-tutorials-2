#Logistic regression model of probability of parasitic infection based on covariates
#Latrine density and sex

model {
  
  #Likelihood component: iterate through each mouse
  for (i in 1:n){
    #Linear predictor (transformed via logit)
    logit(p[i]) <- alpha + b.latrine*latrine.sc[i] + b.sex*sex[i]
    
    #Model observed data - Bernoulli/coin flip
    obs.inf[i] ~ dbern(p[i])
    
    #Calculations for derived parameters
    hold.M[i] <- p[i]*(1-sex[i])
    hold.F[i] <- p[i]*sex[i]
    
    #Posterior predictive check calculations
    #absolute residual for observed data
    res[i] <- abs(obs.inf[i] - p[i])
    
    #Simulate new datapoint
    obs.inf.new[i] ~ dbern(p[i])
    #absolute residual for simulated data
    res.new[i] <- abs(obs.inf.new[i] - p[i])
    
  }
  
  #Priors (non-informative)
  alpha ~ dunif(-10,10)
  b.latrine ~ dnorm(0,0.01)
  b.sex ~ dnorm(0,0.01)
  
  #Derived parameters
  
  #mean prob. of infection by sex
  mean.M <- sum(hold.M[]) / 150
  mean.F <- sum(hold.F[]) / 150
  
  #Model fitting - posterior predictive check
  fit <- sum(res[])
  fit.new <- sum(res.new[])
  
}