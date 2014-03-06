#Simple model of the mean in JAGS/BUGS
model {
#model files always begin with this line
  
#There are  five types of  variables in a JAGS/BUGS model:
#1. From data, inputted into the model (includes covariates and indexes)
#2. The response variable (also provided as data to the model)
#3. Estimated in the model (requires a prior!)
#4. Calculated from estimated parameters (i.e., 'derived' parameters)
#these are functions of estimated parameters
#5. Latent state parameters (a special case of #3 we will talk about later)

#We need to make sure that every variable we 'name' in the model 
#is accounted for.
  
  # Likelihood - how is each of our datapoints distributed?
  #Iterate through each and define its distribution using a for loop
  
  for(i in 1:nobs){ #nobs is DATA (type #2), we will define as our sample size
    
    mass[i] ~ dnorm(population.mean, precision)
    #our RESPONSE variable is contained in the mass data object
    #each value in mass (mass[i]) is distributed normally with certain parameters: 
    #mean "population.mean" and precision "precision"
    #Precision is the inverse of variance, used instead of variance in Bayesian analysis
    
    #population.mean is an ESTIMATED parameter (type #3)
    #precision is a DERIVED parameter (type #4), a function of another parameter we haven't defined yet: SD
  }
  
  
  
  # Prior Distributions on each model parameter (uninformative)
  # Priors can be specified before or after the model likelihood (or "kernel")
  
  population.mean ~ dunif(0,5000) #uniform distribution from 0 to 5000
  population.sd ~ dunif(0,100) #uniform distribution from 0  to 100
  
  #Since the model must use precision instead of variance we need to calculate it:
  #This is an example of a derived parameter. note use of "<-" which indicates "
  #a calculation as opposed to "~" which is "distributed as"
  
  population.variance <- population.sd * population.sd   #calculate variance from sd 
  precision <- 1 / population.variance # Precision = 1/variance
  
}
#and always end with a closing bracket