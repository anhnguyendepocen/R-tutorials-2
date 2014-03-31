#Simulate data
#Examining probability of parasitic infection in mice as a function of 
#exposure (i.e. latrine density), and later, population membership as a random effect

#How would you add random effects (specifically, a random intercept based on population)
#To this model?

#Generate data for this example

npop = 15 #n pops
nmice = 20 #n in each pop

n = npop*nmice

#Assign population membership

pop = gl(n = npop, k = nmice)

#Generate random latrine densities (per ha)

latrine = runif(n, 5,20)
latrine.sc = scale(latrine)

#Set parameter values
#First for case with common intercept

#Random effect

#Hyperparameters
intercept.mean <- 1.2    # mu_alpha
intercept.sd <- 0.5  	# sigma_alpha
#Generate random intercepts
intercept.effects<-rnorm(n = npop, mean = intercept.mean, sd = intercept.sd)
#True slope value
b.latrine = 0.8

truth = cbind(intercept.mean,intercept.sd,b.latrine)

#Generate observed data
#Note double-indexing of intercept effects to get the appropriate intercept for a mouse of
#population i

#Expected mean probability of infection for each mouse
meanp=vector(length=n)
#Observed infection status for each mouse
obs.inf=vector(length=n)
for (i in 1:n){
  meanp[i] = exp(intercept.effects[pop[i]] + b.latrine*latrine.sc[i])/
            (1+exp(intercept.effects[pop[i]]+b.latrine*latrine.sc[i]))
  obs.inf[i] = rbinom(1,p=meanp[i],n=1)                                              
}

#Create dataframe
data = data.frame(obs.inf,pop,latrine.sc)
names(data) = c('obs.inf','pop','latrine')

#Fit JAGS model

#Specify model file

modFile = 'models/model_parasite_mixed.R'

#Bundle data

my.data <- list(infect = data[,1], 
                   nobs = length(data[,1]), 
                   pop = data[,2],
                   npop = length(unique(data[,2])),
                   latrine = data[,3]
                   )

#Tell JAGS which parameters to 'save'

params <- c("alpha.mean", "alpha.sd", "beta.latrine","fit","fit.new")

#Information about how to run MCMC

nc <- 3 # Number of chains
ni <- 5000 # Number of draws from posterior (for each chain)
nb <- 2000 # Number of draws to discard as burn-in
nt <- 5 # Thinning rate

#Run JAGS
require(R2jags)
out <- jags(
            data = my.data, 
            inits = NULL, 
            parameters.to.save = params,
            model.file = modFile, 
            n.thin = nt, n.chains = nc, n.burnin = nb, n.iter = ni, 
            )

#Look at output and compare
out

truth

#Check fit
plot(out$BUGSoutput$sims.list$fit,out$BUGSoutput$sims.list$fit.new)
abline(0,1)

#Bayesian p-value
mean(out$BUGSoutput$sims.list$fit>out$BUGSoutput$sims.list$fit.new)

#Check in R
library(MASS)

lme.fit = glmmPQL(obs.inf ~ latrine, random = ~1|pop, data=data,family=binomial)

lme.fit

truth
