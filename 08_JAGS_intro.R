mass <- rnorm(n = 1000, mean = 600, sd = 30) # Sample of 1000 birds
nobs <- length(mass)

# Plot data
hist(mass, col = 'grey', main = ' Body mass (g) of 1000 birds')
mean(mass)
sd(mass)

#Frequentist analysis
freq = lm(mass ~ 1)
summary(freq)

#Bayesian Analysis
library(R2jags)

###What do we need for this analysis?

#1. JAGS Model Specification
  #a. Priors
  #b. Likelihood

#2. Our actual collected data (and related values needed by the model)

#3. Tell JAGS which parameters you
#   Want "saved" - i.e., which parameters you are interested in

#4. Information about how to run MCMC: number of chains, thin rate, burn in

#5. Initial values for those chains

#Then, we bundle this information together in a certain way and
#feed it into R2jags, which sends it to JAGS and then returns the output.


####Step 1: Specify your model 

#Models for JAGS are defined in a separate file (either .txt or .R)
#Model syntax is very similar to R with some key differences
#For example "<-" must be used instead of "="

#make sure the file is in current working directory
getwd()
setwd("Models")
list.files()

#The model itself is in a separate R script - put the path into an object
modFile = "model_mean.R"

####Step 2: Actual collected data

#Our data includes both the mass measurements and sample size needed
#By the jags model we just specified

#We need to call the measurements data 'mass' and the sample size 'nobs'
#(names must correspond to model file)
#then bundle them together into a list
#Response variables, covariates, and indexes all belong in this list/vector

inp.data <- c('mass','nobs')

#####Step 3: Tell JAGS which parameters to 'save'

#Will be estimated parameters and derived parameters

#Basically, a vector of names of parameters in the model we
#want posterior distribution output for
#Note the parameter names must be in quotes
#Not all parameters in model file need to be saved (for example we aren't saving precision)

params <- c("population.mean", "population.sd")

#####Step 4: Information about how to run MCMC

nc <- 3 # Number of chains
ni <- 1000 # Number of draws from posterior (for each chain)
nb <- 500 # Number of draws to discard as burn-in
nt <- 10 # Thinning rate (avoids autocorrelation)

#####Step 5: Specify initial values

#Unlike BUGS, JAGS can generally specify its own initial values automatically
#Generally it is smart to let it do so
#Sometimes you will need to specify initial values though (for example, certain
#latent variables)
#To specify initial values we create a function that generates a list of random values
#for each parameter to start the chain
#But we will not use it for this analysis

inits <- function(){
list (
      population.mean = rnorm(1,600), 
      population.sd = runif(1, 1, 30)
      )
}

#####That's everything - now bundle into R2jags

#Function jags() is called to perform the analysis in JAGS
# and put its results into an R object called out:

out <- jags(
            model.file = modFile,
            data = inp.data, 
            parameters.to.save = params,
            n.thin = nt, n.chains = nc, n.burnin = nb, n.iter = ni,
            #No inits necessary for this run 
            inits = NULL
            )

#Progress bar(s) will be shown

#Look at results
out

names(out)
names(out$BUGSoutput)

out$BUGSoutput$sims.array

#Look at convergence plots manually
plot(out$BUGSoutput$sims.array[,1,2],type="l",col="red",ylim=c(590,610))
lines(out$BUGSoutput$sims.array[,2,2],type="l",col="blue")
lines(out$BUGSoutput$sims.array[,3,2],type="l",col="green")

#easier way
traceplot(out)

#Look at posterior distributions for important parameters using hist()
hist(out$BUGSoutput$sims.list$population.mean)
hist(out$BUGSoutput$sims.list$population.sd)

#Inference example
#What is the probability the mean is greater than 598?
#This question cannot be answered directly with frequentist stats
#We simply calculate the proportion of hte posterior that is > 598

prob <- mean(out$BUGSoutput$sims.list$population.mean>598)

prob

#####Example 2: Simple linear regression (intercept + 2 covariates)

#Squirrel mass as a function of mast availability and sex

#Generate data
#200 squirrels

#Males = 0, females = 1
sex = c(rep(0,100),rep(1,100))

#Index of mast availability in a given squirrel's home range (mean acorn count or something)

mast = rnorm(200, mean=100, sd=30)

#True relationship

intercept = 350

#Betas
coef.sex = 50
coef.mast = 0.5
#Episilon - individual variation
resid.sd = 30

true = cbind(intercept, coef.sex, coef.mast, resid.sd)

#Generate 'observed' data

sq.mass = vector(length=200)
for (i in 1:200){
  sq.mass[i] = intercept + coef.sex*sex[i] + coef.mast*mast[i] + rnorm(1,0,resid.sd)
}

sq.mass
hist(sq.mass)

#Frequentist Analysis

freq.sq = lm(sq.mass ~ sex + mast)
summary(freq.sq)

#Start Analysis

####Step 1: Specify model 

getwd()
setwd("Models")
list.files()

modFile = "model_sq.R"

####Step 2: Actual collected data

#Another way to pass data to jags - list of actual data instead of a list of the data object names
sq.data <- list(
                sq.mass = sq.mass, 
                nobs = length(sq.mass), 
                sex=sex, 
                mast=mast
                )

#####Step 3: Tell JAGS which parameters to 'save'

#Basically, a list of names of parameters in the model we
#want posterior distributions for

params <- c("alpha", "beta.sex", "beta.mast", "pop.sd")

#####Step 4: Information about how to run MCMC

nc <- 3 # Number of chains
ni <- 1000 # Number of draws from posterior (for each chain)
nb <- 100 # Number of draws to discard as burn-in
nt <- 10 # Thinning rate

#####Step 5: Specify initial values

#NOTE: INITIAL VALUES CANNOT BE OUTSIDE PRIOR DISTRIBUTION!!!
#As before, we don't actually need to use this in JAGS
#You can put an initial value on anything you specified a prior for.


inits <- function(){
list (
      alpha = runif(1, -500, 500), 
      pop.sd = runif(1, 1, 50),
      beta.sex = rnorm(1,mean=0, sd=100),
      beta.mast = rnorm(1, mean=0, sd=100)
      )
}

#####That's everything - now bundle into R2jags

sq.out <- jags(
            model.file = modFile,
            data = sq.data, 
            parameters.to.save = params,
            n.thin = nt, n.chains = nc, n.burnin = nb, n.iter = ni,
            inits = NULL
            )

sq.out
            
true   

traceplot(sq.out)
            
#Inference
            
hist(sq.out$BUGSoutput$sims.list$beta.mast, xlim=c(0,1))


