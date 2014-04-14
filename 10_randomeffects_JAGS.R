#Random effects in BUGS
#Mixed model
#Re-visit oats example dataset from tutorial 6

#New package for running JAGS:
#http://code.kenkellner.com/simplejags/downloads/simplejags_0.01-1.zip

library(nlme)
library(MASS)
data(oats)

#Give it reasonable names
names(oats) = c('block', 'variety', 'nitrogen', 'yield')

oats[1:10,]

N  <- dim(oats)[1]
ngrp <- length(unique(oats$block))

yield <- oats$yield

#Convert blocks from roman numerals to integers

block <- as.numeric(oats$block)

#BUGS cannot handle regular factor variables - need to convert to dummy variables

#Variety: 3 levels so 2 variables needed (we will set 'victory' as the baseline)
var.golden <- var.marv <- rep(0,N)
var.golden[oats$variety=='Golden.rain'] = 1
var.marv[oats$variety=='Marvellous'] = 1

#Nitrogen: 4 levels so 3 variables needed (0 cwt will be baseline)
n6 <- n4 <- n2 <- rep(0,N)
n2[oats$nitrogen=='0.2cwt'] = 1
n4[oats$nitrogen=='0.4cwt'] = 1
n6[oats$nitrogen=='0.6cwt'] = 1

data.frame(block,var.golden,var.marv,n2,n4,n6,yield)[1:10,]

inp.data <- list(N=N,ngrp=ngrp,block=block,yield=yield,
                 var.golden=var.golden,var.marv=var.marv,n2=n2,n4=n4,n6=n6)

modFile <- 'models/model_oats.R'

params <- c('block.mean','block.sd','block.intercept','ind.sd','beta.golden', 'beta.marv',
            'beta.n2','beta.n4','beta.n6','fit','fit.new')

require(simplejags)

out <- simplejags(data=inp.data,inits=NULL,model.file=modFile,parameters.to.save=params,
                  n.chains=3,n.adapt=0,n.iter=4000,n.burnin=2000,n.thin=2)

out

#compare to frequentist
DF <- within(oats, variety <- relevel(variety, ref = 'Victory'))
oats.mixed = lme(yield ~ variety + nitrogen, random = ~1|block, data=DF)
summary(oats.mixed)

#Look at diagnostics
xyplot(out)
traceplots(out)
densityplot(out)

hist(out$sims.list$beta.n6)

postPredCheck(out,actual='fit',new='fit.new')

#GLMM

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
latrine.sc = as.numeric(scale(latrine))

#Set parameter values
#First for case with common intercept

#Random effect

#Hyperparameters
intercept.mean <- 1.7    # mu_alpha
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
require(simplejags)
out <- simplejags(
            data = my.data, 
            inits = NULL, 
            parameters.to.save = params,
            model.file = modFile, 
            n.thin = nt, n.chains = nc, n.burnin = nb, n.iter = ni 
            )

#Look at output and compare
out

xyplot(out)
traceplots(out)

densityplot(out)

truth

#Check fit
xyplot(out)
postPredCheck(out)

#Check in R
library(MASS)

lme.fit = glmmPQL(obs.inf ~ latrine, random = ~1|pop, data=data,family=binomial)

summary(lme.fit)

truth
