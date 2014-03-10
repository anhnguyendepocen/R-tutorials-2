##GLMs in JAGS/BUGS

##Binomial/Logistic Regression

##############
##Simulate data
#Examining probability of parasitic infection in mice as a function of 
#exposure (i.e. latrine density) and sex

#300 mice
n = 300

#Generate random latrine densities (per ha)
#and standardize

latrine = runif(n, 5,20)
latrine.sc = as.numeric(scale(latrine))

#Generate M/F vector M=0 F=1

sex = c(rep(0,150),rep(1,150))

#Set parameter values
#First for case with common intercept

alpha = 1.2
b.latrine = 0.8
b.sex = 2

truth = cbind(alpha,b.latrine,b.sex)

#Generate observed data

#empty vector of linear predictors
linpred = vector(length=n)
#empty vector of infection probabilities
meanp=vector(length=n)
#empty vector of observed infection status
obs.inf=vector(length=n)

#loop over every individual
for (i in 1:n){
  linpred[i] = alpha + b.latrine*latrine.sc[i] + b.sex*sex[i]
  #inverse logit transform linpred to get p
  meanp[i] = exp(linpred[i])/(1+exp(linpred[i]))
  #generate actual observations
  obs.inf[i] = rbinom(1,p=meanp[i],n=1)                                              
}

###############################
##Setup analysis in JAGS
library(R2jags)

#Identify model file
modFile = "models/model_parasite.R"

#Bundle data
inp.data = c('n','obs.inf','latrine.sc','sex')

#Parameters to save
params = c('alpha','b.latrine','b.sex',
           #derived parameters
           'mean.M','mean.F',
           #posterior predictive check
           'fit','fit.new'
           )

#Inits
#going with JAGS defaults

#MCMC settings
nc = 3
ni = 4000
nb = 2000
nt = 20

#JAGS call
require(R2jags)
out <- jags(
  model.file = modFile,
  data = inp.data, 
  parameters.to.save = params,
  n.thin = nt, n.chains = nc, n.burnin = nb, n.iter = ni,
  #No inits necessary for this run 
  inits = NULL
)

#Look at results
out

#Compare to frequentist output
summary(glm(obs.inf~latrine.sc+sex, family=binomial))

#Compare to truth
truth

#Plot derived parameters
attach(out$BUGSoutput$mean)
barplot(c(mean.M,mean.F),names=c('Male','Female'),ylab="Probability of Infection",ylim=c(0,1))
detach()

#Posterior predictive check: check model fit
#Points should be scattered evenly above and below line

plot(out$BUGSoutput$sims.list$fit,out$BUGSoutput$sims.list$fit.new)
abline(0,1, lwd = 2, col = "black")

#Calculate Bayesian p-value

Bpval = mean(out$BUGSoutput$sims.list$fit.new > out$BUGSoutput$sims.list$fit)

#Values near 0.5 are ideal
Bpval
#reasonable fit


########################################################

##Poisson Regression

#########
##Simulate data
##Acorn counts at trees as a function of oak species and minimum april temperature

ntrees = 300

#Generate minimum temperatures in F and scale

mintemp = as.numeric(scale(rnorm(300,39.8,10)))

#Generate species assignment: 3 species, so 2 covariates

species2 = c(rep(0,100),rep(1,100),rep(0,100))
species3 = c(rep(0,100),rep(0,100),rep(1,100))

#Set parameter values

alpha = 0.4
b.temp = 2
b.species2 = -1
b.species3 = 1

truth = cbind(alpha,b.temp,b.species2,b.species3)

#Generate data
#Hold linear predictors
linpred = vector(length=ntrees)
#Hold mean values (lambdas)
lam = vector(length=ntrees)
#Hold observed values
counts = vector(length=ntrees)

for (i in 1:ntrees){
  linpred[i] = alpha + b.temp*mintemp[i] + b.species2*species2[i] + b.species3*species3[i]
  #log-transform
  lam[i] = exp(linpred[i])
  #Generate observed data
  counts[i] = rpois(1,lambda=lam[i])
}

#Look at data 
counts

###################
##Analysis in JAGS
library(R2jags)

#Identify model file
modFile = "models/model_acorns.R"

#Bundle data
inp.data = c('ntrees','counts','mintemp','species2','species3')

#Parameters to save
params = c('alpha','b.mintemp','b.species2','b.species3',
           #derived parameters
           'diff.sp3.sp2'
)

#Inits
#going with JAGS defaults

#MCMC settings
nc = 3
ni = 4000
nb = 2000
nt = 20

#JAGS call

out <- jags(
  model.file = modFile,
  data = inp.data, 
  parameters.to.save = params,
  n.thin = nt, n.chains = nc, n.burnin = nb, n.iter = ni,
  #No inits necessary for this run 
  inits = NULL
)

#Look at results
out

#Compare to frequentist approach
summary(glm(counts ~ mintemp + species2 + species3, family=poisson))

#Compare to truth

truth