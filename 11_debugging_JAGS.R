#####################################################################
#Baseline values

library(jagsUI)
modFile = 'models/model_debug.R'
inits = NULL
params = c('alpha','b.sex','b.latrine')

#####################################################################

#1. Problems with input data

data = read.csv('data/mice.csv',header=TRUE)
data

inp.data <- c(data$sex,scale(data$latrine),data$obs.inf,length(data$obs.inf))

###########################################################################################
out1 = jags(data=inp.data,inits=NULL,parameters.to.save=params,model.file=modFile,
                  n.adapt=100,n.iter=1000,n.burnin=500,n.thin=2,n.chains=3)
###########################################################################################

#ERROR: 'Input data must be a list of data objects or a vector of object names (as strings)'
#inp.data above is currently a vector.

inp.data

inp.data <- list(data$sex,scale(data$latrine),data$obs.inf,length(data$obs.inf))

#ERROR: 'At least one of the elements in your data list does not have a name'
#We need to name the list elements to match the model

inp.data

inp.data <- list(sex=data$sex,latrine=scale(data$latrine),obs.inf=data$obs.inf,n=length(data$obs.inf))

#ERROR: sex is a factor, we need it to be numeric.

data$sex <- as.numeric(data$sex)
data$sex[data$sex==2] = 0

inp.data <- list(sex=data$sex,latrine=scale(data$latrine),obs.inf=data$obs.inf,n=length(data$obs.inf))

#ERROR: same thing, now with a  bunch of warning messages!
#All the variables in our data statement are 'unused' - meaning they aren't found in the model
#We've probably specified the wrong model.

modFile = 'models/model_debug.R'

#ERROR: error in node - inconsistent with unobserved parents
#this can mean a lot of things. Take a look at the specified value and see if it seems weird.

data$obs.inf[150] #supposed to be 0 or 1

data$obs.inf[150] = 1 #remember to read in inp.data again!!

inp.data <- list(sex=data$sex,latrine=as.numeric(scale(data$latrine)),
                 obs.inf=data$obs.inf,n=length(data$obs.inf))

#Now the model should run.

##########################################################################################

#2. Problems with the model file syntax

#Switch to screwed up model file
modFile = 'models/model_broken.R'

###########################################################################################
out2 = jags(data=inp.data,inits=NULL,parameters.to.save=params,model.file=modFile,
                  n.adapt=100,n.iter=1000,n.burnin=500,n.thin=2,n.chains=3)
###########################################################################################

#Make a copy of model_broken.R and call it model_fixed.R

modFile = 'models/model_fixed.R'

#Error on line 1 at "for" - the very first word
#Probably we forgot to say it was a model file!
#Add model { }

#Error near "=" - can't use equal signs!!

#Unknown function dunif: it's a distribution, not a function, so can't use <- but rather ~

#Attempt to redefine node: you are looping a variable that shouldn't be looped! BUGS is trying to
#generate multiple values of b.latrine but there should be only a single value.

#Unknown distribution: probably something is spelled wrong ('dbrn' should be 'dbern')

##########################################################################################

#3. Problems with priors

modFile = 'models/model_brokenpriors.R'

###########################################################################################
out3 = jags(data=inp.data,inits=NULL,parameters.to.save=params,model.file=modFile,
                  n.adapt=100,n.iter=1000,n.burnin=500,n.thin=2,n.chains=3)
###########################################################################################

#The same 'inconsistent with unobserved parents at initialization - alpha value is forced 
#to a very high value and therefore cannot realistically be part of the linear predictor

#Now shrink to dunif(5,10)

#Seems like it ran - but check posterior distributions

densityplot(out3)

#Crunched up at 5 - estimate should be below 5
#Change to dunif(-10,10)
