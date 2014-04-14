#####################################################################
#Baseline values

require(simplejags)
modFile = 'models/model_parasite_mixed.R'
inits = NULL
params = c('alpha','b.sex','b.latrine')

#####################################################################

#1. Problems with input data

data = read.csv('data/mice.csv',header=TRUE)
data

inp.data <- c(data$sex,scale(data$latrine),data$obs.inf,length(data$obs.inf))

###########################################################################################
out1 = simplejags(data=inp.data,inits=NULL,parameters.to.save=params,model.file=modFile,
                  n.adapt=100,n.iter=1000,n.burnin=500,n.thin=2,n.chains=3)
###########################################################################################

#ERROR: 'data must be a list or environment'
#inp.data above is currently a vector.

inp.data

inp.data <- list(data$sex,scale(data$latrine),data$obs.inf,length(data$obs.inf))

#ERROR: 'cannot evaluate upper index of counter i'
#What is counter i? Look at model file.
#JAGS doesn't know what n is, because we haven't told it!
#We need to name the list elements to match the model

inp.data

inp.data <- list(sex=data$sex,latrine=scale(data$latrine),obs.inf=data$obs.inf,n=length(data$obs.inf))

#ERROR: same thing, now with a  bunch of warning messages!
#All the variables in our data statement are 'unused' - meaning they aren't found in the model
#We've probably specified the wrong model.

modFile = 'models/model_debug.R'

#ERROR: 'dimension mismatch' related to data latrine. Don't worry so much about the wording -
#look at 'latrine' and see if something is up. It's not numeric because we scaled it!

inp.data <- list(sex=data$sex,
                 latrine=as.numeric(scale(data$latrine)),
                 obs.inf=data$obs.inf,n=length(data$obs.inf))

#ERROR: error in node - inconsistent with unobserved parents
#this can mean a lot of things. Take a look at the specified value and see if it seems weird.

data$obs.inf[150] #supposed to be 0 or 1

data$obs.inf[150] = 1 #remember to read in inp.data again!!

inp.data <- list(sex=data$sex,latrine=as.numeric(scale(data$latrine)),
                 obs.inf=data$obs.inf,n=length(data$obs.inf))

#NOTE: it runs even though sex is a factor in inp.data! JAGS converts it to an indicator variable.
#I do not suggest doing this. Always convert to an indicator variable first (this would crash
#if there were more than 2 categories anyways)
#In this case you get the opposite inference because JAGS converts male to '1' and female to '0'
#even though the opposite is true

inp.data$sex = as.numeric(inp.data$sex)
inp.data$sex[inp.data$sex==2] = 0

#Now, the inference is correct.

##########################################################################################

#2. Problems with the model file syntax

#Switch to screwed up model file
modFile = 'models/model_broken.R'

###########################################################################################
out2 = simplejags(data=inp.data,inits=NULL,parameters.to.save=params,model.file=modFile,
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
out3 = simplejags(data=inp.data,inits=NULL,parameters.to.save=params,model.file=modFile,
                  n.adapt=100,n.iter=1000,n.burnin=500,n.thin=2,n.chains=3)
###########################################################################################

#The same 'inconsistent with unobserved parents at initialization - alpha value is forced 
#to a very high value and therefore cannot realistically be part of the linear predictor

#Now shrink to dunif(5,10)

#Seems like it ran - but check posterior distributions

densityplot(out3)

#Crunched up at 5 - estimate should be below 5
#Change to dunif(-10,10)
