#Review of Linear models - use lm()

data(longley) #economic dataset  built into R
longley

#Simple linear regression
plot(longley$GNP, longley$Employed)

f = lm(formula= Employed ~ GNP, data=longley)
f = lm(formula= longley$Employed ~ longley$GNP)

summary(f)
abline(f)

abline(v=300)
abline(h=64)

#Multiple regression
#Remember model notation: * indicates all possible interactions
g = lm(formula= Employed ~ GNP*Population, data=longley)
summary(g)

#Look at some plots

plot(g)

#GLM: logistic
#Used when you have proportion data (between 0 and 1 - e.g. probabilities)
#Link function: logit ln(p/(1-p))
#Converts 0-1 values into a much wider range (~-6 to 6)

#Visualize
help(seq)
x = seq(0.001,0.999,0.001) #generate sequence of values

y = log(x/(1-x))

plot(x,y, ylab="logit(x)", type="l")
abline(v=0.5)
abline(h=0)

#Grab some of my own data: acorns infested with weevils
#acorns are either infested (1) or uninfested (0), therefore
#it's a binary (specifically, a Bernoulli) response
#Three years of data

setwd("data")
weevil = read.csv('weevil.csv', header=TRUE)
setwd("..")
weevil[1:20,]

plot(weevil$infest, weevil$julian)

#modeling infestation as a function of species and year
#Three years of data - why do I only have 2 year dummy variables?
#Along the same lines - why only 1 species variable?

#Use glm function: setup is similar, just need to specify family

help(glm)

glm.logistic = glm(formula= infest ~ species + y07 + y08, data=weevil, 
family=binomial)

summary(glm.logistic)

#Logistic regression with weights

#Create some data
#Probability of seedling survival based on whether or not the seed was buried, and
#whether or not it received an anti-predator hot sauce treatment (red)
#2x2 factorial design within each of 10 plots for total of 40 subplots
#25 seeds per subplot

nplots = 10
nsubplots = 4 #per plot
nseeds = 25 #per subplot

#Generate dummy variables for buried and red
buried = rep(c(0,0,1,1),nplots)
red = rep(c(1,0,1,0),nplots)

#Generate intercept column (could also use model.matrix)
int = rep(1,40)

#Generate subplot id list
subplot = c(1:40)

#Create design matrix
mat = cbind(int,buried,red)

#"True" parameter values (int, buried, red)
b = c(-1,4,3)

#Calculate linear predictor using matrix multiplication
lin.pred = mat %*% b

#Transform using inverse logit to probabilities
exp.p = exp(lin.pred)/(1+exp(lin.pred))

#Generate actual observed values using probabilities
obs = rbinom(40,25,exp.p)
#Column of total seeds planted
total = rep(25,40)
#Calculate percent surviving in each subplot
pct = obs/total

#Bind data together and look at it
my.data = data.frame(subplot,buried,red,obs,total,pct)

my.data

#Conduct glm using pooled subplot-level data (i.e. percents) weighted
#"Frequency" form
model1 = glm(formula = pct ~ buried + red, family = binomial, 
             weights = total, data = my.data)
b
summary(model1)

#Now, expand data so that each seed is in a single row ("case" form)

my.data2.a = cbind(my.data[rep(1:nrow(my.data),my.data$obs),c(1,2,3)],1)
names(my.data2.a)[4] = "survive"
my.data2.b = cbind(my.data[rep(1:nrow(my.data),(25-my.data$obs)),c(1,2,3)],0)
names(my.data2.b)[4] = "survive"

my.data2 = rbind(my.data2.a,my.data2.b)

dim(my.data2)

#Check
nplots*nsubplots*nseeds

#Run GLM again without weights this time
model2 = glm(formula = survive ~ buried + red, family = binomial, data = my.data2)

summary(model2)

#Compare 
b
summary(model1)

#GLM: Poisson
#Used when you have count data (i.e., positive integers only)
#Problems with Poisson: single parameter makes it hard to fit sometimes
#Negative binomial (has 2 parameters) is an alternative
#I'll stop being lazy and make my own data

help(rpois)
counts = c(rpois(20,lambda=20),rpois(20,lambda=5))

counts

#Assign group membership
group = c(rep(1,20),rep(2,20))

#Make another junk variable

junk = rnorm(40,mean=0,sd=1)

data = cbind(counts,group,junk)
data

#Log link in Poisson glm - need positive number for prediction of lambda
x = seq(-3,3) #generate sequence of values

y = exp(x)

plot(x,y, ylab  ="exp(x)", type="l")

#Glm Uses a log link function by default, which is what we want
glm.Pois = glm(counts ~ group*junk, family=poisson())

summary(glm.Pois)



#################For Later
###########################################


##Simple Linear Mixed Model (Fixed and Random Effects)

library(nlme)
library(MASS)
data(oats)

#Give it reasonable names
names(oats) = c('block', 'variety', 'nitrogen', 'yield')

summary(oats)

#First a linear model

oats.lm = lm(yield ~ variety + nitrogen, data=oats)
summary(oats.lm)

#Now we'll add random effects of block
#Block is basically repeated versions of the experiment in different plots
#We're not really interested in these variables, but you can think of block
#as random because you picked these blocks from a distribution of potential sites

#use lme()function - arguments are similar
#random  = ~1|(something) is the format you will use for random intercepts
oats.mixed = lme(yield ~ variety + nitrogen, random = ~1|block, data=oats)

summary(oats.mixed)
#Random effects listed first, then fixed effects

#Finally, the combination: General Linear Mixed Model

#Using function glmmPQL in package MASS
#My weevil data, but with a random intercept based on tree
#Tree makes a lot of sense as a random effect - they are a subset of many
#trees that could have been chosen for the study, and I'm not particularly interested
#in the variation between trees other than making sure it doesn't pollute my other inferences

weevil[1:10,]
weevil$treecode = as.factor(weevil$treecode)

weevil.mixed = glmmPQL(infest ~ species + y07 + y08, random = ~1|treecode, data=weevil, 
family=binomial(link='logit'))

summary(weevil.mixed)

