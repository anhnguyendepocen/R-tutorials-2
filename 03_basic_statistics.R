#Summary statistics

ginseng <- read.table(file.choose(), header=T, sep=',')

ginseng[1:10,]

#Basic Summary Stats
mean(x=ginseng$treedens,na.rm=TRUE)
sd(x=ginseng$treedens)
median(x=ginseng$treedens)
range(x=ginseng$treedens)

#Standardize a variable (tree density treedens)
tr.mean = mean(ginseng$treedens)
tr.sd = sd(ginseng$treedens)

treedens.stand = (ginseng$treedens - tr.mean) / tr.sd

#Alternatively

help(scale)
treedens.stand2 = scale(ginseng$treedens)
treedens.stand2

#Issues with importing into other functions/programs
is.vector(treedens.stand2)
as.vector(treedens.stand2)

#More complex
quantile(x=ginseng$treedens,probs=0.95)
quantile(x=ginseng$treedens,probs=c(0.025,0.5,0.975))

summary(ginseng)

#Comparison scatterplot

plot(x=ginseng$avgdbh,y=ginseng$treedens,xlab="Avg DBH",ylab="Tree Density",main="Graph")

#Add trendline
#Simple linear regression model
lm(ginseng$treedens ~ ginseng$avgdbh)

#Plug into abline() - draws line over previous plot
abline(lm(ginseng$treedens~ginseng$avgdbh))

#T-tests
require(graphics)
data(sleep)

#Look at data
sleep

is.numeric(sleep$extra)
is.numeric(sleep$group)
is.factor(sleep$group)

help(t.test)
T = t.test(extra ~ group, data=sleep)

#Bayesian alternative
require(BayesianFirstAid)
Tb = bayes.t.test(extra~group, data=sleep)
Tb
summary(Tb)
plot(Tb)

#Specify different alternative hypothesis
T2 = t.test(extra ~ group, data=sleep, alternative = ('less'))

#ANOVA

#read in dataset - stress reduction treatments
setwd("Data")
stress = read.csv("stress.csv",header=TRUE)
setwd("..")

stress

#Check what format the columns are in
stress$Treatment
is.factor(stress$Treatment)

stress$Age
is.factor(stress$Age)

stress$StressReduction
is.numeric(stress$StressReduction)

#Two independent, one dependent variable

#Try a simple one-way ANOVA
help(aov)

my.anova = aov(formula= StressReduction ~ Age, data= stress)

my.anova #not all the info
summary(my.anova)

#Two way ANOVA

my.anova2 = aov(formula= StressReduction ~ Age + Treatment, data= stress)
summary(my.anova2)


#With interaction

my.anova3 = aov(formula= StressReduction ~ Age + Treatment + Age*Treatment, data= stress)
my.anova3 = aov(formula= StressReduction ~ Treatment*Age, data= stress)
summary(my.anova3)

#aov() uses type I sum of squares by default. What if we want type III?

library(car)

help(Anova)