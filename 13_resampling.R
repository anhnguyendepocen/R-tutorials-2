#####################################################################
##Resampling Techniques
##Useful for testing hypotheses, getting confidence intervals, etc.
##when parametric tests are inappropriate or weak.
#####################################################################

#First, read in some sample R data and conduct a traditional t-test
data(sleep)
sleep

out = t.test(extra~group,data=sleep)
out

names(out)
actual = out$statistic

#One-sided alternative
out1tail = t.test(extra~group, data=sleep, alternative='less')

#pval = 0.07939

#What was our power to detect differences?

?power.t.test

p.out = power.t.test(n=length(sleep$extra[sleep$group==1]),delta=(2.33-0.75),sd=sd(sleep$extra),alternative="two.sided")
#not great

###############################
#Randomization/Permutation Test
###############################

#We will reorder group membership randomly using sample()
#yielding a new group vector with the same number of 1s and 2s
#Then, calculate the test statistic again
#Repeat 1000+ times

#Look at sample()
?sample
sample(x=sleep$group,size=length(sleep$group),replace=FALSE)
test = sample(sleep$group)

#Check if new and old have same number in each group
summary(sleep$group)
summary(test)

#Create function

sleep.perm = function(niter){
  
  #create vector to hold results
  hold = vector(length=niter)
  
  #generate each random iteration
  for (i in 1:niter){
    
    #Permute group
    newgroup = sample(x=sleep$group)
    #Conduct test
    test = t.test(sleep$extra~newgroup)
    #Grab test statistic (t)
    hold[i] = test$statistic
  }
  
  return(hold)
  
}

#Now, run function for 1000 iterations (to generate 1000 t-test values)

perm.out = sleep.perm(niter=1000)

perm.out

#2-tailed: convert all samples to absolute value
abs.perm.out = abs(perm.out)

#Look at output
hist(abs.perm.out)

#Compare to actual value
abline(v=abs(actual),col='red',lwd=3)

#Quasi-p value: what proportion of samples from permutation test are 'more extreme'?
#I.E., what perecentage are to the left of the red line on the graph above?
mean(abs.perm.out>abs(actual))

#1-tailed

hist(perm.out)

#Compare to actual statistic from real dataset

abline(v=actual,col='red',lwd=3)

#1-tailed quasi-p (significant)
mean(perm.out<actual)


#################################
##Test on Real Data
##Spatial point pattern of tweets
##During Purdue Lockdown in Jan.
#################################

library(spatstat)

#Geo-tagged tweets from January 2014
tweets = read.csv('data/tweets.csv',header=TRUE)
head(tweets)
xrange = range(tweets$easting)
yrange = range(tweets$northing)

#Convert to Poisson point process dataset
all.ppp = ppp(x=tweets$easting,y=tweets$northing,xrange,yrange)
plot(all.ppp)

#Purdue building map
load('data/images.Rda')
plot(building.map,add=TRUE,col='red')
#Obviously, tweets clustered into buildings

#Converted to tesselation (=categorical covariate)
plot(build.tess)

#Statistical analysis of clustering
t = quadratcount(all.ppp,tess=build.tess)
quadrat.test(t)
#Highly significant (i.e., clustered)

#Tweets during lockdown
event = tweets[tweets$day==21&tweets$hour>11&tweets$hour<14,]
event.ppp = ppp(x=event$easting,y=event$northing,xrange,yrange)
dim(event)
plot(event.ppp,add=TRUE)

t2 = quadratcount(event.ppp,tess=build.tess)
t2.test = quadrat.test(t2)
t2.test
#Again, clustered into  buildings

#How to test if "more" clustered during lockdown? Randomization.
#Given constant n, larger chi-square value = dataset more clustered

#Create normal "baseline" dataset containing daytime tweets not on day of lockdown

baseline = tweets[tweets$day!=21&tweets$hour>9&tweets$hour<17,]
dim(baseline)

rand.tweets = function(niter){
  
  out = vector(length=niter)

  for (i in 1:niter){
    #create new dataset with same sample size as lockdown dataset
    index = sample(1:dim(baseline)[1],dim(event)[1],replace=FALSE)
    temp = baseline[index,]
    #Create poisson point process dataset with new data
    temp.ppp = ppp(x=temp$easting,y=temp$northing,xrange,yrange)
    
    #Conduct chi-square test on new dataset
    count = quadratcount(temp.ppp,tess=build.tess)
    chi = quadrat.test(count)
    #Save chi-square value to output vector
    out[i] = chi$statistic
  }
  #Calculate quasi-p value
  #What proportion of resampled datasets have more extreme X^2 than actual lockdown dataset?
  quasip = mean(t2.test$statistic<out)
  return(list(out=out,quasip=quasip))  
}

#Run randomization function
#This will take a while
dist = rand.tweets(200)

#Visualize distribution and actual value
hist(dist$out)
abline(v=t2.test$statistic,col='red',lwd=3)

#Significant quasi-p
dist$quasip




