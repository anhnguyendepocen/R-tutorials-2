#How can we estimate unknown parameter values from a set of data?

#Generate a set of random Bernoulli data with known p
n = 1
p = 0.3
samples = 1000
Y = rbinom(samples, n, p)

mean(Y) / 1
#Maximum likelihood estimator

#What does the probability distribution of a Binomial look like?
#Take the log; sum for each value of Y

#The value of theta which maximizes this sum is the 'maximum likelihood' estimate

#Can be solved analytically using calculus because it's simple

#Function to do this in R (typically we call the vector of parameters 'theta')

loglike = function(theta, n, y){

sum(y*log(theta) + (n-y)*log(1-theta))
}

loglike(theta=0.7, n=1, y=Y)
loglike(theta=0.1, n=1, y=Y)
loglike(theta=0.3, n=1, y=Y)

#Plot this to get a better idea of what's going on

#First, generate sequence of theta values from 0 to 1

theta.seq = seq(0.001,0.999,0.001)

#Generate likelihoods for each theta.seq value using the function and our data
#First, make a vector to hold the theta.seq values and corresponding likelihoods
like.output = vector(length=length(theta.seq))

for (i in 1:length(theta.seq)){
  like.output[i] = loglike(theta=theta.seq[i],n=1,y=Y)
}

plot(theta.seq,like.output,xlab='Theta Values', ylab='Likelihood', type='l')

#Maximized at around 0.3 - what if we wanted to calculate the exact value?

#Alternative - demonstrate optim() function in R
#Optim looks for global minima, not maximia, so we need to flip our curve
#Define a new function negloglike

negloglike = function(theta, n, y){

-(sum(y*log(theta) + (n-y)*log(1-theta)))
}

neg.like.output = vector(length=length(theta.seq))

for (i in 1:length(theta.seq)){
  neg.like.output[i] = negloglike(theta=theta.seq[i],n=1,y=Y)
}
plot(theta.seq,neg.like.output,xlab='Theta Values', ylab='-logLikelihood', type='l')

#Now use optim() to find minimum

help(optim)

mle.out=optim(par=0.5, negloglike, method="L-BFGS-B",
lower=c(0.0001), upper=c(0.9999), hessian=T, n=1, y=Y )

mle.out

mean(Y)
#Same answer - because ML estimator (obtained via calculus) for binomial is the mean

#CIs from optim(): standard error is the square root of the inverse of the hessian matrix

se = sqrt(solve(mle.out$hessian))
ci = c(mle.out$par-1.96*se,mle.out$par+1.96*se)
ci


#Likelihood ratio test for hypotheses

#Null hypothesis (which we know to be true)
theta.null = 0.3

#Our obtained value
theta.alt = mle.out$par

#Calculation of deviance, or likelihood ratio
#Deviance is equal to 2*the difference between log(null) - log(alternative))

deviance = function(theta.null, theta.alt, n, y){
  -2*((sum(y*log(theta.null) + (n-y)*log(1-theta.null))) - 
    (sum(y*log(theta.alt) + (n-y)*log(1-theta.alt))))    
}
  
TS = deviance(theta=theta.null, theta.alt=theta.alt, n=1, y=Y)

#Deviance is distributed chi-square with one degree of freedom

critvalue = qchisq(0.95,1)

pvalue = 1-pchisq(TS,1)

critvalue

TS

pvalue