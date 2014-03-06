#Brief Introduction to R - Tutorial 2

#Loading libraries example

install.packages("R2jags")
#Or you can do via GUI

#Must be loaded every time
library(R2jags)
??R2jags
#Only load if needed
require(R2jags)


#Getting your data into R

#Working directory - where files are loaded from/saved to

getwd()

#Don't forget quotes!
setwd("Data")
list.files()

#Also can use GUI

#Input data - csv file method

my.data = read.csv('dispersal.csv',header=TRUE)
#or
my.data = read.table('dispersal.csv',header=TRUE,sep=',')
#or via GUI (this only works in Windows)
my.data = read.csv(file.choose(),header=TRUE)

#Always look at data to make sure it's right
my.data

#Data frames - a flexible data structure

is.data.frame(my.data)
#See column names
names(my.data)

#Multiple types of data

#Selecting a column of data
my.data[,11]

my.data$dist

#Writing out full path could get tedious

dist
#This doesn't work


attach(my.data)
#Now it does!
dist
Tree

#Different types of data (numeric vs. character)
is.numeric(dist)
is.numeric(Tree)
is.factor(Tree)

#Usually a good idea to detach when you are done to avoid confusion later
detach()
#Doesn't work anymore
dist

#Making a data frame from scratch

#Generate some fake normal data
col1 = as.vector(rnorm(10))
help(rnorm)

#Coin-flip data
col2 = as.vector(rbinom(10,1,0.5))

mat = as.matrix(cbind(col1,col2))

frame = as.data.frame(mat)

names(frame) = c('normal','int')

##Managing Workspaces
#Save what you are doing

save.image()
#Saves a file".Rdata" containing everything you've done this session
#Also can be done via GUI

save(my.data,file="mydata.Rdata")
#Saves an R object (like a dataset)

load("mydata.Rdata")
#Re-load into workspace
#Also can be done in GUI

##Projects in Rstudio - like an extended workspace