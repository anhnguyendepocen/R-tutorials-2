#Brief Introduction to R - Tutorial 1

#Why use RStudio?
#1. Organization
#2. Syntax highlighting!
#3. Advanced features - Projects, Debugging, plot managment

#Working in the console - basic operations

#This is a comment

3 + 4
3 * 4
3 / 4
3 ^ 4
(3 + 4) ^ 2

#Basic math

log(0.5)
log10(0.5)
exp(0.5)
abs(-1)
#sin, cos, tan, etc.

#Slightly more complex functions

max(0,1,2,3,4)
min(0,1,2,3,4)


#Aside: using the help files in R

help(max)
?max
#Search
??max

#Types of data
#numeric: double, integer
is.numeric(1)
is.double(1)
is.double(1.0)
is.integer(1)

#string/character: quotation marks

is.numeric("1")
is.character("1")
is.character("Hello world")

#Conversion
as.numeric("1")
as.character(1)


#factor: categorical data; useful in analyses; we'll come back to them later

#How is data stored in R?
#Types of 'data structures'

#Vector: 1 dimension, contains only a single type of data
#Assigning a variable - c() denotes vector

x = 1
x <- 1

x = c(0,1,2,3,4)
x <- c(0,1,2,3,4)
x

y = vector(length=10)
y

#What happens when you try to combine data types?
z = c("1",2)
z

#Operations on vector

mean(x)
sd(x)

2*x
x/2

#Select/replace element of vector

x[1]

x[1] = 10

#More complex

x[1:3]
x[x > 2]

#Combining vectors 
x
y = c(5,6,7,8,9)
cbind(x,y)
rbind(x,y)

#Matrix: 2 dimensions

help(matrix)

#Select part of our x vector and name it something else
new.x = x[1:4]

#Create matrix (default)
z = matrix(data=new.x, nrow=2, ncol=2)
#Create matrix and fill in data  by row
y = matrix(data=new.x, nrow=2, ncol=2, byrow=T)

#Operations on matrix
2*y
log(y)

y[1,1]

#Matrix multiplication
y %*% y

#Select/replace rows and columns
y[,1]
y[1,]

y[1,1] = 10

#Array: >=2 dimensions

help(array)

#Create a 2x2x3 array (3 copies of matrix above)
z = array(data=y, dim=c(2,2,3))

#Operations on array
2*z

z[1,1,1]

#Filling a blank array

help(array)

my.array = array(data=NA, dim=c(2,2,3))

my.array[,,1] = y

##Variable Conversion and Information

is.vector(x)
is.matrix(y)
is.array(z)

as.vector(y[1,])
as.matrix(z[,,1])