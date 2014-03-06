#Conditional Statements in R

is.numeric(5)
is.character("test")
is.numeric("5")

z = c(1,2,3,NA)
is.na(z[1])
is.na(z[4])

y = 7
y <- 7

#Double equals sign: checks if statements on either side are equal to each other
#Returns TRUE or FALSE

y == 5
y == 7

#Exclamation point - "not" - reverses the meaning, false becomes true and vice versa

y != 7

!is.numeric(5)
!is.na(z[1])


#Greater than/less than: also return TRUE or FALSE
y < 10
y > 8

#greater than or equal to
y >= 7

#Combine multiple conditions with &&; TRUE returned if  both are true, FALSE if either or both
#are false
y < 10 && y > 3

#Tell R to do something if a particular statement is TRUE or FALSE

#if() else() ifelse()
#Syntax: if(TRUE/FALSE STATEMENT) {Result if true}
#Note brackets
#Make sure argument in parentheses resolves to TRUE/FALSE!

x = 5
x= "5"

if(is.numeric(x)) {
  print("x is numeric")
}

#Add an "else" situation
#Else must come on same line as bracket closing previous statement!

if(is.numeric(x)){
  print("x is numeric")
} else{
  print("x is not numeric")
}

#this doesn't work
if(is.numeric(x)){
  print("x is numeric")
} 
else{
  print("x is not numeric")
}

#This does
if(is.numeric(x)){print("x is numeric")}else {print("x is not numeric")}

#Combine in if-else statement (similar to excel)
#Test, action if true, action if false

ifelse(is.numeric(x),"x is numeric","x is not numeric")

#More than two conditional evaluations example: else if

x = as.factor(5)

if(is.numeric(x)){
  print("x is numeric")
} else if(is.character(x)) {
  print("x is character")
} else{
  print("x is not numeric or character")
}

##Multiple conditionals chained with &&

if(is.numeric(x) && x==5){
  print("x is numeric and equal to 5")
} else if (is.numeric(x)){
  print("x is numeric but not 5")
} else{
  print("x is not numeric")
}


#Loops in R - very important for custom functions
#Though perhaps sometimes inefficient

#"While" loop example
#note {} (brackets) are necessary because >1 
#lines of code are being executed in the loop

i = 0 #index - starts at 0
while(i < 10){
  print(i)
  i = i+1  #advancing index
}

#"For" loop example

for (m in a:z){
  print(m)
  
}

#Nested loops

#silly example - fill a matrix with letters of the alphabet

letters = c('a','b','c','d','e','f','g','h','i')

#make a blank matrix

mat.example = matrix(data=NA,ncol=3,nrow=3)

mat.example

dim(mat.example)

#begin loop
nrows = dim(mat.example)[1]
ncols = dim(mat.example)[2]

k=1 #index for which letter to use
for(i in 1:nrows){
  for (j in 1:ncols){
    mat.example[i,j] = letters[k]
    k = k+1 #shift index
  } 
}

#Making custom functions

#Simple function to generate a list of random values
#(no arguments)
#Brackets again necessary here

my.randoms = function(){
  list(x = rnorm(1,0,1),
       y = rbinom(1,10,.5)
    )
}

#Now with arguments
#Simple t-test (Welches - assume var/n to be unequal)

my.ttest = function(y1,y2){
  n1 = length(y1)
  n2 = length(y2)
  
  mean1 = mean(y1)
  mean2 = mean(y2)
  
  sd1 = sd(y1)
  sd2 = sd(y2)
  
  s = sqrt(sd1^2/n1+sd2^2/n2)
  
  tst = round((mean1-mean2)/s,2)
  
  pval = round(
    1-pt(tst,df=(n1+n2-2)),2
    )
  
  result = data.frame(tst,pval)
  names(result) = c("t-value","p-value")
  
  return(result)
}

#try out the function by comparing treedensity under absence/presence of ginseng
setwd("Data")
ginseng <- read.table("GINSENG.csv", header=TRUE, sep=',')
group1= ginseng[ginseng$Abspres==0,2]
group2 = ginseng[ginseng$Abspres==1,2]

plot(group1,col="red")
points(group2, col="blue")

my.ttest(group1,group2)

#test with R built-in function
t.test(group1,group2)


