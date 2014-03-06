#Plots in R

#Simple scatterplot - review

ginseng <- read.table('data/ginseng.csv', header=TRUE, sep=',')
names(ginseng)

plot(ginseng$avgdbh,ginseng$treedens,xlab="Avg DBH",ylab="Tree Density")

#Add trendline using lm() and abline()
abline(lm(ginseng$avgdbh~ginseng$treedens))

#Add equation of trendline
lm(ginseng$avgdbh~ginseng$treedens)

text(x=25,y=25,labels="y = 56.07 + -1.090x")

#Add your own lines
abline(h=15)
abline(v=35)

#New point with new color
points(30,20,col="blue")

#Add some additional points
points(ginseng$avgdbh, ginseng$canopy, col="red")

#Plot a single variable over time: line graph: made up bird abundances

birds = c(5,3,7,6,10,12,11,14,17,15)

plot(birds)

#That's ok, but let's re-name the axes to something that makes sense

plot(birds, xlab="Year",ylab="Number of Birds", main="Birds over Time")

#Focus on only first 6 years

plot(birds, xlab="Year", ylab="Number of Birds", xlim=c(0,6), ylim=c(0,14),main="Birds Years 1-6")

#Change points: http://www.harding.edu/fmccown/r/symbols.png
plot(birds, pch=22, col="red", xlab="Year",ylab="Number of Birds", 
     main="Birds over Time")

plot(birds, pch="h", col="red", xlab="Year",ylab="Number of Birds", 
     main="Birds over Time")

#add line between points

plot(birds, type="o", col="blue", xlab="Year",ylab="Number of Birds", main="Birds over Time")

#Change style of line

plot(birds, type="l", col="blue", xlab="Year",ylab="Number of Birds", main="Birds over Time")
plot(birds, type="l", lty=4, col="blue", xlab="Year",ylab="Number of Birds", main="Birds over Time")

#Two or more lines / sets of points on a graph

birds2 = c(14,13,9,10,7,6,5,4,4,2)

#Start by graphing the first plot again

plot(birds, type="o", col="blue", xlab="Year",ylab="Number of Birds", main="Birds over Time", ylim=c(0,17))

#add new dataset with lines()

lines(birds2, type="o", col="red", lty=2)

#More customization - add year labels to x-axis

plot(birds, type="o", col="blue", xlab="Year",ylab="Number of Birds", main="Birds over Time", 
     ylim=c(0,17), axes=FALSE)
lines(birds2, type="o", col="red", lty=2)

axis(side=1, at=c(1,2,3,4,5,6,7,8,9,10), 
labels=c('2000','2001','2002','2003','2004','2005','2006','2007','2008','2009'))
axis(side=2, at=0:18)
box()

#Make a legend
legend(x=1,y=17.5, legend=c("Species1","Species2"),col=c("blue","red"),lty=1:2)

#This graph is pretty nice - let's save it to an image file
png(filename="bird_graph2.png",height=400, width=500, bg="white")
#start

plot(birds, type="o", col="blue", xlab="Year",ylab="Number of Birds", main="Birds over Time", 
     ylim=c(0,17), axes=FALSE)
lines(birds2, type="o", col="red", lty=2)

axis(side=1, at=c(1,2,3,4,5,6,7,8,9,10), 
     labels=c('2000','2001','2002','2003','2004','2005','2006','2007','2008','2009'))
axis(side=2, at=0:18)
box()

#end
dev.off()

#Higher resolution in tiff file (better for publications)
tiff(filename="Fig1.tiff",width=4.5,height=3.5,units="in",res=400, pointsize=8,
     compression = "lzw")
#repeat plot code here
dev.off()

#Bar graphs

#Some mast data for two species black vs. white oak
data06 = c(235,120)
data07 = c(299,179)
data08 = c(6,86)
data09 = c(7,0)
data10 = c(159,353)

all = cbind(data06,data07,data08,data09,data10)

barplot(all)
#Now put the bars beside each other
barplot(all, beside=TRUE)
barplot(all, beside=TRUE, col=c('black','gray'),names=c("2006","2007","2008","2009","2010"),xlab="Year",
        ylab="Collected Acorns", ylim=c(0,375))


legend(1,360, legend=c('Black Oak', 'White Oak'), fill=c('black','gray'), bty="n")

#Histograms
#Grab a built in dataset
data()
data(Nile)
hist(Nile)
hist(Nile, breaks=5)

#Piecharts: pie() function

#Plotting distributions
#Generate x and y values
xvec = seq(-3,3,0.01)
yvec = dnorm(xvec, mean=0, sd=1)

plot(xvec,yvec,xlab="X Value", ylab="Probability Density")
plot(xvec,yvec, type="l",xlab="X Value", ylab="Probability Density")

#Alternatively
curve(dnorm(x),-3,3)
curve(x^2,-3,3)

abline(v=1)

#Custom Error Bars

birds = c(5,3,7,6,10,12,11,14,17,15)
birds.se = c(0.5,0.2,0.7,0.5,1.0,0.9,0.9,0.7,1.3,1.2)

plot(birds, type="o", col="blue", xlab="Year",ylab="Number of Birds", 
     main="Birds over Time",ylim=c(0,18))

#Draw many individual lines - sounds like a job for a loop

segments(x0=2,y0=10,x1=2,y1=15)


for (i in 1:length(birds)){
  segments(x0=i,y0=birds[i]-birds.se[i],
           x1=i,y1=birds[i]+birds.se[i],col="red")
}

#Plot several graphs together: use par()

par(mfrow=c(2,2))
plot(birds)
plot(birds2)
hist(Nile)
plot(ginseng$avgdbh,ginseng$treedens)

#reset
par(mfrow=c(1,1))

#Broken - axis graph
#Generate some random data

x = rnorm(15,mean=5, sd=2)

#Y is from 2 different normal dists with different means
y.1 = rnorm(7, mean=5, sd=2)
y.2 = rnorm(8, mean=50, sd=4)

y = c(y.1,y.2)

plot(x,y, ylim=c(0,60))

#That looks kind of bad - let's try a broken axis

library(plotrix)

gap.plot(x,y,c(15,38), ylab="Y values", xlab="X Values")

#Add different tics with xtics/ytics

gap.plot(x,y,c(16,38), ytics=c(5,10,15,40,45,50,55),
         ylab="Y values", xlab="X Values")

#Not the most flexible solution - but it's one of many


