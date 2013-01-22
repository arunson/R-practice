#week3
#d for density
#r for random number generation
#p for cumulative distribuiton
#q for quantile function

#to ensure reproducibility
#set.seed(1)

#set.seed(20)
#x <- rnorm(100)
#e <- rnorm(100, 0, 2)
#y <- 0.5 + 2 * x + e
#summary(y)
#plot(x,y)

#random sampling
#set.seed(1)
#sample(1:10,4)
#sample(letters, 5)
#sample(1:10) ##permutation
#sample(1:10, replace=TRUE) ## sample w/replacement

#x<-rnorm(100)
#hist(x)
#y<-rnorm(100)
#plot(x,y)
#z<-rnorm(100)
#plot(x,z)
#par(mar = c(4,4,2,2))
#plot(x,y)
#plot(x,y,pch=20)
#example(points)
#title("Scatterplot)
#text(-2,-2,"Lable")
#legend("topleft",legend="Data",pch=20)
#fit<-lm(y~x)
#abline(fit)
#abline(fit, lwd=3)
#abline(fit, lwd=3, col="blue")
#plot(x,y,xlab="Weight", ylab="Height", main="Scatterplot", pch=20)
#legend("topright", legend="Data", pch =20)
#fit<-lm(y~x)
#abline(fit,lwd=3, col="red")

#z<-rpois(100,2)
#par(mfrow=c(2,1))
#plot(x,y,pch=20)
#plot(x,z,pch19)
#par(mar=c(2,2,1,1))
#plot(x,y,pch=20)
#plot(x,y,pch=20)
#plot(x,z,pch19)
#par(mfrow=c(1,2))
#plot(x,y,pch=20)
#plot(x,z,pch19)
#par(mfrow=c(2,2))
#plot(x,y)
#plot(x,z)
#plot(z,x)
#plot(y,x)
#par(mfcol=c(2,2))
#plot(x,y)
#plot(x,z)
#plot(z,x)
#plot(y,x)

#par(mfrow=c(1,1))
#x<-rnorm(100)
#y<-x+rnorm(100)
#g<-gl(2,50,labels=c("Male","Female")
#str(g)
#plot(x,y)
#plot(x,y,type="n") #except data
#points(x[g=="Male"], y[g=="Male"], col = "green")
#points(x[g=="Female"], y[g=="Female"], col = "blue", pch=19)


xyplot(y ~ x | f,
       panel = function(x, y, ...) {
panel.xyplot(x, y, ...)
panel.abline(h = median(y),
lty = 2)
})

xyplot(y ~ x | f,
       panel = function(x, y, ...) {
               panel.xyplot(x, y, ...)
               panel.lmline(x, y, col = 2)
})

library(lattice)
package ? lattice
library(help=lattice)
data(environmental)
?environmental
head(environmental)
xyplot(ozone ~ radiation, data=environmental)
xyplot(ozone ~ radiation, data=environmental, main="Ozone vs. Radiation")
summary(environmental$temperature)
temp.cut <- equal.count(environmental$temperature, 4)
xyplot(ozone ~ radiation|temp.cut, data=environmental)
xyplot(ozone ~ radiation|temp.cut, data=environmental, layout=c(1,4), as.table=TRUE)
xyplot(ozone ~ radiation|temp.cut, data=environmental, as.table=TRUE)
 xyplot(ozone ~ radiation|temp.cut, data=environmental, as.table=TRUE, panel = function(x,y,...) {
 	panel.xyplot(x,y,...)
 	fit <- lm(y~x)
 	panel.abline(fit,lmd=2)
 })
xyplot(ozone ~ radiation|temp.cut, data=environmental, as.table=TRUE, panel = function(x,y,...) {
 	panel.xyplot(x,y,...)
 	panel.loess(x,y)
 	 })
 	 
xyplot(ozone ~ radiation|temp.cut, data=environmental, as.table=TRUE, panel = function(x,y,...) {
 	panel.xyplot(x,y,...)
 	panel.loess(x,y)
 	 }, xlab="Solor Radiation", ylab="Ozone (ppb)", main="Ozone vs. Solar Radiation")
 	 
wind.cut <- equal.count(environmental$wind,4)
xyplot(ozone ~ radiation|temp.cut * wind.cut, data=environmental, as.table=TRUE, panel = function(x,y,...) {
 	panel.xyplot(x,y,...)
 	panel.loess(x,y)
 	 }, xlab="Solor Radiation", ylab="Ozone (ppb)", main="Ozone vs. Solar Radiation")
 	 
#If you want to look at all possible combinations in this data set
splom(~ environmental)

histogram(~temperature, data = environmental)
histogram(~temperature|wind.cut, data = environmental)
histogram(~ozone|wind.cut, data = environmental)
histogram(~ozone|temp.cut*wind.cut, data = environmental)

#Mathematical annotation
plot(0, 0, main = expression(theta == 0),
     ylab = expression(hat(gamma) == 0),
     xlab = expression(sum(x[i] * y[i], i==1, n)))

x <- rnorm(100)
hist(x,
     xlab=expression("The mean (" * bar(x) * ") is " *
                       sum(x[i]/n,i==1,n)))

#Substituting
x <- rnorm(100)
y <- x + rnorm(100, sd = 0.5)
plot(x, y,
     xlab=substitute(bar(x) == k, list(k=mean(x))),
     ylab=substitute(bar(y) == k, list(k=mean(y)))
)

par(mfrow = c(2, 2))
for(i in 1:4) {
  x <- rnorm(100)
  hist(x, main=substitute(theta==num,list(num=i)))
}
