outcome <- read.csv("coursera/week3/ProgAssignment3-data/outcome-of-care-measures.csv", colClasses = "character")
head(outcome)
outcome[, 11] <- as.numeric(outcome[, 11])
hist(outcome[, 11], xlab="30-day Death Rate", main="Heart Attack 30-day Death Rate")

outcome[, 17] <- as.numeric(outcome[, 17])
outcome[, 23] <- as.numeric(outcome[, 23])

par(mfrow=c(3,1))
rangeOfThis <- range(outcome[,11],outcome[,17],outcome[,23],na.rm=TRUE)
hist(outcome[, 11], xlab="30-day Death Rate", main="Heart Attack", xlim=rangeOfThis)
hist(outcome[, 17], xlab="30-day Death Rate", main="Heart Failure", xlim=rangeOfThis)
hist(outcome[, 23], xlab="30-day Death Rate", main="Pneumonia", xlim=rangeOfThis)

par(mfrow=c(1,3))
hist(outcome[, 11], xlab="30-day Death Rate", main="Heart Attack", xlim=rangeOfThis)
hist(outcome[, 17], xlab="30-day Death Rate", main="Heart Failure", xlim=rangeOfThis)
hist(outcome[, 23], xlab="30-day Death Rate", main="Pneumonia", xlim=rangeOfThis)

hist(outcome[, 11], xlab="30-day Death Rate", main="Heart Attack", xlim=rangeOfThis)
abline(v=median(outcome[,11],na.rm=TRUE))
hist(outcome[, 17], xlab="30-day Death Rate", main="Heart Failure", xlim=rangeOfThis)
abline(v=median(outcome[,17],na.rm=TRUE))
hist(outcome[, 23], xlab="30-day Death Rate", main="Pneumonia", xlim=rangeOfThis)
abline(v=median(outcome[,23],na.rm=TRUE))

hist(outcome[, 11], xlab="30-day Death Rate", main=substitute("Heart Attack (" * hat(X) == k * ")", list(k=mean(outcome[,11],na.rm=TRUE))), xlim=rangeOfThis)
hist(outcome[, 17], xlab="30-day Death Rate", main=substitute("Heart Failure (" * hat(X) == k * ")", list(k=mean(outcome[,17],na.rm=TRUE))), xlim=rangeOfThis)
hist(outcome[, 23], xlab="30-day Death Rate", main=substitute("Pneumonia (" * hat(X) == k * ")", list(k=mean(outcome[,23],na.rm=TRUE))), xlim=rangeOfThis)

hist(outcome[, 11], xlab="30-day Death Rate", main=substitute("Heart Attack (" * hat(X) == k * ")", list(k=mean(outcome[,11],na.rm=TRUE))), xlim=rangeOfThis, prob=TRUE)
lines(density(outcome[,11],na.rm=TRUE),col="red")
hist(outcome[, 17], xlab="30-day Death Rate", main=substitute("Heart Failure (" * hat(X) == k * ")", list(k=mean(outcome[,17],na.rm=TRUE))), xlim=rangeOfThis, prob=TRUE)
lines(density(outcome[,17],na.rm=TRUE),col="red")
hist(outcome[, 23], xlab="30-day Death Rate", main=substitute("Pneumonia (" * hat(X) == k * ")", list(k=mean(outcome[,23],na.rm=TRUE))), xlim=rangeOfThis, prob=TRUE)
lines(density(outcome[,23],na.rm=TRUE),col="red")

t = table(outcome$State)
outcome2<-outcome[outcome$State %in% names(t[t>20]),]

par(mfrow=c(1,1))
par(las = 2)
boxplot(death~state, ylab = "30-day Death Rate", main="Heart Attack 30-day Death Rate by State")

outcome <- read.csv("coursera/week3/ProgAssignment3-data/outcome-of-care-measures.csv",colClasses="character")
hospital <- read.csv("coursera/week3/ProgAssignment3-data/hospital-data.csv", colClasses="character")
outcome.hospital<-merge(outcome, hospital, by = "Provider.Number")
death <- as.numeric(outcome.hospital[, 11])  ## Heart attack outcome
npatient <- as.numeric(outcome.hospital[, 15])
owner <- factor(outcome.hospital$Hospital.Ownership)
xyplot(death~npatient|owner, data=outcome.hospital, xlab="Number of Patients Seen", ylab="30-day Death Rate", main="Heart Attack 30-day Death Rate by Ownership")
xyplot(death~npatient|owner, data=outcome.hospital, xlab="Number of Patients Seen", ylab="30-day Death Rate", main="Heart Attack 30-day Death Rate by Ownership", panel=function(x,y,...) {
  panel.xyplot(x,y,...)
  panel.lmline(x,y)
})
