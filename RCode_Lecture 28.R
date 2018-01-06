
library(faraway)

##reading in data

data(chredlin)
attach(chredlin)
chredlin

##5 number summaries

summary(chredlin)

##boxplots of variables, graphical summary


par(mfrow=c(2,3))
boxplot(race, main="race")
boxplot(fire, main="fire")
boxplot(theft, main="theft")
boxplot(age, main="age")
boxplot(income, main="income")
boxplot(involact, main="involact")

##check for multicollinearity

vif(chredlin[,-c(5,7)])

##initial regression. response against main predictor of interest

summary(lm(involact~race))

##plot of fire with response and predictor

par(mfrow=c(1,2))
plot(race,involact, main="involact against race")
abline(lm(involact~race))
plot(race,fire, main="fire against race")
abline(lm(fire~race))

##fit full model

full<-lm(involact~race+fire+theft+age+log(income))
summary(full)

##model assumptions

par(mfrow=c(1,2))
plot(full$fitted.values,rstandard(full), xlab="Fitted", ylab="Standardized res", main="Residual plot")
abline(h=0, col="red")
qqnorm(full$residuals)
qqline(full$residuals)

##loess curve for residuals to check mean 0. not shown in class. 

yhat<-full$fitted.values
standard.res<-rstandard(full)

plot(full$fitted.values,rstandard(full), xlab="Fitted", ylab="Standardized res", main="Residual plot")
abline(h=0, col="red")
loess.fit<-loess(y~x, data.frame(x=yhat,y=standard.res))
yhat.grid<-seq(from=min(yhat),to=max(yhat),length=200)
tmp<-predict(loess.fit,newdata=data.frame(x=yhat.grid),
se=T)
lines(yhat.grid,tmp$fit,lwd=3, col="blue")
lines(yhat.grid,tmp$fit-2*tmp$se.fit, lwd=2, lty=2, col="blue")
lines(yhat.grid,tmp$fit+2*tmp$se.fit, lwd=2, lty=2, col="blue")

##check for influential points

COOKS<-cooks.distance(full)
sort(COOKS)
qf(0.5,6,41)
COOKS[COOKS>qf(0.2,6,41)]


##what are the influential points

chredlin[c(6,24),]

##model with 2 influential points removed

removed<-lm(involact~race+fire+theft+age+log(income), data=chredlin, subset=-c(6,24))
summary(full)
summary(removed)

##added variable plots

par(mfrow=c(1,2))
prplot(full,1)
prplot(full,2)

par(mfrow=c(1,3))
prplot(full,3)
prplot(full,4)
prplot(full,5)

##create new dataset with the 2 influential points removed

data.red<-chredlin[-c(6,24),]
attach(data.red)

##all possible regressions

library(leaps)

b<-regsubsets(involact~race+fire+theft+age+log(income), force.in=1, data=data.red)
summary(b)

summary(b)$adj
summary(b)$cp
summary(b)$bic

##fit the 2 competitive models

result<-lm(involact~race+fire+theft+age)
result2<-lm(involact~race+fire)

summary(result)
summary(result2)

##general linear F test

anova(result2,result)

##go back to full data 

data(chredlin)
attach(chredlin)

##fit model with full data

result.full<-lm(involact~race+fire)
summary(result.full)

##check residuals

par(mfrow=c(1,2))
plot(result.full$fitted.values, rstandard(result.full), main="Residual Plot")
qqnorm(result.full$residuals)
qqline(result.full$residuals)

##transform response and refit model and check diagnostics

sqrt.y<-sqrt(involact)

result.2<-lm(sqrt.y~race+fire)
summary(result.2)

par(mfrow=c(1,2))
plot(result.2$fitted.values, rstandard(result.2), main="Residual Plot")
qqnorm(result.2$residuals)
qqline(result.2$residuals)

##add side in.

south<-lm(involact~race+fire, subset=(side=="s"),chredlin)
north<-lm(involact~race+fire, subset=(side=="n"),chredlin)

summary(south)
summary(north)

