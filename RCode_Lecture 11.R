#################
##Senic Example##
#################

data<-read.table("Senic.txt", header=TRUE,sep="")
attach(data)

par(mfrow=c(2,2))

plot(Stay, InfctRsk, main="Plot of Infection Risk vs Stay")
plot(Age, InfctRsk, main="Plot of Infection Risk vs Age")
plot(Xrays, InfctRsk, main="Plot of Infection Risk vs Xrays")

result<-lm(InfctRsk~Stay + Age + Xrays)
summary(result)

plot(result$fitted.values,result$residuals,xlab="Fitted Values",ylab="Residuals",main="Plot of residuals against fits")
abline(h=0,col="red")

acf(rstandard(result), main="ACF of Residuals")

qqnorm(result$residuals, ylab="Residuals", main="Normal Probability Plot of Residuals: Senic Data")
qqline(result$residuals, col="red")

detach(data)