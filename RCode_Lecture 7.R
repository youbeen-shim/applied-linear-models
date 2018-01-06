##################
##toluca example##
##################

data<-read.table("toluca.txt", header=FALSE,sep="")
colnames(data)<-c("size","hours")
attach(data)
result<-lm(hours~size)
plot(size,hours, main="Plot of Hours against Size")
abline(result,col="red") ##adds regression line to plot

plot(result$fitted.values,result$residuals, 
xlab="Fitted Values", ylab="Residuals", main="Plot of 
Residuals vs Fits") 
##plot of residuals vs fitted values
abline(h=0,col="red") ## add horizontal line at 0

acf(result$residuals, main="Autocorrelation of Residuals")


qqnorm(result$residuals,main="Normal Probability Plot of Residuals: Hours against Size ") ##qq plot
qqline(result$residuals, col="red") ##add qqline 

detach(data)

#################
##senic example##
#################

data<-read.table("Senic.txt", header=TRUE,sep="")
attach(data)
result<-lm(Nurses~Services)
plot(Services,Nurses, main="Plot of Nurses and Services")
abline(result,col="red")

plot(result$fitted.values,result$residuals, xlab="Fitted Values", ylab="Residuals", main="Plot of Residuals vs Fits")
abline(h=0,col="red")

acf(result$residuals, main="Autocorrelation of Residuals")



qqnorm(result$residuals,main="Normal Probability Plot of Residuals: Nurses against Services") ##qq plot
qqline(result$residuals, col="red") ##add qqline 

detach(data)
