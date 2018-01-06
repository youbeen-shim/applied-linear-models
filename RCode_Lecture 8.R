######### LOF method

data <- read.table("Copier.txt", header=TRUE ,sep="")

attach(data)

reduced<-lm(Minutes~Serviced)
full<-lm(Minutes~0+as.factor(Serviced))
anova(reduced,full)

detach(data)



############## Transformation of the data

data<-read.table("Senic.txt", header=TRUE,sep="")
attach(data)
result<-lm(Nurses~Services)


######### Regression with Original Data
plot(Services,Nurses, main="Senic Data")
abline(result, col="red")

plot(result$fitted.values,result$residuals, main="Residuals against Fitted Values")
abline(h=0, col="red")


######### Regression with Transformed Y (Nurses)

log.Nurses<-log(Nurses)

result.trans<-lm(log.Nurses~Services)

plot(Services,log.Nurses, main="Senic Data with transformed Y")
abline(result.trans, col="red")

plot(result.trans$fitted.values,result.trans$residuals, xlab="Fitted Values", ylab="Residuals",main="Residuals against Fitted Values")
abline(h=0, col="red")

summary(result.trans)

######### Regression with Transformed X (Services)


log.Services<-log(Services)
result.trans2<-lm(Nurses~log.Services)

par(mfrow=c(1,2))
plot(log.Services,Nurses, main="Senic Data with transformed X")
abline(result.trans2, col="red")

plot(result.trans2$fitted.values,result.trans2$residuals, xlab="Fitted Values", ylab="Residuals",main="Residuals against Fitted Values")
abline(h=0, col="red")


######### Regression with Transformed Y and X 



result.trans3<-lm(log.Nurses~log.Services)
mfrow=c(1,2)
plot(log.Services,log.Nurses, main="Senic Data with transformed Y and X")
abline(result.trans3, col="red")

plot(result.trans3$fitted.values,result.trans3$residuals, xlab="Fitted Values", ylab="Residuals",main="Residuals against Fitted Values")
abline(h=0, col="red")

