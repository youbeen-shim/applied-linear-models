#########
##Senic##
#########

data<-read.table("Senic.txt", header=TRUE, sep="")

data<-subset(data, Stay<=14)
attach(data)

is.numeric(Region) ##see that region is treated as quantitative
levels(Region)

Region<-factor(Region) ##Convert region as categorical
is.factor(Region) ##check Region is factor
contrasts(Region) ##see coding scheme

levels(Region)
levels(Region) <- c("NE", "MW", "South", "West") ##Give names to the classes
contrasts(Region)


##separate regressions for each firm type, obtain respective fitted lines


a1<-subset(data,Region=="1") 
a2<-subset(data,Region=="2") 
a3<-subset(data,Region=="3") 
a4<-subset(data,Region=="4") 


reg1<-lm(InfctRsk~Stay,data=a1)
reg2<-lm(InfctRsk~Stay,data=a2)
reg3<-lm(InfctRsk~Stay,data=a3)
reg4<-lm(InfctRsk~Stay,data=a4)

plot(Stay,InfctRsk, main="Plot of InfctRsk against Stay, by Region")
points(a1$Stay,a1$InfctRsk, pch=1) 
points(a2$Stay,a2$InfctRsk, pch=2, col="red") 
points(a3$Stay,a3$InfctRsk, pch=12, col="blue")
points(a4$Stay,a4$InfctRsk, pch=20, col="green")
abline(reg1,lty=1)
abline(reg2,lty=2, col="red") 
abline(reg3,lty=3, col="blue")
abline(reg4,lty=6, col="green")
legend("topleft", c("NE","MW","South","West"), lty=c(1,2,3,6), pch=c(1,2,12,20), col=c("black","red","blue","green")) 


####### Regression with Interaction Effect

result<-lm(InfctRsk~Region*Stay)
summary(result)

####### Check Assumption

plot(result$fitted.values,result$residuals,xlab="Fitted Values", ylab="Residuals",main="Residual plot")
abline(h=0,col="red")

library(lawstat)
levene.test(InfctRsk,Region)

####### Reduced Model

reduced<-lm(InfctRsk~Region+Stay)
anova(reduced,result)

summary(reduced)
round(vcov(reduced),3)

