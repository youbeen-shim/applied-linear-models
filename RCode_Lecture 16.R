#####################
##insurance example##
#####################


data<-read.table("insurance.txt", header=FALSE,sep="")
colnames(data)<-c("time","size","firm")
attach(data)

result<-lm(time~size+firm)
summary(result)

##separate data by firm type

mutual<-subset(data,firm==0) 
stock<-subset(data,firm==1)

##separate regressions for each firm type, obtain respective fitted lines

reg1<-lm(time~size,data=mutual)
reg2<-lm(time~size,data=stock)

plot(size,time, main="Plot of Time against Size, by company type")
points(mutual$size,mutual$time, pch=20,col='red') ##scatter plots for the observations of mutual firms, the argumenet pch=20 is used to specify the type of points
points(stock$size,stock$time, pch=1,col='blue') ##scatter plots for the observations of mutual firms, a different type of points is used to plot the data
abline(reg1,lty=1,col='red') ##overlay the respective fitted lines, the type of line is specified by lty=1
abline(reg2,lty=2,col='blue') ##overlay the fitted line, with different line type
legend("topright", c("Mutual","Stock"), lty=c(1,2), pch=c(20,1),col=c('red','blue')) ##add legend

detach(data)



####################
##property example##
####################

data<-read.table("property.txt", header=TRUE, sep="")
attach(data)


##separate data by whether the house has with air conditioning or not, and perform seperate regression analysis

aircon<-subset(data,Air==1) 
no<-subset(data,Air==0)

reg1<-lm(SalePrice~SqrFeet,data=aircon)
reg2<-lm(SalePrice~SqrFeet,data=no)

plot(SqrFeet,SalePrice, main="Plot of Sale Price against Sq Footage, with and without aircon")
points(aircon$SqrFeet,aircon$SalePrice, pch=20,col='red') 
points(no$SqrFeet,no$SalePrice, pch=1,col='blue') 
abline(reg1,lty=1,col='red')
abline(reg2,lty=2,col='blue') 
legend("bottomright", c("AC","No AC"), lty=c(1,2), pch=c(20,1),col=c('red','blue'))



##note that in order to specify interaction model, use * instead of + between the name of variables

result<-lm(SalePrice~SqrFeet*Air)
summary(result)

detach(data)

################
##soap example##
################

data<-read.table("soap.txt", header=FALSE,sep="")
colnames(data)<-c("scrap","speed", "line")
attach(data)

##separate regressions based on different production line


line1<-subset(data,line==1) 
line2<-subset(data,line==0)

reg1<-lm(scrap~speed,data=line1)
reg2<-lm(scrap~speed,data=line2)

plot(speed,scrap, main="Plot of Scrap against Speed, by Production Line")
points(line1$speed,line1$scrap, pch=20,col='red') 
points(line2$speed,line2$scrap, pch=1,col='blue') 
abline(reg1,lty=1,col='red') 
abline(reg2,lty=2,col='blue') 
legend("bottomright", c("Line 1","Line 2"), lty=c(1,2), pch=c(20,1),col=c('red','blue')) 


##test for equality of variances across different classes of production line

library(lawstat)
levene.test(scrap,line) ##1st argument is Y, 2nd argument is the categorical predictor



## Regression Model with interactivve effect

result<-lm(scrap~speed*line)

summary(result)


# Check Residuals:

plot(result$fitted.values,result$residuals,xlab="Fitted Value", ylab="Residuals", main="Residual vs Fitted Value")
abline(h=0,col='red')

plot(line,result$residuals, xlab="Production Line", ylab="Residuals", main="Residuals vs Production Line")
abline(h=0,col='red')

# F-test
anova(result)


##fit additive model

result.add<-lm(scrap~speed+line)
summary(result.add)

plot(line,result.add$residuals, main="Residuals against Production Line")


