
data<-read.table("charles.txt", header=FALSE, sep="")
colnames(data)<-c("year","Airy","Charleston")
attach(data)

plot(Airy,Charleston,xlab="Mount Airy Avg Summer Temp", ylab="Charleston Avg Summer Temp")
result<-lm(Charleston~Airy)
abline(result)

####### abline is a function that adds the estimate regression line to the existing scatter plot. you need to use this function with an active plot window. 

summary(result)
cor(Airy,Charleston)
