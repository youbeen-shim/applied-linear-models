data<-read.table("charles.txt", header=FALSE, sep="")
colnames(data)<-c("year","Airy","Charleston")
attach(data)
result<-lm(Charleston~Airy)


confint(result,level = 0.95) ##confidence intervals for beta 0 and beta 1

summary(result) #p-value can be found in the output

