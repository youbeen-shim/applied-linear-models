data<-read.table("charles.txt", header=FALSE, sep="")
colnames(data)<-c("year","Airy","Charleston")
attach(data)
result<-lm(Charleston~Airy)



newdata <- data.frame(Airy=24) ##setting up the new value X=24
prd<-predict.lm(result,newdata,interval="prediction") ##prediction interval for Y when X=24
conf<-predict.lm(result,newdata,interval="confidence") ##confidence interval for E(Y) when X=24
prd
conf

newdata2<-data.frame(Airy=c(21,22,23)) #### Here set up three new values, and the function prediction.lm can still be used to construct 3 different prediction intervals. 
prd2<-predict.lm(result,newdata2,interval="prediction") 
conf2<-predict.lm(result,newdata2,interval="confidence") 
prd2
conf2 


plot(Airy,Charleston,ylab="Avg summer temp Charleston", xlab="Avg summer temp Mount Airy",main="Pointwise Confidence interval for E(Y), Prediction interval for Y")
abline(result) ##plot the estimated regression line
newx<-seq(min(Airy),max(Airy),by=0.01) ##set up a sequence of points for predictor variable, ranging from min(X) to max(X) in increments of 0.01
pointwise.conf.int<-predict.lm(result,newdata=data.frame(Airy=newx),interval="confidence",type="response") ##compute pointwise CI for the different values of X set up in newx
lines(newx,pointwise.conf.int[,2],col="blue") ##plot lower bound of CI
lines(newx,pointwise.conf.int[,3],col="blue") ##plot upper bound of CI
abline(v=mean(Airy),col="green") ##plot a vertical line when X=mean(X)
pointwise.prd.int<-predict.lm(result,newdata=data.frame(Airy=newx),interval="prediction",type="response") ##compute pointwise PI for the different values of X set up in newx
lines(newx,pointwise.prd.int[,2],col="red") ##plot lower bound of PI
lines(newx,pointwise.prd.int[,3],col="red") ##plot upper bound of PI
legend("topleft",c("confidence","prediction","Mean of X"), col=c("blue","red","green"),lty=c(1,1,1))



summary(result)
anova(result)
