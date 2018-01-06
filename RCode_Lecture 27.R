##################
##Lumber example##
##################

data<-read.table("lumber.txt", header=FALSE,sep="")
colnames(data)<-c("customers","units","income","age","competitor","distance")
attach(data)

result<-glm(customers~.,data=data,family=poisson)
summary(result)

################
##Dose example##
################

data<-read.table("dose.txt", header=T)
log.size<-log(data$size)
data<-cbind(data,log.size)
attach(data)

result<-glm(died~logdose+offset(log.size),family=poisson)
summary(result)

pearson<-residuals(result,type="pearson")
X2<-sum(pearson^2)
X2
1-pchisq(X2,7)

