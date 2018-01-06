data<-read.table("uscrime.txt", header=TRUE, sep="")
attach(data)


#########
##leaps Package##
#########

library(leaps)

##find best 2 subsets for up to size 8

b<-regsubsets(Crime~.,data=data,nbest=2,nvmax=8)
####### nbest is used to set up the numbers of models of each size to record. No matter how we set up this argument, this function will still examine all the modes as specified by the other arguments. However, it will only record the best models up to the number provided by this arguemnt. This feature will be useful when we have a large number of predictors. 
####### nvmax can be used to set a limit on the number of predictors considered in each models.
####### For other useful arguments,refer to the R tutorial for detail.

summary(b)
plot(b,scale="bic", main="BIC criterion")
plot(b,scale="adjr2", main="adj R2 criterion")
plot(b,scale="Cp", main="Cp criterion")


#####################
##forward selection##
#####################

start<-lm(Crime~1, data=data)

end<-lm(Crime~.,data=data)

result<-step(start, scope=list(lower=start,
upper=end), direction="forward", trace=FALSE)
summary(result)

result<-step(start, scope=list(lower=start,
upper=end), direction="forward")

########################
##backward elimination##
########################

result.b<-step(end, direction="backward",trace=FALSE)
summary(result.b)

result.b<-step(end, direction="backward")

#######################
##stepwise regression##
#######################

result.s<-step(start, scope=list(upper=end), 
direction="both",trace=FALSE)
summary(result.s)

result.s<-step(start, scope=list(upper=end), 
direction="both")

##different starting model

result.s2<-step(lm(Crime~Time+M+Po2+U1), 
scope=list(upper=end), direction="both",
trace=FALSE)
summary(result.s2)

result.s2<-step(lm(Crime~Time+M+Po2+U1), 
scope=list(upper=end), direction="both")


detach(data)
