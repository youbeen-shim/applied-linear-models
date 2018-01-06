###############
##CDI Example##
###############

data<-read.table("CDI.txt", header=TRUE,sep="")
attach(data)

result<-lm(Unemploy~BSDegree + Income + PercentOver65)
reduced<-lm(Unemploy~BSDegree)
anova(reduced,result)

summary(result)

detach(data)

####################
##Body Fat example##
####################

data<-read.table("fat.txt", header=FALSE,sep="")
colnames(data)<-c("fat","triceps","thigh","midarm")
attach(data)

result1<-lm(fat~triceps+thigh)
result2<-lm(fat~thigh+triceps)

anova(result1)
anova(result2)

####################
##Peruvian example##
####################

data<-read.table("Peruvian.txt", header=TRUE,sep="")
attach(data)

result<-lm(Systol_BP~Age + Years + fraclife + Weight + Height + Chin + Forearm + Calf + Pulse)  #### You may also use result<-lm(Systol_BP~., data=data)
summary(result)
anova(result)

reduced<-lm(Systol_BP~Age + Years + fraclife + Weight)
anova(reduced,result)


