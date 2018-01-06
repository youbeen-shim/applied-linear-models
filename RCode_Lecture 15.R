

###################
##Punting example##
###################

data<-read.table("punting.txt", header=TRUE, sep="")
data$Hang<-NULL
attach(data)

result<-lm(Distance~R_Strength+L_Strength+R_Flexibility+L_Flexibility+O_Strength)

pairs(data)

summary(result)

cor(data)

library(faraway)

vif(result)

result2<-lm(Distance~R_Flexibility+L_Flexibility+O_Strength+R_Strength+L_Strength)
anova(result2)

reduced<-lm(Distance~R_Flexibility+L_Flexibility+O_Strength)
summary(reduced)

vif(reduced)




#####################
##insurance example##
#####################

data<-read.table("insurance.txt", header=FALSE,sep="")
colnames(data)<-c("time","size","firm")
attach(data)

result<-lm(time~size+firm)
summary(result)

detach(data)