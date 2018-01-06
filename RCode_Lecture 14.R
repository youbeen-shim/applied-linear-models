
###############################
##Partial correlation example##
###############################

data<-read.table("Measurements.txt", header=TRUE,sep="")
attach(data)

result<-lm(Height~LeftArm + LeftFoot)
anova(result)

reduced<-lm(Height~LeftArm)
anova(reduced)

detach(data)
#############################
##multicollinearity example##
#############################

x1<-c(4, 4, 4, 4, 6, 6, 6, 6)
x2<-c(2, 2, 3, 3, 2, 2, 3, 3)
y<-c(42, 39, 48, 51, 49, 53, 61, 60)

full1<-lm(y~x1+x2)
reduced2<-lm(y~x2)
full2<-lm(y~x2+x1)
reduced1<-lm(y~x1)
anova(full1)
anova(reduced2)
anova(full2)
anova(reduced1)

detach(data)

####################
##Body Fat example##
####################

data<-read.table("fat.txt", header=FALSE,sep="")
colnames(data)<-c("triceps","thigh","midarm","fat")
data$midarm<-NULL
attach(data)

pairs(~fat+triceps+thigh, main="Scatterplot matrix for body fat")

cor(data)

result<-lm(fat~triceps+thigh)

summary(result)

detach(data)


####################
##Install Package##
####################

library(faraway)
help(vif)


