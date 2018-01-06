data<-read.table("fat.txt", header=FALSE,sep="")
colnames(data)<-c("triceps","thigh","midarm","fat")
attach(data)

### Residuals
result<-lm(fat~triceps+thigh)
result$residuals

##obtain leverages

tmp<-lm.influence(result)
tmp$hat

## Find MSE:
anova(result)

## Find critical value for determing outlier in response
qt(1-0.05/(2*20),20-3-1)


detach(data)