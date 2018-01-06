############ Drunk Driving Example (Ungrouped Data)

data<-read.table("students.txt", header=T)
attach(data)

result2<-glm(DrivDrnk~DaysBeer+Gender, family=binomial)
summary(result2)

########## Dosage Example (Grouped Data)

data<-read.table("dose.txt", header=T)
attach(data)

p<-died/size

plot(logdose, log(p/(1-prop)), xlab= "Log Dose", ylab="log odds")

result<-glm(p~logdose, family=binomial, weights=size)
summary(result)

pearson<-residuals(result,type="pearson")
X2<-sum(pearson^2)
X2
1-pchisq(X2,9-2)

