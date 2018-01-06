data<-read.table("students.txt", header=T)
attach(data)


table(DrivDrnk)   ############ DrivDrnk is a categorical variable
contrasts(DrivDrnk)   ############ Use contrasts function to check that R treats the value "Yes" as 1


result<-glm(DrivDrnk~DaysBeer, family=binomial)  ######## Logistic Regression. 
summary(result)

plot(DaysBeer,result$fitted.values, ylab="Predicted Probability", xlab="DaysBeer", main="Predicted Probability vs Daysbeer")

contrasts(Gender)

result2<-glm(DrivDrnk~DaysBeer+Gender, family=binomial)
summary(result2)

##
