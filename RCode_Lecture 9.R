########### Boxcox Transformation with Senic data

data<-read.table("Senic.txt", header=TRUE,sep="")
attach(data)
result<-lm(Nurses~Services)


library(MASS) ##boxcox function in MASS package.
boxcox(result)
boxcox(result, lambda = seq(-0.5, 0.5, 0.001))

