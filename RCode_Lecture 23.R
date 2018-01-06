####################
##Body Fat Example##
####################

data<-read.table("fat.txt", header=FALSE,sep="")
colnames(data)<-c("triceps","thigh","midarm","fat")
attach(data)

result<-lm(fat~triceps+thigh)
summary(result)

result$residuals

tmp<-lm.influence(result)
tmp$hat

rstudent(result)

dffits(result)
cooks.distance(result)
dfbetas(result)

data.reduced<-data[-3,]

result.reduced<-lm(data.reduced$fat~data.reduced$triceps+data.reduced$thigh)
summary(result)
summary(result.reduced)

detach(data)


#######################
##Teacher pay example##
#######################

data<-read.table("teacher_pay.txt", header=TRUE, sep="")
attach(data)

AREA<-factor(AREA)
levels(AREA) <- c("NE/NC", "South", "West") ##Give names to the classes

result<-lm(PAY~AREA*SPEND)
summary(result)

hii<-lm.influence(result)$hat ##leverages
student.d<-rstudent(result) ## studentized 
##residuals
plot(hii,student.d, main="Studentized res vs leverages")
n<-nrow(data)
p<-6
abline(v=2*p/n) ##plot vertical line corresponding 
##to 2p/n
abline(v=3*p/n)
abline(v=5*p/n)
identify(hii,student.d,labels=dimnames(data)[[1]]) 
##identify the states with high leverages

##identify states with DFFITS>1
DFFITS<-dffits(result)
DFFITS[abs(DFFITS)>1]
data[abs(DFFITS)>1,]

##compute yhat and yhat(i)
y<-PAY
yhat<-y-result$res
res.PRESS<-result$res/(1-hii)
yhat.i<-y-res.PRESS
cbind(yhat,yhat.i)[DFFITS>1,]

##display DFBETAS
DFBETAS<-dfbetas(result)
round(DFBETAS,3)

##Display Cook's distances
COOKS<-cooks.distance(result)
round(COOKS,3)
qf(0.5,p,n-p)


detach(data)


##remove AK
data.no.ak<-data[-50,] ##remove row 50, which is AK
attach(data.no.ak)
AREA<-factor(data.no.ak$AREA) ##set AREA as factor
levels(AREA) <- c("NE/NC", "South", "West")
result.no.ak<-lm(PAY~AREA*SPEND) 
summary(result.no.ak)
detach(data.no.ak)

##remove dc
data.no.dc<-data[-24,]
attach(data.no.dc)
AREA<-factor(data.no.dc$AREA)
levels(AREA) <- c("NE/NC", "South", "West")
result.no.dc<-lm(PAY~AREA*SPEND)
summary(result.no.dc)
detach(data.no.dc)

##remove AK and DC
data.no.akdc<-data[-c(24,50),]
attach(data.no.akdc)
AREA<-factor(data.no.akdc$AREA)
levels(AREA) <- c("NE/NC", "South", "West")
result.no.akdc<-lm(PAY~AREA*SPEND)
summary(result.no.akdc)
detach(data.no.akdc)


##plot, with AK
attach(data)
AREA<-factor(data$AREA)
levels(AREA) <- c("NE/NC", "South", "West") ##Give names to the classes

a1<-subset(data,AREA=="1") 
a2<-subset(data,AREA=="2") 
a3<-subset(data,AREA=="3") 

reg1<-lm(PAY~SPEND,data=a1)
reg2<-lm(PAY~SPEND,data=a2)
reg3<-lm(PAY~SPEND,data=a3)

plot(SPEND,PAY, main="Plot of PAY against Expenditure, by Region")
points(a1$SPEND,a1$PAY,pch=16,col="black") 
points(a2$SPEND,a2$PAY,pch=16, col="red") 
points(a3$SPEND,a3$PAY,pch=16, col="blue")
abline(reg1,lty=1)
abline(reg2,lty=2, col="red") 
abline(reg3,lty=3, col="blue")
text(SPEND[50]-150,PAY[50],"AK",col="blue")
legend("topleft", c("NE/NC","South","West"), lty=c(1,2,3), pch=c(16,16,16), col=c("black","red","blue")) 

detach(data)

##plot, without AK
attach(data.no.ak)
AREA<-factor(data.no.ak$AREA) ##set AREA as factor
levels(AREA) <- c("NE/NC", "South", "West")

b1<-subset(data.no.ak,AREA=="1") 
b2<-subset(data.no.ak,AREA=="2") 
b3<-subset(data.no.ak,AREA=="3") 

reg1<-lm(PAY~SPEND,data=b1)
reg2<-lm(PAY~SPEND,data=b2)
reg3<-lm(PAY~SPEND,data=b3)

plot(PAY~SPEND, data=data,main="Plot of PAY against Expenditure, by Region")
points(b1$SPEND,b1$PAY,pch=16,col="black") 
points(b2$SPEND,b2$PAY,pch=16, col="red") 
points(b3$SPEND,b3$PAY,pch=16, col="blue")
abline(reg1,lty=1)
abline(reg2,lty=2, col="red") 
abline(reg3,lty=3, col="blue")
legend("topleft", c("NE/NC","South","West"), lty=c(1,2,3), pch=c(16,16,16), col=c("black","red","blue"))
detach(data.no.ak)