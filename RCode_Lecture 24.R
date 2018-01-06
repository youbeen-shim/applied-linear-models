############################
##Body Measurement example##
############################

data<-read.table("body.dat", header=T)
attach(data)

##convert predictors
meters<-height*2.54/100
kg<-weight*0.45359237

log.bmi<-log(kg/meters^2)
log.chol<-log(chol)
morph<-waist/hips

result<-lm(log.chol~log.bmi+morph)

##store residuals and fitted values
res<-result$residuals
yhat<-result$fitted.values

plot(yhat,res,main="Residuals against fitted values")
abline(h=0, col="red")
loess.fit<-loess(y~x, data.frame(x=yhat,y=res)) ##compute loess fit
yhat.grid<-seq(from=min(yhat),to=max(yhat),length=200) ##generate a sequence for fitted values, ranging from min to max, 200 points
tmp<-predict(loess.fit,newdata=data.frame(x=yhat.grid),
se=T) ##compute the predicted values from loess fit based on values of yhat above. se=T means to compute standard errors for loess fit.
lines(yhat.grid,tmp$fit,lwd=3, col="blue")
lines(yhat.grid,tmp$fit-2*tmp$se.fit, lwd=3, lty=2, col="blue")
lines(yhat.grid,tmp$fit+2*tmp$se.fit, lwd=3, lty=2, col="blue")



standard.res<-rstandard(result)
plot(yhat,standard.res, main="Standardized residuals against fitted values")
abline(h=0,col="red")

abs.res<-abs(standard.res)
plot(yhat,abs.res,main="Absolute values of standardized residuals against fitted values")
abline(lm(abs.res~yhat))

detach(data)

####################
##Peruvian example##
####################



data<-read.table("Peruvian.txt", header=TRUE, sep="")
attach(data)

result.y<-lm(Systol_BP~Age+Weight)
result.x<-lm(Height~Age+Weight)

res.y<-result.y$residuals
res.x<-result.x$residuals

summary(lm(res.y~res.x))

summary(lm(Systol_BP~Age+Weight+Height))

plot(res.x,res.y, main="Added-variable plot")
abline(h=0, col="red")


plot(res.x,res.y, main="Added-variable plot")
abline(h=0, col="black")
loess.fit<-loess(y~x, data=data.frame(x=res.x,y=res.y))
x.grid<-seq(from=min(res.x),to=max(res.x), length=200)
tmp<-predict(loess.fit,newdata=data.frame(x=x.grid),se=T)
lines(x.grid,tmp$fit,lwd=3, col="blue")
lines(x.grid,tmp$fit-2*tmp$se.fit, lwd=3, lty=2, col="blue")
lines(x.grid,tmp$fit+2*tmp$se.fit, lwd=3, lty=2, col="blue")

