setwd("C:/Users/DELL/Google Drive/JVN couse materials/Projects/Practice projects/Time series project")
library("forecast")
library("Metrics")
library("ggplot2")
library('rugarch')
library('lubridate')
library('tseries')
MAPE<-function(actual,predict){
  mape=mean(abs((actual-predict)/actual))
  return(mape)
}

data=read.csv("energydata_complete.csv",row.names = 'date')

#Take a sample 
sample=ts(data$Appliances[43:1494], frequency=144)

train=window(sample, end=c(10,144))
test=window(sample, start=c(11,1), end=c(11,12))

#Fit the model using simple HoltWinter on original series
#Configuration
HoltWinter_model=HoltWinters(train, alpha=0.5, beta=0.1, gamma=0.1, seasonal="additive")
HoltWinter_model

#fitting and goodness of fit
mape(train[145:1440],HoltWinter_model$fitted[,1])

plot.ts(train,type="l",xlab="Time point",ylab="Energy(Wh)",
        main="HoltWinter(0.5,0.1,0.1)-fitted-original series",col='red', lty=2)
lines(HoltWinter_model$fitted[,1],lty=1, col='blue')
grid()
legend("topleft", legend=c("Fitted", "Observed"), col=c("blue","red"), 
       lty=1:2, cex=0.8, box.lty=1)

residuals=train[145:1440]-HoltWinter_model$fitted[,1]
hist(residuals,breaks=25, freq=FALSE, 
     main="Fitted residual-HoltWinter(0.5,0.1,0.1)-original series", col='blue')
qqnorm(residuals, main="Fitted QQ plot-HoltWinter(0.5,0.1,0.1)-log series",col='blue')

acf(residuals, lag.max=144*4, type='correlation', 
    plot=TRUE, main="ACF Fitted residual-HoltWinter(0.5,0.1,0.1)-original series")

#forecast
HoltWinter_model_forecast=forecast(HoltWinter_model,h=12)
autoplot(HoltWinter_model_forecast, main="HoltWinter(0.5,0.1,0.1)-original series", 
         ylab='Energy',xlim=c(10.8,11.2))+autolayer(test, series='data')

mape(test,HoltWinter_model_forecast$mean)

#Experiment with alpha
par(mfrow=c(1,3))
RMSE=c(1:10)
for(i in seq(0,0.9,by=0.1)){
  Holtlinear_model=HoltWinters(train, alpha=i+0.1, beta=0.1, gamma=0.1, seasonal="additive")
  RMSE[i*10+1]=rmse(train[145:1440],Holtlinear_model$fitted[,1])
}
RMSE

plot(x=seq(0.1,1,by=0.1),y=RMSE, main='RMSE vs alpha, beta=0.1, gamma=0.1', 
     xlab='alpha', xlim=c(0,1),
     ylab='RMSE', type='l', col='red')

#Experiment with beta
RMSE=c(1:10)
for(i in seq(0,0.9,by=0.1)){
  Holtlinear_model=HoltWinters(train, alpha=0.8, beta=i+0.1, gamma=0.1, seasonal="additive")
  RMSE[i*10+1]=rmse(train[145:1440],Holtlinear_model$fitted[,1])
}
RMSE

plot(x=seq(0.1,1,by=0.1),y=RMSE, main='RMSE vs beta, alpha=0.8, gamma=0.1 ', 
     xlab='beta', xlim=c(0,1),
     ylab='RMSE', type='l', col='red')

#Experiment with gamma
RMSE=c(1:10)
for(i in seq(0,0.9,by=0.1)){
  Holtlinear_model=HoltWinters(train, alpha=0.8, beta=0.1, gamma=i+0.1, seasonal="additive")
  RMSE[i*10+1]=rmse(train[145:1440],Holtlinear_model$fitted[,1])
}
RMSE

plot(x=seq(0.1,1,by=0.1),y=RMSE, main='RMSE vs gamma, alpha=0.1, beta=0.1', 
     xlab='beta', xlim=c(0,1),
     ylab='RMSE', type='l', col='red')

#Optimize alpha, beta and gamma values to give the best Holt's linear model
bestHoltWinter_model=HoltWinters(train, alpha=NULL, beta=NULL, gamma=NULL, seasonal="additive")
bestHoltWinter_model
#the model is HoltWinter(0.8411529,0,1)

#fitting and goodness of fit
MAPE(train[145:1440],bestHoltWinter_model$fitted[,1])

plot.ts(train,type="l",xlab="Time point",ylab="Energy(Wh)",
        main="HoltWinter(0.8411529,0,1)-fitted-original series",col='red', lty=2, ylim=c(-200,1100))
lines(bestHoltWinter_model$fitted[,1],lty=1, col='blue')
grid()
legend("topleft", legend=c("Fitted", "Observed"), col=c("blue","red"), 
       lty=1:2, cex=0.8, box.lty=1)

residuals=train[145:1440]-bestHoltWinter_model$fitted[,1]
hist(residuals,breaks=25, freq=FALSE, 
     main="Fitted residual-HoltWinter(0.8411529,0,1)-original series",col='blue')
qqnorm(residuals, main="Fitted QQ plot-HoltWinter(0.8411529,0,1)-original series",col='blue')

acf(residuals, lag.max=144*3, type='correlation', 
    plot=TRUE, main="ACF Fitted residual-HoltWinter(0.8411529,0,1)-original series")

#forecast
bestHoltWinter_model_forecast=forecast(bestHoltWinter_model,h=12)
autoplot(bestHoltWinter_model_forecast, main="HoltWinter(0.8411529,0,1)-original series", 
         ylab='Energy',xlim=c(11,11+12/144))+autolayer(test, series='actual series')

MAPE(test,bestHoltWinter_model_forecast$mean)


