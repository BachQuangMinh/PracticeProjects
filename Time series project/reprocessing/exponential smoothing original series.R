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

sample=ts(data$Appliances[43:1494])

train=window(sample, end=1440)
test=window(sample, start=1441, end=1452)
#Fit the model using simple exponential on original series
#Configuration
exp_model=HoltWinters(train, alpha=0.1, beta=FALSE, gamma=FALSE)
exp_model

#fitting and goodness of fit
mape(train[2:1440],exp_model$fitted[,1])

plot.ts(train,type="l",xlab="Time point",ylab="Energy(Wh)",
        main="exponential smoothing-fitted-original series",col='red', lty=2)
lines(exp_model$fitted[,1],lty=1, col='blue')
grid()
legend("topleft", legend=c("Fitted", "Observed"), col=c("blue","red"), 
       lty=1:2, cex=0.8, box.lty=1)
residuals=train[2:1440]-exp_model$fitted[,1]
hist(residuals,breaks=25, freq=FALSE, 
     main='Fitted residual-exponential smoothing-original series',col='blue')
qqnorm(residuals, main='Fitted QQ plot-exponential smoothing-log series',col='blue')

acf(residuals, lag.max=144*4, type='correlation', 
    plot=TRUE, main='ACF Fitted residual-exponential smoothing-original series')

#forecast
exp_model_forecast=forecast(exp_model,h=12)
autoplot(exp_model_forecast, main='exponential smoothing-original series', 
         ylab='Energy',xlim=c(1400,1453))+autolayer(test, series='data')

mape(test,exp_model_forecast$mean)

#Experiment with alpha
RMSE=c(1:10)
for(i in seq(0,0.9,by=0.1)){
  exp_model=HoltWinters(train, alpha=i+0.1, beta=FALSE, gamma=FALSE)
  RMSE[i*10+1]=mape(train[2:1440],exp_model$fitted[,1])
}
RMSE

plot(x=seq(0.1,1,by=0.1),y=RMSE, main='RMSE vs Alpha', 
     xlab='alpha', xlim=c(0,1),
     ylab='RMSE', type='l', col='red')

#Optimize alpha value to give the best exponential smoothing model
bestexp_model=HoltWinters(train, alpha=NULL, beta=FALSE, gamma=FALSE)
bestexp_model

#fitting and goodness of fit
MAPE(train[2:1440],bestexp_model$fitted[,1])

plot.ts(train,type="l",xlab="Time point",ylab="Energy(Wh)",
        main="exponential smoothing(0.9999339)-fitted-original series",col='red', lty=2)
lines(bestexp_model$fitted[,1],lty=1, col='blue')
grid()
legend("topleft", legend=c("Fitted", "Observed"), col=c("blue","red"), 
       lty=1:2, cex=0.8, box.lty=1)
residuals=train[2:1440]-bestexp_model$fitted[,1]
hist(residuals,breaks=25, freq=FALSE, 
     main='Fitted residual-exponential smoothing(0.9999339)-original series',col='blue')
qqnorm(residuals, main='Fitted QQ plot-exponential smoothing(0.9999339)-log series',col='blue')

acf(residuals, lag.max=144*3, type='correlation', 
    plot=TRUE, main='ACF Fitted residual-exponential smoothing(0.9999339)-original series')

#forecast
bestexp_model_forecast=forecast(bestexp_model,h=12)
autoplot(bestexp_model_forecast, main='exponential smoothing(0.9999339)-original series', 
         ylab='Energy',xlim=c(1441,1452))+autolayer(test, series='actual series')

MAPE(test,bestexp_model_forecast$mean)
