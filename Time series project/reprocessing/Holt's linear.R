setwd("C:/Users/DELL/Google Drive/JVN couse materials/Projects/Practice projects/Time series project")
library("forecast")
library("Metrics")
library("ggplot2")
library('rugarch')
library('lubridate')
library('tseries')
data=read.csv("energydata_complete.csv",row.names = 'date')

#Take a sample 
sample=ts(data$Appliances[43:1494])

train=window(sample, end=1440)
test=window(sample, start=1441, end=1452)

#Fit the model using simple Holt's linear on original series
#Configuration
Holtlinear_model=HoltWinters(train, alpha=0.1, beta=0.1, gamma=FALSE)
Holtlinear_model

#fitting and goodness of fit
mape(train[3:1440],Holtlinear_model$fitted[,1])

plot.ts(train,type="l",xlab="Time point",ylab="Energy(Wh)",
        main="Holt's linear(0.1,0.1)-fitted-original series",col='red', lty=2)
lines(Holtlinear_model$fitted[,1],lty=1, col='blue')
grid()
legend("topleft", legend=c("Fitted", "Observed"), col=c("blue","red"), 
       lty=1:2, cex=0.8, box.lty=1)
residuals=train[3:1440]-Holtlinear_model$fitted[,1]
hist(residuals,breaks=25, freq=FALSE, 
     main="Fitted residual-Holt's linear(0.1,0.1)-original series", col='blue')
qqnorm(residuals, main="Fitted QQ plot-Holt's linear(0.1,0.1)-log series",col='blue')

acf(residuals, lag.max=144*4, type='correlation', 
    plot=TRUE, main="ACF Fitted residual-Holt's linear(0.1,0.1)-original series")

#forecast
Holtlinear_model_forecast=forecast(Holtlinear_model,h=12)
autoplot(Holtlinear_model_forecast, main="Holt's linear(0.1,0.1)-original series", 
    ylab='Energy',xlim=c(1400,1453))+autolayer(test, series='data')

mape(test,Holtlinear_model_forecast$mean)

#Experiment with alpha
RMSE=c(1:10)
for(i in seq(0,0.9,by=0.1)){
  Holtlinear_model=HoltWinters(train, alpha=i+0.1, beta=0.1, gamma=FALSE)
  RMSE[i*10+1]=rmse(train[3:1440],Holtlinear_model$fitted[,1])
}
RMSE

plot(x=seq(0.1,1,by=0.1),y=RMSE, main='RMSE vs Alpha, Beta=0.1', 
     xlab='alpha', xlim=c(0,1),
     ylab='RMSE', type='l', col='red')

#Experiment with beta
RMSE=c(1:10)
for(i in seq(0,0.9,by=0.1)){
  Holtlinear_model=HoltWinters(train, alpha=0.9, beta=i+0.1, gamma=FALSE)
  RMSE[i*10+1]=rmse(train[3:1440],Holtlinear_model$fitted[,1])
}
RMSE

plot(x=seq(0.1,1,by=0.1),y=RMSE, main='RMSE vs Beta, Alpha=0.9', 
     xlab='beta', xlim=c(0,1),
     ylab='RMSE', type='l', col='blue')

#Optimize alpha and beta values to give the best Holt's linear model
bestHoltlinear_model=HoltWinters(train, alpha=NULL, beta=NULL, gamma=FALSE)
bestHoltlinear_model

"There was a warning message about this configuration:
(Warning message:
In HoltWinters(train, alpha = NULL, beta = NULL, gamma = FALSE) :
  optimization difficulties: ERROR: ABNORMAL_TERMINATION_IN_LNSRCH)
Online search: a bona fide singularity-here:https://stats.stackexchange.com/questions/112668/holt-winters-and-abnormal-termination-in-lnsrch
It could be due to the data set. Try fitting again on the logsample"

logsample=log10(sample)
train=window(logsample, end=1440)
test=window(logsample, start=1441, end=1452)

bestHoltlinear_model=HoltWinters(train, alpha=NULL, beta=NULL, gamma=FALSE)
bestHoltlinear_model
#it works! The model is Holt's linear(1,0.01253426)

#fitting and goodness of fit
MAPE(10^train[3:1440],10^bestHoltlinear_model$fitted[,1])

plot.ts(train,type="l",xlab="Time point",ylab="Energy(Wh)",
        main="Holt's linear(1,0.01253426)-fitted-log series",col='red', lty=2)
lines(bestHoltlinear_model$fitted[,1],lty=1, col='blue')
grid()
legend("topleft", legend=c("Fitted", "Observed"), col=c("blue","red"), 
       lty=1:2, cex=0.8, box.lty=1)

residuals=train[3:1440]-bestHoltlinear_model$fitted[,1]
hist(residuals,breaks=25, freq=FALSE, 
     main="Fitted residual-Holt's linear(1,0.01253426)-log series",col='blue')
qqnorm(residuals, main="Fitted QQ plot-Holt's linear(1,0.01253426)-log series",col='blue')

acf(residuals, lag.max=144*4, type='correlation', 
    plot=TRUE, main="ACF Fitted residual-Holt's linear(1,0.01253426)-log series")

#forecast
bestHoltlinear_model_forecast=forecast(bestHoltlinear_model,h=12)
autoplot(bestHoltlinear_model_forecast, main="Holt's linear(1,0.01253426)-log series", 
         ylab='Energy',xlim=c(1441,1452))+autolayer(test, series='actual series')

MAPE(10^test,10^bestHoltlinear_model_forecast$mean)

#Return forecast to original series
plot(10^bestHoltlinear_model_forecast$mean,ylab='Energy (Wh)', 
     main="Forecast-Holt's linear(1,0.01253426)-original series",
     col='blue',ylim=c(0,1000))
lines(10^test,col='red')
lines(c(1441:1452),10^bestHoltlinear_model_forecast$upper[,1],col='black',lty=2)
lines(c(1441:1452),10^bestHoltlinear_model_forecast$upper[,2],col='black',lty=3)
lines(c(1441:1452),10^bestHoltlinear_model_forecast$lower[,1],col='black',lty=2)
lines(c(1441:1452),10^bestHoltlinear_model_forecast$lower[,2],col='black',lty=3)
legend("topleft", legend=c("Fitted", "Observed",'80 level','95 level'), 
       col=c("blue","red",'black','black'), 
       lty=c(1,1,2,3), cex=0.8, box.lty=1)