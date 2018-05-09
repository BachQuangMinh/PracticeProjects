setwd("C:/Users/DELL/Google Drive/JVN couse materials/Projects/Practice projects/Time series project")
library("forecast")
library("Metrics")
library("ggplot2")
library('rugarch')
library('lubridate')
library('tseries')
data=read.csv("energydata_complete.csv",row.names = 'date')

#Take a sample 
sample=ts(data$Appliances[43:1494],frequency=144)
logsample=log10(sample)

#Prepare train and test sets
train=window(logsample, end=c(10,144))
test=window(logsample, start=c(11,1), end=c(11,12))

#arima111_101_144 model on logdiff1 
#Configuration
arima111_101_144_model_logdiff1=Arima(y=train, order=c(1L, 0L, 1L), seasonal=c(1,1,1), method='ML')
arima111_101_144_model_logdiff1

#fitting and goodness of fit
MAPE(train,arima111_101_144_model_logdiff1$fitted)

#These plots are produced after undifference 
plot.ts(train,type="l",xlab="Time point",ylab="Energy(Wh)",
        main="arima101_111_144-fitted-log series",col='red', lty=2)
lines(arima111_101_144_model_logdiff1$fitted,lty=1, col='blue')
grid()
legend("topleft", legend=c("Fitted", "Observed"), col=c("blue","red"), 
       lty=1:2, cex=0.8, box.lty=0)

hist(arima111_101_144_model_logdiff1$residuals,breaks=25, freq=FALSE, 
     main='Fitted residual-arima101_111_144-log series',col='blue')
qqnorm(arima111_101_144_model_logdiff1$residuals, main='Fitted QQ plot-arima101_111_144-log series',col='blue')

acf(arima111_101_144_model_logdiff1$residuals, lag.max=144*3, type='correlation', 
    plot=TRUE, main='ACF Fitted residual-arima101_111_144-log series')

#forecast
arima111_101_144_model_logdiff1_forecast=forecast(arima111_101_144_model_logdiff1)
autoplot(arima111_101_144_model_logdiff1_forecast, main='Forecast-arima101_111_144-log series', 
         ylab='log energy',xlim=c(11,11+12/144))+autolayer(test, series='actual series')

MAPE(test,arima111_101_144_model_logdiff1_forecast$mean[1:12])

#Return arima111_101_144 to the original series
plot.ts(10^(train),type="l",xlab="Time point",ylab="Energy(Wh)",
        main="arima101_111_144-fitted-original series",col='red', lty=2)
lines(10^(arima111_101_144_model_logdiff1$fitted),lty=1, col='blue')
grid()
legend("topleft", legend=c("Fitted", "Observed"), col=c("blue","red"), 
       lty=1:2, cex=0.8, box.lty=1)

originalresiduals=10^(train)-10^(arima111_101_144_model_logdiff1$fitted)
hist(originalresiduals,breaks=25, freq=FALSE, 
     main='Fitted residual-arima101_111_144-original series',col='blue')
qqnorm(originalresiduals, main='Fitted QQ plot-arima101_111_144-original series',col='blue')

acf(originalresiduals, lag.max=144*4, type='correlation', 
    plot=TRUE, main='ACF Fitted residual-arima101_111_144-original series')

plot(10^test,ylab='Energy (Wh)', main='Forecast-arima101_111_144-original series',
     col='red',ylim=c(0,900), type='l')
lines(10^arima111_101_144_model_logdiff1_forecast$mean[1:12],col='blue')
lines(10^arima111_101_144_model_logdiff1_forecast$upper[1:12,1],col='black',lty=2)
lines(10^arima111_101_144_model_logdiff1_forecast$upper[1:12,2],col='black',lty=3)
lines(10^arima111_101_144_model_logdiff1_forecast$lower[1:12,1],col='black',lty=2)
lines(10^arima111_101_144_model_logdiff1_forecast$lower[1:12,2],col='black',lty=3)
legend("topleft", legend=c("Fitted", "Observed",'80 level','95 level'), 
       col=c("blue","red",'black','black'), 
       lty=c(1,1,2,3), cex=0.8, box.lty=1)
#arima111_101_144 model on logdiff1 
#Configuration
arima111_101_144_model_logdiff1=Arima(y=train, order=c(1L, 0L, 1L), seasonal=c(1,1,1), method='ML')
arima111_101_144_model_logdiff1

#fitting and goodness of fit
MAPE(train,arima111_101_144_model_logdiff1$fitted)

#These plots are produced after undifference 
plot.ts(train,type="l",xlab="Time point",ylab="Energy(Wh)",
        main="arima101_111_144-fitted-log series",col='red', lty=2)
lines(arima111_101_144_model_logdiff1$fitted,lty=1, col='blue')
grid()
legend("topleft", legend=c("Fitted", "Observed"), col=c("blue","red"), 
       lty=1:2, cex=0.8, box.lty=0)

hist(arima111_101_144_model_logdiff1$residuals,breaks=25, freq=FALSE, 
     main='Fitted residual-arima101_111_144-log series',col='blue')
qqnorm(arima111_101_144_model_logdiff1$residuals, main='Fitted QQ plot-arima101_111_144-log series',col='blue')

acf(arima111_101_144_model_logdiff1$residuals, lag.max=144*3, type='correlation', 
    plot=TRUE, main='ACF Fitted residual-arima101_111_144-log series')

#forecast
arima111_101_144_model_logdiff1_forecast=forecast(arima111_101_144_model_logdiff1)
autoplot(arima111_101_144_model_logdiff1_forecast, main='Forecast-arima101_111_144-log series', 
         ylab='log energy',xlim=c(11,11+12/144))+autolayer(test, series='actual series')

MAPE(test,arima111_101_144_model_logdiff1_forecast$mean[1:12])

#Return arima111_101_144 to the original series
plot.ts(10^(train),type="l",xlab="Time point",ylab="Energy(Wh)",
        main="arima101_111_144-fitted-original series",col='red', lty=2)
lines(10^(arima111_101_144_model_logdiff1$fitted),lty=1, col='blue')
grid()
legend("topleft", legend=c("Fitted", "Observed"), col=c("blue","red"), 
       lty=1:2, cex=0.8, box.lty=1)

originalresiduals=10^(train)-10^(arima111_101_144_model_logdiff1$fitted)
hist(originalresiduals,breaks=25, freq=FALSE, 
     main='Fitted residual-arima101_111_144-original series',col='blue')
qqnorm(originalresiduals, main='Fitted QQ plot-arima101_111_144-original series',col='blue')

acf(originalresiduals, lag.max=144*4, type='correlation', 
    plot=TRUE, main='ACF Fitted residual-arima101_111_144-original series')

plot(10^test,ylab='Energy (Wh)', main='Forecast-arima101_111_144-original series',
     col='red',ylim=c(0,900), type='l')
lines(10^arima111_101_144_model_logdiff1_forecast$mean[1:12],col='blue')
lines(10^arima111_101_144_model_logdiff1_forecast$upper[1:12,1],col='black',lty=2)
lines(10^arima111_101_144_model_logdiff1_forecast$upper[1:12,2],col='black',lty=3)
lines(10^arima111_101_144_model_logdiff1_forecast$lower[1:12,1],col='black',lty=2)
lines(10^arima111_101_144_model_logdiff1_forecast$lower[1:12,2],col='black',lty=3)
legend("topleft", legend=c("Fitted", "Observed",'80 level','95 level'), 
       col=c("blue","red",'black','black'), 
       lty=c(1,1,2,3), cex=0.8, box.lty=1)
#arima111_101_144 model on logdiff1 
#Configuration
arima111_101_144_model_logdiff1=Arima(y=train, order=c(1L, 0L, 1L), seasonal=c(1,1,1), method='ML')
arima111_101_144_model_logdiff1

#fitting and goodness of fit
MAPE(train,arima111_101_144_model_logdiff1$fitted)

#These plots are produced after undifference 
plot.ts(train,type="l",xlab="Time point",ylab="Energy(Wh)",
        main="arima101_111_144-fitted-log series",col='red', lty=2)
lines(arima111_101_144_model_logdiff1$fitted,lty=1, col='blue')
grid()
legend("topleft", legend=c("Fitted", "Observed"), col=c("blue","red"), 
       lty=1:2, cex=0.8, box.lty=1)

hist(arima111_101_144_model_logdiff1$residuals,breaks=25, freq=FALSE, 
     main='Fitted residual-arima101_111_144-log series',col='blue')
qqnorm(arima111_101_144_model_logdiff1$residuals, main='Fitted QQ plot-arima101_111_144-log series',col='blue')

acf(arima111_101_144_model_logdiff1$residuals, lag.max=144*3, type='correlation', 
    plot=TRUE, main='ACF Fitted residual-arima101_111_144-log series')

#forecast
arima111_101_144_model_logdiff1_forecast=forecast(arima111_101_144_model_logdiff1)
autoplot(arima111_101_144_model_logdiff1_forecast, main='Forecast-arima101_111_144-log series', 
         ylab='log energy',xlim=c(11,11+12/144))+autolayer(test, series='actual series')

MAPE(test,arima111_101_144_model_logdiff1_forecast$mean[1:12])

#Return arima111_101_144 to the original series
plot.ts(10^(train),type="l",xlab="Time point",ylab="Energy(Wh)",
        main="arima101_111_144-fitted-original series",col='red', lty=2)
lines(10^(arima111_101_144_model_logdiff1$fitted),lty=1, col='blue')
grid()
legend("topleft", legend=c("Fitted", "Observed"), col=c("blue","red"), 
       lty=1:2, cex=0.8, box.lty=1)

originalresiduals=10^(train)-10^(arima111_101_144_model_logdiff1$fitted)
hist(originalresiduals,breaks=25, freq=FALSE, 
     main='Fitted residual-arima101_111_144-original series',col='blue')
qqnorm(originalresiduals, main='Fitted QQ plot-arima101_111_144-original series',col='blue')

acf(originalresiduals, lag.max=144*4, type='correlation', 
    plot=TRUE, main='ACF Fitted residual-arima101_111_144-original series')

plot(10^test,ylab='Energy (Wh)', main='Forecast-arima101_111_144-original series',
     col='red',ylim=c(0,200), type='l')
lines(10^arima111_101_144_model_logdiff1_forecast$mean[1:12],col='blue')
lines(10^arima111_101_144_model_logdiff1_forecast$upper[1:12,1],col='black',lty=2)
lines(10^arima111_101_144_model_logdiff1_forecast$upper[1:12,2],col='black',lty=3)
lines(10^arima111_101_144_model_logdiff1_forecast$lower[1:12,1],col='black',lty=2)
lines(10^arima111_101_144_model_logdiff1_forecast$lower[1:12,2],col='black',lty=3)
legend("topleft", legend=c("Fitted", "Observed",'80 level','95 level'), 
       col=c("blue","red",'black','black'), 
       lty=c(1,1,2,3), cex=0.8, box.lty=1)
#arima111_101_144 model on logdiff1 
#Configuration
arima111_101_144_model_logdiff1=Arima(y=train, order=c(1L, 0L, 1L), seasonal=c(1,1,1), method='ML')
arima111_101_144_model_logdiff1

#fitting and goodness of fit
MAPE(train,arima111_101_144_model_logdiff1$fitted)

#These plots are produced after undifference 
plot.ts(train,type="l",xlab="Time point",ylab="Energy(Wh)",
        main="arima101_111_144-fitted-log series",col='red', lty=2)
lines(arima111_101_144_model_logdiff1$fitted,lty=1, col='blue')
grid()
legend("topleft", legend=c("Fitted", "Observed"), col=c("blue","red"), 
       lty=1:2, cex=0.8, box.lty=0)

hist(arima111_101_144_model_logdiff1$residuals,breaks=25, freq=FALSE, 
     main='Fitted residual-arima101_111_144-log series',col='blue')
qqnorm(arima111_101_144_model_logdiff1$residuals, main='Fitted QQ plot-arima101_111_144-log series',col='blue')

acf(arima111_101_144_model_logdiff1$residuals, lag.max=144*3, type='correlation', 
    plot=TRUE, main='ACF Fitted residual-arima101_111_144-log series')

#forecast
arima111_101_144_model_logdiff1_forecast=forecast(arima111_101_144_model_logdiff1,h=12)
autoplot(arima111_101_144_model_logdiff1_forecast, main='Forecast-arima101_111_144-log series', 
         ylab='log energy',xlim=c(11,11+12/144))+autolayer(test, series='actual series')

MAPE(10^test,10^arima111_101_144_model_logdiff1_forecast$mean)

#Return arima111_101_144 to the original series
plot.ts(10^(train),type="l",xlab="Time point",ylab="Energy(Wh)",
        main="arima101_111_144-fitted-original series",col='red', lty=2)
lines(10^(arima111_101_144_model_logdiff1$fitted),lty=1, col='blue')
grid()
legend("topleft", legend=c("Fitted", "Observed"), col=c("blue","red"), 
       lty=1:2, cex=0.8, box.lty=1)

originalresiduals=10^(train)-10^(arima111_101_144_model_logdiff1$fitted)
hist(originalresiduals,breaks=25, freq=FALSE, 
     main='Fitted residual-arima101_111_144-original series',col='blue')
qqnorm(originalresiduals, main='Fitted QQ plot-arima101_111_144-original series',col='blue')

acf(originalresiduals, lag.max=144*4, type='correlation', 
    plot=TRUE, main='ACF Fitted residual-arima101_111_144-original series')

plot(10^test,ylab='Energy (Wh)', main='Forecast-arima101_111_144-original series',
     col='red',ylim=c(0,900), type='l')
lines(10^arima111_101_144_model_logdiff1_forecast$mean[1:12],col='blue')
lines(10^arima111_101_144_model_logdiff1_forecast$upper[1:12,1],col='black',lty=2)
lines(10^arima111_101_144_model_logdiff1_forecast$upper[1:12,2],col='black',lty=3)
lines(10^arima111_101_144_model_logdiff1_forecast$lower[1:12,1],col='black',lty=2)
lines(10^arima111_101_144_model_logdiff1_forecast$lower[1:12,2],col='black',lty=3)
legend("topleft", legend=c("Fitted", "Observed",'80 level','95 level'), 
       col=c("blue","red",'black','black'), 
       lty=c(1,1,2,3), cex=0.8, box.lty=1)

