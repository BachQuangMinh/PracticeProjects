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
logsample=log10(sample)

#Prepare train and test sets
train=window(logsample, end=1440)
test=window(logsample, start=1441, end=1452)

#MA1 model on logdiff1 
#Configuration
ma1_model_logdiff1=Arima(y=train, order = c(0L, 1L, 1L))
ma1_model_logdiff1

#fitting and goodness of fit
MAPE(train,ma1_model_logdiff1$fitted)

#These plots are produced after undifference 
plot.ts(train,type="l",xlab="Time point",ylab="Energy(Wh)",
        main="MA1-fitted-log series",col='red', lty=2)
lines(ma1_model_logdiff1$fitted,lty=1, col='blue')
grid()
legend("topleft", legend=c("Fitted", "Observed"), col=c("blue","red"), 
       lty=1:2, cex=0.8, box.lty=1)

hist(ma1_model_logdiff1$residuals,breaks=25, freq=FALSE, 
     main='Fitted residual-MA1-log series',col='blue')
qqnorm(ma1_model_logdiff1$residuals, main='Fitted QQ plot-MA1-log series',col='blue')

acf(ma1_model_logdiff1$residuals, lag.max=144*4, type='correlation', 
    plot=TRUE, main='ACF Fitted residual-MA1-log series')

#forecast
ma1_model_logdiff1_forecast=forecast(ma1_model_logdiff1,h=12)
autoplot(ma1_model_logdiff1_forecast, main='Forecast-MA1-log series', 
         ylab='log energy', xlim=c(1441,1452))+autolayer(test, series='actual series')

MAPE(10^test,10^ma1_model_logdiff1_forecast$mean)

#Return ma1 to the original series
plot.ts(10^(train),type="l",xlab="Time point",ylab="Energy(Wh)",
        main="MA1-fitted-original series",col='red', lty=2)
lines(10^(ma1_model_logdiff1$fitted),lty=1, col='blue')
grid()
legend("topleft", legend=c("Fitted", "Observed"), col=c("blue","red"), 
       lty=1:2, cex=0.8, box.lty=0)

originalresiduals=10^(train)-10^(ma1_model_logdiff1$fitted)
hist(originalresiduals,breaks=25, freq=FALSE, 
     main='Fitted residual-MA1-original series',col='blue')
qqnorm(originalresiduals, main='Fitted QQ plot-MA1-original series',col='blue')

acf(originalresiduals, lag.max=144*4, type='correlation', 
    plot=TRUE, main='ACF Fitted residual-MA1-original series')

plot(10^ma1_model_logdiff1_forecast$mean,ylab='Energy (Wh)', main='Forecast-MA1-original series',
     col='blue',ylim=c(0,500))
lines(10^test,col='red')
lines(10^ma1_model_logdiff1_forecast$upper[,1],col='black',lty=2)
lines(10^ma1_model_logdiff1_forecast$upper[,2],col='black',lty=3)
lines(10^ma1_model_logdiff1_forecast$lower[,1],col='black',lty=2)
lines(10^ma1_model_logdiff1_forecast$lower[,2],col='black',lty=3)
legend("topleft", legend=c("Fitted", "Observed",'80 level','95 level'), 
       col=c("blue","red",'black','black'), 
       lty=c(1,1,2,3), cex=0.8, box.lty=1)