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

#autoarima model on logdiff1 
#Configuration
AutoArima=auto.arima(train)
AutoArima #the model returnes arma33

#fitting and goodness of fit
MAPE(train,AutoArima$fitted)

#These plots are produced after undifference 
plot.ts(train,type="l",xlab="Time point",ylab="Energy(Wh)",
        main="arma33-fitted-log series",col='red', lty=2)
lines(AutoArima$fitted,lty=1, col='blue')
grid()
legend("topleft", legend=c("Fitted", "Observed"), col=c("blue","red"), 
       lty=1:2, cex=0.8, box.lty=1)

hist(AutoArima$residuals,breaks=25, freq=FALSE, 
     main='Fitted residual-arma33-log series',col='blue')
qqnorm(AutoArima$residuals, main='Fitted QQ plot-arma33-log series',col='blue')

acf(AutoArima$residuals, lag.max=144*3, type='correlation', 
    plot=TRUE, main='ACF Fitted residual-arma33-log series')

#forecast
AutoArima_forecast=forecast(AutoArima,h=12)
autoplot(AutoArima_forecast, main='Forecast-arma33-log series', 
         ylab='log energy',xlim=c(1441,1452))+autolayer(test, series='actual series')

MAPE(test,AutoArima_forecast$mean)

#Return arma33 to the original series
plot.ts(10^(train),type="l",xlab="Time point",ylab="Energy(Wh)",
        main="arma33-fitted-original series",col='red', lty=2)
lines(10^(AutoArima$fitted),lty=1, col='blue')
grid()
legend("topleft", legend=c("Fitted", "Observed"), col=c("blue","red"), 
       lty=1:2, cex=0.8, box.lty=0)

originalresiduals=10^(train)-10^(AutoArima$fitted)
hist(originalresiduals,breaks=25, freq=FALSE, 
     main='Fitted residual-arma33-original series',col='blue')
qqnorm(originalresiduals, main='Fitted QQ plot-arma33-original series',col='blue')

acf(originalresiduals, lag.max=144*4, type='correlation', 
    plot=TRUE, main='ACF Fitted residual-arma33-original series')

plot(10^test,ylab='Energy (Wh)', main='Forecast-arma33-original series',
     col='red',ylim=c(0,500), type='l')
lines(10^AutoArima_forecast$mean,col='blue')
lines(10^AutoArima_forecast$upper[,1],col='black',lty=2)
lines(10^AutoArima_forecast$upper[,2],col='black',lty=3)
lines(10^AutoArima_forecast$lower[,1],col='black',lty=2)
lines(10^AutoArima_forecast$lower[,2],col='black',lty=3)
legend("topleft", legend=c("Fitted", "Observed",'80 level','95 level'), 
       col=c("blue","red",'black','black'), 
       lty=c(1,1,2,3), cex=0.8, box.lty=1)
