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
sample=ts(data$Appliances[43:1494])

logsample=log10(sample)

logdiff1=diff(logsample,lag=1,differences=1)
#Prepare train and test sets

train=window(sample, end=1440)
test=window(sample, start=1441, end=1451)

#GARCH model
xt=logdiff1
xtsquared=xt**2
par(mfrow=c(1,2))
acf(xt, lag.max=288, type='correlation', plot=TRUE, main='xt ACF plot')
acf(xtsquared, lag.max=288, type='correlation', plot=TRUE, main='xt squared ACF plot')
dev.off()
plot.ts(xt,type="l",xlab="Time point",ylab="xt",main="xt series",col='red')

#apply the garch model on xt
p = 1;
q = 1;  # orders of the GARCH model
PP = 1;
QQ = 1; # orders of the ARMA model for the observed process.

spec = ugarchspec(variance.model=list(model="sGARCH",garchOrder=c(p,q)),
                  mean.model=list(armaOrder=c(PP,QQ),include.mean = TRUE))
GARCHfit=ugarchfit(data=sample, spec=spec, solver='solnp', out.sample = 12)

GARCHfit

MAPE(sample[1:1440],GARCHfit@fit$fitted.values)

plot.ts(sample[1:1440],type="l",xlab="Time point",ylab="Log energy(Wh)",main="GARCH(1,1)-ARMA(1,1)",col='red')
lines(GARCHfit@fit$fitted.values,lty=1,col="blue")
grid()
legend("topleft", legend=c("Fitted", "Observed"), col=c("blue","red"), lty=1:1, cex=0.8, box.lty=0)

hist(GARCHfit@fit$residuals,breaks=25, freq=FALSE, main='Residual Plot - GARCH(1,1)-ARMA(1,1)',col='blue')
plot(GARCHfit@fit$residuals, main='Residual Plot - GARCH(1,1)-ARMA(1,1)',col='blue',type='l')
qqnorm(GARCHfit@fit$residuals, main='Quantile-Quantile plot - GARCH(1,1)-ARMA(1,1)',col='blue')
acf(GARCHfit@fit$residuals, lag.max=144*3, type='correlation', plot=TRUE, main='ACF residual plot-GARCH(1,1)-ARMA(1,1)')

GARCHforecast=ugarchforecast(fitORspec=GARCHfit, n.ahead=12, n.roll=12)
MAPE(sample[1441:1452],GARCHforecast@forecast$seriesFor[,1])
show(GARCHforecast)
plot(GARCHforecast)

pgrid=1:6
qgrid=1:6
PPgrid=1:6
QQgrid=1:6
aic=100
for(p in pgrid){
  for(q in qgrid){
    for(PP in PPgrid){
      for(QQ in QQgrid){
        spec = ugarchspec(variance.model=list(model="sGARCH",garchOrder=c(p,q)),
                          mean.model=list(armaOrder=c(PP,QQ),include.mean = TRUE))
        GARCHfit=ugarchfit(data=sample, spec=spec, solver='solnp', out.sample = 12)
        if(infocriteria(GARCHfit)[1]<aic){
          aic=infocriteria(GARCHfit)[1]
          bestmodel=GARCHfit
        }
      }
    }
  }
}


#lines(train[1429:1440],col='black')
#optimal arima model
opt.arimamodel=auto.arima(logsample)
plot.ts(logsample,type="l",xlab="Time point",ylab="Log energy(Wh)",main="ARIMA(3,0,3) model",col='red')
lines(fittedvalues,lty=1,col="blue")
grid()
legend("topleft", legend=c("Fitted", "Observed"), col=c("blue","red"), lty=1:1, cex=0.8, box.lty=0)

hist(opt.arimamodel$residuals,breaks=25, freq=FALSE, main='Residual Plot - ARIMA(3,0,3)',col='blue')
qqnorm(opt.arimamodel$residuals, main='Quantile-Quantile plot - ARIMA(3,0,3)',col='blue')

acf(opt.arimamodel$residuals, lag.max=1440, type='correlation', plot=TRUE, main='ACF residual plot-ARIMA(3,0,3)')

opt.arimaforecast=forecast(opt.arimamodel,h=12)
autoplot(opt.arimaforecast, main='Forecast-log scale-ARIMA(3,0,3)', ylab='log energy', xlim=c(1400,1453))
lines(c(1441:1452),logtestsample[1441:1452],col='red')

plot.ts(logsample[1400:1440],main='Forecast-log scale-ARIMA(3,0,3)',xlab='Time',ylab='log energy',xlim=c(1400,1453),ylim=c(-3,3),type='l')
lines(c(1441:1452),logtestsample[1:12],col='red')
lines(c(1441:1452),opt.arimaforecast$fitted,col='blue')
lines(c(1441:1452),opt.arimaforecast$lower[,1],col='red',lty=2)
lines(c(1441:1452),opt.arimaforecast$upper[,1],col='red',lty=2)
lines(c(1441:1452),opt.arimaforecast$lower[,2],col='green',lty=2)
lines(c(1441:1452),opt.arimaforecast$upper[,2],col='green',lty=2)
