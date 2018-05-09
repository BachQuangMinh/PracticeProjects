setwd("C:/Users/DELL/Google Drive/JVN couse materials/Projects/Practice projects/Time series project")
library("forecast")
library("Metrics")
library("ggplot2")
library('rugarch')
library('lubridate')
rawdata=read.csv("energydata_complete.csv",row.names = 'date')
selectedcol=rawdata[2]
appliances_ts=ts(selectedcol)
#appliances_ts=ts(selectedcol, frequency = 144)
#summary
summary(appliances_ts)

#take a sample of the first 10 days
#sample=ts(appliances_ts[43:1482],frequency=144)
sample=ts(appliances_ts[43:1482])
logsample=log10(sample)
n=length(sample)
logdiff1=diff(logsample,lag=144,differences=1) #log transformation

#take the next 10 days as for testing
#testsample=ts(appliances_ts[1482:2922], frequency=144)
testsample=ts(appliances_ts[1482:2922])
logtestsample=log10(testsample)
testlogdiff1=c(0,log(testsample[2:n])-log(testsample[1:(n-1)]))

#plot the sample
plot.ts(sample,type="l",xlab="Time point",ylab="Energy(Wh)",main="Energy Consumption - The first ten days",col='red')

#Plot the transformation of the time series
plot.ts(logdiff1,type="l",xlab="Time point",ylab="Log difference energy(Wh)",main="Log Difference Energy Consumption - The first ten days",col='red')

#plot acf 
acfResult <- acf(logdiff1, lag.max=400
                 , type='correlation', main='ACF plot',
                 plot = FALSE)
plot(acfResult)
abline(v=c(144,288))
#->show sign of stationary 

#plot pacf
pacf(logdiff1, lag.max=400, plot=TRUE, main='PACF plot')
abline(v=c(144,288))
#->pretty nice pacf-> highest correlated lag is 255 - pacf=0.65


#decompose to locate trend, seasonal and random
decomposelogdiff1=decompose(ts(logdiff1,frequency = 144),'additive')
plot(decomposelogdiff1)
#->the trend indicates that this is not a stationary 

#AR(1) model with 1st difference order
ar1_model=arima(x=logsample, order = c(1L, 1L, 0L))
ar1_model
fittedvalues=fitted(ar1_model)
mape(logsample,fittedvalues)

plot.ts(logsample,type="l",xlab="Time point",ylab="Log energy(Wh)",main="AR1 model",col='red')
lines(fittedvalues,lty=1,col="blue")
grid()
legend("topleft", legend=c("Fitted", "Observed"), col=c("blue","red"), lty=1:1, cex=0.8, box.lty=0)

hist(ar1_model$residuals,breaks=25, freq=FALSE, main='Residual Plot - AR(1)',col='blue')
qqnorm(ar1_model$residuals, main='Quantile-Quantile plot - AR(1)',col='blue')

acf(ar1_model$residuals, lag.max=1440, type='correlation', plot=TRUE, main='ACF residual plot-AR1')

ar1_forecasts=forecast(ar1_model,h=12)
autoplot(ar1_forecasts, main='Forecast-log scale-AR(1)', ylab='log energy', xlim=c(1400,1453))

#AR(6) with 1st difference order, lag 1hour 
ar6_model=arima(x=logsample, order = c(6L, 1L, 0L))
ar6_model
fittedvalues=fitted(ar6_model)
mape(logsample,fittedvalues)

plot.ts(logsample,type="l",xlab="Time point",ylab="Log difference energy(Wh)",main="AR6 model",col='red')
lines(fittedvalues,lty=1,col="blue")
grid()
legend("topleft", legend=c("Fitted", "Observed"), col=c("blue","red"), lty=1:1, cex=0.8, box.lty=0)

hist(ar6_model$residuals,breaks=25, freq=FALSE, main='Residual Plot - AR(6)',col='blue')
qqnorm(ar6_model$residuals, main='Quantile-Quantile plot- AR(6)',col='blue')

acf(ar6_model$residuals, lag.max=1440, type='correlation', plot=TRUE, main='ACF residual plot-AR6')

ar6_forecasts=forecast(ar6_model,h=12)
autoplot(ar6_forecasts, main='Forecast-log scale-AR(6)', ylab='log energy', xlim=c(1400,1453))

#ARMA(6,6) with 1st difference order
arma66_model=arima(x=logsample, order = c(1L, 1L, 1L))
arma66_model
fittedvalues=fitted(arma66_model)
mape(logsample,fittedvalues)

plot.ts(logsample,type="l",xlab="Time point",ylab="Log energy(Wh)",main="ARMA(6,6) model",col='red')
lines(fittedvalues,lty=1,col="blue")
grid()
legend("topleft", legend=c("Fitted", "Observed"), col=c("blue","red"), lty=1:1, cex=0.8, box.lty=0)

hist(arma66_model$residuals,breaks=25, freq=FALSE, main='Residual Plot - ARMA(6,6)',col='blue')
qqnorm(arma66_model$residuals, main='Quantile-Quantile plot - ARMA(6,6)',col='blue')

acf(arma66_model$residuals, lag.max=1440, type='correlation', plot=TRUE, main='ACF residual plot-ARMA66')

arma66_forecasts=forecast(arma66_model,h=12)
autoplot(arma66_forecasts, main='Forecast-log scale-ARMA(6,6)', ylab='log energy', xlim=c(1400,1453))

#ARIMA(6,2,6) with 1st difference order
arima626_model=arima(x=logsample, order = c(6L, 2L, 6L))
arima626_model
fittedvalues=fitted(arima626_model)
mape(logsample,fittedvalues)

plot.ts(logsample,type="l",xlab="Time point",ylab="Log energy(Wh)",main="ARIMA(6,2,6) model",col='red')
lines(fittedvalues,lty=1,col="blue")
grid()
legend("topleft", legend=c("Fitted", "Observed"), col=c("blue","red"), lty=1:1, cex=0.8, box.lty=0)

hist(arima626_model$residuals,breaks=25, freq=FALSE, main='Residual Plot - ARIMA(6,2,6)',col='blue')
qqnorm(arima626_model$residuals, main='Quantile-Quantile plot - ARIMA(6,2,6)',col='blue')

acf(arima626_model$residuals, lag.max=1440, type='correlation', plot=TRUE, main='ACF residual plot-ARIMA626')

arima626_forecasts=forecast(arima626_model,h=12)
autoplot(arima626_forecasts, main='Forecast-log scale-ARIMA(6,2,6)', ylab='log energy', xlim=c(1400,1453))

#Exponential smoothing
MAPE=c(1:4)
for (i in 1:4){
  HW_ADD_model = HoltWinters(ts(logsample,frequency=144), 
                             alpha=i/10+0.4, beta=FALSE, gamma=FALSE,
                             l.start=logsample[1])
  MAPE[i]=mape(logsample[2:1440],HW_ADD_model$fitted[,1])
}
MAPE
#Alpha plot
plot(c(0.5,0.6,0.7,0.8),MAPE, main='Exponential Smoothing - Alpha', xlab='Alpha', ylab='MAPE', type='l')


#plot for optimal model
exp_model = HoltWinters(logsample, beta=FALSE, gamma=FALSE, l.start=logsample[1])
exp_model
mape(logsample,exp_model$fitted[,1])

plot(logsample, main = 'Exponential Smoothing', ylab='Log Energy', col='red')
lines(c(2:1440),exp_model$fitted[,1], col='blue')
legend("topleft", legend=c("Fitted", "Observed"), col=c("blue","red"), lty=1:1, cex=0.8, box.lty=0)

exp_forecast=forecast(exp_model, h=12)
autoplot(exp_forecast, main='Forecast-log scale-Exponential smoothing', ylab='log energy', xlim=c(1400,1453))

#Holt-Winter - Addictive 
MAPE=c(1:4)
for (i in 1:4){
  HW_ADD_model = HoltWinters(ts(logsample,frequency=144), 
                             alpha=0.6, beta=0.1, gamma=i/10,
                             l.start=logsample[1],
                             seasonal='additive')
  MAPE[i]=mape(logsample[145:1440],HW_ADD_model$fitted[,1])
}
MAPE

HW_ADD_model
#alpha plot 
plot(c(0.1,0.2,0.3,0.4),MAPE, main='alpha=0.6-beta=0.1-Gamma', xlab='Gamma', ylab='MAPE', type='l')


#plot for the obtimal model
HW_ADD_model = HoltWinters(ts(logsample,frequency=144), 
                           alpha=NULL, beta=NULL, gamma=NULL,
                           l.start=logsample[1],
                           seasonal='additive',
                           optim.start = c(alpha = 0.3, beta = 0.1, gamma = 0.1)
                           )
mape(logsample[145:1440],HW_ADD_model$fitted[,1])
plot(logsample, main = 'HW_ADDITIVE model', ylab='Log Energy', col='red')
lines(c(145:1440),HW_ADD_model$fitted[,1], col='blue')
legend("topleft", legend=c("Fitted", "Observed"), col=c("blue","red"), lty=1:1, cex=0.8, box.lty=0)

HW_ADD_forecast=forecast(HW_ADD_model, h=12)
autoplot(HW_ADD_forecast, main='Forecast-log scale-HW_ADDITIVE model', ylab='log energy', xlim=c(10.5,11.1))

#GARCH model
xt=logdiff1
xtsquared=xt**2
par(mfrow=c(1,2))
acf(xt, lag.max=288, type='correlation', plot=TRUE, main='xt ACF plot')
acf(xtsquared, lag.max=288, type='correlation', plot=TRUE, main='xt squared ACF plot')
dev.off()

p = 6;
q = 6;  # orders of the GARCH model
PP = 6;
QQ = 6; # orders of the ARMA model for the observed process.
# More complicated model with the observed y_t as an ARMA(2,2) with GARCH(1,1) for the variance
spec = ugarchspec(variance.model=list(model="sGARCH",garchOrder=c(p,q)),
                  mean.model=list(armaOrder=c(PP,QQ),include.mean = TRUE))
GARCHfit=ugarchfit(data=logsample, spec=spec, solver='solnp')
mape(logsample,GARCHfit@fit$fitted.values)
m=4
AIC=-2*prod(GARCHfit@fit$log.likelihoods)+2*m

plot.ts(logsample,type="l",xlab="Time point",ylab="Log energy(Wh)",main="GARCH-ARMA(6,6)",col='red')
lines(c(1:1440),GARCHfit@fit$fitted.values,lty=1,col="blue")
grid()
legend("topleft", legend=c("Fitted", "Observed"), col=c("blue","red"), lty=1:1, cex=0.8, box.lty=0)

hist(GARCHfit@fit$residuals,breaks=25, freq=FALSE, main='Residual Plot - GARCH-ARMA(6,6)',col='blue')
plot(GARCHfit@fit$residuals, main='Residual Plot - GARCH-ARMA(6,6)',col='blue',type='l')
qqnorm(GARCHfit@fit$residuals, main='Quantile-Quantile plot - GARCH-ARMA(6,6)',col='blue')
acf(GARCHfit@fit$residuals, lag.max=1440, type='correlation', plot=TRUE, main='ACF residual plot-GARCH-ARMA(6,6)')

GARCHforecast=ugarchforecast(fitORspec=GARCHfit, n.ahead=12)
show(GARCHforecast)
plot(GARCHforecast)

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
