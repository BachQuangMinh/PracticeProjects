setwd("C:/Users/DELL/Google Drive/JVN couse materials/Projects/Practice projects/Time series project")
library("forecast")
library("Metrics")
library("ggplot2")
library('rugarch')
library('lubridate')
library('tseries')
data=read.csv("energydata_complete.csv",row.names = 'date')

#Take a sample 
sample=ts(data$Appliances[43:1494], frequency=144)
logsample=log10(sample)
train=window(logsample, end=c(10,144))
test=window(logsample, start=c(11,1), end=c(11,12))

#decompose function
decom_series=decompose(train)
autoplot(decom_series)
decom_series_forecast=forecast(decom_series)

#stop here 

deseasonalized_series=train-decom_series$seasonal
deseasonalized_diff1_series=diff(deseasonalized_series,lag=3,differences = 1)
autoplot(deseasonalized_diff1_series)



acf(deseasonalized_diff1_series,lag.max = 144*3)
pacf(deseasonalized_diff1_series,lag.max = 144*3)

MA1model=Arima(y=deseasonalized_diff1_series,c(3,0,1))
MA1model

mape(deseasonalized_diff1_series,MA1model$fitted)

autoplot(deseasonalized_diff1_series)+autolayer(MA1model$fitted)

#decompose model using additive method yi=alpha+beta*i+phi144*y(i-144)+ei i=145:1440
time=145:1440
lag144=ts(train[1:1296])
outcome=ts(train[145:1440])

#fit
decom_model=lm(outcome~time+lag144)
summary(decom_model)
decom_model=decompose(x=train, type="additive")
fittedvalues=fitted(decom_model)

plot(train,ylab='log energy',main='trend-season-error model', col='red', lty=2)
lines(seq(2,11-1/144,by=1/144),decom_model$fitted.values, col='blue', lty=1)

#plot trend
trend=decom_model$coefficients[1]+decom_model$coefficients[2]*time
plot(trend, main='Trend',lty=1)

#plot seasonality
seasonality=decom_model$coefficients[3]*lag144
plot(seasonality, main='Seasonality')

hist(decom_model$residuals, breaks=25, freq=FALSE)

forecast=1441:1452

for(i in 1:12){
  forecast[i]=decom_model$coefficients[1]+decom_model$coefficients[2]*(i+1440)
  +decom_model$coefficients[3]*train[i+1440-144]
}
forecast

plot(forecast,type='l')




