setwd("C:/Users/DELL/Google Drive/JVN couse materials/Projects/Practice projects/Time series project")
rawdata=read.csv("energydata_complete.csv",row.names = 'date')
selectedcol=rawdata[2]
appliances_ts=ts(selectedcol)

#summary
summary(appliances_ts)

#take a sample of the first 10 days
#sample=ts(appliances_ts[43:1482],frequency=144)
sample=ts(appliances_ts[43:1482])
logsample=log10(sample) #log transformation

#take the next 10 days as for testing
#testsample=ts(appliances_ts[1482:2922], frequency=144)
testsample=ts(appliances_ts[1482:2922])
logtestsample=log10(testsample)

#plot the sample
plot.ts(sample,type="l",xlab="Time point",ylab="Energy(Wh)",main="Energy Consumption - The first ten days",col='red')

#Plot the transformation of the time series
plot.ts(logsample,type="l",xlab="Time point",ylab="Log energy(Wh)",main="Log Energy Consumption - The first ten days",col='red')

#plot acf 
acf(logsample, lag.max=1440, type='correlation', plot=TRUE, main='ACF plot')
#->show some seasonality 

#plot pacf
pacf(logsample, lag.max=1439, plot=TRUE, main='PACF plot')
#->pretty nice pacf

#decompose to locate trend, seasonal and random
decomposelogsample=decompose(ts(logsample,frequency = 144),'additive')
plot(decomposelogsample)
#->the trend indicates that this is not a stationary 

#fit AR(144)
n=length(logsample)
c=1:144
for (i in 1:144){c[i]=NA}
logsamplelag144=c(c,logsample[1:(n-144)])
ar144_model=lm(logsample~logsamplelag144,na.action=na.exclude)
summary(ar1_model)
MAPE=mape(logsample[145:n],ar144_model$fitted.values)
AIC=AIC(ar144_model)

plot.ts(logsample,type="l",xlab="Time point",ylab="Log energy(Wh)",main="AR144 model",col='red')
lines(ar144_model$fitted.values,lty=1,col="blue")
grid()
legend("topleft", legend=c("Fitted", "Observed"), col=c("blue","red"), lty=1:1, cex=0.8, box.lty=0)

plot(ar1_model$residuals,breaks=25, freq=FALSE, main='Residual Plot',col='blue')
qqnorm(ar1_model$residuals, main='Quantile-Quantile plot',col='blue')

#Forecasting 
k_step = 12
input = logsamplelag144[n]
prediction_values <- data.frame(index = 1:k_step, predicted_value = 1:k_step , hi95 = 1:k_step, lo95 = 1:k_step)
s=sum(ar144_model$residuals**2)/(n-2)
mean=mean(logsamplelag144[145:n])
sumsq=sum((logsamplelag144[145:n]-mean)**2)
for (index in 1:k_step){
  prediction_values[index,2] = ar144_model$coefficients[1] + ar144_model$coefficients[2]*input
  input  = prediction_values[index,2]
  prediction_interval = 2*s*sqrt(1 + 1/(n-k_step) + ((input - mean)**2/sumsq))
  prediction_values[index,3] = prediction_values[index,2] + prediction_interval
  prediction_values[index,4] = prediction_values[index,2] - prediction_interval
  s=sqrt((1+(ar1_model$coefficients[2])**2)*(s**2))
}

#Plot and compare the results
plot.ts(logtestsample[1:10],
        xlab="Time point",
        ylab="Log energy(Wh)",
        main="Forecast for the next 10 time points",
        col='red',ylim=c(1,3))
lines(prediction_values$hi95,lty=2,col="black")
lines(prediction_values$lo95,lty=2,col="black")
lines(prediction_values$predicted_value,lty=1,col="blue")
grid()
legend("topleft", legend=c("Forecast", "Observed"), col=c("blue","red"), lty=1:1, cex=0.8, box.lty=0)
###########################

#draft:

input=logsamplelag1[2:n]
s=sum(ar1_model$residuals**2)/(n-2)
mean=mean(logsamplelag1[2:n])
sumsq=sum((logsamplelag1[2:n]-mean)**2)
prediction_values <- data.frame(index = 1:(n-1), predicted_value = 1:(n-1) , hi95 = 1:(n-1), lo95 = 1:(n-1))
s=sum(ar1_model$residuals**2)/(n-2)
prediction_interval = 2*s*sqrt(1 + 1/(n-k_step) + ((input - mean)**2/sumsq))
prediction_values[,2]=ar1_model$fitted.values
prediction_values[,3] = prediction_values[,2] + prediction_interval
prediction_values[,4] = prediction_values[,2] - prediction_interval




