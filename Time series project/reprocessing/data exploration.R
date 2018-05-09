setwd("C:/Users/DELL/Google Drive/JVN couse materials/Projects/Practice projects/Time series project")
library("forecast")
library("Metrics")
library("ggplot2")
library('rugarch')
library('lubridate')
library('tseries') 

data=read.csv("energydata_complete.csv",row.names = 'date')

#Take a sample 
sample=ts(data$Appliances[43:1482])

#Prepare test set
test=ts(data$Appliances[1483:1494])

#Have a look at the plot
plot.ts(sample,type="l",xlab="Time point",ylab="Energy(Wh)",
        main="Energy Consumption - The first ten days",col='red',xaxt='n')
axis(1, at=seq(0, 144*11, by=144))
abline(v=1396)

plot.ts(sample[1:144*1],type='l', xlab='Time point', ylab='Energy(Wh)',
        main='Energy Consumption - 10 days Overlaid', col=1, ylim=c(1,1100))
for(i in 1:9){
  lines(1:144,sample[(1+i*144):(144+144*i)], col=1+i)
}
legend("topleft", legend=c("day 1","day 2","day 3","day 4","day 5",
                           "day 6","day 7","day 8","day 9","day 10"), 
       col=c(1:10), 
       lty=1, cex=0.8, box.lty=1)

#Apply log10 to stabilize the variance
logsample=log10(sample)

#Plot the log sample
plot.ts(logsample,type="l",xlab="Time point",ylab="Log Energy(Wh)",
        main="Energy Consumption - The first ten days",col='red')

par(mfrow=c(1,2))
#ACF
acf(logsample,lag.max = 144*3, main='ACF Log sample', xaxt='n')
abline(v=c(144,144*2,144*3,144*4))
axis(1, at=seq(0, 144*4, by=144))
#Waving
#The acf show signifcant sign of seasonality.
#Stationarization should be done

#PACF
pacf(logsample,lag.max = 144*3, main='PACF Log sample',plot=TRUE,xaxt='n')
abline(v=c(144,144*2,144*3,144*4))
axis(1, at=seq(0, 144*4, by=144))
dev.off()
#Strong correlation could be recognize within the first day (144 first points).
#There was a peek at the lag 132, goes to zero from there

#Stationarize by taking the first order difference, lag 1 of the log sample
logdiff1=diff(logsample, lag=1, differences=1)

#Plot the 1st order difference lag 1 of log sample 
plot.ts(logdiff1,type="l",xlab="Time point",ylab="logdiff1",
        main="First difference order - Lag 1 - Log series",col='red')
#The plot looks stationary with mean = 0 

#ACF logdiff1
par(mfrow=c(1,2))
acf(logdiff1,lag.max = 144*3, main='ACF First difference order - Lag 1 - Log series')
#the acf looks quite nice though still there exists some slight waving pattern
#Spike at lag 1 then go to zero

#PACF logdiff1
pacf(logdiff1,lag.max = 144*3, main='PACF First difference order - Lag 1 - Log series',plot=TRUE)
#Still showing some sign of correlation. Exponential decay shape like
#ACF and PACF show signs of an MA(1) model on the first order difference series lag 1 of log sample
dev.off()
#test for stationary
adf.test(logdiff1,alternative="stationary", k=0)
#the result looks good, p-value=0.01 -> cannot reject 'non-stationary' hypothesis


#Consider this:
#Stationarize by taking the first order difference, lag 144 (seasonal) and lag 1 of the log sample
logdiff144_1=diff(diff(logsample, lag=144, differences=1), lag=1, differences=1)

#Plot the 1st order difference lag 1 of log sample 
plot.ts(logdiff144_1,type="l",xlab="Time point",ylab="logdiff144_1 (Wh)",
        main="Log - Lag 144 - Lag 1 - Difference 1",col='red')

#ACF logdiff144_1
acf(logdiff144_1,lag.max = 144*4, main='ACF Log - Lag 144 - Lag 1 - Difference 1')
#Still showing strong correlation at the lag 144

#PACF logdiff1
pacf(logdiff144_1,lag.max = 144*4, main='PACF Log - Lag 144 - Lag 1 - Difference 1',plot=TRUE)
#Still showing strong correlation at the lag 144

#After investigating the first few transformations and data explorations, 
#the results show that ARIMA with seasonal component could be used for this data.
#The lag 144 show highest correlation to the series 