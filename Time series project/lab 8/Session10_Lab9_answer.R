# Time Series Analytics and Forecasting
# Session 10: Lab 9

# ################################################################################################
# PART I. Plotting the crude oil price data and its return
# ################################################################################################
library(lubridate)
# load in the data from a csv file
oil_data <- read.csv("oil_price_data.csv")
# the dates in the csv file are in text.  Convert them to a proper date type of the 
# format day/month/year using the strptime command
oil_data$date <- strptime(oil_data$date,format="%d/%m/%Y")
# turn the crude oil price from text to a number
oil_data$crude <- as.numeric(oil_data$crude)
# similarly for Brent oil price
oil_data$Brent <- as.numeric(oil_data$Brent)
# compute log return and add to data frame
n = length(oil_data$crude)
oil_data$logreturn_crude <- c(0,log(oil_data$crude[2:n])-log(oil_data$crude[1:(n-1)]))
# Plot the price and then log return, and some autocorrelation functions
# Arrange plots in a 2x2 grid
par(mfrow=c(2,2))
plot(oil_data$date,oil_data$crude,type="l",xlab="Date",ylab="Price ($)",main="Crude Oil Price")
grid()
plot(oil_data$date,oil_data$logreturn_crude,type="l",xlab="Date",ylab="Log return",main="Log Return of Crude Oil Price")
grid()
acf(oil_data$logreturn_crude,main="ACF of log return")
grid()
acf(oil_data$logreturn_crude*oil_data$logreturn_crude,main="ACF of square of log return")
grid()
# ################################################################################################
# ################################################################################################


# ################################################################################################
# PART II. Setting up a GARCH model using the package rugarch
# ################################################################################################
library(rugarch)
# Specify a simple GARCH(p,q) model as in Section 23
p = 1;
q = 1;  # orders of the GARCH model
spec = ugarchspec(variance.model=list(model="sGARCH",garchOrder=c(p,q)),
                  mean.model=list(armaOrder=c(0,0),include.mean=FALSE))
PP = 1;
QQ = 1; # orders of the ARMA model for the observed process.
# More complicated model with the observed y_t as an ARMA(2,2) with GARCH(1,1) for the variance
spec2 = ugarchspec(variance.model=list(model="sGARCH",garchOrder=c(1,1)),
                   mean.model=list(armaOrder=c(2,2),include.mean = TRUE))
# ################################################################################################
# ################################################################################################


# ################################################################################################
# PART III. Forecasting with known model parameters
# ################################################################################################
# Specify a GARCH(2,1) model with no ARMA model on the observed values
spec_knownparams = ugarchspec(variance.model=list(model="sGARCH",garchOrder=c(1,2)),
                              mean.model = list(armaOrder=c(1,1),include.mean=TRUE),
                              fixed.pars=list(mu=0,ar1=0.28,ma1=-0.4,omega=0,alpha1=0.25,beta1=0.35,beta2=0.3))
logreturn.forecast = ugarchforecast(fitORspec=spec_knownparams, 
                                    data=oil_data$logreturn_crude[1:5000], 
                                    n.ahead=100)
show(logreturn.forecast)
plot(logreturn.forecast)
# ################################################################################################
# ################################################################################################


# ################################################################################################
# PART IV. Fitting and forecasting
# ################################################################################################
logreturn.fit <- ugarchfit(spec=ugarchspec(variance.model = list(model="sGARCH",garchOrder=c(1,2)),
                                           mean.model     = list(armaOrder=c(1,1),include.mean=TRUE)),
                           data=oil_data$logreturn_crude[1:5000])
show(logreturn.fit)
plot(logreturn.fit)
logreturn.fit_forecast = ugarchforecast(fitORspec=logreturn.fit, 
                                        data=oil_data$logreturn_crude[1:5000], 
                                        n.ahead=100)
show(logreturn.fit_forecast)
plot(logreturn.fit_forecast)
residual = oil_data$logreturn_crude[5001:5100] - fitted(logreturn.fit_forecast)
rmse = sqrt(mean(residual*residual))
# ################################################################################################
# ################################################################################################