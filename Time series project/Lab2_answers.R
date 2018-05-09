# Time Series Analytics and Forecasting
# Session 2: Lab 2


## PART I #############################################################################
# Read data from a csv file and put into the data frame "government_accounts"
government_accounts <- read.csv("Session_2_Lab_2_data.csv")

# Print the first few rows of data to the screen
head(government_accounts)

# Print a summary of the data to the screen (maybe not much use in this case!)
summary(government_accounts)

# Plot the year against the account value. 
plot(government_accounts$Year,government_accounts$Account_Value,type="l",
     xlab="Year",ylab="€ millions",main="Irish government accounts")
# Add a "*" to each data point in the plot
points(government_accounts$Year,government_accounts$Account_Value,pch="*")
# Place a grid on the plot
grid()

# Plot the log account value. Place a grid on the plot
plot(government_accounts$Year,log10(1000000*government_accounts$Account_Value),type="l",
     xlab="Year",ylab="log €",main="Irish government accounts")
points(government_accounts$Year,log10(1000000*government_accounts$Account_Value),pch="*")
grid()

# Add log account as an extra column to the data frame
government_accounts$log_Account_Value <- log10(1000000*government_accounts$Account_Value)

# Fit the multiple linear regression model of log account to year and quarter
# Save results into the object trend_season_model
trend_season_model <- lm(government_accounts$log_Account_Value ~ 
                           government_accounts$Year + 
                           government_accounts$Q1   +
                           government_accounts$Q2   + 
                           government_accounts$Q3)

# Print out a summary of the model fit
summary(trend_season_model)

# Plot the data as a dashed line (lty=2)  
plot(government_accounts$Year,government_accounts$log_Account_Value, type="l",lty=2,
     xlab="Year",ylab="log €",main="Regression model with trend and seasonality")
# Lay the fitted values from the regression
lines(government_accounts$Year,trend_season_model$fitted.values,lty=1)
# Add a legend to the plot
legend(1983,10.2,c("Fitted","Observed"),lty=c(1,2))
grid()
#######################################################################################

## PART II ############################################################################

# Add a column with lag 1 to the data
# n is the number of observations.  The first element of the lag 1 does not exist and in R
# is given the value "NA" for "not available"
n <- length(government_accounts$log_Account_Value)
government_accounts$log_Account_Value_lag1 = c(NA,government_accounts$log_Account_Value[1:(n-1)])

# Do the same for lag 4
government_accounts$log_Account_Value_lag4 = c(NA,NA,NA,NA,government_accounts$log_Account_Value[1:(n-4)])

# Regress log account value on lag1 and lag4 e.g. an AR model with lags 1 and 4
# The option na.action=na.exclude tells R to ignore any rows of data where there is an NA (e.g. the 
# first 4 rows of data before we have lag4 values)
ar_model <- lm(government_accounts$log_Account_Value ~ government_accounts$log_Account_Value_lag1 +
                 government_accounts$log_Account_Value_lag4,na.action=na.exclude)

# Print out a summary of the model fit
summary(ar_model)

# Plot the data against the predicted values from the fitted model.  Add a legend to the plot
plot(government_accounts$Year,government_accounts$log_Account_Value, type="l",lty=2,
     xlab="Year",ylab="log €",main="Autoregressive model with lags 1 and 4")
# We only have fitted values from the 5th observation, so have to use values 5:n of the Year vector
# on the x-axis
lines(government_accounts$Year[5:n],ar_model$fitted.values,lty=1)
legend(1983,10.2,c("Fitted","Observed"),lty=c(1,2))
grid()


## PART III ###########################################################################
# (write your own code)
# Finally, backtransform the fitted models on the log scale to the original scale
plot(government_accounts$Year,government_accounts$Account_Value,type="l",
     xlab="Year",ylab="€ millions",main="Irish government accounts",ylim=c(0,18000))

# Add a "*" to each data point
points(government_accounts$Year,government_accounts$Account_Value,pch="*")
# The multiple regression model on the original scale. lty defines line type (solid, dashed, etc.)
# lwd controls line thickness
lines(government_accounts$Year,(10^trend_season_model$fitted.values)/1000000,lty=2,lwd=2)
# The AR model on the original scale
lines(government_accounts$Year[5:n],(10^ar_model$fitted.values)/1000000,lty=3,lwd=2)
# Add a legend
legend(1983,17000,c("Observed","Regression Fit","AR Fit"),lty=c(1,2,3),lwd=c(1,2,2))
grid()
