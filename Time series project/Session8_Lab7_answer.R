# Time Series Analytics and Forecasting
# Session 8: Lab 7

## PART I ############################################################################## Simulate the Kalman filter
# Simulate the state process and observations from a Kalman Filter model for 
# T time periods, with starting value of the state process X1, and with fixed
# parameters F, G, V and W (e.g. they are not a function of time).
T = 100
X1 = 0
F = 1
G = 0.9
W = 1
V = 4
# Set up vectors to store the state process and the observations
x <- vector("numeric",length=T)
y <- vector("numeric",length=T)
# Define the first value of the state process and then the first observation
x[1] <- X1
y[1] <- rnorm(1,mean=F*x[1],sd=sqrt(V))
# Generate each successive value of the state process and then the corresponding observation
for (i in 2:T) {
  x[i] <- rnorm(1,mean=G*x[i-1],sd=sqrt(W))
  y[i] <- rnorm(1,mean=F*x[i],sd=sqrt(V))  
}
# Plot the processes as a time series
maxy <- max(c(x,y))
miny <- min(c(x,y))
plot(1:T,x,xlab="t",type="l",lty=2,ylim=c(miny,maxy))
lines(1:T,y,type="l",lty=1)
points(1:T,x,pch=".",cex=3)
points(1:T,y,pch=".",cex=3)
grid()
legend("topleft",c("Observation y_t","State x_t"),lty=c(1,2))
################################################################################

## PART II #############################################################################
# Implement the Kalman Filter prediction and updating equations for the data that
# you've just generated.
# Define initial mean and variance for X0
mu0 = 0
Sigma0 = 1
# Set up vectors that will store the mean and variance of the one-step ahead predictions
# for x_t and y_t:
predict_x_mean = vector("numeric",length=T)
predict_x_var = vector("numeric",length=T)
predict_y_mean = vector("numeric",length=T)
predict_y_var = vector("numeric",length=T)
# Set up vectors that will store the mu and Sigma:
mu    = vector("numeric",length=T+1)
Sigma = vector("numeric",length=T+1)
# Put mu0 and Sigma0 into the first element
# NOTE: this means that mu(t) will be stored in mu[t+1] etc.
mu[1] = mu0
Sigma[1] = Sigma0
# Now it's your turn to do some work!  Write the prediction and updating equations in
# this for loop
for (t in 1:T) {
  # First the predictions:
  predict_x_mean[t] = G*mu[t]
  predict_x_var[t]  = G*G*Sigma[t] + W
  predict_y_mean[t] = F*G*mu[t]
  predict_y_var[t]  = F*F*predict_x_var[t] + V
  # Then the update after observing y[t]:
  R = predict_x_var[t]
  mu[t+1] = G*mu[t] + R*R*F*F*(y[t]-F*G*mu[t])/(F*F*R + V)
  Sigma[t+1] = R - R*R*F*F/(F*F*R + V) 
}

## PART III #############################################################################
# Please do a separate time series plot of: predict_x_mean, predict_x_var, 
# predict_y_mean, predict_y_var, mu, Sigma.

# Plot the observations on top of the plot of predict_y_mean
par(mfrow=c(1,2))
plot(1:T,y,type="l",lty=2,xlab="t")
lines(1:T,predict_y_mean,lty=1)
grid()
legend("topleft",c("Predicted Mean y_t","Observed y_t"),lty=c(1,2))
plot(y,predict_y_mean,pch="*",xlab="Observed y_t",ylab="Predicted Mean y_t")
# Plot the state process on top of the estimated values in mu
par(mfrow=c(1,2))
plot(1:T,x,type="l",lty=2,xlab="t")
lines(1:T,mu[2:(T+1)],lty=1)
grid()
legend("topleft",c("Estimated Mean x_t","Actual x_t"),lty=c(1,2))
plot(x,mu[2:(T+1)],pch="*",xlab="Actual x_t",ylab="Estimated Mean x_t")

# Add in error bars
par(mfrow=c(1,1))
plot(1:T,y,type="l",lty=2,xlab="t")
lines(1:T,predict_y_mean,lty=1)
for (t in 1:T) {
  lower_bound = predict_y_mean[t] - 2*sqrt(predict_y_var[t])
  upper_bound = predict_y_mean[t] + 2*sqrt(predict_y_var[t])
  lines(c(t,t),c(lower_bound,upper_bound),col=2)
}
grid()
legend("topleft",c("Predicted Mean y_t","Observed y_t"),lty=c(1,2))
# Plot the state process on top of the estimated values in mu
plot(1:T,x,type="l",lty=2,xlab="t")
lines(1:T,mu[2:(T+1)],lty=1)
for (t in 1:T) {
  lower_bound = mu[t+1] - 2*sqrt(Sigma[t+1])
  upper_bound = mu[t+1] + 2*sqrt(Sigma[t+1])
  lines(c(t,t),c(lower_bound,upper_bound),col=2)
}
grid()
legend("topleft",c("Estimated Mean x_t","Actual x_t"),lty=c(1,2))


## PART IV #############################################################################
# Calculating RMSE and MAPE
rmse = mean((y-predict_y_mean)*(y-predict_y_mean))
mape = mean(abs(y-predict_y_mean))


