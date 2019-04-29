data=read.csv("Forecast project.csv", header=TRUE)
#####Cut data to model from 2014 to 2017
data1 = data[1:46,]

library(tseries)
library(forecast) 
library(lmtest)

##### to generate ACF, PACF plots
pval=rep(0,ncol(data1)-1)
pdf("myplot.pdf")

data_log <- log(data1[,2:ncol(data1)]+1)

for (i in 1:ncol(data_log)) {
  k=adf.test(data_log[,i], alternative = "stationary")
  pval[i]=k$p.value
  
  acf(data_log[,i],main=paste(colnames(data_log)[i]," - ACF"))
  pacf(data_log[,i],main=paste(colnames(data_log)[i]," - PACF"))
}
dev.off()
########

############# prepare the data and plot the log data to check the stationary
nifty=data1$Nifty.index.Adjust.close. 
niftyseries=ts(nifty, frequency=12,start=c((2014),3)) 
plot.ts(niftyseries) 
niftyseries=log(niftyseries) 
plot.ts(niftyseries) 

######### Decomposition of time series
niftycomp=decompose(niftyseries) 
plot(niftycomp) 


#####Holt-Winters Exponential Smoothing - with trend and seasonality, 

### build holt-winter model
niftyseriesforecasts5=HoltWinters(niftyseries) 
niftyseriesforecasts5
### plot the graph for actual and in-sample data
plot(niftyseriesforecasts5,xaxt='n') 
axis(side=1, at=c(2015,2015.5, 2016,2016.5, 2017, 2017.5, 2018), labels=c("Jan 2015","Jul 2015", "Jan 2016", "Jul 2016", "Jan 2017", "Jul 2017", "Jan 2018")) 
legend("bottomright", legend=c("Observed", "Fitted"), lty=c(1, 1), lwd=c(2, 2), col=c("Black", "Red")) 

### forecast for next 11 months 
niftyseriesforecasts6=forecast(niftyseriesforecasts5, h=11) 

### plot for the forecasted values
#plot(niftyseriesforecasts6, xlab="Time", ylab="log(NiftyIndex)") 
actual=ts(log(data[,2]), frequency=12,start=c((2014),3)) 
autoplot(niftyseriesforecasts6,ylab="log(NiftyIndex)",xlab="Time",shadecols = c("#63F3A6", "#E5FAEF")) +  autolayer(actual, series="Data") +  autolayer(niftyseriesforecasts6$mean, series="Forecasts") 

### get the actual data for 2018
actual_2018 = actual[(length(actual)-10):length(actual)]

### calculating the sum of squared errors
h_sse = sum((niftyseriesforecasts6$mean - actual_2018)^2 )

#### testing for residuals
acf(niftyseriesforecasts6$residuals, lag.max=20,na.action = na.pass) 
Box.test(niftyseriesforecasts6$residuals, lag=20, type="Ljung-Box") 
plot.ts(niftyseriesforecasts6$residuals) 
hist(niftyseriesforecasts6$residuals,breaks=10) 


##########ARIMA 

### testing the raw data to check stationary
adf.test(niftyseries) 
acf(niftyseries,lag.max=20) 
pacf(niftyseries) 

## testing the first order difference data to check stationary
niftyseriesdiff1=diff(niftyseries, differences=1) 
plot.ts(niftyseriesdiff1) 
adf.test(niftyseriesdiff1) 
pacf(niftyseriesdiff1, lag.max=20) 
acf(niftyseriesdiff1, lag.max=20) 

## testing the second order difference data to check stationary
niftyseriesdiff2=diff(niftyseries, differences=2) 
plot.ts(niftyseriesdiff2) 
adf.test(niftyseriesdiff2) 
pacf(niftyseriesdiff2, lag.max=20) 
acf(niftyseriesdiff2, lag.max=20) 

#### choose the p,q for arima model
rmse = rep(0,9)
count = 0;
for (i in 0:2) {
  for (j in 0:2){
    count = count + 1
    arimamod=arima(niftyseries,order=c(j,2,i)) 
    rmse[count] = accuracy(arimamod)[2]
  }
}

### build the arima best model 
arimamod=arima(niftyseries,order=c(0,2,1)) 
summary(arimamod)
coeftest(arimamod)
niftyseriesforecasts7=forecast(arimamod,h=11) 
a_sse = sum((niftyseriesforecasts7$mean - actual_2018)^2 )

#plot(niftyseriesforecasts7, xlab="Time", ylab="log(NiftyIndex)") 
autoplot(niftyseriesforecasts7,ylab="log(NiftyIndex)",xlab="Time",shadecols = c("#63F3A6", "#E5FAEF")) +  autolayer(actual, series="Data") +  autolayer(niftyseriesforecasts7$mean, series="Forecasts") 

#### testing for residuals
acf(niftyseriesforecasts7$residuals, lag.max=20,na.action = na.pass) 
Box.test(niftyseriesforecasts7$residuals, lag=20, type="Ljung-Box") 
plot.ts(niftyseriesforecasts7$residuals) 
hist(niftyseriesforecasts7$residuals) 

#### plotting the graph for acutal data , forecasted data from arima and holt-winter
autoplot(niftyseriesforecasts7,ylab="log(NiftyIndex)",xlab="Time",PI = FALSE) +  autolayer(actual, series="Data") +  autolayer(niftyseriesforecasts7$mean, series=paste("Arima (SSE -", round(a_sse,3), ")", sep = " ")) +  autolayer(niftyseriesforecasts6$mean, series=paste("Holt-Winter (SSE -", round(h_sse,3), ")", sep = " ")) 


