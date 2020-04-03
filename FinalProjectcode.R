install.packages("zoo")
install.packages("tseries")
install.packages("fracdiff")
install.packages("forecast")
install.packages("xts")
library(ggplot2)
library(zoo) 
library(tseries)
library(fracdiff)
library(forecast) 
library(xts)
library(lubridate)

#Blue Road Site Model Fit 

BR=read.csv("/Users/mgarriga/BSC6926/BRfinal.csv")


WD1 <- ts(BR$Depth, start = 1, frequency = 30 )

plot( WD1, typ="l", ylab= "Depth", xlab="")

WD1.d <- decompose(WD1, 'multiplicative')

#Decomposing the series

plot(WD1.d)

adf.test(WD1)

WD2 <- diff(WD1)

adf.test(WD1)

adf.test(WD2)

acf(WD2, lag.max=45)

pacf(WD2, lag.max=45)

#Fitting ARIMA

arima.depth1 <-auto.arima(WD1, trace=TRUE)

tsdisplay(residuals(arima.depth1), lag.max=45)

arima.depth2 <-auto.arima(WD2, trace=TRUE)

tsdisplay(residuals(arima.depth2), lag.max=45)

#Analyzing residuals

AIC(arima.depth2,arima.depth1)

par(mfrow=c(1,1))
plot(WD1, typ="l"); lines(fitted(arima.depth1),col="red")

checkresiduals(arima.depth1, lag=36)

#Forecast ARIMA

plot(forecast(arima.depth1, h=60))

#Fitting ARIMA Model
arima.depth1 <-auto.arima(Depth1, trace=TRUE)
tsdisplay(residuals(arima.depth1), lag.max=45)
arima.depth1.2 <-arima(WD1, seasonal= list(order=c(2,0,2)))


Forecast <- forecast(arima.depth1)

plot(Forecast)

#Explanatory Series 1: Salinity 

sal <- ts(BR$Salinity, start= 1, frequency=30)
par(mfrow=c(1,1), mai=c(0.25,0.8,0.1, 0.1))
plot(sal , typ="l", ylab= "Salinity", xlab="")

lines(tsclean(sal) , col="red")
sal <-tsclean(sal)
sal.d <- decompose(sal, 'multiplicative')
plot(sal.d)

adf.test(sal)
adf.test(diff(sal))

ccf( diff(sal),WD1, na.action = na.pass, lag.max=40, plot=TRUE)

arima.depthsal <-auto.arima(WD1, xreg=c(diff(sal),0), trace=TRUE)
tsdisplay(residuals(arima.depthsal), lag.max=45)

AIC(arima.depth1, arima.depthsal )

checkresiduals(arima.depthsal)

par(mfrow=c(1,1))
plot(WD1 , typ="l"); lines(fitted(arima.depthsal),col="red")

Forecast2 <- forecast(arima.depthsal)
plot(Forecast2)

#Explanatory series 2: Temperature 
temp <- ts(BR$Temperature..C., start= 1, frequency=30)
par(mfrow=c(1,1), mai=c(0.25,0.8,0.1, 0.1))
plot(temp , typ="l", ylab= "Temperature", xlab="")

lines(tsclean(temp) , col="red")
temp <-tsclean(temp)
temp.d <- decompose(temp, 'multiplicative')
plot(temp.d)

adf.test(temp)
adf.test(diff(temp))

ccf( diff(temp),WD1, na.action = na.pass, lag.max=40, plot=TRUE)

arima.depthtemp <-auto.arima(WD1, xreg=c(diff(temp),0), trace=TRUE)

AIC(arima.depth1, arima.depthtemp )
#Model did not improve







