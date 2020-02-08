load("/Users/Maby/BSC6926/BSC6926/ARIMA_Workshop.Rdata")
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
#Create timeseries object
nee <- ts( mangroves$nee, start= 1, frequency=30)
nee
#Visualize data
par(mfrow=c(1,1), mai=c(0.25,0.8,0.1, 0.1)) 
plot( nee, typ="l", ylab= "NEE", xlab="")
#removing outliers
plot(nee)
lines(tsclean(nee), col="red")
nee <- tsclean(nee)
#Decompose timeseries
nee.d <- decompose(nee, 'multiplicative') 
plot(nee.d)
#Test for stationarity (assumptions)
adf.test(nee)
#Detecting Autocorrelation
acf(nee, lag.max=45)
pacf(nee, lag.max=45)
#Fitting ARIMA Model
arima.nee1 <-auto.arima(nee, trace=TRUE)
tsdisplay(residuals(arima.nee1), lag.max=45)
arima.nee2 <-arima(nee , order=c(10,1,3), seasonal= list(order=c(2,0,2)))
tsdisplay(residuals(arima.nee2), lag.max= 30)
#Minimizing AIC
AIC(arima.nee1, arima.nee2)
par(mfrow=c(1,1))
plot(nee , typ="l"); lines(fitted(arima.nee2),col="red")
checkresiduals(arima.nee2, lag=36)
par(mfrow=c(1,1))
plot(nee , typ="l"); lines(fitted(arima.nee2),col="red")
plot(forecast(arima.nee2, h=30))

#Salnity
plot(forecast(arima.nee2, h=30))

sal <- ts(mangroves$salinity.max, start= 1, frequency=30)

par(mfrow=c(1,1), mai=c(0.25,0.8,0.1, 0.1))
plot(sal , typ="l", ylab= "Salinity", xlab="")

plot(sal , typ="l", ylab= "Salinity", xlab="")
lines(tsclean(sal) , col="red")

sal <- tsclean(sal)

sal.d <- decompose(sal, 'multiplicative')
plot(sal.d)
# Explore correlations
adf.test(sal)
adf.test(diff(sal))

ccf( diff(sal),nee, na.action = na.pass, lag.max=40, plot=TRUE)

arima.nee3 <-auto.arima(nee, xreg=c(diff(sal),0), trace=TRUE)

#Compare Models

sal.i <- sal
sal.i[sal.i < 25 ]<- 0
sal.i[sal.i >= 25 ]<- 1
plot(sal.i)

arima.nee4 <-auto.arima(nee, xreg=sal.i, trace=TRUE)

AIC(arima.nee2,arima.nee4 )

checkresiduals(arima.nee4, lag=36)

par(mfrow=c(1,1))

plot(nee , typ="l"); lines(fitted(arima.nee4),col="red")
AIC(arima.nee2, arima.nee3 )

#Other Explanatory Series: Photosynthetic Active Radiation

par <- ts( mangroves$par, start= 1, frequency=30)

par(mfrow=c(1,1), mai=c(0.25,0.8,0.1, 0.1))
plot(par , typ="l", ylab= "PAR", xlab="")
lines(tsclean(par) , col="red") #outliers removed

par <- tsclean(par)

par.d <- decompose(par, 'multiplicative')
plot(par.d)

adf.test(par)
adf.test(diff(par))

ccf( diff(par),nee, na.action = na.pass, lag.max=40, plot=TRUE)

arima.nee5 <-auto.arima(nee, xreg=c(diff(par),0), trace=TRUE)

AIC(arima.nee2,arima.nee5 )

checkresiduals(arima.nee5, lag=36)

par(mfrow=c(1,1))

plot(nee , typ="l"); lines(fitted(arima.nee5),col="red")
AIC(arima.nee2, arima.nee5 )










