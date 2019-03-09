################# residuals ##################
#For stock market prices and indexes, the best forecasting 
#method is often the naïve method. That is, each forecast 
#is simply equal to the last observed value, or ^yt=yt−1. 
#Hence, the residuals are simply equal to the difference 
#between consecutive observations: et=yt−^yt=yt−yt−1.

autoplot(goog200) +
  xlab("Day") + ylab("Closing Price (US$)") +
  ggtitle("Google Stock (daily ending 6 December 2013)")

res <- residuals(naive(goog200))

autoplot(res) + xlab("Day") + ylab("") +
  ggtitle("Residuals from naïve method")

gghistogram(res) + ggtitle("Histogram of residuals")

ggAcf(res) + ggtitle("ACF of residuals")

fits <- fitted(naive(goog200))
autoplot(goog200, series="Data") +
  autolayer(fits, series="Fitted") +
  xlab("Day") + ylab("Closing Price (US$)") +
  ggtitle("Google Stock (daily ending 6 December 2013)")

checkresiduals(naive(goog200))

######### check residuals
Box.test(res, lag=10, fitdf=0)
Box.test(res,lag=10, fitdf=0, type="Lj")

ggAcf(res) + ggtitle("ACF of residuals")

######## Ejercicio3:
# Haz una prediccion basada en naive seasonal 
# para los datos de consumo de cerveza cuatrimestral en AUS desde 1992
beer <- window(ausbeer, start=1992)
fc <- snaive(beer)
autoplot(fc)
#Are the residuals white noise?
checkresiduals(fc)
#What do you conclude?


######## Medidas de forecast accuracy: diferencia entre prediccion 
# y valor real en el test set, que no ha sido nunca visto por el modelo
beer2 <- window(ausbeer, start=1992, end=c(2007,4))
beer3 <- window(ausbeer, start=2008)
beerfit1 <- meanf(beer2, h=10)
beerfit2 <- rwf(beer2, h=10)
beerfit3 <- snaive(beer2, h=10)
accuracy(beerfit1, beer3)
accuracy(beerfit2, beer3)
accuracy(beerfit3, beer3)

# non-seasonal
googfc1 <- meanf(goog200, h=40)
googfc2 <- rwf(goog200, h=40)
googfc3 <- rwf(goog200, drift=TRUE, h=40)
autoplot(subset(goog, end = 240)) +
  autolayer(googfc1, PI=FALSE, series="Mean") +
  autolayer(googfc2, PI=FALSE, series="Naïve") +
  autolayer(googfc3, PI=FALSE, series="Drift") +
  xlab("Day") + ylab("Closing Price (US$)") +
  ggtitle("Google stock price (daily ending 6 Dec 13)") +
  guides(colour=guide_legend(title="Forecast"))

googtest <- window(goog, start=201, end=240)
accuracy(googfc1, googtest)
accuracy(googfc2, googtest)
accuracy(googfc3, googtest)

############# CV time series
e <- tsCV(goog200, rwf, drift=TRUE, h=1)
sqrt(mean(e^2, na.rm=TRUE))

sqrt(mean(residuals(rwf(goog200, drift=TRUE))^2, na.rm=TRUE))

e <- tsCV(goog200, forecastfunction=naive, h=8)
# Compute the MSE values and remove missing values
mse <- colMeans(e^2, na.rm = T)
# Plot the MSE values against the forecast horizon
data.frame(h = 1:8, MSE = mse) %>%
  ggplot(aes(x = h, y = MSE)) + geom_point()
