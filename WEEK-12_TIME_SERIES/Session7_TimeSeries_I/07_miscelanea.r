# ajuste seasonal
autoplot(birthstimeseries, series="Data") +
  autolayer(seasadj(fit), series="Seasonally-adjusted")

#ajuste tendencia
trModel=lm(birthstimeseries~seq(1,length(birthstimeseries)))
plot(resid(trModel), type="l")

#autocorrelacion
acfRes <- acf(AirPassengers) # autocorrelation
pacfRes <- pacf(AirPassengers)  # partial autocorrelation
ccfRes <- ccf(mdeaths, fdeaths, ylab = "cross-correlation") # computes cross correlation between 2 timeseries.
head(ccfRes[[1]])

acfkings<- acf(kingstimeseries) # autocorrelation
pacfRes <- pacf(kingstimeseries) 

acfbirth<- acf(birthstimeseries) # autocorrelation
pacfbirth <- pacf(birthstimeseries) 

acfsouvenir<- acf(souvenirtimeseries) # autocorrelation
pacfsouvenir <- pacf(souvenirtimeseries) 

acfsouvenir<- acf(souvenirtimeserieslog) # autocorrelation
pacfsouvenir <- pacf(souvenirtimeserieslog) 

