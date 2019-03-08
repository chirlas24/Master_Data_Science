#### descomposicion de series estacionales

##### opcion 1: decompose
birthstimeseriescomponents <- decompose(birthstimeseries)
plot(birthstimeseriescomponents)

####### opcion 2: stl

fit <- stl(birthstimeseries,s.window=7)
autoplot(fit)+xlab("Year")

ggsubseriesplot(seasonal(fit))

autoplot(birthstimeseries, series="Data") +
  autolayer(trendcycle(fit), series="Trend-cycle")

autoplot(birthstimeseries, series="Data") +
  autolayer(seasadj(fit), series="Seasonally-adjusted")

######## opcion 3: X11
souvenir <- scan("http://robjhyndman.com/tsdldata/data/fancy.dat")
souvenirtimeseries <- ts(souvenir, frequency=12, start=c(1987,1))
#souvenirtimeseries
ts.plot(souvenirtimeseries)

# step1: detrend with smoothing
souvenirs12 <- SMA(souvenirtimeseries,n=12)
plot.ts(souvenirs12)
souvenirs12.detrend=souvenirtimeseries-souvenirs12
plot.ts(souvenirs12.detrend)

#Step 3: Estimador preliminar de la componente estacional de los datos
#Dividimos cada punto por su punto anterior:
  
souvenirs12.des=souvenirs12[2:84]/souvenirs12[1:83]
plot.ts(souvenirs12.des)
souvenirs12.detrend.des=souvenirs12.detrend[2:84]/souvenirs12.detrend[1:83]
plot.ts(souvenirs12.detrend.des)

#equivalente a: 
library(seasonal)
fit <- seas(souvenirtimeseries, x11="")
autoplot(fit)
autoplot(seasadj(fit))

#SEAS: model-based
fit2 <- seas(souvenirtimeseries)
autoplot(fit2)

#stl: control sobre tendencia y estacionalidad
fit3 <- stl(souvenirtimeserieslog,s.window="periodic",t.window=15,robust=T)
autoplot(fit3)+xlab("Year")
