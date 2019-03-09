# Dataset1: Edad de los reyes de UK desde William 
library(fpp2)
kings <- scan("http://robjhyndman.com/tsdldata/misc/kings.dat",skip=3)
kingstimeseries <- ts(kings)
ts.plot(kingstimeseries)

#Testeamos para comprobar si estacionario y/o estacional
kpss.test(kingstimeseries)
adf.test(kingstimeseries)

# Dataset2: Número de nacimientos por mes en NY 
births <- scan("http://robjhyndman.com/tsdldata/data/nybirths.dat")
birthstimeseries <- ts(births, frequency=12, start=c(1946,1))
ts.plot(birthstimeseries)

#Testeamos para comprobar si estacionario y/o estacional
kpss.test(birthstimeseries)
adf.test(birthstimeseries)

# Dataset3: tienda de souvenirs
souvenir <- scan("http://robjhyndman.com/tsdldata/data/fancy.dat")
souvenirtimeseries <- ts(souvenir, frequency=12, start=c(1987,1))
ts.plot(souvenirtimeseries)

souvenirtimeserieslog <- ts(log(souvenir), frequency=12, start=c(1987,1))
ts.plot(souvenirtimeserieslog)

###############################################################################
# Mean prediction
###############################################################################
# Al ser una serie estacionaria se predice bien el siguiente valor usando la media
#de toda la serie
meanf(kingstimeseries,1)
meanf(kingstimeseries,5)
autoplot(meanf(kingstimeseries,5))

## Naïve method
#Se predice la siguiente observación con el último valor de la serie. 
#Funciona bien para $random walk$ time series: series temporales en 
#los que dónde sucederá el siguiente punto es un proceso aleatorio $y_t-y_{t-1}=\epsilon$
# Mejor para datos no estacionarios donde el siguiente paso depende del anterior pero
# puede tomar una dirección inesperada.
#Se suelen usar en modelos económicos donde pueden suceder cambios de direccion inesperados
  
###############################################################################
# Naive prediction
###############################################################################
autoplot(naive(kingstimeseries, 5))
autoplot(naive(souvenirtimeserieslog,5))
#autoplot(rwf(kingstimeseries, 5))

##############################################################################
# Seasonal Naive prediction
###############################################################################
autoplot(naive(birthstimeseries,10))
autoplot(snaive(birthstimeseries,10))

##############################################################################
# drift: last value plus change
###############################################################################
autoplot(rwf(birthstimeseries,10))
autoplot(rwf(birthstimeseries,drift=T,10))
rwf(birthstimeseries,drift=T,10)

(births[168]-births[1])/168

############# Ejercicio 1: 
# Predice los 100 proximos datos para las series goog y auscafe
# pinta las predicciones usando autoplot

#Eje1.a
data(goog)

ts.plot(goog)

kpss.test(goog)
adf.test(goog)

autoplot(naive(goog, 10))
autoplot(rwf(goog, drift = T, 10))

         
goog2 = ts(goog[600:1000])

autoplot(goog2)

rwf(goog2, drift=T, 10)
autoplot(rwf(goog2, drift=T, 10))

#Eje1.b

autoplot(auscafe)

kpss.test(auscafe)
adf.test(auscafe)

auscafelog <- log(auscafe)


