############################################################
# DATA TRANSFORMATION
###########################################################
# Ajustamos datos de series temporales porque en gral, datos mas limpios y claros
# nos llevan a una mejor y más sencilla predicción. 
# Hay cuatro tipos de ajustes:
# ajustes de calendario

dframe <- cbind(Monthly = milk,
                DailyAverage = milk/monthdays(milk))
autoplot(dframe, facet=TRUE) +
  xlab("Years") + ylab("Pounds") +
  ggtitle("Milk production per cow")

# ajustes de poblacion (per 100.000, per 10^6...)

# ajustes por inflación
# If zt denotes the price index and yt denotes the 
# original house price in year t, then xt=yt/zt∗z2000 
# gives the adjusted house price at year 2000 dollar values. 

# transformaciones matemáticas

# Al final, se trata de quiar todas las fuentes de variacion 
# conocidas con el fin de tener menos variabilidad que explicar

# DATOS MAS CLAROS = PREDICCIONES MAS PRECISAS

###### NOTA: Predicciones sobre descomposiciones son mejores
fit <- stl(elecequip, t.window=13, s.window="periodic")
fit %>% seasadj() %>% naive() %>%
  autoplot() + ylab("New orders index") +
  ggtitle("ETS forecasts of seasonally adjusted data")

fit %>% forecast(method='naive') %>%
  autoplot() + ylab("New orders index") + xlab("Year")

elecequip %>% stlf(method='naive') %>%
  autoplot() + ylab("New orders index") + xlab("Year")



################## VSN ###################################
# use three variance stabilizating methods on souvenir data

autoplot(ts(log(souvenir),start=1,frequency=12))
autoplot(ts(sqrt(souvenir),start=1,frequency=12))
autoplot(ts((souvenir)^1/3,start=1,frequency=12))

autoplot(ts(elec,start=1,frequency=12))
autoplot(ts(log(elec),start=1,frequency=12))
autoplot(ts(sqrt(elec),start=1,frequency=12))
autoplot(ts(elec^(1/3),start=1,frequency=12))
autoplot(BoxCox(elec,lambda = 1/3))
# BoxCox puede encontrar de manera automatica la
# lambda que mejor estabilice los datos
lambda=BoxCox.lambda(elec)
autoplot(BoxCox(elec,lambda))

# la funcion de prediccion snaive busca lambda automáticamente
fit<-snaive(elec,lambda=1/3)
autoplot(fit,include=120)


# Ejercicio 2: Busca lambda para el data set gas
lambda=BoxCox.lambda(gas)
fit<-snaive(gas,lambda=lambda)
autoplot(fit,include=120)

################## Bias adjustment #########################
# Si se realiza el ajuste de una serie usando transformaciones de Box-Cox
# la "back-transformation" de la media es la mediana en la escala original. 
# Esto puede ser un pb si por ejemplo queremos sumar territorios.
# EN esos casos podemos preferir un ajuste del sesgo

#Forecasts of egg prices using a random walk with drift applied 
#to the logged data (lambda=0)

#Notice how the skewed forecast distribution pulls up the 
#point forecast when we use the bias adjustment.
#Bias adjustment is not done by default in the forecast package. 
#If you want your forecasts to be means rather than medians, 
#use the argument biasadj=TRUE when you select your Box-Cox 
#transformation parameter.

fc <- rwf(eggs, drift=TRUE, lambda=0, h=50, level=80)
fc2 <- rwf(eggs, drift=TRUE, lambda=0, h=50, level=80,
           biasadj=TRUE)
autoplot(eggs) +
  autolayer(fc, series="Simple back transformation") +
  autolayer(fc2, series="Bias adjusted", PI=FALSE) +
  guides(colour=guide_legend(title="Forecast"))

