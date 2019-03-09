#### Seasonal differing

usmelec %>% autoplot()
usmelec %>% log() %>% autoplot()
#remove tendency
usmelec %>% log() %>% diff(lag=12) %>%
  autoplot()
#remove seasonality
usmelec %>% log() %>% diff(lag=12) %>%
  diff(lag=1) %>% autoplot()


########que orden de diferenciacion debemos tomar?
library(urca)

goog %>% autoplot()
summary(ur.kpss(goog))
kpss.test(goog)

# to estimate the number of differences required to make 
#a given time series stationary. ndiffs estimates the number 
#of first differences necessary using adf or kpss test

ndiffs(goog)

usmelec %>% log() %>% nsdiffs()
usmelec %>% log() %>% diff(lag=1) %>% autoplot

# Ejercicio: encuentra la differenciacion mas apropiada 
# para la serie visitors

ndiffs(visitors)
visitors %>% autoplot()
ndiffs(log(visitors))

visitors %>% log() %>% diff(1)%>% autoplot() 

####### Autoregressive models: AR(t)

summary(lm(kings[3:42]~kings[1:40]+kings[2:41]))
acf(kingstimeseries)

# simulamos datos multivariantes correlacionados
library(MASS)
data<-mvrnorm(100,c(0,0,0),as.matrix(cbind(c(1,0.95,0.95),c(0.95,1,0.95),c(0.95,0.5,1))))
summary(lm(data[1:98,1]~data[2:99,1]+data[3:100,1]))

####### Moving average models: MA(t)

####### ARIMA(p,q,d) models
data("uschange")
fit<-auto.arima(uschange[,"Consumption"])

ggtsdisplay(internet)
ggtsdisplay(diff(internet))

(fit <- Arima(internet,order=c(3,1,0)))

auto.arima(internet)

auto.arima(internet, stepwise=FALSE,
           approximation=FALSE)

checkresiduals(fit)

fit %>% forecast %>% autoplot


##### Ejemplo: 

eeadj <- seasadj(stl(elecequip, s.window="periodic"))
autoplot(eeadj) + xlab("Year") +
  ylab("Seasonally adjusted new orders index")

ggtsdisplay(diff(eeadj))

(fit <- Arima(eeadj, order=c(3,1,1)))

checkresiduals(fit)

fit %>% forecast %>% autoplot

