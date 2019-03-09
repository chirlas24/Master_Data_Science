# smoothing como herramienta para conocer la tendencia en una serie temporal no estacional
library(TTR)
kingstimeseriesSMA3 <- SMA(kingstimeseries,n=3)
plot.ts(kingstimeseriesSMA3)

kingstimeseriesSMA5 <- SMA(kingstimeseries,n=5)
plot.ts(kingstimeseriesSMA5)

kingstimeseriesSMA14 <- SMA(kingstimeseries,n=14)
plot.ts(kingstimeseriesSMA14)

## smoothing para entender la evolucion de clinton y trump
clintonSMA7 <- SMA(polls_clinton_ts,n=7)
plot.ts(clintonSMA7)
clintonSMA14 <- SMA(polls_clinton_ts,n=14)
plot.ts(clintonSMA14)
clintonSMA30 <- SMA(polls_clinton_ts,n=30)
plot.ts(clintonSMA30)
clintonSMA60 <- SMA(polls_clinton_ts,n=60)
plot.ts(clintonSMA60)

trumpSMA7 <- SMA(polls_trump_ts,n=7)
plot.ts(trumpSMA7)
trumpSMA14 <- SMA(polls_trump_ts,n=14)
plot.ts(trumpSMA14)
trumpSMA30 <- SMA(polls_trump_ts,n=30)
plot.ts(trumpSMA30)
trumpSMA60 <- SMA(polls_trump_ts,n=60)
plot.ts(trumpSMA60)

ts.plot(trumpSMA7,clintonSMA7,gpars = list(col = c("red", "blue")))
ts.plot(trumpSMA14,clintonSMA14,gpars = list(col = c("red", "blue")))
ts.plot(trumpSMA30,clintonSMA30,gpars = list(col = c("red", "blue")))
ts.plot(trumpSMA60,clintonSMA60,gpars = list(col = c("red", "blue")))

