library(imputeTS)

x <- ts(c(1, 2, 3, 4, 5, 6, 7, 8, NA, NA, 11, 12))

#impute using the mean of the whole series
na.mean(x)

# impute using the median
na.mean(x, option="median")

#since our data has an order might make sense to interpolate
na.interpolation(x)

tsAirgap
statsNA(tsAirgap)
plotNA.distribution(tsAirgap)
plotNA.distributionBar(tsAirgap, breaks = 20)

#Kalman filter
imp <- na.kalman(tsAirgap)
plotNA.imputations(tsAirgap, imp, tsAirgapComplete)

tsAirgap.imp <- na.mean(tsAirgap)
plotNA.imputations(tsAirgap, tsAirgap.imp)

