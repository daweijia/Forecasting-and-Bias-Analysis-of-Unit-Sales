setwd("/Users/jiadawei/Desktop/Market Analytics")
DB <- read.csv("Homework 3 Student Data.csv")
install.packages('forecast')
library('forecast')
## Part1: Forecasting
# order data
DB <- DB[order(DB$overallWeekNum),]
# build holdout data
DBweek <- aggregate(totalCost~overallWeekNum, data = DB, sum)
holdoutDB <- DBweek[313:364,]
DBweek <- DBweek[-(313:364),]
# build timeseries object
totalcost_timeseries <- ts(data = DBweek$totalCost,freq=52)
# plot time series
plot(totalcost_timeseries)
# plot autocorrelation
plot(acf(totalcost_timeseries))
# estimate ARMA model
ARMAmodel <- Arima(totalcost_timeseries,order=c(3,0,2),include.drift=TRUE)
# non-seasonal ARMA 
autoARMAnonseason <- auto.arima(totalcost_timeseries,approximation=FALSE,D=0,seasonal=FALSE,stepwise=FALSE)
autoARMAseason <- auto.arima(totalcost_timeseries,approximation=FALSE,D=0,seasonal=TRUE,stepwise=FALSE)
# AIC of two models
AIC(autoARMAnonseason) #3812.139
AIC(autoARMAseason) #3799.118
# MSE
mean((holdoutDB$totalCost-forecast(autoARMAnonseason,h=52)$mean)^2) #12766.32
mean((holdoutDB$totalCost-forecast(autoARMAseason,h=52)$mean)^2) #9226.053
# plot forecast
nonseasonplot <- plot(forecast(autoARMAnonseason,h=52))
seasonplot <- plot(forecast(autoARMAseason,h=52))
# confident interval 
CInonseason <- c(mean(nonseasonplot$lower),mean(nonseasonplot$upper))
CIseason <- c(mean(seasonplot$lower),mean(seasonplot$upper))

## Part2: Attenuation Bias
# Price per Can
DB$price_per_can <- DB$totalCost/DB$units
# b) regressison log(units)~pricePerCan
ppc_regression <- lm(log(units)~price_per_can, data = DB)
summary(ppc_regression)
# c) aggregate by productNum+overallWeekNum and run regression again
aggregate_productNum_overallWeekNum <- merge((aggregate(totalCost~productNum+overallWeekNum, data = DB, mean)),
              (aggregate(units~productNum+overallWeekNum, data = DB, mean)), by=c("productNum","overallWeekNum"))
aggregate_productNum_overallWeekNum$price_per_can <- aggregate_productNum_overallWeekNum$totalCost/
                                                      aggregate_productNum_overallWeekNum$units
ppc_regression2 <- lm(log(units)~price_per_can, data = aggregate_productNum_overallWeekNum)
summary(ppc_regression2)
# d) aggregate by overallWeekNum and run regression again
aggregate_overallWeekNum <- merge((aggregate(totalCost~overallWeekNum, data = DB, mean)),
         (aggregate(units~overallWeekNum, data = DB, mean)), by=c("overallWeekNum"))
aggregate_overallWeekNum$price_per_can <- aggregate_overallWeekNum$totalCost/aggregate_overallWeekNum$units
ppc_regression3 <- lm(log(units)~price_per_can, data = aggregate_overallWeekNum)
summary(ppc_regression3)

## Part3: Omitted Variable Bias
# Price per Can
DB$price_per_can <- DB$totalCost/DB$units
DB$storeNum <- as.factor(DB$storeNum)
DB$productNum <- as.factor(DB$productNum)
DB$weekInYearNum <- as.factor(DB$weekInYearNum)
# regressison log(units)~pricePerCan
part3_regression1 <- lm(log(units)~price_per_can, data = DB)
summary(part3_regression1)
# regressison log(units)~isFeature
part3_regression2 <- lm(log(units)~price_per_can+isFeature, data = DB)
summary(part3_regression2)
# regressison log(units)~isDisplay
part3_regression3 <- lm(log(units)~price_per_can+isFeature+isDisplay, data = DB)
summary(part3_regression3)
# regressison log(units)~storeNum
part3_regression4 <- lm(log(units)~price_per_can+isFeature+isDisplay+storeNum, data = DB)
summary(part3_regression4)
# regressison log(units)~productNum
part3_regression5 <- lm(log(units)~price_per_can+isFeature+isDisplay+storeNum+productNum, data = DB)
summary(part3_regression5)
# regressison log(units)~weekInYearNum
part3_regression6 <- lm(log(units)~price_per_can+isFeature+isDisplay+storeNum+productNum+weekInYearNum, data = DB)
summary(part3_regression6)
