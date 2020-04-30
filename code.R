library(tidyverse)
library(lubridate)
library(ggfortify)
library(forecast)
library(urca)

raw.ts <- read_csv("EXCSRESNS.csv")

raw.ts$DATE <- date(raw.ts$DATE)
raw.ts$DTBSPCKM <- as.numeric(raw.ts$EXCSRESNS)
raw.ts <- raw.ts[order(raw.ts$DATE), ]

# select year after 2009
raw.ts <- raw.ts[300:length(raw.ts$DATE),]
raw.ts <- ts(raw.ts$EXCSRESNS, start=c(2009,1), freq=12)

# unit root test
summary(ur.df(raw.ts, type="trend", selectlags="BIC"))
raw.dif.ts <- diff(raw.ts)
summary(ur.df(raw.dif.ts, type="trend", selectlags="BIC"))

# general plot
autoplot(raw.dif.ts)

# plot acf pacf
acf(raw.dif.ts) # MA(1) or MA(3)
pacf(raw.dif.ts) # AR(3)

# trend and seasonality
raw.dif.trend.lin <- tslm(raw.dif.ts~trend+season)

summary(raw.trend.lin)

auto.arima(raw.dif.ts)

# model
arima.mod <- function(x, h){
  temp <- Arima(order=c(1,0,3), x)
  temp2 <- tryCatch(forecast(temp, h=h), error=function(e){print("error"); NA})
  temp2
}

eCV <- tsCV(raw.ts, arima.mod, h=1)
rmseCV <- sqrt( mean( eCV^2,na.rm=TRUE))

exptrend.mod <- function(x, h){
  ses(x, h=1, alpha=0.6)
}

eCV.exp <- tsCV(raw.ts, exptrend.mod, h=1)
rmseCV.exp <- sqrt( mean( eCV.exp^2,na.rm=TRUE))

store <- c()
n=1
while(n<=length(raw.ts)){
  temp <- tryCatch(Arima(order=c(1,0,3), raw.ts[1:n]), error=function(e){print("error"); NA})
  temp2 <- tryCatch(forecast(temp, h=1), error=function(e){print("error"); NA})
  store <<- c(store, ifelse(any(is.na(temp2)), NA, temp2$mean))
  n <- n+1
}

store <- ts(store, start=c(2009,1), freq=12)

# Prediction Plot
autoplot(raw.ts) +
  autolayer(store, series="one-step ahead prediction", color="red")

