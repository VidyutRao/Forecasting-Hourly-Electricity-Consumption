rm(list = ls())

library(ggplot2)
library(xts)
library(mgcv)
library(xgboost)
library(forecast)
library(fGarch)
library(rugarch)
library(tseries)
library(randomForest)
library(TSA)

setwd('C:/Users/vidyu/Desktop/Georgia Tech/Time Series Analysis/Final Project')
getwd()

df2016a <- read.csv('EIA930_BALANCE_2016_Jan_Jun.csv')
df2016b <- read.csv('EIA930_BALANCE_2016_Jul_Dec.csv')
df2017a <- read.csv('EIA930_BALANCE_2017_Jan_Jun.csv')
df2017b <- read.csv('EIA930_BALANCE_2017_Jul_Dec.csv')
df2018a <- read.csv('EIA930_BALANCE_2018_Jan_Jun.csv')
df2018b <- read.csv('EIA930_BALANCE_2018_Jul_Dec.csv')
df2019a <- read.csv('EIA930_BALANCE_2019_Jan_Jun.csv')
df2019b <- read.csv('EIA930_BALANCE_2019_Jul_Dec.csv')


df2016a <- df2016a[, c(1,5,7)]
df2016b <- df2016b[, c(1,5,7)]
df2017a <- df2017a[, c(1,5,7)]
df2017b <- df2017b[, c(1,5,7)]
df2018a <- df2018a[, c(1,5,7)]
df2018b <- df2018b[, c(1,5,7)]
df2019a <- df2019a[, c(1,5,7)]
df2019b <- df2019b[, c(1,5,7)]


dffinal <- rbind(df2016a, df2016b, df2017a, df2017b,df2018a,df2018b,df2019a,df2019b)

colnames(dffinal)[2:3] <- c('date','demand')
dffinal$date <- as.POSIXct(dffinal$date,format = '%m/%d/%Y %I:%M:%S %p', tz = "UTC")
dffinal$demand <- as.numeric(gsub(",","",dffinal$demand))


soco <- dffinal[dffinal$Balancing.Authority == 'SOCO',]

soco[is.na(soco$demand), 'demand'] <- mean(soco[soco$demand >=0, 'demand'], na.rm = TRUE)
soco[soco$demand <0, 'demand'] = 0

rmse <- function(y,yhat) {
  sqrt(mean((y-yhat)^2))
}

date <- soco[,2]

soco <- xts(soco[,3],soco[,2])
colnames(soco) <- 'Demand'

date.test <- date[date > '2019-11-20 09:00:00']
date <- date[date<='2019-11-20 09:00:00']

ggplot(data = soco, aes(x = index(soco), y = Demand))+
  geom_line(color = "#00AFBB", size = 0.5) + ggtitle('Electricity Demand in Georgia')+
  xlab('Date') + ylab('Demand in MW')

soco.org <-  soco
soco <- diff(soco$Demand,diff = 1)
soco <- soco[!is.na(soco)]
soco.test <- soco[date.test]
soco <- soco[date]

soco.org.train <- soco.org[date]
soco.org.test<- soco.org[date.test]

date <- date[2:length(date)]


## Create equally spaced time points for fitting trends
time.pts = c(1:length(soco))
time.pts = c(time.pts - min(time.pts))/max(time.pts)

#More features  

x1 = .indexhour(soco)
x2 = .indexmon(soco)
x3 = .indexwday(soco)
x4 = .indexweek(soco)

rm(PredImp)
PredImp = randomForest(soco ~ x1 + x2 + x3 + x4 + time.pts, ntree = 350,
                       mtry  = 4, maxnodes = 300, keep.forest = FALSE, importance = TRUE)
importance(PredImp, type = 1)


## Fit linear regression models
lm.fit = lm(soco~ x1  )
summary(lm.fit)

lm.fit.2 = lm(soco ~ time.pts + time.pts^2)
summary(lm.fit.2)

## Is there a trend? 
ec.fit.lm = xts(fitted(lm.fit), date)
plot(soco,ylab="Energy Consumption")
lines(ec.fit.lm,lwd=2,col="purple")

ec.fit.lm.2 = xts(fitted(lm.fit.2), date)
plot(soco,ylab="Energy Consumption")
lines(ec.fit.lm.2,lwd=2,col="purple")


## Local Polynomial Trend Estimation
loc.fit.1 = loess(soco~time.pts)
ec.fit.loc.1 = xts(fitted(loc.fit.1),date)

plot(soco,ylab="Energy Consumption",main = 'Loess fit')
lines(ec.fit.loc.1,lwd=2,col="purple")

loc.fit.2 = loess(soco~x1 + x2)
ec.fit.loc.2 = xts(fitted(loc.fit.2),date)

plot(soco,ylab="Energy Consumption",main = 'Loess Estimation')
lines(ec.fit.loc.2,lwd=2,col="purple")


## Splines Trend Estimation
gam.fit.1 = gam(soco~s(time.pts) )
ec.fit.gam.1 = xts(fitted(gam.fit.1),date) 

plot(soco,ylab="Energy Demand", main = 'Generalised Additive Model')
lines(ec.fit.gam.1,lwd=2,col="purple")

gam.fit.2 = gam(soco~s(x1))
ec.fit.gam.2 = xts(fitted(gam.fit.2),date) 

plot(soco,ylab="Energy Demand")
lines(ec.fit.gam.2,lwd=2,col="purple")

gam.fit.3 = gam(soco~s(x2))
ec.fit.gam.3 = xts(fitted(gam.fit.3),date) 

plot(soco,ylab="Energy Demand")
lines(ec.fit.gam.3,lwd=2,col="purple")

#XGBoost Model
params <- list(booster = "gbtree", objective = "reg:linear",
               eta=1, gamma=1, max_depth=3, min_child_weight=1,
               subsample=0.6);
xgb.model <- xgboost(params = params, data = as.matrix(cbind(x1,x2,x3,x4)), 
                     label = as.matrix(soco$Demand),nrounds = 500);
pred <- predict(xgb.model, as.matrix(cbind(x1,x2,x3,x4)))
ec.fit.xgb <- xts(pred, order.by =  date)

plot(soco,ylab="Energy Demand")
lines(ec.fit.xgb,lwd=2,col="purple")

#Sine- cosine model
sin.fit <- gam(soco~ time.pts + sin(2*pi/(365*24)*time.pts)+cos(2*pi/(365*24)*time.pts))
ec.fit.sin = xts(fitted(sin.fit), date)
plot(soco,ylab="Energy Consumption")
lines(ec.fit.sin,lwd=2,col="purple")


#Picking Loess as the final model for seasonality and trend
soco <- soco - ec.fit.loc.2
#soco <- soco + ec.fit.xgb
arima.model <- auto.arima(soco, max.p = 20, max.q = 20, max.d = 20,max.P = 20, max.Q = 20, max.D = 20)

summary(arima.model)
resids <- arima.model$residuals
plot(resids)
par(mfrow = c(1,1));
acf(resids, main = 'Arima Residuals ACF')
pacf(resids, main = 'Arima Residuals PACF')
acf(resids^2, main = 'Arima Residuals squared ACF')
pacf(resids^2, main = 'Arima Residuals squared PACF')
Box.test(resids, type = "Box-Pierce")
Box.test(resids^2, type = "Box-Pierce")


soco <- soco[!is.na(soco)]



final.bic = Inf
final.order.garch = c(0,0)
for (m in 1:1) for (n in 3:3){
  print(c(m,n))
  spec = ugarchspec(variance.model=list(garchOrder=c(m,n)),
                    mean.model=list(armaOrder=c(6,4), 
                                    include.mean=T), distribution.model="std")    
  fit = ugarchfit(spec, soco, solver = 'hybrid')
  current.bic = infocriteria(fit)[2] 
  print(current.bic)
  if (current.bic < final.bic){ 
    final.bic = current.bic
    final.order.garch = c(m, n)
  }
} 

final.bic
final.order.garch

spec <-  ugarchspec(variance.model=list(garchOrder=c(1,3)),
                   mean.model=list(armaOrder=c(6,4),
                                   include.mean=T),
                   distribution.model="std")
fit <- ugarchfit(spec, soco, solver = 'hybrid')
residuals.garch <- residuals(fit)
acf(residuals.garch, main = 'GARCH Residuals ACF')





nfore = length(soco.test)
fore.series.1 = NULL
fore.sigma.1 = NULL
data = soco
for(f in seq(1,nfore,50)){	
  final.model.1 = ugarchfit(spec, data, solver = 'hybrid')    
  fore = ugarchforecast(final.model.1, n.ahead=50)
  fore.series.1 = c(fore.series.1, fore@forecast$seriesFor)
  fore.sigma.1 = c(fore.sigma.1, fore@forecast$sigmaFor)
  temp <- xts (fore@forecast$seriesFor, index(soco.test)[f:(f+49)])
  data = rbind(data,temp)
}





## Adding the trend, seasonality and differencing back forecasted data
x1.test = .indexhour(soco.test)
x2.test = .indexmon(soco.test)
testtf <- cbind(x1.test, x2.test)
test.ts <- predict(loc.fit.2, testtf)
fore.cast <- xts(fore.series.1 + test.ts, date.test)
cum.sum <- c(as.numeric(soco.org.train$Demand[length(soco.org.train)]))
for (i in 1:length(soco.test)) {
  cum.sum <- c(cum.sum, cum.sum[i] + as.numeric(fore.cast[i]))
}
fore.cast <- xts(cum.sum[2:length(cum.sum)], index(soco.test))
colnames(fore.cast) <- 'Demand'



plot(soco.org,type="l",main="Mean Prediction Comparison") 
points(soco.org.test,lwd= 2, col="blue") 
points(fore.cast,lwd= 2, col="green") 


rmse(fore.cast, soco.org.test)



#Comparison to XGB model
x1 = .indexhour(soco.org.train)
x2 = .indexmon(soco.org.train)
x3 = .indexwday(soco.org.train)
x4 = .indexweek(soco.org.train)


x1.test = .indexhour(soco.org.test)
x2.test = .indexmon(soco.org.test)
x3.test = .indexwday(soco.org.test)
x4.test = .indexweek(soco.org.test)

time.pts = c(1:length(soco.org))
time.pts = c(time.pts - min(time.pts))/max(time.pts)
time.pts.1 <- time.pts[1:length(soco.org.train)]
time.pts.test <- time.pts[length(soco.org.train)+1:length(soco.org.test)]
time.pts <- time.pts.1

params <- list(booster = "gbtree", objective = "reg:linear",
               eta=1, gamma=1, max_depth=3, min_child_weight=1,
               subsample=0.8);
xgb.model <- xgboost(params = params, data = as.matrix(cbind(x1,x2,x3,x4,time.pts)), 
                     label = as.matrix(soco.org.train$Demand),nrounds = 500);

xgb.data <- cbind(x1.test, x2.test, x3.test, x4.test, time.pts.test)
colnames(xgb.data) <- c('x1', 'x2', 'x3', 'x4','time.pts')
preds <- predict(xgb.model, xgb.data)
rmse(soco.org.test$Demand, preds)

preds <- xts(preds, index(soco.org.test))
plot(soco.org)
points(soco.org.test,lwd= 2, col="blue") 
points(preds,lwd= 2, col="green") 

mean.model <- mean(soco.org.train)

rmse(soco.org.test, mean.model)
