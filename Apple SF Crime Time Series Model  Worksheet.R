## R  Notebook for Brian Kao Apple  Cast Study

library('TTR')
library('ggplot2')
library('forecast')
library('tseries')
library('readxl')
library(zoo)
install.packages("forecast")
install.packages("devtools")
library(devtools)

# Retrieve Daily FEPS
DailyFEPS <- read.csv( "https://s3.us-east-2.amazonaws.com/rudydata/AppleDay.csv", header =  FALSE)
# Retrieve Weekly FEPS
WeeklyFEPS <- read.csv( "https://s3.us-east-2.amazonaws.com/rudydata/AppleWeek3.csv", header = FALSE)
DailyCalls <- read.csv( "https://s3.us-east-2.amazonaws.com/rudydata/CallsArima.csv", header = FALSE)
# Rerieve Monthly FEPS
#MonthlyFEPS <- read.csv( "", header = TRUE)

dfdaily = data.frame(DailyFEPS[2])
dfweekly = data.frame(WeeklyFEPS[15])
dfdailyDrug = data.frame(DailyFEPS[3])
dfweeklyDrug = data.frame(WeeklyFEPS[3])
dfweekNorthern = data.frame(WeeklyFEPS[576:783,5])
dfcalls = data.frame(DailyCalls[2])
#dfmonthly = data.frame(MonthlyFEPS[2])
dfweeklyDrug5year = data.frame(WeeklyFEPS[697:804,4])
dfweeklyAll5year = data.frame(WeeklyFEPS[697:804,3])
dfweeklydrug = data.frame(WeeklyFEPS[697:804,3])
#df1 = (df[9])
#df3 = (df2[9])

#head(dfmonthly)
head(dfweekly)
head(dfdaily)
head(dfdaily)
dfweekly = data.frame(WeeklyFEPS[628:783,15])
dataweek = ts(dfweekly,start = c(2015,01,04), frequency =52)
plot(dataweek, xlab='Weeks', ylab = 'Incidents')
plot(diff(dataweek),ylab='Incidents')
ARIMAfit = auto.arima(dataweek, lambda = 0)
summary(ARIMAfit)
fcst <- forecast(ARIMAfit, h = 52)
plot(fcst)
pred = predict(ARIMAfitweekly, n.ahead = 52)
pred 


# Timeseries Object
datadaily = ts(dfdaily,start = c(2003,1,1), frequency =365.25)
dataweekly = ts(dfweekly, start = c(2015,1, 04), frequency =52)
#datamonthly = ts(dfmonthly,start = c(2017,4,1), frequency =12)
datadailydrug = ts(dfdailyDrug,start = c(2003,1,1), frequency =365.25)

dataweeklydrug = ts(dfweeklyDrug,start = c(2003,1,1), frequency =52)
dataweeklyAll5year = ts(dfweeklyAll5year,start = c(2014,1,06), frequency =52)
datacalls = ts(dfcalls,start = c(2016,4,01), frequency =365.25)


dataweekdrug = ts(dfweeklyAll5year,start = c(2014,1,06), frequency =52)


dfweekNorthern = ts(dfweekNorthern,start = c(2014,01,04), frequency =52)
Bad Areas Timeseries Object
dfweekdom = data.frame(WeeklyFEPS[628:783,24])
dfweeklar = data.frame(WeeklyFEPS[628:783,25])
dfweeknon = data.frame(WeeklyFEPS[628:783,26])
dfweekoff = data.frame(WeeklyFEPS[628:783,27])
dfweekother = data.frame(WeeklyFEPS[628:783,28])
dfweekprop = data.frame(WeeklyFEPS[628:783,29])
dfweeksocial = data.frame(WeeklyFEPS[628:783,30])
dfweeksus = data.frame(WeeklyFEPS[628:783,31])
dfweekveh = data.frame(WeeklyFEPS[628:783,32])
dfweekwhite = data.frame(WeeklyFEPS[628:783,33])

#Good Areas Timeseries Object
dfweekdom = data.frame(WeeklyFEPS[628:783,34])
dfweeklar = data.frame(WeeklyFEPS[628:783,35])
dfweeknon = data.frame(WeeklyFEPS[628:783,36])
dfweekoff = data.frame(WeeklyFEPS[628:783,37])
dfweekother = data.frame(WeeklyFEPS[628:783,38])
dfweekprop = data.frame(WeeklyFEPS[628:783,39])
dfweeksocial = data.frame(WeeklyFEPS[628:783,40])
dfweeksus = data.frame(WeeklyFEPS[628:783,41])
dfweekveh = data.frame(WeeklyFEPS[628:783,42])
dfweekwhite = data.frame(WeeklyFEPS[628:783,43])

dfdataweek = data.frame(WeeklyFEPS[628:783,3])

dfweeklar = data.frame(WeeklyFEPS[628:783,15])


dfweekdom = ts(dfweekdom,start = c(2015,01,05), frequency =52)
dfweeklar = ts(dfweeklar,start = c(2015,01,05), frequency =52)
dfweeknon = ts(dfweeknon,start = c(2015,01,05), frequency =52)
dfweekoff = ts(dfweekoff,start = c(2015,01,05), frequency =52)
dfweekother = ts(dfweekother,start = c(2015,01,05), frequency =52)
dfweekprop = ts(dfweekprop,start = c(2015,01,05), frequency =52)
dfweeksocial = ts(dfweeksocial,start = c(2015,01,05), frequency =52)
dfweeksus = ts(dfweeksus,start = c(2015,01,05), frequency =52)
dfweekveh = ts(dfweekveh,start = c(2015,01,05), frequency =52)
dfweekwhite = ts(dfweekwhite,start = c(2015,01,05), frequency =52)
dfdataweek = ts(dfdataweek,start = c(2015,01,05), frequency =52)


dataweek = dfweeklar

# ARIMA MODEL

require(forecast)
library(forecast)

plot(dataweek, xlab='Weeks', ylab = 'Incidents')
plot(diff(dataweek),ylab='Incidents')
ARIMAfit = auto.arima(dataweek, lambda = 0)
summary(ARIMAfit)
fcst <- forecast(ARIMAfit, h = 52)

fcst

# ARIMA MODEL TRAIN vs. TEST 12 Weeks Back


require(forecast)
library(forecast)
datadailytrain = dataweek[c(1:144),]
datadailymodel = ts(datadailytrain,start = c(2015,01,05), frequency =52)
datadailyfinal <- auto.arima(datadailymodel)
fcst <- forecast(datadailyfinal, h = 52)
plot(fcst)
datadaily
accuracy(f=fcst,x=dataweek[145:156])






# TBATS  MODEL 
fit_tbats <- tbats(dataweek) 
fit <- forecast(fit_tbats, h=52) 
summary(fit)
plot(fit, PI  = FALSE)
fit

# TBATS  MODEL TRAIN vs. TEST 12 Weeks Back
require(forecast)
library(forecast)
datadailytrain = dataweek[c(1:144),]
datadailymodel = ts(datadailytrain,start = c(2015,01,05), frequency =52)
datadailyfinal <- tbats(datadailymodel)
fcst <- forecast(datadailyfinal, h = 52)
plot(fcst)
datadaily
accuracy(f=fcst,x=dataweek[145:156])


# NNET  MODEL 
fit <- nnetar(dataweek, decay=0.5, maxit=1000)
fcst = (forecast(fit, h = 52))
fit['residuals']
plot(fcst, level = FALSE)
fcst


# NNET  MODEL TRAIN vs. TEST 12 Weeks Back
require(forecast)
library(forecast)
datadailytrain = dataweek[c(1:144),]
datadailymodel = ts(datadailytrain,start = c(2015,01,05), frequency =52)
datadailyfinal <- nnetar(datadailymodel)
fcst <- forecast(datadailyfinal, h = 52)
plot(fcst)
datadaily
accuracy(f=fcst,x=dataweek[145:156])




# Plot Daily 
plot(datadaily, xlab='Days', ylab = 'Incidents')
plot(datadailydrug, xlab='Days', ylab = 'Incidents')
plot(datacalls, xlab='Days', ylab = 'calls')
#Plot Weekly 
plot(dataweekly, xlab='Weeks', ylab = 'Incidents')

plot(dataweeklydrug5year, xlab='Weeks', ylab = 'Incidents')
plot(dfweekNorthern, xlab='Weeks', ylab = 'Incidents')

plot(dataweekly, xlab='Weeks', ylab = 'Incidents')
plot(dataweeklydruglog)
plot(dataweeklydrugsma, xlab='Weeks', ylab = 'Incidents')

#Plot Monthly 
#plot(datamonthly, xlab='Months', ylab = 'FEPS')


# Plot 1st order differnecing Daily 
plot(diff(datadaily),ylab='Incidents')
plot(diff(datadailydrug),ylab='Incidents')
plot(diff(datacalls),ylab='calls')
# Plot 1st order differnecing Weekly 
plot(diff(dataweekly),ylab='Incidents')
plot(diff(dataweeklydrug),ylab='Incidents')
plot(diff(dataweeklydrug5year),ylab='Incidents')

plot(diff(dfweekNorthern),ylab='Incidents')
# Plot 1st order differnecing Monthly
#plot(diff(datamonthly),ylab='FEPS')


# log smoothing
dataweeklydruglog<- as.data.frame(log10(dataweeklydrug))
dataweeklydrugsma<- as.data.frame(SMA(dataweeklydrug, n = 5))
head(dataweeklydruglog)

# Plot Stationary data through Log transform Daily 
plot(log10(datadaily),ylab='Log (Incidents)')
plot(log10(datadailydrug),ylab='Log (Incidents)')
# Plot Stationary data through Log transform Weekly
plot(log10(dataweekly),ylab='Log (Incidents)')
plot(log10(dataweeklydrug5year),ylab='Log (Incidents)')
# Plot Stationary data through Log transform 
#plot(log10(datamonthly),ylab='Log (FEPS)')


# check ACF Daily
par(mfrow = c(1,2))
acf(ts(diff((datadaily))),main='ACF')
pacf(ts(diff((datadaily))),main='PACF')

acf(ts(diff((datadailydrug))),main='ACF')
pacf(ts(diff((datadailydrug))),main='PACF')

# check ACF Weekly
par(mfrow = c(1,2))
acf(ts(diff((dataweekly))),main='ACF')
pacf(ts(diff((dataweekly))),main='PACF')

acf(ts(diff((dfweekNorthern))),main='ACF')
pacf(ts(diff((dfweekNorthern))),main='PACF')


# Model Daily
require(forecast)
library(forecast)
ARIMAfit = auto.arima((datadaily), approximation=FALSE,trace=FALSE)
summary(ARIMAfit)

require(forecast)
ARIMAfit2 = auto.arima((dataweekly), approximation=FALSE,trace=FALSE)
summary(ARIMAfit2)


par(mfrow = c(1,1))
pred = predict(ARIMAfit, n.ahead = 365)
pred
plot(data2,type='l',xlim=c(2016,2020),ylim=c(1,2000000),xlab = 'Year',ylab = 'FEPS')
lines(10^(pred$pred),col='blue')
lines(10^(pred$pred+2*pred$se),col='orange')
lines(10^(pred$pred-2*pred$se),col='orange')



par(mfrow = c(1,1))
pred2 = predict(ARIMAfit2, n.ahead = 52)
pred2
plot(data2,type='l',xlim=c(2016,2020),ylim=c(1,2000000),xlab = 'Year',ylab = 'FEPS')
lines(10^(pred2$pred2),col='blue')
lines(10^(pred2$pred2+2*pred2$se),col='orange')
lines(10^(pred2$pred2-2*pred2$se),col='orange')

future = forecast(ARIMAfit, h = 365)

plot(future)
par(mfrow = c(1,1))


future2 = forecast(ARIMAfit2, h = 52)

plot(future2)
par(mfrow = c(1,1))

# DAILY Train against test 1 Quarter back
require(forecast)
library(forecast)
datadailytrain = datadaily[c(1:1000),]
datadailymodel = ts(datadailytrain,start = c(2016,1,1), frequency =365.25)
datadailyfinal <- auto.arima(datadailymodel)
fcst <- forecast(datadailyfinal, h = 96)
plot(fcst)
datadaily
accuracy(f=fcst,x=datadaily[1001:1096])

# DAILY Train againist whole set and predict
ARIMAfitdaily = auto.arima((datadaily), approximation=FALSE,trace=FALSE)
summary(ARIMAfitdaily)
fcst <- forecast(ARIMAfitdaily, h = 365.25)
plot(fcst)

ARIMAfitdailycalls = auto.arima((datacalls), approximation=FALSE,trace=FALSE)
summary(ARIMAfitdailycalls)
fcst <- forecast(ARIMAfitdailycalls, h = 365.25)
plot(fcst)

pred = predict(ARIMAfitdaily, n.ahead = 365 )
pred 

ARIMAfitdailydrug = auto.arima((datadailydrug), approximation=FALSE,trace=FALSE)
summary(ARIMAfitdailydrug)
fcst <- forecast(ARIMAfitdailydrug, h = 365.25)
plot(fcst)

pred = predict(ARIMAfitdaily, n.ahead = 365 )
pred 



# Weekly Train against test 1 Quarter back
require(forecast)
library(forecast)
dataweeklytrain = dataweekly[c(1:148),]
dataweeklytrain
dataweeklymodel = ts(dataweeklytrain,start = c(2016,1,3), frequency =52)
dataweeklyfinal <- auto.arima(dataweeklymodel, approximation=FALSE,trace=FALSE)
fcst <- forecast(dataweeklyfinal, h = 52)
plot(fcst)
dataweekly
accuracy(f=fcst,x=dataweekly[149:157])

# Weekly  Train againist whole set and predict
ARIMAfitweekly = auto.arima((dataweekly), approximation=FALSE,trace=FALSE)
summary(ARIMAfitweekly)
fcst <- forecast(ARIMAfitweekly, h = 52)
plot(fcst)

# Weekly  Train againist whole set and predict
ARIMAfitweekly = auto.arima((dataweeklydrug), approximation=FALSE,trace=FALSE)
summary(ARIMAfitweekly)
fcst <- forecast(ARIMAfitweeklydrug, h = 52)
plot(fcst)



pred = predict(ARIMAfitweekly, n.ahead = 52)
pred 

library( 'TStools')
if (!require("devtools"))
  install.packages("devtools")
devtools::install_github("trnnick/TStools"7)



