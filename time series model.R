rm(list=ls())
setwd("/Users/gaibaiting/Desktop/competitive-data-science-predict-future-sales")
sales <- read.csv("data_sales.csv")
sales = sales[c(2,7)]
sales$date
str(sales)
sales$date = as.Date(sales$date, "%Y-%m-%d" )
library(dplyr)

##########daily sales############
#group by date
sales_bydate <- sales %>% 
  group_by(date) %>% 
  summarize_all(sum)
#time series type
dailysales=ts(sales_bydate$item_cnt_day, start=c(2013,1), frequency=365)
ts.plot(dailysales)
ts.plot(diff(dailysales))
#stationary test
library(tseries)
library(forecast)
# is stationary
adf.test(dailysales, alternative="stationary")
#acf and pacf
acf(dailysales)
pacf(dailysales)

#seasonal deconposition
# multiplicative decomposition
plot(decompose(dailysales, type="mult"))
# additive decomposition
plot(decompose(dailysales, type="additive"))

#de-trend
new_dailysales = diff(dailysales)
plot(new_dailysales)
#de-seasonalization
new2_dailysales = diff(dailysales, 30)
plot(new2_dailysales)


#stlf
model0 = stl(dailysales,s.window = "periodic")
pred0 = forecast(model0,h=304)
summary(pred0)
objects(pred0)
plot(pred0)

#stlf
model1 = stlf(dailysales)
pred1 = forecast(model1,h=304)
summary(pred1)
objects(pred1)
plot(pred1)

##auto arima
model2 = auto.arima(dailysales)
pred2 = forecast(model2, h=304)
summary(pred2)
checkresiduals(model2)
objects(pred2)
plot(pred2)
acf(resid(model2))