
library(dplyr)
library(prophet)

data<-read.csv('C:/Users/15189/Desktop/MGMT6160/final/data_sales.csv')

a<-data[which(data$shop_id==59),]
b<-a[which(a$item_id==5587),]


daily <- group_by(data, date)

daily_sale <- summarise(daily,sum_sales = sum(item_cnt_day))     

index<-which(daily_sale$date=='2014-12-31')

train<-daily_sale[1:index,]
test<-daily_sale[(index+1):1034,]

rmse<-function(actual, predict)
  round((sum((actual-predict)^2)/length(actual))^.5,2)
daily_train<-mutate(train, ds=date,y=sum_sales)
daily_sale<-mutate(daily_sale, ds=date,y=sum_sales)
#model
prophet_use<-prophet(daily_sale)
#prophet_none
prophet_train_none<-prophet(daily_train)
future_none<-make_future_dataframe(prophet_train_none,periods = 304)
forecast_train_none<-predict(prophet_train_none,future_none)

plot(prophet_train_none, forecast_train_none)

#rmse
prophet_rmse_none<-rmse(daily_sale$sum_sales,forecast_train_none$trend)


#prophet-seasonality
prophet_train<-prophet(daily_train, yearly.seasonality = TRUE, weekly.seasonality= TRUE, daily.seasonality = TRUE)
future<-make_future_dataframe(prophet_train,periods = 304)
forecast_train<-predict(prophet_train,future)

plot(prophet_train, forecast_train)
plot(prophet_use, forecast_train)
#rmse
prophet_rmse_seasonality<-rmse(daily_sale$sum_sales,forecast_train$trend)


