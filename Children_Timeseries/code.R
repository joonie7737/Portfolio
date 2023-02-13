##경남 영유아인구수 1년 시계열 예측## 

library(ggplot2)
library(dplyr)
library(xts)
library(tsibble)
library(anytime)
library(zoo)
library(lubridate)
library(feasts)
library(urca)
library(forecast)
library(seastests)
library(car)
library(lmtest)
library(prophet)
options(scipen = 999)

#시계열데이터는 먼저 AR(1) 모델로 회귀계수 테스트해보기(확률보행과정 체크)

data_path = 'D:\\GNI\\data\\analysis\\Ts\\src'
setwd(data_path)

#데이터 로드 및 전처리
df <- read.csv('영유아인구.csv', header = TRUE, fileEncoding = 'euc-kr')

df1 <- aggregate(df[,-c(1,2)], list(행정구역 = df$행정구역별), sum) %>% t()
colnames(df1) <- df1[1,]
df1 <- df1[-c(1),] %>% as.data.frame()

df1$Date <- row.names(df1)
df1$Date <- gsub('X','',df1$Date)
df1$Date <- anydate(paste0(df1$Date, '-01'))

head(df1)

df_ts <- ts(as.numeric(df1$경상남도), start = c(2011, 01), frequency = 12)

plot(df_ts) #Raw Data check

df_ts %>% ur.kpss() %>% summary() #Stationary test
df_ts %>% combined_test() %>% summary() #계절성 검정

df_ts %>% decompose() %>% autoplot() #decomposition

ndiffs(df_ts) #차분 횟수
nsdiffs(df_ts) #계절성 차분 횟수
ggtsdisplay(df_ts) #ACF, PACF 확인


 #모델 선택 : tslm(), arima(), auto.arima(), ets(), snaive(), rwf(), tbats()

#tslm()

ts_tslm <- tslm(df_ts ~ season + trend + I(trend^2))
ts_tslm %>% summary()
ts_tslm %>% checkresiduals()
ts_tslm %>% forecast() %>% autoplot()
accuracy(ts_tslm$fitted.values, df_ts)

#arima()
df_ts %>% diff() %>% diff() %>% ggtsdisplay()

ts_arima <- arima(df_ts, order = c(2,2,1), seasonal = list(order = c(1,0,0), period = 12))
ts_arima %>% summary()
ts_arima %>% checkresiduals()
ts_arima %>% forecast() %>% autoplot()
ts_arima_forecast <- forecast(ts_arima)
accuracy(ts_arima_forecast$fitted, df_ts)

#auto.arima()
ts_autoArima <- auto.arima(df_ts)
ts_autoArima %>% summary()
ts_autoArima %>% checkresiduals()
ts_autoArima_forecast <- forecast(ts_autoArima)
accuracy(ts_autoArima$fitted, df_ts)

#ets()
ts_ets <- ets(df_ts, model = 'MAN')
ts_ets %>% summary()
ts_ets %>% checkresiduals()

#snaive()
ts_snaive <- snaive(df_ts)
ts_snaive %>% summary()
ts_snaive %>% checkresiduals()
accuracy(ts_snaive$fitted, df_ts)

#rwf()
ts_rwf <- rwf(df_ts, drift = TRUE)
ts_rwf %>% summary()
ts_rwf %>% checkresiduals()
accuracy(ts_rwf$fitted, df_ts)
ts_rwf %>% forecast() %>% autoplot()

#tbats()
ts_tbats <- tbats(df_ts, use.trend = TRUE, seasonal.periods = 12)
ts_tbats
ts_tbats %>% checkresiduals()
accuracy(ts_tbats$fitted.values, df_ts)
ts_tbats %>% forecast() %>% autoplot()



 #모델별 교차검증 실시(tsCV())
 #모델 선택 : tslm(), arima(), auto.arima(), ets(), snaive(), rwf(), tbats()

#tslm()
tslm_tscv <- function(x, h){
  tslm(x ~ season + trend + I(trend^2)) %>% forecast(h = h)
}

tslm_tscv_residual <- tsCV(df_ts, tslm_tscv, h = 12)
tslm_tscv_residual_rmse <- (mean(tslm_tscv_residual^2, na.rm = T))^0.5

tslm_tscv_residual


#arima()
arima_tscv <- function(x, h){
  arima(x, order = c(2,2,1), seasonal = list(order = c(1,0,0), period = 12)) %>% forecast(h = h)
}

arima_tscv_residual <- tsCV(df_ts, arima_tscv, h = 12)
arima_tscv_residual_rmse <- (mean(arima_tscv_residual^2, na.rm = T))^0.5

arima_tscv_residual

#auto.arima()
autoArima_tscv <- function(x, h){
  auto.arima(x) %>% forecast(h = h)
}

autoArima_tscv_residual <- tsCV(df_ts, autoArima_tscv, h = 12)
autoArima_tscv_residual_rmse <- (mean(autoArima_tscv_residual^2, na.rm = T))^0.5

autoArima_tscv_residual

#ets()
ets_tscv <- function(x, h){
  ets(x, model = 'MAN') %>% forecast(h = h)
}

ets_tscv_residual <- tsCV(df_ts, ets_tscv, h = 12)
ets_tscv_residual_rmse <- (mean(ets_tscv_residual^2, na.rm = T))^0.5

ets_tscv_residual

#snaive()
snaive_tscv <- function(x, h){
  snaive(x, h = h) %>% forecast(h = h)
}

snaive_tscv_residual <- tsCV(df_ts, snaive_tscv, h = 12)
snaive_tscv_residual_rmse <- (mean(snaive_tscv_residual^2, na.rm = T))^0.5

snaive_tscv_residual

#rwf()
rwf_tscv <- function(x, h){
  rwf(x, drift = TRUE, h = h) %>% forecast(h = h)
}

rwf_tscv_residual <- tsCV(df_ts, rwf_tscv, h = 12)
rwf_tscv_residual_rmse <- (mean(rwf_tscv_residual^2, na.rm = T))^0.5

rwf_tscv_residual

#tbats()
tbats_tscv <- function(x, h){
  tbats(x, use.trend = TRUE, seasonal.periods = 12) %>% forecast(h = h)
}

tbats_tscv_residual <- tsCV(df_ts, tbats_tscv, h = 12)
tbats_tscv_residual_rmse <- (mean(tbats_tscv_residual^2, na.rm = T))^0.5

tbats_tscv_residual



###BoxCox 적용####

" " "
Even easier, use the built-in transformations in the package which handles it all for you, like this:

model <- Arima(data, order = c(3,1,4), lambda = lambda)
modelforecast <- forecast(model, h=10)
The resulting forecasts will be back-transformed as required.
" " "

(lambda = BoxCox.lambda(df_ts, method = 'loglik')) #최적 lambda 결정


#tslm()
tslm_tscv_box <- function(x, h){
  tslm(x ~ season + trend + I(trend^2), lambda = 2) %>% forecast(h = h, biasadj = TRUE)
}

tslm_tscv_residual_box <- tsCV(df_ts, tslm_tscv_box, h = 12)
tslm_tscv_residual_rmse_box <- (mean(tslm_tscv_residual_box^2, na.rm = T))^0.5

#arima()
arima_tscv_box <- function(x, h){
  Arima(x, order = c(2,2,1), seasonal = list(order = c(1,0,0), period = 12), lambda = 2) %>% forecast(h = h, biasadj = TRUE)
}

arima_tscv_residual_box <- tsCV(df_ts, arima_tscv_box, h = 12)
arima_tscv_residual_rmse_box <- (mean(arima_tscv_residual_box^2, na.rm = T))^0.5

#ets()
ets_tscv_box <- function(x, h){
  ets(x, lambda = 2) %>% forecast(h = h, biasadj = TRUE)
}

ets_tscv_residual_box <- tsCV(df_ts, ets_tscv_box, h = 12)
ets_tscv_residual_rmse_box <- (mean(ets_tscv_residual_box^2, na.rm = T))^0.5

ets_tscv_residual_box

#snaive()
snaive_tscv_box <- function(x, h){
  snaive(x, h = h, lambda = 2) %>% forecast(h = h, biasadj = TRUE)
}

snaive_tscv_residual_box <- tsCV(df_ts, snaive_tscv_box, h = 12)
snaive_tscv_residual_rmse_box <- (mean(snaive_tscv_residual_box^2, na.rm = T))^0.5


#rwf()
rwf_tscv_box <- function(x, h){
  rwf(x, drift = TRUE, h = h, lambda = 2) %>% forecast(h = h, biasadj = TRUE)
}

rwf_tscv_residual_box <- tsCV(df_ts, rwf_tscv_box, h = 12)
rwf_tscv_residual_rmse_box <- (mean(rwf_tscv_residual_box^2, na.rm = T))^0.5


#tbats()
tbats_tscv_box <- function(x, h){
  tbats(x, use.trend = TRUE, seasonal.periods = 12, use.box.cox = TRUE) %>% forecast(h = h, biasadj = TRUE)
}

tbats_tscv_residual_box <- tsCV(df_ts, tbats_tscv_box, h = 12)
tbats_tscv_residual_rmse_box <- (mean(tbats_tscv_residual_box^2, na.rm = T))^0.5



##최종 모델 결정(RMSE 기준)##
data.frame(tslm = tslm_tscv_residual_rmse,
           tslm_box = tslm_tscv_residual_rmse_box,
           arima = arima_tscv_residual_rmse,
           arima_box = arima_tscv_residual_rmse_box,
           autoArima = autoArima_tscv_residual_rmse,
           ets = ets_tscv_residual_rmse,
           ets_box = ets_tscv_residual_rmse_box,
           snaive = snaive_tscv_residual_rmse,
           snaive_box = snaive_tscv_residual_rmse_box,
           rwf = rwf_tscv_residual_rmse,
           rwf_box = rwf_tscv_residual_rmse_box,
           tbats = tbats_tscv_residual_rmse,
           tbats_box = tbats_tscv_residual_rmse_box) #tbats

 ###최종 모델(TBATS)###
df_ts %>% tbats(use.trend = TRUE, seasonal.periods = 12, use.box.cox = TRUE) %>% 
  forecast(h = 12, biasadj = TRUE) %>% autoplot() 

  #모델 해석 : 2023년 9월쯤에는 영유아인구가 10만명 주변으로 될 것이라고 예상한다.




