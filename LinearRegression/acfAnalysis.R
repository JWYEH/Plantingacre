devtools::load_all("../BPI")
pkg <- c("data.table", "dplyr", "ggplot2", "kableExtra", "tseries", "forecast")
lapply(pkg, library, character.only = T)


acre <- fread("../data/raw/acreplanted_national_corn_nass_1926-2020.csv")
price <- fread("../data/raw/price_soy_corn_doe_1866-2019.csv")
acre_corn <- merge(acre, price, all.x = T)
acre_corn <- acre_corn[Year!=2020, c("Year", "CORN - ACRES PLANTED  -  <b>VALUE</b>", "corn_adjusted", "soy_adjusted")]
names(acre_corn) <- c("year", "acre_corn", "price_corn", "price_soy")
acre_corn[, acre_corn:=rm_comma(acre_corn)] %>% glimpse()



scaler = 0.3*10E6
ggplot(acre_corn, aes(x= year)) +
  geom_col(aes(y = acre_corn),  fill = "gray70", color = "gray90") +
  geom_line(aes(y = acre_corn, color = "acre corn"), size = 1) +
  geom_line(aes(y = price_corn*scaler,color = "price corn"), size = 1) +
  geom_line(aes(y = price_soy*scaler,color = "price soy"),  size = 1) +
  scale_y_continuous(name = "acre planted (acre)",sec.axis = sec_axis(~. /scaler, name = "USD/BU adjusted by CPI"))



acre_ts <- ts(acre_corn$acre_corn, start = min(acre_corn$year))
plot(acre_ts)
par(mfrow=c(1,2))
acf(acre_ts, lag.max=20, main = NULL)
pacf(acre_ts, lag.max=20, main = NULL)
par(mfrow=c(1,1))




summary(urca::ur.df(acre_ts, type = "none", lags= 0))
adf.test(acre_ts)
auto.arima(acre_ts)



acre_ts_df1 <- diff(acre_ts)
auto.arima(acre_ts_df1)
adf.test(acre_ts_df1)


plot.ts(acre_ts_df1)
par(mfrow=c(1,2))
acf(acre_ts_df1, lag.max=20, main = NULL)
pacf(acre_ts_df1, lag.max=20, main = NULL)
par(mfrow=c(1,1))


MASS::boxcox((acre_ts_df1-min(acre_ts_df1) +1E-2) ~ 1, lambda = seq(-2, 2, .1), plotit = T)




