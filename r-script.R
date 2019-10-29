#libraries
library(ggplot2)
library(dplyr)
library(readr)
library(Rcpp)
library(rlang)
library(prophet)
library(forecast)

names(NBA_TICKETS_DAILY) <- c('ds','y')

getwd()

#Manipulation/Cleaning Data
#correlation between installs and tickets

tail(nba)

nbainstalls <- read_csv("NBA2K20 Tickets Project/nba.csv")
dau <- read_csv("NBA2K20 Tickets Project/dau.csv")

#merge dau + installs
nba <- merge.data.frame(NBA_TICKETS_DAILY, nbainstalls, by.x = "ds", by.y = "INSTALLDATE")
nba <- merge.data.frame(nba, dau, by.x = "ds", by.y = "LOGINDATE")

#EDA
qplot(nba$y, nba$`SUM(PLAYERCOUNT)`, main = "Tickets vs Installs")
qplot(nba$y, nba$DAU, main = "Tickets vs DAU")
qplot(ds,y, data = nba, main = "TS: Tickets")

cor.test(nba$y, log(nba$`SUM(PLAYERCOUNT)`), method = "pearson")
cor.test(nba$y, log(nba$DAU), method = "pearson", conf.level = 0.99)
cor.test(nba$`SUM(PLAYERCOUNT)`, log(nba$DAU), method = "pearson")

#outliers

nba %>%
  filter(y >= 15000)

#2017-09-15,16,17,18 -> NBA 2K18 launcher crashes, won't launch leads to high bugs
#2018-09-07,13,29 -> NBA 2K19 My Career Glitch: 
#VC loss, stuck in never-ending game, custom characters disappearing
#2018-12-27 -> NBA2K19 NEW VC GLITCH After Patch 7



#1st Model: Only Tickets


df1 <- NBA_TICKETS_DAILY %>%
  filter(ds >= '2016-09-16')



model <- prophet(df1, daily.seasonality = FALSE, yearly.seasonality = TRUE, weekly.seasonality = TRUE, 
                 seasonality.mode = "multiplicative")

#prediction

future <- data.frame(df1$ds)
names(future) <- 'ds'
tail(future)

forecast <- predict(model, future)


#plot forecast

plot(model, forecast %>%
              filter(ds <= '2020-09-01'), xlabel = "Date", ylabel = "Tickets", col.axis = "blue")

prophet_plot_components(model, forecast)

diagnos <- data.frame(rmse = rmse(forecast$yhat, df1$y),
                            mape = mape(forecast$yhat, df1$y),
                            mae = mae(forecast$yhat, df1$y))

print(diagnos)
#2nd Model: Tickets + Installs + DAU + Launch


df <- data.frame(nba %>%
  select(ds, y, `SUM(PLAYERCOUNT)`, DAU))

names(df) <- c('ds','y','installs','dau')

head(df)

#adding launch regressor

launch <- function(ds) {
  dates <- as.Date(ds)
  month <- as.numeric(format(dates, '%m'))
  as.numeric(months(dates) == "September")
}
df$launch <- launch(df$ds)

#model with regressors

model <- prophet(daily.seasonality = TRUE, yearly.seasonality = TRUE, weekly.seasonality = TRUE, 
              seasonality.mode = "additive")
model <- add_regressor(model, name = 'installs')
model <- add_regressor(model, name = 'dau')
model <- add_regressor(model, name = 'launch')
model <- add_country_holidays(model, country_name = 'US')
model <- fit.prophet(model, df)

model$extra_regressors

tail(df)

#train

train <- data.frame(ds = df$ds, 
                      installs = df$installs, 
                      launch = df$launch,
                      dau = df$dau)
train <- data.frame(ds = df$ds,
                    launch = df$launch)
train_forecast <- predict(model, train)

plot(model, train_forecast %>%
       filter(ds <= '2020-09-01', ds >= '2016-09-01'))

prophet_plot_components(model, train_forecast)


#validation
library(Metrics)

train_metrics <- data.frame(rmse = rmse(train_forecast$yhat, df$y),
                            mape = mape(train_forecast$yhat, df$y),
                            mae = mae(train_forecast$yhat, df$y))

print(train_metrics)


#forecast

#installs assume 15% growth YoY, 
#dau assumes 10% growth YoY


installs_forecast <- read_csv("NBA2K20 Tickets Project/installs_forecast.csv")
dau_forecast <- read_csv("NBA2K20 Tickets Project/dau_forecast.csv")
head(test)
tail(test)
test <- make_future_dataframe(model, periods = 415)
test$launch <- launch(test$ds)
test$installs <- installs_forecast$INSTALLS
test$dau <- dau_forecast$DAU

test_forecast <- predict(model, test)


plot(model, test_forecast %>%
       filter(ds <= '2020-09-03', ds >= '2016-09-16'))

prophet_plot_components(model, test_forecast)


#save

excel <- test_forecast %>%
  select(ds, yhat, yhat_lower, yhat_upper) %>%
  filter(ds >= '2019-09-05',ds <= '2020-09-03')
names(excel) <- c('date','tickets','lower CI', 'upper CI')
write.csv(excel,'NBA2K20 Tickets Project/nba2k20_tickets.csv')

  
