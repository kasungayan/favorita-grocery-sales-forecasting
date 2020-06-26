# Favorita-grocery-sales-forecasting
# Prophet Forecasting script
# Kasun Bandara, June 2020

# @input: train_ts.txt, holiday_modified_train.txt, holiday_modified_test.txt
# @output: Prophet-forecast.txt

# Please uncomment the below command, in case you haven't installed the following pakcage in your enviornment.
# install.packages("prophet")

# Loading the required libraries.
# (https://cran.r-project.org/web/packages/prophet/prophet.pdf)
require(prophet)
library(prophet)

# Setting the seed
set.seed(1234)

# Loading the required datasets
df_train <- read.csv("train_ts.txt", sep = ",", header = FALSE)
df_calander_train <-
  read.csv(
    "holiday_modified_train.txt",
    sep = ",",
    header = TRUE,
    stringsAsFactors = TRUE
  )
df_calander_test <-
  read.csv(
    "holiday_modified_test.txt",
    sep = ",",
    header = TRUE,
    stringsAsFactors = TRUE
  )

train_length <- 1660
test_length <- 28

forecast_df = matrix(nrow = 528, ncol = 28)


for (i in 1:nrow(sales_df)) {
  # Declaring training external variables for calender variables.
  weekday_train <-
    as.numeric(df_calander_train$dayweek)[1:train_length]
  month_train <- as.numeric(df_calander_train$month)[1:train_length]
  dateofMonth_train <-
    as.numeric(df_calander_train$daymonth)[1:train_length]
  holiday_type_train <-
    as.numeric(df_calander_train$type)[1:train_length]
  holiday_type_description <-
    as.numeric(df_calander_train$description)[1:train_length]
  
  # Declaring testing external variables for calender variables.
  weekday_test <-
    as.numeric(df_calander_test$dayweek)[1:test_length]
  month_test <- as.numeric(df_calander_test$month)[1:test_length]
  dateofMonth_test <-
    as.numeric(df_calander_test$daymonth)[1:test_length]
  holiday_type_test <-
    as.numeric(df_calander_test$type)[1:test_length]
  holiday_type_description_test <-
    as.numeric(df_calander_test$description)[1:test_length]
  
  ts <-
    seq(from = as.Date("2013-01-01"),
        length.out =  train_length,
        by = "day")
  item_df <- as.numeric(df_train[i, ])
  history <- tibble(ds = ts, y = item_df)
  
  history$weekday <- weekday_train
  history$monthtrain <- month_train
  history$dateofMonth <- dateofMonth_train
  history$type <- holiday_type_train
  history$description <- holiday_type_description
  
  
  m <- prophet(daily.seasonality = FALSE)
  m <- add_regressor(m, 'weekday')
  m <- add_regressor(m, 'monthtrain')
  m <- add_regressor(m, 'dateofMonth')
  m <- add_regressor(m, 'type')
  m <- add_regressor(m, 'description')
  m <- fit.prophet(m, history)
  
  
  future <-
    tibble(ds = seq(
      from = as.Date("2017-07-19") ,
      length.out = test_length,
      by = "day"
    ))
  
  future$weekday <- weekday_test
  future$monthtrain <- month_test
  future$dateofMonth <- dateofMonth_test
  future$type <- holiday_type_test
  future$description  <- holiday_type_description_test
  
  forecastProphet = predict(m, future)
  forecastProphet <- as.numeric(forecastProphet$yhat)
  forecastProphet[forecastProphet < 0] <- 0
  forecast_df[i,] <- forecastProphet
  
}

# Writing the results to file
write.table(
  forecast_df,
  "Prophet-forecasts.txt",
  sep = ",",
  col.names = FALSE,
  row.names = FALSE
)