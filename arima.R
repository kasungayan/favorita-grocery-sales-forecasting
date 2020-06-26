# Favorita-grocery-sales-forecasting
# ARIMAX Forecasting script
# Kasun Bandara, June 2020

# @input: train_ts.txt, holiday_modified_train.txt, holiday_modified_test.txt
# @output: ARIMAX-forecast.txt


# Please uncomment the below command, in case you haven't installed the following pakcage in your enviornment.
# install.packages("forecast")

# Loading the required libraries.
# (https://cran.r-project.org/web/packages/forecast/forecast.pdf)
require(forecast)
library(forecast)

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

# For loop that iterates through the itemids to generate forecasts.
for (i in 1:nrow(df_train)) {
  # Declaring training external variables for calender variables
  weekday_train <-
    as.numeric(df_calander_train$dayweek)[1:train_length]
  month_train <- as.numeric(df_calander_train$month)[1:train_length]
  dateofMonth_train <-
    as.numeric(df_calander_train$daymonth)[1:train_length]
  holiday_type_train <-
    as.numeric(df_calander_train$type)[1:train_length]
  holiday_type_description <-
    as.numeric(df_calander_train$description)[1:train_length]
  
  # Declaring testing external variables for calender variables
  weekday_test <-
    as.numeric(df_calander_test$dayweek)[1:test_length]
  month_test <- as.numeric(df_calander_test$month)[1:test_length]
  dateofMonth_test <-
    as.numeric(df_calander_test$daymonth)[1:test_length]
  holiday_type_test <-
    as.numeric(df_calander_test$type)[1:test_length]
  holiday_type_description_test <-
    as.numeric(df_calander_test$description)[1:test_length]
  
  
  item_df <- as.numeric(df_train[i, ])
  arima_ts = ts(item_df)
  
  # Using try catch in case auto.arima fails in the ARIMAX setting.
  tryCatch({
    fit <- auto.arima(
      arima_ts,
      xreg = cbind(
        weekday_train,
        month_train,
        dateofMonth_train,
        holiday_type_train,
        holiday_type_description
      ),
      seasonal = FALSE
    )
    weekday_train <- weekday_test
    month_train <- month_test
    dateofMonth_train <- dateofMonth_test
    holiday_type_train <- holiday_type_test
    holiday_type_description <- holiday_type_description_test
    
    arima_forecast = forecast(
      fit,
      xreg = cbind(
        weekday_train,
        month_train,
        dateofMonth_train,
        holiday_type_train,
        holiday_type_description
      ),
      h = 28
    )
    arima_forecast_mean <- as.numeric(arima_forecast$mean)
    arima_forecast_mean[arima_forecast_mean < 0] <- 0
  }, error = function (e) {
    arima_forecast_mean <- rep(0, 28)
  })
  forecast_df[i,] <- arima_forecast_mean
  
}

# Writing the results to file
write.table(
  forecast_df,
  "ARIMAX-Forecasts.txt",
  row.names = FALSE,
  col.names = FALSE,
  sep = ","
)