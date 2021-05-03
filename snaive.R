# Favorita-grocery-sales-forecasting
# Naive Seasonality Forecasting script
# Kasun Bandara, June 2020

# Testing branch

# @input: train_ts.txt
# @output: SNAIVE_forecasts.txt


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

forecast_df <- matrix(NA, ncol = 28, nrow = 1660)


# For loop that iterates through the itemids to generate forecasts.
for (idr in 1:nrow(df_train)) {
  print(idr)
  timeseries <- as.numeric(df_train[idr, ])
  timeseries <- ts(timeseries, frequency = 7)
  timeseries_model <- snaive(timeseries, h = 28)
  timeseries_forecast <- timeseries_model
  
  timeseries_forecast_final <- (timeseries_forecast$mean)
  timeseries_forecast_final <- as.numeric(timeseries_forecast_final)
  timeseries_forecast_final[timeseries_forecast_final < 0] <- 0
  
  forecast_df[idr, ] <- timeseries_forecast_final
}


write.table(
  forecast_df,
  "SNAIVE_forecasts.txt",
  sep = ",",
  col.names = FALSE,
  row.names = FALSE
)