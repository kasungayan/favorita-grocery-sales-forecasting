# Favorita-grocery-sales-forecasting
# ETS Forecasting script
# Kasun Bandara, June 2020

# @input: train_ts.txt
# @output: ETS-forecasts.txt


# Please uncomment the below command, in case you haven't installed the following pakcage in your enviornment.
# install.packages("forecast")

# Loading the required libraries.
# (https://cran.r-project.org/web/packages/forecast/forecast.pdf)
require(forecast)
library(forecast)

# Setting the seed
set.seed(1234)

# Loading the dataset and the target test dataset.
df_train <- read.csv("train_ts.txt", sep = ",", header = FALSE)

forecast_df <- matrix(NA, ncol = 28, nrow = 528)

# For loop that iterates through the itemids to generate forecasts.
for (idr in 1:nrow(df_train)) {
  timeseries <- as.numeric(df_train[idr, ])
  timeseries <- log(timeseries + 1)
  timeseries <- ts(timeseries, frequency = 7)
  timeseries_model <- ets(timeseries)
  timeseries_forecast <- forecast(timeseries_model, h = 28)
  
  timeseries_forecast_final <- exp(timeseries_forecast$mean) - 1
  timeseries_forecast_final <- as.numeric(timeseries_forecast_final)
  timeseries_forecast_final[timeseries_forecast_final < 0] <- 0
  
  forecast_df[idr, ] <- timeseries_forecast_final
}

# Writing the results to file
write.table(
  forecast_df,
  "ETS-forecasts.txt",
  sep = ",",
  col.names = FALSE,
  row.names = FALSE
)