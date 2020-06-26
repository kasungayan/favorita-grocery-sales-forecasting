# Favorita-grocery-sales-forecasting.
# Eror calculation script.
# Kasun Bandara, June 2020


# Define the generated forecasts and actual values file name.
generated_forecast <- "GENERATED_FORECAST_FILE_NAME"
actual_forecast <- "ACTUA_FILE_NAME"

df_forecast <-
  read.csv(generated_forecast, sep = ",", header = FALSE)
df_forecast <- df_forecast[1:528, ]

# Define the actual test file name.
df_actual <- read.csv(actual_forecast, sep = ",", header = FALSE)

# Get the perishable information.
df_perishable <-
  read.csv("Sales_test_final.txt", sep = ",", header = TRUE)

actual_df <- matrix(NA, ncol = 28, nrow = 528)
forecast_df <- matrix(NA, ncol = 28, nrow = 528)

weightError = list()
weights = list()

for (idr in 1:528) {
  actual_predictions <- as.numeric(df_actual[idr,])
  model_predictions <- as.numeric(df_forecast[idr,])
  
  forecast_df[idr, ] <- model_predictions
  actual_df[idr, ] <- actual_predictions
  
  perishable <- as.numeric(df_perishable$perishable)[idr]
  if (perishable == 1) {
    weight <- 1.25
  } else{
    weight <- 1.00
  }
  error <-
    log(model_predictions + 1, base = exp(1)) - log(actual_predictions + 1, base = exp(1))
  weighted_error <-  weight * (error ^ 2)
  weightError[[idr]] <- sum(weighted_error)
  weights[[idr]] <- sum(rep(weight, 28))
}


NWRMSLE_error <- sqrt(Reduce("+", weightError) / Reduce("+", weights))

# Printing NWRMSLE_error
print(NWRMSLE_error)

epsilon = 0.1
sum = NULL
comparator = data.frame(matrix((0.5 + epsilon),
                               nrow = nrow(df_actual),
                               ncol = ncol(df_actual)
))
sum = pmax(comparator, (abs(df_forecast) + abs(df_actual) + epsilon))
time_series_wise_SMAPE <- 2 * abs(df_forecast - df_actual) / (sum)
SMAPEPerSeries <- rowMeans(time_series_wise_SMAPE, na.rm = TRUE)

# Printing SMAPE_error
print(mean(SMAPEPerSeries))