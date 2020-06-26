# Favorita-grocery-sales-forecasting
# Pooled Regression script
# Kasun Bandara, June 2020


# @input: Sales_train_final.txt, holiday_modified_all.txt
# @output: PooledRegression_forecasts.txt

# Loading the required libraries.
require(stringr)
require(dplyr)

library(stringr)
library(dplyr)


# Sourcing the pooled regression fitting method.
source("./pooled_regression_base.R")

# Setting the seed
set.seed(1234)

horizon = 28

# sales data
all_sales_data = read.csv("Sales_train_final.txt", sep = ",", header = TRUE)

# calendar data
all_calendar_data = read.csv("holiday_modified_all.txt",
                             sep = ",",
                             header = TRUE)

calendar_data_train = all_calendar_data[1:1660, ]
calendar_data_test = all_calendar_data[1661:1688, ]

# Defining the number of time lags and meta data columns.
lag = 35
meta_data_columns <- 9

# Defining the output file
output_file_name = "PooledRegression_forecasts.txt"

unlink(output_file_name)

model_results = NULL

sales_data = all_sales_data[, (meta_data_column + 1):ncol(all_sales_data)]
metda_data = all_sales_data[, 1:meta_data_column]

# convert sales data to matrix
sales_data <- as.matrix(sales_data)

# normalize sales data by the mean
series_means = rowMeans(sales_data, na.rm = TRUE)
sales_data_mean_normalized = sales_data / series_means

# convert sales data to list of vectors
sales_data_mean_normalized.list <-
  split(sales_data_mean_normalized, seq(nrow(sales_data_mean_normalized)))

# perform embedding on the list elements
embedded_sales_data.list <-
  lapply(sales_data_mean_normalized.list, embed, dimension = lag + 1)

no_of_rows_in_each_series = ncol(all_sales_data) - meta_data_column - lag
meta_data_embedded = metda_data[rep(seq_len(nrow(metda_data)), each = no_of_rows_in_each_series),]

# create one matrix and combine the metadata
embedded_sales_data = do.call(rbind, embedded_sales_data.list)
embedded_data = cbind(meta_data_embedded, embedded_sales_data)

# combine the calendar data
calendar_data_train_embedded = calendar_data_train[(lag + 1):nrow(calendar_data_train), ]
embedded_data = cbind(embedded_data, calendar_data_train_embedded)

# remove the na data
#embedded_data_clean = na.omit(embedded_data)

# combine the price data
#final_embedded_train_data = inner_join(embedded_data_clean, all_price_data)
final_embedded_train_data = embedded_data

# create the series means vector
modified_no_of_rows_in_each_series = final_embedded_train_data %>% group_by(item_id, store_id) %>% count() %>% ungroup() %>% pull(n)
series_means_vector = as.numeric(rep(series_means, modified_no_of_rows_in_each_series))

# drop unwanted columns
to_drop = c("date", "transferred")
final_embedded_train_data = final_embedded_train_data[,!(names(final_embedded_train_data) %in% to_drop)]

# convert to correct data types
final_embedded_train_data$dayweek = factor(final_embedded_train_data$dayweek)
final_embedded_train_data$month = factor(final_embedded_train_data$month)
final_embedded_train_data$daymonth = factor(final_embedded_train_data$daymonth)
final_embedded_train_data$type = factor(final_embedded_train_data$type)
final_embedded_train_data$description = factor(final_embedded_train_data$description)

final_embedded_train_data$item_id = factor(final_embedded_train_data$item_id)
final_embedded_train_data$store_id = factor(final_embedded_train_data$store_id)
final_embedded_train_data$family = factor(final_embedded_train_data$family)
final_embedded_train_data$class = factor(final_embedded_train_data$class)
final_embedded_train_data$perishable = factor(final_embedded_train_data$perishable)

final_embedded_train_data$city = factor(final_embedded_train_data$city)
final_embedded_train_data$state = factor(final_embedded_train_data$state)
final_embedded_train_data$state_type = factor(final_embedded_train_data$state_type)
final_embedded_train_data$cluster = factor(final_embedded_train_data$cluster)

# fit a normal model
colnames(final_embedded_train_data)[which(names(final_embedded_train_data) == "1")] <-
  "y"
for (i in 2:(lag + 1)) {
  colnames(final_embedded_train_data)[which(names(final_embedded_train_data) == toString(i))] <-
    paste("Lag", (i - 1), sep = "")
}

model = fit_normal_model(fitting_data = final_embedded_train_data)

# create the final test data
test_data = final_embedded_train_data %>% group_by(item_id, store_id) %>% slice(n()) %>% ungroup()
test_data = test_data[,!(names(test_data) %in% c(paste0("Lag", lag)))]

# recursively predict for all the series until the forecast horizon
predictions = NULL
for (i in 1:horizon) {
  print(paste0("horizon: ", i))
  for (j in (lag - 1):1) {
    name = paste("Lag", j, sep = "")
    colnames(test_data)[which(names(test_data) == name)] <-
      paste("Lag", (j + 1), sep = "")
  }
  if (i == 1) {
    colnames(test_data)[which(names(test_data) == "y")] <-
      paste("Lag", 1, sep = "")
  } else{
    colnames(test_data)[which(names(test_data) == "new_predictions")] <-
      paste("Lag", 1, sep = "")
  }
  
  # create the new external calendar data
  external_variables = calendar_data_test[i,]
  
  # add new external calendar data into the embedded data
  test_data = cbind(test_data, external_variables)
  
  new_predictions = predict.glm(object = model, newdata = test_data)
  predictions = cbind(predictions, new_predictions)
  
  # update the final lags
  last_lag = paste("Lag", lag, sep = "")
  test_data = test_data[,!(names(test_data) %in% last_lag)]
  
  # save the current week
  previous_week = external_variables$wm_yr_wk
  
  test_data = cbind(new_predictions, test_data)
  
}
# renormalize the predictions
true_predictions = predictions * as.vector(series_means)
true_predictions = as.data.frame(true_predictions)
true_predictions = cbind(all_sales_data$id, true_predictions)

# Writing the results to file
write.table(
  true_predictions,
  file = output_file_name,
  row.names = F,
  sep = ",",
  quote = F
)