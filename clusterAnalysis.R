# Favorita-grocery-sales-forecasting
# Cluster Analysis script
# Kasun Bandara, June 2020

# @input: Sales_train_final.txt

# Please uncomment the below command, in case you haven't installed the following pakcage in your enviornment.
# install.packages("factoextra")

require(factoextra)
library(factoextra)

# Setting the seed
set.seed(1234)

#Loading the required files
df <-
  read.csv(
    "Sales_train_final.txt",
    sep = ",",
    header = TRUE,
    stringsAsFactors = TRUE
  )

meta_data_size <- 9


df_filter <- df[, c(1:meta_data_size)]

# Adding a new product to the dataset (new product to be forecast)
df_filter.new <-
  rbind(df_filter,
        c(1, 888888, "BREAD/BAKERY", 2702, 1, "Quito", "Pichincha", "D", 13))

# Preparing the categorical data for clustering.
df_filter.new$store_id <- as.numeric(df_filter.new$store_id)
df_filter.new$item_id <- as.numeric(df_filter.new$item_id)
df_filter.new$class <- as.numeric(df_filter.new$class)
df_filter.new$perishable <- as.numeric(df_filter.new$perishable)
df_filter.new$cluster <- as.numeric(df_filter.new$cluster)

df_filter.new$family <- as.numeric(df_filter.new$family)
df_filter.new$city <- as.numeric(df_filter.new$city)
df_filter.new$state <- as.numeric(df_filter.new$state)
df_filter.new$state_type <- as.numeric(df_filter.new$state_type)

df_filter.new <- df_filter.new[, c(-2)]

# Cluster visulaisation.
res.km <- eclust(df_filter.new, "kmeans", nstart = 25)

cluster_records <- df_filter.new[res.km$cluster == 1, ]