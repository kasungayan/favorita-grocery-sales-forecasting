# Favorita-grocery-sales-forecasting.
# Item Sales Visulaisation Script.
# Kasun Bandara, June 2020


# Loading the required libraries.
# install.packages("fpp3")
# install.packages("readr")
require(fpp3)
require(readr)

library(fpp3)
library(readr)

# Loading the require diles
df_stores <- read_csv("stores.csv", col_names = TRUE)
df_price <- read_csv("oil.csv")

df_sales <- load("train.rda")
df_sales <- df

df_sales_store <-
  df_sales  %>%  select(-c('id', 'onpromotion')) %>% group_by(item_nbr) %>%
  summarise(total_sales = sum(unit_sales))

df_sales_store_sorted <-
  df_sales_store %>% arrange(desc(total_sales))

df_sales_store_sorted <- head(df_sales_store_sorted, n = 10)

highest_sales_items <- df_sales_store_sorted$item_nbr[1:10]


ggplot(data = df_sales_store_sorted,
       mapping = aes(
         x = total_sales,
         y = factor(item_nbr),
         fill = factor(item_nbr)
       )) +
  geom_col() +
  theme(legend.position = "none") + ylab("Item ID") +
  xlab("Total Sales")


df_sales_filter <-
  df_sales %>% filter(item_nbr %in% highest_sales_items)
save(df_sales_filter, file = 'sales_filer.rda')


load('sales_filer.rda')
df_item <- read_csv("items.csv")

df_sales_filter$unit_sales[df_sales_filter$unit_sales < 0] <- 0

df_sales_desc <-
  df_sales_filter %>% left_join(df_item, by = "item_nbr")

df_sales_desc_timeseries <-
  df_sales_desc %>% select(-c('id', 'onpromotion')) %>% as_tsibble(index = date,
                                                                   key = c(store_nbr, item_nbr, family,
                                                                           class, perishable))

df_sales_desc_timeseries2 <-
  df_sales_desc %>% select(-c('id')) %>% as_tsibble(index = date,
                                                    key = c(store_nbr, item_nbr, family,
                                                            class, perishable))
df_sales_desc_timeseries2_filter <-
  df_sales_desc_timeseries2 %>% filter(onpromotion %in% c('FALSE', 'TRUE'))

df_promotion_sales <-
  df_sales_desc_timeseries2_filter %>% group_by(item_nbr, onpromotion) %>% summarise(Total_sales = sum(unit_sales))

df_promotion_sales  <-
  df_promotion_sales %>% as_tibble(df_promotion_sales)

ggplot(data = df_promotion_sales,
       mapping = aes(x = factor(onpromotion), y = Total_sales)) +
  geom_boxplot() +
  facet_grid(. ~ item_nbr) +
  theme(legend.position = "none") + ylab("Total Sales") +
  xlab("On Promotion (TRUE/FALSE)")


df_sales_desc_timeseries2_filter %>% group_by(item_nbr, onpromotion) %>% summarise(total_sales = sum(unit_sales)) %>%
  filter(item_nbr == 1473474) %>% fill_gaps(total_sales = 0L) %>%  gg_subseries(total_sales, "week") +
  ylab("Total Sales") +
  xlab("Year")

df_sales_desc_timeseries <-
  df_sales_desc_timeseries %>% fill_gaps(unit_sales = 0L, .full = TRUE)


df_sales_desc_timeseries %>% group_by(item_nbr) %>% summarise(Total_sales = sum(unit_sales)) %>%
  ggplot(aes(x = date, y = Total_sales, colour = item_nbr)) +
  geom_line() +
  facet_grid(vars(item_nbr), scales = "free_y") + theme(legend.position = "none") +
  ylab("Total Sales") +
  xlab("Year")

df_sales_desc_timeseries %>% group_by(item_nbr) %>% summarise(Total_sales = sum(unit_sales)) %>% gg_season(Total_sales)

df_sales_desc_timeseries %>% group_by(item_nbr) %>% summarise(Total_sales = sum(unit_sales)) %>% fill_gaps(Total_sales = 0L) %>%
  gg_season(Total_sales, period = "week")

df_sales_desc_timeseries %>% group_by(item_nbr) %>% summarise(Total_sales = sum(unit_sales)) %>% fill_gaps(Total_sales = 0L) %>%
  gg_season(Total_sales, period = "month")


df_sales_desc_timeseries %>% group_by(perishable) %>% summarise(Total_sales = sum(unit_sales)) %>%
  ggplot(aes(x = date, y = Total_sales, colour = perishable)) +
  geom_line() +
  facet_grid(vars(perishable), scales = "free_y")

df_sales_desc_timeseries %>% group_by(perishable) %>% summarise(Total_sales = sum(unit_sales)) %>% fill_gaps(Total_sales = 0L) %>%
  gg_season(Total_sales)

df_sales_desc_timeseries %>% group_by(perishable) %>% summarise(Total_sales = sum(unit_sales)) %>%
  gg_season(Total_sales, period = "week") +
  ylab("Total Sales") +
  xlab("Day")

df_sales_desc_timeseries %>% group_by(perishable) %>% summarise(Total_sales = sum(unit_sales)) %>%
  gg_subseries(Total_sales, period = "week") +
  ylab("Total Sales") +
  xlab("Year")

df_sales_desc_timeseries %>% group_by(perishable) %>% summarise(Total_sales = sum(unit_sales)) %>% fill_gaps(Total_sales = 0L) %>%
  gg_season(Total_sales, period = "month")


df_sales_desc_timeseries %>% group_by(family) %>% summarise(Total_sales = sum(unit_sales)) %>%
  ggplot(aes(x = date, y = Total_sales, colour = family)) +
  geom_line() +
  facet_grid(vars(family), scales = "free_y")

df_sales_desc_timeseries %>% group_by(family) %>% summarise(Total_sales = sum(unit_sales)) %>%
  gg_season(Total_sales)

df_sales_desc_timeseries %>% group_by(family) %>% summarise(Total_sales = sum(unit_sales)) %>%
  gg_season(Total_sales, period = "week")

df_sales_desc_timeseries %>% group_by(family) %>% summarise(Total_sales = sum(unit_sales)) %>% fill_gaps(Total_sales = 0L) %>%
  gg_subseries(Total_sales, period = "week") +
  ylab("Total Sales") +
  xlab("Year")