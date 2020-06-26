# Favorita-grocery-sales-forecasting.
# Store Transcations Visulaisation Script.
# Kasun Bandara, June 2020


# Loading the required libraries.
# install.packages("fpp3")
# install.packages("readr")
require(fpp3)
require(readr)

library(fpp3)
library(readr)


# Loading the required files.
df_stores <- read_csv("stores.csv", col_names = TRUE)
df_transcations <- read_csv("transactions.csv", col_names = TRUE)

# Coverting the data tsibble() and tibble() formats accordingly.
df_transcation_store <-
  df_transcations  %>% left_join(df_stores, by = "store_nbr") %>% as_tsibble(index = date, key = c(store_nbr))

df_transcation_ts <-
  df_transcation_store  %>% summarise(Total_transcations = sum(transactions)) %>% fill_gaps(Total_transcations = 0L)

df_total_transcation_ts <-
  df_transcation_ts %>% autoplot(Total_transcations) +
  labs(y = "Total transcations", x = "Time (Years)", title = "Total store transcations: Corporación Favorita Grocery")



df_total_transcation_ts_2015 <-
  df_transcation_ts %>% filter_index("2014-12-15" ~ "2015-01-05")  %>%
  autoplot(Total_transcations) + scale_x_date(date_breaks = "day") +
  theme(axis.text.x = element_text(
    angle = 90,
    vjust = 0.5,
    hjust = 1
  )) +
  labs(y = "Total transcations", x = "Time (Years)", title = "Christmas and New year effect")

df_total_transcation_ts_2016 <-
  df_transcation_ts %>% filter_index("2015-12-15" ~ "2016-01-05")  %>%
  autoplot(Total_transcations) + scale_x_date(date_breaks = "day") +
  theme(axis.text.x = element_text(
    angle = 90,
    vjust = 0.5,
    hjust = 1
  )) +
  labs(y = "Total transcations", x = "Time (Years)", title = "Total transcations: Corporación Favorita Grocery")



df_total_transcations_weekly_lags <-
  df_transcation_ts %>% ACF(Total_transcations, lag_max = 56) %>%  autoplot() +
  labs(y = "acf", x = "lags", title = "Transcation Lags: Corporación Favorita Grocery (Weekly Seasonality)")

df_total_transcations_yearly_lags <-
  df_transcation_ts %>% ACF(Total_transcations, lag_max = 800) %>%  autoplot() +
  theme(axis.text.x = element_text(
    angle = 90,
    vjust = 0.5,
    hjust = 1
  )) +
  labs(y = "acf", x = "lags", title = "Transcation Lags: Corporación Favorita Grocery (Yearly Seasonality)")

df_total_seasonal_plot <-
  df_transcation_ts  %>% mutate(year = year(date)) %>% gg_season(Total_transcations, period = "year") + ylab("Transcations") +
  #facet_grid(.~ year) +
  facet_grid(vars(year), scales = "free_y") +
  xlab("Month") + ggtitle("Seasonal Plot: Corporación Favorita Grocery")

df_total_seasonal_plot <-
  df_transcation_ts  %>% gg_season(Total_transcations, period = "year") + ylab("Transcations") +
  xlab("Month") + ggtitle("Seasonal Plot: Corporación Favorita Grocery")

df_total_seasonal_plot <-
  df_transcation_ts  %>% gg_season(Total_transcations, period = "week") + ylab("Transcations") +
  xlab("Month") + ggtitle("Seasonal Plot: Corporación Favorita Grocery")


df_total_sub_plot <-
  df_transcation_ts  %>% gg_subseries(Total_transcations, period = "year") + ylab("Transcations") +
  xlab("Month") + ggtitle("Seasonal Plot: Corporación Favorita Grocery")

df_total_sub_plot <-
  df_transcation_ts  %>% gg_subseries(Total_transcations, period = "month") + ylab("Transcations") +
  xlab("Month") + ggtitle("Seasonal Plot: Corporación Favorita Grocery")

df_total_sub_plot <-
  df_transcation_ts  %>% gg_subseries(Total_transcations, period = "week") + ylab("Transcations") +
  xlab("Month") + ggtitle("Seasonal Plot: Corporación Favorita Grocery")


df_monthly <-
  df_transcations  %>% left_join(df_stores, by = "store_nbr") %>% mutate(Month = yearmonth(date)) %>%
  select(-c('date')) %>% group_by(Month) %>% summarise(Monthly_transcations = sum(transactions)) %>%
  as_tsibble(index = Month)  %>% mutate(year = year(Month)) %>% filter(year != 2017)

df_total_seasonal_plot <- df_monthly  %>%
  gg_season(Monthly_transcations, period = "year") + ylab("Transcations") +
  xlab("Month") + ggtitle("Seasonal Plot: Corporación Favorita Grocery")

df_total_sub_plot <-
  df_monthly %>% gg_subseries(Monthly_transcations, period = "year") + ylab("Transcations") +
  xlab("Month") + ggtitle("Seasonal Plot: Corporación Favorita Grocery")