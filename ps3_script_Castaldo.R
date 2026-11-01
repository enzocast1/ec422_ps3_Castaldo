library(tidyverse)


# 2.2a: Read in data and check the date column
retail_raw <- read_csv("data/retail_sales_raw.csv")
head(retail_raw)
# Answer: observation_date is <date>, so read_csv guessed the type correctly


retail_raw <- read_csv(
  "data/retail_sales_raw.csv",
  na = ".",
  col_types = cols(
    observation_date = col_date(format = "%Y-%m-%d"),
    RSAFS = col_double(),
    RSAFSNA = col_double()
  )
)

retail_raw_long <- retail_raw |>
  select(observation_date, RSAFS, RSAFSNA) |>
  pivot_longer(cols = c(RSAFS, RSAFSNA), names_to = "series_id", values_to = "sales")

head(retail_raw_long)

library(fpp3)

retail_ts <- retail_raw_long |>
  mutate(Month = yearmonth(observation_date)) |>
  as_tsibble(index = Month, key = series_id)


head(retail_ts)
has_gaps(retail_ts)


head(canadian_gas)
key_vars(canadian_gas)
index_var(canadian_gas)

can_gas <- canadian_gas |>
  select(Month, Volume)

has_gaps(can_gas)

head(can_gas)

can_gas_recent <- can_gas |>
  filter(Month >= yearmonth("1990 Jan"))

head(can_gas_recent)

can_gas_recent <- can_gas_recent |>
  mutate(year    = year(Month),
         month   = month(Month, label = TRUE),
         quarter = quarter(Month))

can_gas_recent |>
  as_tibble() |>
  group_by(month) |>
  summarize(avg_volume = mean(Volume))

#Dec has the highest average volume. The lowest average volume is in Feb.

can_gas_recent |>
  mutate(next_month           = Month + 1,
         same_month_last_year = Month - 12) |>
  head()

can_gas_q <- can_gas |>
  filter(Month <= yearmonth("2004 Dec")) |>
  index_by(Quarter = yearquarter(Month)) |>
  summarize(Volume = sum(Volume))

head(can_gas_q)

can_gas_q |> autoplot(Volume)

#2.6
#We sum the monthly values for gas rather than taking their mean because the monthly values are not averages.
#Summing them gives us the total volume for each quarter. Yes the quarterly series still appears to be seasonal.

retail_ts |> autoplot(sales)

# 2.7d: Clean up the y-axis labels
retail_ts |>
  autoplot(sales) +
  labs(
    title = "U.S. Retail and Food Services Sales",
    subtitle = "Seasonally adjusted vs. not seasonally adjusted",
    x = NULL, y = "Millions of dollars", color = NULL
  ) +
  scale_y_continuous(labels = scales::comma) +
  theme_minimal()

#Seasonal adjustment removes the seasonal component from the data, which lets us see the trend more clearly. 
#The NSA series has a repeating pattern every year: sales spike in December and drop sharply in January, while the SA series is smooth and has no such pattern. 
#Both series follow the same long-run path, rising over time and dropping during the 2008 recession and the 2020 COVID shutdown, so seasonal adjustment does not change the overall trend or the recessions. The NSA seasonal swings also get larger over time as the level of sales grows.

can_gas |>
  autoplot(Volume) +
  labs(
    title = "Canadian Natural Gas Production",
    subtitle = "Monthly, not seasonally adjusted",
    x = NULL, y = "Billions of cubic meters"
  ) +
  theme_minimal()

# 2.7d: Clean up the y-axis labels so we have numbers instead of scientific notation 

#the size of the seasonal swings grow and then decrease around 1990 onwards. 

# 3.1b: Add log of Volume and plot it
can_gas <- can_gas |>
  mutate(log_volume = log(Volume))

can_gas |>
  autoplot(log_volume) +
  labs(
    title = "Log of Canadian Natural Gas Production",
    subtitle = "Monthly, not seasonally adjusted",
    x = NULL, y = "Log(billions of cubic meters)"
  ) +
  theme_minimal()
