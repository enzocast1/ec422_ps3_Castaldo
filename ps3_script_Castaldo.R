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
# 3.1b: After taking logs, the seasonal swings are still uneven: largest in the
# 1970s-80s and smallest after the mid-1990s, so logging does not make the
# seasonal swings the same size. The trend grows steeply in the 1960s, flattens
# out around the mid-1970s, rises again in the late 1980s-1990s, and levels off
# around 2000. I don't think logs are necessary for can_gas, because logs work
# best when the seasonal swings grow in proportion to the level of the series,
# and here the swings shrink in the later years even as production keeps rising.

# 3.1c: Log and compare for the NSA retail series
retail_nsa <- retail_ts |>
  filter(series_id == "RSAFSNA") |>
  mutate(log_sales = log(sales))

# Plot the level
retail_nsa |>
  autoplot(sales) +
  labs(
    title = "U.S. Retail Sales (NSA)",
    subtitle = "Level",
    x = NULL, y = "Millions of dollars"
  ) +
  scale_y_continuous(labels = scales::comma) +
  theme_minimal()

# Plot the log
retail_nsa |>
  autoplot(log_sales) +
  labs(
    title = "U.S. Retail Sales (NSA)",
    subtitle = "Log",
    x = NULL, y = "Log(millions of dollars)"
  ) +
  theme_minimal()

# 3.1c: The case for logging is stronger than it was for can_gas. In levels,
# the seasonal swings in retail sales grow as sales grow, and after taking logs
# the swings at the start of the sample are roughly the same size as at the end,
# which means the swings grew in proportion to the level of the series. This
# wasn't true for can_gas, where the swings grew and then shrank. The log trend
# is also close to a straight line, consistent with roughly steady percentage
# growth.

can_gas <- can_gas |>
  mutate(
    growth_direct = 100 * difference(Volume) / lag(Volume),
    growth_logdiff = 100 * difference(log_volume)
  )

can_gas |>
  autoplot(growth_direct) +
  labs(
    title = "Growth in Canadian Natural Gas Production",
    subtitle = "Month-over-month percent change (direct)",
    x = NULL, y = "Percent"
  ) +
  theme_minimal()

# 3.2b: Gap between direct and log-difference growth rates
can_gas <- can_gas |>
  mutate(growth_gap = abs(growth_direct - growth_logdiff))

can_gas |>
  autoplot(growth_gap) +
  labs(
    title = "Gap Between Direct and Log-Difference Growth Rates",
    subtitle = "Canadian natural gas production",
    x = NULL, y = "Absolute difference (percentage points)"
  ) +
  theme_minimal()

# 3.2a: The direct and log-difference growth rates are very close to each other, with the gap being less than 0.1 percentage points for most months. The gap is largest in the early years of the sample, when production was lower and the swings were larger in absolute terms. The gap is also larger during periods of rapid growth or decline, such as the early 1970s and the early 1980s. Overall, the log-difference growth rate is a good approximation of the direct growth rate for this series. 

# 3.3 SA vs. NSA Growth Rates

retail_ts <- retail_ts |>
  group_by_key() |>
  mutate(growth_mom = 100 * difference(log(sales))) |>
  ungroup()

retail_ts |>
  autoplot(growth_mom) +
  labs(
    title = "Month-over-Month Growth: SA vs. NSA Retail Sales",
    x = NULL, y = "Growth rate (%)", color = NULL
  ) +
  theme_minimal()

# 3.3: The SA series' month-over-month growth rates hover around 0 percent,
# apart from large moves during the 2020 COVID shutdown and rebound. The NSA
# series is much noisier, with growth rates swinging from about -30 to almost
# 30 percent. This has to do with the NSA series not being seasonally adjusted:
# those swings come from its seasonal component. This connects back to 2.7b,
# where the NSA level spiked every December and dropped every January. In
# growth rates, that pattern shows up as a large positive growth rate in
# December and a large negative one in January every year. The SA series has
# had the seasonal component removed, so its growth rates are much smoother
# and more stable.

# 4.1a 
retail_ts |>
  filter(series_id == "RSAFSNA") |>
  gg_season(sales) +
  labs(
    title = "Seasonal Pattern in NSA Retail Sales",
    y = "Millions of dollars"
  ) +
  theme_minimal()

#4.1b 
retail_ts |>
  filter(series_id == "RSAFS") |>
  gg_season(sales) +
  labs(
    title = "Seasonal Pattern in SA Retail Sales",
    y = "Millions of dollars"
  ) +
  theme_minimal()

# 4.1b: In the NSA season plot, every year's line traces the same shape: sales
# are low in January and February and spike in December, so there is clear
# seasonality. In the SA season plot, the lines are flat across the months
# with no repeating pattern, because the seasonal component has been removed.
# The only big movement in the SA plot comes from one-time shocks like the
# 2020 COVID shutdown, which seasonal adjustment does not remove.

#4.1c 

can_gas |> 
  gg_season(log_volume) +
  labs(
    title = "Seasonal Pattern in Log of Canadian Natural Gas Production",
    y = "Log(billions of cubic meters)"
  ) + 
  theme_minimal()
# 4.1c: Production is lowest in the summer months (June through September),
# rises from October onward, and peaks in the winter months of December and
# January. This matches intuition: natural gas is heavily used for heating,
# so demand in Canada is highest in the cold winter months and lowest in the
# summer. The small dip in February is likely because February has fewer days.
# The seasonal swings are also smaller in the most recent years, consistent
# with what we saw in 3.1.

#4.2
can_gas |>
  gg_subseries(log_volume) +
  labs(
    title = "Subseries Plot: Canadian Gas Production",
    y = "log(Volume)"
  )


#4.3 
retail_nsa_x13 <- retail_ts |>
  filter(series_id == "RSAFSNA") |>
  model(x13 = X_13ARIMA_SEATS(sales)) |>
  components()

retail_nsa_x13 |> autoplot()

