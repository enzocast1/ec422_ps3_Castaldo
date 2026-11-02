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

# 4.3a: December has the highest seasonal factor, at about 1.2 in the 1990s,
# meaning December sales are roughly 20% above trend. Sales then dip in
# January and February, to about 10-13% below trend. The seasonal swings
# appear to shrink over time: the December peak falls from about 1.2 in the
# 1990s to about 1.15 in recent years, so the holiday spike is a smaller
# share of sales than it used to be.

#4.3b 

sa_compare <- retail_nsa_x13 |>
  as_tibble() |>
  select(Month, mine = season_adjust) |>
  left_join(
    retail_ts |>
      filter(series_id == "RSAFS") |>
      as_tibble() |>
      transmute(Month, census = sales),
    by = "Month"
  ) |>
  mutate(pct_diff = 100 * (log(mine) - log(census)))

sa_compare |>
  ggplot(aes(x = Month, y = pct_diff)) +
  geom_line() +
  labs(
    title = "My X-13 Seasonal Adjustment Minus the Census Bureau's",
    x = NULL, y = "Difference (%)"
  ) +
  theme_minimal()

#4.3b # 4.3b: My X-13 seasonal adjustment matches the official RSAFS series closely:
# the percent difference is mostly between about -0.5% and +0.5%. The two
# differ most after the start of COVID, especially around 2021-2023, with
# differences up to about 1.6%. 

#4.4 
can_gas_fit <- can_gas |>
  model(TSLM(log_volume ~ trend() + season()))

report(can_gas_fit)

can_gas_fit |>
  tidy() |>
  select(term, estimate, std.error, statistic, p.value) |>
  knitr::kable(digits = 3)

#4.4a
# 4.4: The estimated average monthly growth rate of gas production is about
# 0.4% per month (trend coefficient of 0.004 x 100), or roughly 4.8% per year.
# June has the most significant seasonal effect, with a t-statistic of -4.777:
# June production is about 0.25 log points (roughly 22%) below January,
# consistent with lower demand for natural gas in the summer.

#4.4b

can_gas_fit |>
  augment() |>
  ggplot(aes(x = Month)) +
  geom_line(aes(y = log_volume, color = "Actual")) +
  geom_line(aes(y = .fitted, color = "Fitted")) +
  labs(title = "Linear Trend + Seasonal Dummies Fit: Canadian Gas Production",
       y = "log(Volume)", color = NULL) +
  theme_minimal()

# 4.4b: The fitted line tracks the actual series well from about the mid-1980s
# to the late 1990s, but it visibly misses in several places: it is far above
# the actual data in the early 1960s, below it during the 1970s and early
# 1980s, and above it again after 2000 as actual production levels off. The
# straight-line trend doesn't capture the bends in the series' growth.


#4.4c
can_gas_fit |> gg_tsresiduals()

can_gas_fit |>
  augment() |>
  features(.innov, ljung_box, lag = 24)

# There seems to be heavy autocorrelation looking at the ACF there is lare autocorreltation that decays slow
# which could indicate that. there are trending variables in the residuals. The Ljung-Box test shows that the residuals are not white noise, with a p-value of 0.000, indicating that the model does not capture all the structure in the data. This suggests that a more complex model may be needed to better fit the series.


gas_cv_fc <- can_gas |>
  select(Month, Volume) |>
  stretch_tsibble(.init = 240, .step = 1) |>
  model(snaive = SNAIVE(Volume),
        tslm   = TSLM(log(Volume) ~ trend() + season())) |>
  forecast(h = 1)

gas_cv_fc |>
  accuracy(can_gas, measures = list(MAE = MAE, RMSE = RMSE))
# 5.1a: The first forecast evaluated is for January 1980 (the first training
# sample is the 240 months from January 1960 to December 1979), and the last
# is for February 2005, the last month of data. SNAIVE forecasts much more
# accurately than the trend + seasonal dummies model: MAE of 0.562 vs. 4.01 and
# RMSE of 0.702 vs. 4.36 (billions of cubic meters). The two metrics agree,
# both ranking SNAIVE as more accurate.

errs <- gas_cv_fc |>
  as_tibble() |>
  left_join(as_tibble(can_gas) |> select(Month, actual = Volume),
            by = "Month") |>
  mutate(e = actual - .mean) |>
  select(Month, .model, e) |>
  pivot_wider(names_from = .model, values_from = e) |>
  na.omit()

forecast::dm.test(errs$snaive, errs$tslm, h = 1, power = 1)

#5.2 the DM = -31.278 pvalue super small so we reject the null hypothesis that the two models have equal predictive accuracy. The SNAIVE model is significantly more accurate than the trend + seasonal dummies model, consistent with the MAE and RMSE results above. The large negative DM statistic indicates that the SNAIVE errors are much smaller than the TSLM errors.

# 5.3 

# 5.3a: Rolling window of 240 months
gas_cv_roll <- can_gas |>
  select(Month, Volume) |>
  slide_tsibble(.size = 240, .step = 1) |>
  model(snaive = SNAIVE(Volume),
        tslm   = TSLM(log(Volume) ~ trend() + season())) |>
  forecast(h = 1)

gas_cv_roll |>
  accuracy(can_gas, measures = list(MAE = MAE, RMSE = RMSE))


# 5.3a: With a rolling window, SNAIVE's accuracy is unchanged (MAE 0.562,
# RMSE 0.702), while TSLM improves a lot (MAE falls from 4.01 to 2.09 and
# RMSE from 4.36 to 2.42), so TSLM's accuracy changed more. SNAIVE only uses
# the value from 12 months earlier, so the length of the training sample does
# not matter. TSLM fits a single straight-line trend, and in 4.4b that line
# was pulled too steep by the fast 1960s growth. A rolling window drops those
# early years, so the trend reflects more recent, slower growth and the
# forecasts overshoot less, though TSLM is still far less accurate than SNAIVE.

