library(tidyquant)
library(tidyverse)

long_info = read_csv("stock_data_2025_5min.csv")
long_info = long_info[, c(-1, -8, -9)]

long_info$timestamp <- ymd_hms(long_info$timestamp, tz = "UTC")
long_info$timestamp <- with_tz(long_info$timestamp, tzone = "America/New_York")
long_info = mutate(long_info, date = date(timestamp))


day_by_day_full = long_info |>
  group_by(date) |>
  summarise(open = first(open), close = last(close), high = max(high), low = min(low)) 

day_by_day_full = day_by_day_full |>
  mutate(
    ema_20 = EMA(close, n = 20) < close,
    ema_14 = EMA(close, n = 14) < close,
    ema_9 = EMA(close, n = 9) < close,
    ema_50 = EMA(close, n = 50) < close,
    ema_200 = EMA(close, n = 200) < close
  )

day_by_day_full = day_by_day_full |>
  mutate(
    positive = open < close
  )

day_by_day_full = day_by_day_full |>
  mutate(
    rsi_14 = RSI(close, n = 14)
  )

length(day_by_day_full$date)

length(unique(day_by_day_full$date))

new_data = filter(hi_fvg, bullish_fvg)
nrow(new_data)

length(unique(new_data$date))

new_data = new_data |>
  left_join(day_by_day_full, by = "date", relationship = "many-to-many")

nrow(new_data)
colnames(new_data)
new_data$results = as.factor(as.logical(res))

ggplot(new_data, aes(ema_200, fill = results)) + 
  geom_bar(position = "fill") + 
  labs(
    title = "Closing Above EMA vs FVG result",
    subtitle = "EMA 200"
  )

ggplot(new_data, aes(ema_50, fill = results)) + 
  geom_bar(position = "fill") + 
  labs(
    title = "Closing Above EMA vs FVG result",
    subtitle = "EMA 50"
  )

ggplot(new_data, aes(ema_20, fill = results)) + 
  geom_bar(position = "fill") + 
  labs(
    title = "Closing Above EMA vs FVG result",
    subtitle = "EMA 20"
  )

ggplot(new_data, aes(ema_14, fill = results)) + 
  geom_bar(position = "fill") + 
  labs(
    title = "Closing Above EMA vs FVG result",
    subtitle = "EMA 14"
  )

ggplot(new_data, aes(ema_9, fill = results)) + 
  geom_bar(position = "fill") + 
  labs(
    title = "Closing Above EMA vs FVG result",
    subtitle = "EMA 9"
  )

ggplot(new_data, aes(x = results, y = rsi_14)) + 
  geom_boxplot() + 
  labs(
    title = "Closing Above EMA vs RSI",
    subtitle = "EMA 14"
  )

ggplot(new_data, aes(x = positive, fill = results)) + 
  geom_bar(position = "FILL") + 
  labs(
    title = "Closing Above EMA vs Day result",
    subtitle = "EMA 14"
  )

