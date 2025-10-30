#FVG setup, Workspace R, Functions, Main

library(dplyr)
library(lubridate)
library(quantmod)
library(ggplot2)

setwd("~/Downloads/Code")

stock_data = read.csv("stock_data_2025.csv")
stock_data = stock_data[, c(-1, -8, -9)]

stock_data$timestamp <- ymd_hms(stock_data$timestamp, tz = "UTC")
stock_data$timestamp <- with_tz(stock_data$timestamp, tzone = "America/New_York")
stock_data = mutate(stock_data, hour = hour(timestamp))

summary(stock_data)
stock_data$hour = as.integer(stock_data$hour)*60
stock_data$hour
stock_data$hour = as.integer(minute(stock_data$timestamp)) + stock_data$hour
stock_data$hour
colnames(stock_data)
stock_data = dplyr::filter(stock_data, hour >= (9*60+30) & hour <= 16*60)
ggplot(stock_data) + geom_histogram(aes(hour), bins = 60)

stock_data = mutate(stock_data, wday = wday(timestamp))

ggplot(stock_data) + geom_histogram(aes(hour))

hi = data.frame(stock_data[0,])

head(stock_data)

hi <- stock_data %>%
  mutate(group = (row_number() - 1) %/% 5) %>%
  group_by(group) %>%
  summarise(
    timestamp = first(timestamp),   # take timestamp from first row
    open      = first(open),        # open = first open
    high      = max(high),          # high = max in group
    low       = min(low),           # low = min in group
    close     = last(close),        # close = last close
    volume    = sum(volume),        # volume = sum
    hour,
    .groups   = "drop"
  )


hi <- hi %>%
  group_by(group = (row_number() - 1) %/% 5) %>%
  slice_head(n = 1) %>%
  ungroup()


head(hi)

typeof(hi$timestamp)

hi = mutate(hi, date = date(timestamp))

hi_fvg = mutate(
  hi,
  lag2_high = lag(high, 2),
  lag2_low  = lag(low, 2),
  plus1low = low,
  plus1high = high,
  bullish_fvg = ifelse(!is.na(lag2_high) & low > lag2_high & lag(date, 2) == date, TRUE, FALSE),
  bearish_fvg = ifelse(!is.na(lag2_low) & high < lag2_low & lag(date, 2) == date, TRUE, FALSE)
)

#calculates FVG size. Needs 2 seperate ones or produces glitches
hi_fvg <- mutate(
  hi_fvg,
  bullish_fvg_size = ifelse(bullish_fvg, low - lag2_high, NA),
  bearish_fvg_size = ifelse(bearish_fvg, lag2_low - high, NA)
)


#pushes everything forward a step to match the actual FVG candle location
hi_fvg$bearish_fvg = c(hi_fvg$bearish_fvg[-1], FALSE)
hi_fvg$bullish_fvg = c(hi_fvg$bullish_fvg[-1], FALSE)
hi_fvg$bearish_fvg_size = c(hi_fvg$bearish_fvg_size[-1], FALSE)
hi_fvg$bullish_fvg_size = c(hi_fvg$bullish_fvg_size[-1], FALSE)

#setting all the NAs to 0 in preparation for combining them
hi_fvg$bullish_fvg_size[is.na(hi_fvg$bullish_fvg_size)] = 0
hi_fvg$bearish_fvg_size[is.na(hi_fvg$bearish_fvg_size)] = 0

#adding them together to give a better sense of FVG sizes
hi_fvg$fvg_size = hi_fvg$bearish_fvg_size + hi_fvg$bullish_fvg_size

#add something to start of lag2 
hi_fvg$lag2_high = c(hi_fvg$lag2_high[2:(nrow(hi_fvg))], 0)
hi_fvg$lag2_low = c(hi_fvg$lag2_low[2:(nrow(hi_fvg))], 0)
hi_fvg$plus1low = c(hi_fvg$plus1low[2:(nrow(hi_fvg))], 0)
hi_fvg$plus1high = c(hi_fvg$plus1high[2:(nrow(hi_fvg))], 0)


#add FVG percentage
hi_fvg = mutate(hi_fvg, fvg_size_p = fvg_size/abs(low - high))

#filter for larger FVGs only
hi_fvg$fvg[hi_fvg$fvg_size_p < 0.33] = FALSE
hi_fvg$bullish_fvg[hi_fvg$fvg_size_p < 0.33] = FALSE
hi_fvg$bearish_fvg[hi_fvg$fvg_size_p < 0.33] = FALSE

sum(hi_fvg$fvg)

hi_fvg$fvg_size_p

#adds any fvg presence
hi_fvg = mutate(hi_fvg, fvg = ifelse(bullish_fvg | bearish_fvg, TRUE, FALSE))

#identifies number of FVGs
nrow(hi_fvg) - sum(is.na(hi_fvg$fvg_size))

#glimpse at the data
summary(hi_fvg)
colnames(hi_fvg)

plt = ggplot(hi_fvg)
plt + geom_histogram(aes(fvg_size))
plt + geom_histogram(aes(volume))
plt + geom_histogram(aes(fvg_size_p))

