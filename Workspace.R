library(ggplot2)
library(tidyquant)

#test case of 6000 for straight up ones. This one is 100%
#use test case 1000 for dip into also works
day1 = filter(hi_fvg, date == date[2])

colnames(day1)

date(day1$timestamp[1])

summary(day1) 

day1$open

day1$number = 1:nrow(day1)


x = day1 %>% 
  filter(bearish_fvg == TRUE)

y = day1 %>% 
  filter(bullish_fvg == TRUE)

fvg_analysis_bull = data.frame(low = y$lag2_high, high = y$plus1low, time = y$timestamp, number = y$number)
fvg_analysis_bear = data.frame(low = x$plus1high, high = x$lag2_low, time = x$timestamp, number = x$number)

fvg_analysis_bear

sum(day1$bearish_fvg + day1$bullish_fvg)

#ggplot(day1, aes(x = timestamp, y = close)) + 
  #geom_barchart(aes(open = open, close = close, high = high, low = low, alpha = fvg)) + 
  #geom_ma(ma_fun = EMA, n = 9, linetype = "solid", color = "darkblue") + 
  #geom_ma(aes(volume = volume), ma_fun = VWMA, color = "purple", linetype = "solid")

bear_fvg_plot(day2)
