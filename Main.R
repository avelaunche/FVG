library(ggplot2)

#test case of 6000 for straight up ones. This one is 100%
#use test case 1000 for dip into also works

y = hi_fvg %>% 
  filter(bearish_fvg == TRUE)

x = hi_fvg %>% 
  filter(bullish_fvg == TRUE)


fvg_analysis_bull = data.frame(low = x$lag2_high, high = x$plus1low, time = x$timestamp, number = x$group, date = x$date)
fvg_analysis_bear = data.frame(low = y$plus1high, high = y$lag2_low, time = y$timestamp, number = y$group, date = y$date)

fvg_analysis_bull
fvg_analysis_bear

for (x in unique(fvg_analysis_bear$date)){
  print(x)
}

for (x in 1:length(unique(fvg_analysis_bear$date))){
  print(unique(fvg_analysis_bear$date)[x])
}

day = filter(fvg_analysis_bear, date == "2025-01-02")
day

day
day2$number

day2$

fvg_analysis_bear$result = NA

count = 0

count = count + 1

print(nrow(fvg_analysis_bear))

print(sum(hi_fvg$bearish_fvg))

res = c()

for (x in 1:length(unique(fvg_analysis_bear$date))){
  z = (unique(fvg_analysis_bear$date)[x])
  day2 = filter(hi_fvg, date == z)
  day2$number = 1:nrow(day2)
  day = filter(day2, bearish_fvg == TRUE)
  print(z)
  
  for (y in 1:nrow(day)){
    print(y)
    hi = day[y,]
    print("results")
    result = analyze_function_bear_fvg(hi, day2)
    print(result)
    
    count = count + 1
    print(count)
    #fvg_analysis_bear$result[count] = result
    res = c(res, result)
  }
}

fvg_analysis_bear$result = res

sum(fvg_analysis_bear$result)/length(fvg_analysis_bear$result)

