library(tidyverse)

results = data.frame("date", "number", "result", "correct")
colnames(results) = results[1,]
results
dates = unique(fvg_analysis_bull$date)

hi_fvg$date
dates
dates[1]

for (x in 27){
  b = c()
  day1 = filter(hi_fvg, date == dates[x])
  print(day1)
  day1$number = 1:nrow(day1)
  a = filter(day1, bullish_fvg == TRUE)
  target_plot(day1, a[1,])
  for (y in 1:nrow(a)){
    result = analyze_bull_fvg_one_to_one(a[y,], day1)
    print(result)
    b = c(b, result)
  }
  res = cbind(data.frame(date = rep(dates[x], length(b)), number = 1:nrow(a), result = b, correct = rep(TRUE, length(b))))
  
}

bull_fvg_plot(day1)

results

res = cbind(data.frame(date = rep(dates[x], length(b)), number = 1:nrow(a), result = b, correct = rep(TRUE, length(b))))
res

results = rbind(results, res)


sum(results$result)/nrow(results)

