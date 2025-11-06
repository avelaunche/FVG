
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

res1 = c()

nrow(fvg_analysis_bull)
print(sum(hi_fvg$bullish_fvg))

fvg_analysis_bull$result = NA

count = 0


for (x in 1:length(unique(fvg_analysis_bull$date))){
  z = (unique(fvg_analysis_bull$date)[x])
  day2 = filter(hi_fvg, date == z)
  day2$number = 1:nrow(day2)
  day = filter(day2, bullish_fvg == TRUE)
  print(z)
  
  for (y in 1:nrow(day)){
    print(y)
    hi = day[y,]
    print("results")
    result = analyze_function_bull_fvg(hi, day2)
    
    count = count + 1
    print(count)
    #fvg_analysis_bear$result[count] = result
    res1 = c(res1, result)
  }
}

fvg_analysis_bull$result = res1

sum(res1)/length(res1)

