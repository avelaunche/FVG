library(ggplot2)
library(dplyr)
library(lubridate)

#test case of 6000 for straight up ones. This one is 100%
#use test case 1000 for dip into also works

bear_fvg_true = hi_fvg %>% 
  filter(bearish_fvg == TRUE)

bull_fvg_true = hi_fvg %>% 
  filter(bullish_fvg == TRUE)

fvg_analysis_bear = data.frame(
  low = bear_fvg_true$plus1high, 
  high = bear_fvg_true$lag2_low, 
  time = bear_fvg_true$timestamp, 
  number = bear_fvg_true$group, 
  date = bear_fvg_true$date
)
fvg_analysis_bull = data.frame(
  low = bull_fvg_true$lag2_high, 
  high = bull_fvg_true$plus1low, 
  time = bull_fvg_true$timestamp, 
  number = bull_fvg_true$group, 
  date = bull_fvg_true$date
)

head(fvg_analysis_bull)
head(fvg_analysis_bear)

fvg_analysis_bear$result = NA

res = c()

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
    result = analyze_bull_fvg_one_to_one(hi, day2)
    print(result)
    
    count = count + 1
    print(count)
    #fvg_analysis_bear$result[count] = result
    res = c(res, result)
  }
}
res = as.logical(res)
res = as.numeric(res)
res
sum(res)
sum(res)/length(res)

sum(res)
length(res)

res

fvg_analysis_bull$results = res

table(res)

fvg_analysis_bull$hour = hour(fvg_analysis_bull$time)
fvg_analysis_bull$weekdays = weekdays(fvg_analysis_bull$time)
fvg_analysis_bull$month = month(fvg_analysis_bull$time)

fvg_analysis_bull$weekdays = as.factor(fvg_analysis_bull$weekdays)

fvg_analysis_bull$weekdays = factor(
  fvg_analysis_bull$weekdays, 
  levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday")
)

fvg_analysis_bull$results = as.factor(fvg_analysis_bull$results)

ggplot(fvg_analysis_bull, aes(hour, fill = results)) + 
  geom_bar()

ggplot(fvg_analysis_bull, aes(hour, fill = results)) + 
  geom_bar(position = 'fill')

ggplot(fvg_analysis_bull, aes(weekdays, fill = results)) + 
  geom_bar()

ggplot(fvg_analysis_bull, aes(weekdays, fill = results)) + 
  geom_bar(position = 'fill')

ggplot(fvg_analysis_bull, aes(month, fill = results)) + 
  geom_bar()

#february having a much lower level of fills makes sense because of the tariff war. 
#i guess last july was also a bad year just like this one?
ggplot(fvg_analysis_bull, aes(month, fill = results)) + 
  geom_bar(position = 'fill')




fv_true = fvg_analysis_bull[res,]

fv_true$minutetime = hour(fv_true$time)

ggplot(fv_true, aes(date)) + 
  geom_histogram()

ggplot(fv_true, aes(minutetime)) + 
  geom_histogram()

true_df = data.frame(time = NA, group = NA)
count = 0

for (x in 1:length(unique(fvg_analysis_bull$date))){
  z = (unique(fvg_analysis_bull$date)[x])
  day2 = filter(hi_fvg, date == z)
  day2$number = 1:nrow(day2)
  day = filter(day2, bullish_fvg == TRUE)
  print(z)
  
  for (y in 1:nrow(day)){
    count = count + 1
    print(y)
    hi = day[y,]
    print("results")
    result = analyze_bull_fvg_extract_box(hi, day2)
    print(typeof(result))
    if (typeof(result) == "double"){
      result = data.frame(time = result)
      true_df = rbind(data.frame(result, group = count), true_df)
    }
    else{
      print(result)
    }
  }
}



true_df
dim(true_df)
ggplot(true_df, aes(time)) + 
  geom_histogram()

true_df$time = as.POSIXct(true_df$time)
true_df$date = as.Date(true_df$time)
true_df1 = true_df[!is.na(true_df$time),]

true_df1

colnames(true_df1) = c("timestamp", "group", "date")
hi_fvg
true_df2 = true_df1 |>
  left_join(dplyr::select(hi_fvg, -group), join_by(timestamp == timestamp))  |>
  select(timestamp, group, open, close, low, high) 

true_df3 = true_df2 |>
  group_by(group) |>
  summarise(n = n(), max = max(high), min = min(low), size = max - min, depth = first(high) - min(low))

ggplot(true_df3, aes(n)) + 
  geom_histogram(bins = 20)

ggplot(true_df3, aes(n)) + 
  geom_boxplot()

nrow(filter(true_df3, n > 35))/nrow(true_df3)

ggplot(true_df3, aes(size)) + 
  geom_histogram(bins = 20)

ggplot(true_df3, aes(n, size)) + 
  geom_point(alpha = 0.01) + 
  geom_smooth(method = "lm", se = FALSE)

ggplot(true_df3, aes(depth)) + 
  geom_histogram(bins = 20)

ggplot(true_df3, aes(depth/size)) + 
  geom_histogram(bins = 20)


ggplot(true_df2, aes(close - open)) + 
  geom_histogram(bins = 100)

ggplot(true_df2, aes(close - open)) + 
  geom_boxplot()

res_type = c()

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
    result = analyze_bull_fvg_one_to_one_return_type(hi, day2)
    print(result)
    
    count = count + 1
    print(count)
    #fvg_analysis_bear$result[count] = result
    res_type = c(res_type, result)
  }
}

#A means no trade
#B means stop loss never hit 
#C means tp never hit
#D means stoploss hit before tp
#E means success
table(res_type)
fvg_analysis_bull$results = res_type

ggplot(fvg_analysis_bull, aes(hour, fill = results)) + 
  geom_bar(position = 'fill')

ggplot(fvg_analysis_bull, aes(weekdays, fill = res_type)) + 
  geom_bar(position = 'fill')

ggplot(fvg_analysis_bull, aes(month, fill = res_type)) + 
  geom_bar(position = 'fill')



