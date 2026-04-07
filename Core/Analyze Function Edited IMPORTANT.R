analyze_bull_fvg_one_to_one <- function(hi, day1){
  high = hi$high
  number = hi$number
  hi$number
  end = (number+1):nrow(day1)
  end_of_day = day1[end,]
  end_true = high > end_of_day$low
  ab = end_of_day[end_true,]
  nrow(ab)
  if (nrow(ab) == 0 | nrow(ab) == 1){
    print("A")
    return(FALSE)
  }
  ab
  IMPORTANT = ab[2,]

  IMPORTANT
  day1left = day1[IMPORTANT$number:nrow(day1),]
  break_through = day1left$timestamp[day1left$low < hi$low]
  break_through
  
  diff = hi$close - hi$open
  
  break_above = day1left$timestamp[day1left$high > (hi$high + diff)]
  break_above
  break_above = break_above[1:length(break_above)]
  
  break_above = break_above[!is.na(break_above)]
  
  diff = day1$close - day1$open
  
  if (length(break_through) == 0){
    print("A")
    return(TRUE)
  }
  
  if (length(break_above) == 0){
    print("A")
    return(FALSE)
  }
  
  if (break_through[1] < break_above[1]) {
    print("C")
    return(FALSE)
  }
  
  print("D")
  return(TRUE)
}

analyze_bull_fvg_one_to_one_return_type <- function(hi, day1){
  high = hi$high
  number = hi$number
  hi$number
  l = (number+1):nrow(day1)
  g = day1[l,]
  a = high > g$low
  ab = g[a,]
  nrow(ab)
  #nothing below is hit meaning no trade ever taken
  if (nrow(ab) == 0 | nrow(ab) == 1){
    return("A")
  }
  ab
  IMPORTANT = ab[2,]
  
  day1left = day1[IMPORTANT$number:nrow(day1),]
  break_through = day1left$timestamp[day1left$low < hi$low]
  break_through
  
  diff = hi$close - hi$open
  
  break_above = day1left$timestamp[day1left$high > (hi$high + diff)]
  break_above
  break_above = break_above[1:length(break_above)]
  
  break_above = break_above[!is.na(break_above)]
  
  diff = day1$close - day1$open
  
  #nothing breaks below sl
  if (length(break_through) == 0){
    return("B")
  }
  
  #nothing breaks above tp
  if (length(break_above) == 0){
    return("C")
  }
  
  #if break below sl occurs before tp
  if (break_through[1] < break_above[1]) {
    return("D")
  }
  #otherwise tp
  return("E")
}



analyze_bull_fvg_extract_box <- function(hi, day1){
  high = hi$high
  number = hi$number
  hi$number
  l = (number+1):nrow(day1)
  l
  g = day1[l,]
  g
  a = high > g$low
  ab = g[a,]
  nrow(ab)
  if (nrow(ab) == 0 | nrow(ab) == 1){
    print("A")
    return(FALSE)
  }
  ab
  IMPORTANT = ab[2,]
  
  day1left = day1[IMPORTANT$number:nrow(day1),]
  break_through = day1left$timestamp[day1left$low < hi$low]
  break_through
  
  break_above = day1left$timestamp[day1left$high > (hi$high + diff)]
  break_above_number = day1left$number[day1left$high > (hi$high + diff)]
  
  break_above
  break_above = break_above[1:length(break_above)]
  
  break_above = break_above[!is.na(break_above)]
  
  diff = day1$close - day1$open
  
  if (length(break_above) == 0){
    print("A")
    return(FALSE)
  }
  
  IMPORTANT$number
  break_above_number[2]
  day$timestamp[IMPORTANT$number:break_above_number[1]]
  
  if (length(break_through) == 0){
    print("A")
    return(day1left$timestamp[1:break_above_number[1]])
  }
  
  
  
  if (break_through[1] < break_above[1]) {
    print("C")
    return(FALSE)
  }
  
  print("D")
  return(day1left$timestamp[1:break_above_number[1]])
}



analyze_bull_fvg_extract_box_false <- function(hi, day1){
  high = hi$high
  number = hi$number
  hi$number
  l = (number+1):nrow(day1)
  l
  g = day1[l,]
  g
  a = high > g$low
  ab = g[a,]
  nrow(ab)
  if (nrow(ab) == 0 | nrow(ab) == 1){
    return(FALSE)
  }
  ab
  IMPORTANT = ab[2,]
  IMPORTANT
  day1left = day1[IMPORTANT$number:nrow(day1),]
  break_through = day1left$timestamp[day1left$low < hi$low]
  break_through_number = day1left$number[day1left$low < hi$low]
  break_through_number
  break_above = day1left$timestamp[day1left$high > (hi$high + diff)]

  break_above
  break_above = break_above[1:length(break_above)]
  
  break_above = break_above[!is.na(break_above)]
  
  diff = day1$close - day1$open
  print(break_through_number)
  if (length(break_above) == 0){
    if (length(break_through) > 0){
      print("F")
      return(day1left$timestamp[1:break_through_number[1]])
    }
    else{
      return(FALSE)
    }
  }

  if (length(break_through) == 0){
    print("C")
    return(day1left$timestamp[1:break_above_number[1]])
  }

  
  if (break_through[1] < break_above[1]) {
    print("C")
    return(day1left$timestamp[1:break_above_number[1]])
  }
  return(FALSE)
}


gemini_analyze_bull_fvg <- function(hi, day1) {
  # 1. SETUP: Define levels
  entry_price <- hi$high
  stop_loss <- hi$low
  risk_amount <- entry_price - stop_loss
  take_profit <- entry_price + risk_amount
  
  # 2. FIND ENTRY: When does price first dip into the gap?
  # We look at all candles after the FVG candle (hi$number)
  subsequent_data <- day1[(hi$number + 1):nrow(day1), ]
  
  # Find the first candle where the low goes below the FVG high
  entry_candidates <- which(subsequent_data$low <= entry_price)
  
  if (length(entry_candidates) == 0) {
    return("No Entry Found") # Price never retraced to the gap
  }
  
  # Get the index of the very first mitigation
  entry_idx <- entry_candidates[1]
  
  # 3. MONITOR TRADE: Look at data starting FROM the entry candle
  # We include the entry candle itself because it could hit SL/TP immediately
  trade_data <- subsequent_data[entry_idx:nrow(subsequent_data), ]
  
  # 4. CHECK OUTCOMES
  # Which candle hits the Stop Loss first?
  sl_hits <- which(trade_data$low < stop_loss)
  # Which candle hits the Take Profit first?
  tp_hits <- which(trade_data$high > take_profit)
  
  # 5. EVALUATE RESULT
  has_sl = length(sl_hits) > 0
  has_tp = length(tp_hits) > 0
  
  if (!has_sl && !has_tp) return("Trade Open")
  if (has_sl && !has_tp) return("Loss")
  if (has_tp && !has_sl) return("Win")
  
  # If both happened in the data, which happened first?
  if (sl_hits[1] < tp_hits[1]) {
    return("Loss")
  } else {
    return("Win")
  }
}
