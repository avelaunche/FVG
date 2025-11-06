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
    return(day1$timestamp[IMPORTANT$number:break_above_number[1]])
  }
  

  
  if (break_through[1] < break_above[1]) {
    print("C")
    return(FALSE)
  }
  
  print("D")
  return(day1left$timestamp[1:break_above_number[1]])
}
