analyze_function_bull_fvg <- function(hi, day1){
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
  
  break_above = day1left$timestamp[day1left$high > (hi$high )]
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




analyze_function_bear_fvg <- function(hi, day1){
  low = hi$low
  number = hi$number
  hi
  hi$number
  l = (number+1):nrow(day1)
  g = day1[l,]
  day1
  g
  a = low < g$high
  ab = g[a,]
  nrow(ab)
  if (nrow(ab) == 0){
    print("A")
    return(FALSE)
  }
  
  IMPORTANT = ab[1,]
  
  day1left = day1[IMPORTANT$number:nrow(day1),]
  break_through = day1left$timestamp[day1left$high > hi$high]
  break_through
  break_below = day1left$timestamp[day1left$low < hi$low]
  break_below
  if (length(break_below) == 1){
    print("A")
    return(FALSE)
  }

  break_below = break_below[2:length(break_below)]
  print(break_through)
  print(break_below)
  
  print("HIODAHODA")
  
  print(break_through[1])
  print(break_below[1])
  print("")
  
  
  if (length(x2) == 0){
    print("B")
    return(TRUE)
  }
  
  break_through[1]
  break_below[1]
  
  
  if (break_below[1] > break_through[1]) {
    print("C")
    return(FALSE)
  }
  
  print("D")
  return(TRUE)
}




