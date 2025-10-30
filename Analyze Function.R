analyze_function_bull_fvg <- function(hi){
  high = hi$high
  number = hi$number
  hi$number
  l = (number+1):nrow(day1)
  g = day1[l,]
  a = high > g$low
  ab = g[a,]
  nrow(ab)
  if (nrow(ab) == 0){
    print("A")
    return(FALSE)
  }
  IMPORTANT = ab[1,]
  IMPORTANT

  day1left = day1[IMPORTANT$number:nrow(day1),]
  break_through = day1left$timestamp[day1left$low < hi$low]
  break_through
  break_above = day1left$timestamp[day1left$high > hi$high]
  break_above = break_above[2:length(break_above)]
  print(break_through)
  print(break_above)
  
  print("HIODAHODA")
  
  print(break_through[1])
  print(break_above[1])

  if (length(x2) == 0){
    print("B")
    return(TRUE)
  }
  
  
  
  if (break_through[1] < break_above[1]) {
    print("C")
    return(FALSE)
  }
  
  print("D")
  return(TRUE)
}



analyze_function_bear_fvg <- function(hi){
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




