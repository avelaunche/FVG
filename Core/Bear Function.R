hi = fvg_analysis_bear[8,]

analyze_function_bear_fvg <- function(hi, day1){
  #define variables
  low = hi$low
  number = hi$number
  l = (number+1):nrow(day1)
  number
  l
  g = day1[l,]
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
  if (length(break_below) == 1 | length(break_below) == 0){
    print("A")
    return(FALSE)
  }
  break_below = break_below[2:length(break_below)]
  
  if (length(break_through) == 0){
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

day1 = day2

analyze_function_bear_fvg(hi, day2)

