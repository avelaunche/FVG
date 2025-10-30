bear_fvg_plot <- function(day1){
  ggplot(day1, aes(x = timestamp, y = close)) + 
    geom_barchart(aes(open = open, close = close, high = high, low = low, alpha = bearish_fvg)) + 
    geom_hline(yintercept = day1$plus1high[(day1$bearish_fvg == TRUE)], col = "lightblue", linetype = "solid") +
    geom_hline(yintercept = day1$lag2_low[(day1$bearish_fvg == TRUE)], col = "lightgreen", linetype = "solid") + 
    geom_ma(ma_fun = EMA, n = 9, linetype = "solid", color = "darkblue") + 
    geom_ma(aes(volume = volume), ma_fun = VWMA, color = "purple", linetype = "solid")
}

bull_fvg_plot <- function(day1){
  ggplot(day1, aes(x = timestamp, y = close)) + 
    geom_barchart(aes(open = open, close = close, high = high, low = low, alpha = bullish_fvg)) + 
    geom_hline(yintercept = day1$lag2_high[(day1$bullish_fvg == TRUE)], col = "lightblue", linetype = "solid") +
    geom_hline(yintercept = day1$plus1low[(day1$bullish_fvg == TRUE)], col = "lightgreen", linetype = "solid") + 
    geom_ma(ma_fun = EMA, n = 9, linetype = "solid", color = "darkblue") + 
    geom_ma(aes(volume = volume), ma_fun = VWMA, color = "purple", linetype = "solid")
}
