calculate_vwap = function(hi_fvg){
  b = hi_fvg |>
    group_by(date) |>
    mutate(vwap = calculate_vwap_1sd(high, low, close, volume))
  
  vwap = b$vwap
  b$vwap = NULL
  b = cbind(b, vwap)
  return(b)
}

graph_vwap = function(day1){
  ggplot(day1, aes()) + 
    geom_barchart(aes(x = timestamp, y = close, open = open, close = close, high = high, low = low, alpha = bullish_fvg)) + 
    geom_line(aes(x = timestamp, y = VWAP), col = "green") + 
    geom_line(aes(x = timestamp, y = Upper_1sd), col = "blue") + 
    geom_line(aes(x = timestamp, y = Lower_1sd), col = "blue")
}

calculate_vwap_1sd <- function(high, low, close, volume, session_id = NULL) {
  price <- (high + low + close) / 3
  session_vwap <- function(p, v) {
    n <- length(p)
    vwap  <- numeric(n)
    sigma <- numeric(n)
    cum_vol <- 0
    cum_pv  <- 0
    cum_p2v <- 0
    
    for (i in seq_len(n)) {
      cum_vol <- cum_vol + v[i]
      cum_pv  <- cum_pv + p[i] * v[i]
      cum_p2v <- cum_p2v + (p[i]^2) * v[i]
      
      vwap[i] <- cum_pv / cum_vol
      var_w   <- (cum_p2v / cum_vol) - (vwap[i]^2)
      sigma[i] <- sqrt(pmax(var_w, 0))
    }
    
    data.frame(
      VWAP      = vwap,
      Upper_1sd = vwap + sigma,
      Lower_1sd = vwap - sigma
    )
  }
  
  if (is.null(session_id)) {
    output <- session_vwap(price, volume)
  } else {
    output <- do.call(rbind, lapply(split(seq_along(price), session_id), function(idx) {
      session_vwap(price[idx], volume[idx])
    }))
    rownames(output) <- NULL
  }
  
  return(output)
}


