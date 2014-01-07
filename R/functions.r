
geom_night <- function(fig, start = 0, end = 5, tz = 'UTC',
                       fill = 'blue', alpha = .1) {
  require(ggplot2)
  require(lubridate)
  require(plyr)
  ts <- ggplot_build(fig)$data[[1]]$x
  ts <- as.POSIXlt(ts, origin = "1970-01-01", tz = tz)
  ts <- data.frame(stamp = ts, 
                   date = floor_date(ts, 'day'))
  df <- ldply(unique(ts$date), function(date) {
    # First day
    if(date == min(ts$date)) {
      stamps <- ts$stamp[ts$date == date]
      # Does first day include night?
      if(hour(min(stamps)) > end) return(c(xmin = NA, xmax = NA))
      
      # Start of night
      startstamp <- ifelse(hour(min(stamps)) >= start,
                           min(stamps),
                           update(date, hour = start))
      endstamp <- update(date, hour = end + 1)
      
      return(c(xmin = startstamp, xmax = endstamp))
    }
    
    # Last day
    if(date == max(ts$date)) {
      stamps <- ts$stamp[ts$date == date]
      # Does last day include night?
      if(hour(max(stamps)) < start) return(c(xmin = NA, xmax = NA))
      
      # End of night
      endstamp <- ifelse(hour(max(stamps)) < end,
                         max(stamps),
                         update(date, hour = end + 1))
      startstamp <- update(date, hour = start)
      
      return(c(xmin = startstamp, xmax = endstamp))
      
    }
    
    startstamp <- update(date, hour = start)
    endstamp <- update(date, hour = end + 1)
    c(xmin = startstamp, xmax = endstamp)
    
    
  })
  df[] <- llply(df, as.POSIXlt, origin = origin)
  
  geom_rect(data = df,
            aes(xmin = xmin, xmax = xmax,
                ymin = -Inf, ymax = Inf), 
            fill = fill, alpha = alpha)
}



