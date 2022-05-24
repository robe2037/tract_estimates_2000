
library(tidyverse)
library(lubridate)

count_between <- function(dates, 
                          count_by = lubridate::days(1), 
                          include_last = TRUE) {
  
  d_end <- lead(dates)[1:(length(dates)-1)]
  d_start <- dates[1:(length(dates)-1)]
  
  cumulative <- cumsum(
    purrr::map2_dbl(
      d_start,
      d_end,
      ~lubridate::interval(.x, .y) %/% count_by
    )
  )
  
  if (!include_last) {
    cumulative[length(cumulative)] <- 0
  }
  
  c(0, cumulative)
  
}

interp <- function(from, to, day_weights) {
  
  out <- purrr::map2(
    from,
    to,
    ~.x + (day_weights / max(day_weights)) * (.y - .x)
  )
  
  if (length(from) == 1) {
    out <- out[[1]]
  }
  
  out
  
}


lag_agg <- function(x, lag) {
  
  last_val <- sum(x[(length(x)-lag):length(x)])
  
  x <- lag(x, lag)
  
  x[length(x)] <- last_val
  
  x
  
}

ccr_cap <- function(pop, ccr, breaks, caps, nan_cap = 0, inf_cap = NULL) {
  
  if(is.null(inf_cap)) inf_cap <- caps[length(caps)]
  
  breaks <- sort(breaks)
  
  if (breaks[1] != 0) breaks <- c(0, breaks)
  if (breaks[length(breaks)] != Inf) breaks <- c(breaks, Inf)
  
  pop_grp <- cut(pop, breaks = breaks, right = FALSE, labels = 1:(length(breaks) - 1))
  
  purrr::walk2(
    as.numeric(levels(pop_grp)),
    caps,
    ~{
      ccr[pop_grp == .x & ccr > .y & !is.na(ccr) & 
            !is.infinite(ccr) & !is.nan(ccr)] <<- .y
    }
  )
  
  ccr[is.nan(ccr)] <- nan_cap
  ccr[is.infinite(ccr)] <- inf_cap
  
  ccr
  
}
