
library(dplyr)
library(readr)
library(tidyr)
library(purrr)
library(lubridate)

#' Calculate the number of days between each of the dates in a vector of dates
#'
#' @param dates Vector of dates to use as breaks
#' @param count_by time interval to count by. Should be a lubridate accessor
#'   function. Defaults to lubridate::days(1)
#' @param include_last Whether to include counts for the last interval or not
#'
#' @return Vector of integers where each corresponds to the number of days between
#'   the provided dates at that index position
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

#' Compute day-weighted linear interpolation from one value to another
#'
#' @param from Starting value for the interpolation
#' @param to Ending value for the interpolation
#' @param day_weights Vector of weights to use in the interpolation
#'
#' @return Vector of interpolated values whose first value corresponds to `from`
#'   and whose last value corresponds to `to`
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


#' Helper to compute lagged values whose final groups are aggregated together
#' 
#' Instead of dropping values that have no corresponding value at a given lag
#' distance, this takes all values that would otherwise be dropped and sums
#' them to form the last value of the lagged vector. This is useful when
#' calculating sums in age bins, and the upper bin is a catch-all (e.g. 85+).
#' 
#' This is used when calculating CCR values.
#'
#' @param x vector of values to aggregate
#' @param lag lag distance
#'
#' @return Vector with same length as x. NA values start the vector where
#'   no lag value was found
#'
#' @examples
#' # This drops last 2 observations:
#' lag(c(1, 2, 3, 4), 2)
#' 
#' # This aggregates last observations and includes in the final element:
#' lag_agg(c(1, 2, 3, 4), 2)
lag_agg <- function(x, lag) {
  
  last_val <- sum(x[(length(x)-lag):length(x)])
  
  x <- lag(x, lag)
  
  x[length(x)] <- last_val
  
  x
  
}

#' Adjust a vector of CCR values based on provided caps.
#' 
#' @param pop Vector of population counts
#' @param ccr Vector of CCR values corresponding to the counts in `pop`
#' @param breaks Population breaks that define which groups should receive which
#'   CCR cap. For instance, c(0, 25, 100, Inf) would define 3 groups. Values of
#'   `pop` would be assigned the cap associated with the group that they fall
#'   into (see `caps`)
#' @param caps Vector of same length as `breaks` that defines the caps used
#'   for each of the groups defined by `breaks`
#' @param nan_cap Cap to use for CCR values that are NaN. This occurs when no
#'   population existed in either of the groups used to calculate the CCR.
#'   Defaults to 0.
#' @param inf_cap Cap to use for CCR values that are Inf. This occurs when no
#'   population existed in the denominator group used when calculating the CCR
#'   value. Defaults to NULL
#'
#' @return Vector of CCR values as provided in `ccr`, but with appropriate values
#'   capped.
ccr_cap <- function(pop, ccr, breaks, caps, nan_cap = 1, inf_cap = NULL) {
  
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
