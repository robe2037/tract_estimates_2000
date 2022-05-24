# funs_tract_county_estimates.r
# David Van Riper
# 2018-11-26
# 
# This script stores the functions used to create annual tract estimates for NHGIS. I am pushing
# off the funs to another script to simplify the rest of the process.
# 

library(tidyverse)
library(lubridate)
library(padr)

# fun select two years of data from an NHGIS standardized extract. 
select_years <- function(x, year1, year2) {
  x <- filter(x, DATAYEAR == year1 | DATAYEAR == year2)
  return(x)
}

# fun selects out a state's worth of county or tract data
# used for testing purposes because processing whole country can be slow
# - expects a fully spelled out state name (e.g., "Minnesota") as a parameter
select_state <- function(x, state) {
  x <- filter(x, STATE == state)
  return(x)
}

# fun computes difference between 2000 and 2010 values of a single variable for a given GISJOIN value
# - stores the difference in diff_var 
# - now this function uses var2 as the base for the _diff variable, which is what I want
# t <- compute_diff(t, d2010, n, var_code, GISJOIN)
compute_diff <- function(x, var1, var2,  group_var_n, group_var_geog){
#  diff_var <- enquo(diff_var)
  var1 <- enquo(var1)
  var2 <- enquo(var2)
  group_var_n <- enquo(group_var_n)
  group_var_geog <- enquo(group_var_geog)
  
  diff_var_name <- paste0(quo_name(var2),"_diff")
  var1_name <- paste0(quo_name(var1))
  
  x <- x %>%
    group_by(!!group_var_geog, !!group_var_n) %>%  # group by GISJOIN and var_code 
    mutate(!!var1_name := lead(!!var2)) %>%
    mutate(!!diff_var_name := !!var1 - !!var2) # compute difference between YYYY1 and YYYY2
  
  return(x)
  
}

# fun uses padr::pad function to insert records for each month between start_date and end_date 
# - then filters for only records that equal start_date, end_date and all July 1sts (which is the 
# day the pop estimates are for)
pad_months <- function(x, group_var_geog, group_var_n, date_var, start_date, end_date){
  group_var_geog <- enquo(group_var_geog)
  group_var_n <- enquo(group_var_n)
  date_var <- enquo(date_var)
  start_date <- enquo(start_date)
  end_date <- enquo(end_date)
  
  start_date_str <- quo_name(start_date)
  end_date_str <- quo_name(end_date)
  
  x <- x %>% 
    group_by(!!group_var_geog, !!group_var_n) %>%
    pad('month', break_above = 180)  %>% 
    filter(!!date_var == start_date_str | !!date_var == end_date_str | month(!!date_var) == 7)
  
  return(x)
}

# fun fills down values from April 1, YYYY1 to April 1, YYYY2 for two variables
fill_down <- function(x, group_var_geog, group_var_n, fill_var1){
  group_var_geog <- enquo(group_var_geog)
  group_var_n <- enquo(group_var_n)
  fill_var1 <- enquo(fill_var1)
#  fill_var2 <- enquo(fill_var2)
  
  fill_var1_diff <- paste0(quo_name(fill_var1),"_diff")
  
  x <- x %>%
    group_by(!!group_var_geog, !!group_var_n) %>%
    fill(!!fill_var1, !!fill_var1_diff)
  
  return(x)
}

# fun fills down GISJOIN_CTY values from April 1, YYYY1 to April 1, YYYY2 for GISJOIN and var_codes 
fill_down_county <- function(x, group_var_geog, group_var_n, fill_var1){
  group_var_geog <- enquo(group_var_geog)
  group_var_n <- enquo(group_var_n)
  fill_var1 <- enquo(fill_var1)
  #  fill_var2 <- enquo(fill_var2)
  
  #fill_var1_diff <- paste0(quo_name(fill_var1),"_diff")
  
  x <- x %>%
    group_by(!!group_var_geog, !!group_var_n) %>%
    fill(!!fill_var1)
  
  return(x)
}
# fun computes number of days between each record for a given GISJOIN
compute_day_interval <- function(x, group_var_geog, group_var_n, day_interval, date_var ){
  group_var_geog <- enquo(group_var_geog)
  group_var_n <- enquo(group_var_n)
  day_interval <- enquo(day_interval)
  date_var <- enquo(date_var)
  
  day_interval_name <- paste0(quo_name(day_interval))
  
  x <- x %>%
    group_by(!!group_var_geog, !!group_var_n) %>%
    mutate(!!day_interval_name := !!date_var - lag(!!date_var, default = first(!!date_var)))
  #    t <- mutate(t, day_interval = date1 - lag(date1, default = first(date1)))
  
  return(x)
}

# fun computes the cumulative sum of days for each record for a given GISJOIN
compute_cum_sum <- function(x, group_var_geog, group_var_n, cum_var, interval_var){
  group_var_geog = enquo(group_var_geog)
  group_var_n = enquo(group_var_n)
  cum_var = enquo(cum_var)
  interval_var = enquo(interval_var)
  
  cum_var_name <- paste0(quo_name(cum_var))
  
  x <- x %>%
    group_by(!!group_var_geog, !!group_var_n) %>%
    mutate(!!cum_var_name := cumsum(as.numeric(!!interval_var)))
  
  return(x) 
}

# fun sets cum_days_interval to particular value based on date value for 2000-2010 time period. I have pre-computed these 
# intervals and hard coded them into the function.
compute_cum_days_interval_2010_2019 <- function(x, date_var, cum_var){
  cum_var = enquo(cum_var)
  date_var = enquo(date_var)
  
  cum_var_name <- paste0(quo_name(cum_var))
  
  x <- x %>%
    mutate(!!cum_var_name := case_when(!!date_var == "2010-04-01" ~ 0,
                                       !!date_var == "2010-07-01" ~ 91,
                                       !!date_var == "2011-07-01" ~ 456,
                                       !!date_var == "2012-07-01" ~ 822,
                                       !!date_var == "2013-07-01" ~ 1187,
                                       !!date_var == "2014-07-01" ~ 1552,
                                       !!date_var == "2015-07-01" ~ 1917,
                                       !!date_var == "2016-07-01" ~ 2283,
                                       !!date_var == "2017-07-01" ~ 2648,
                                       !!date_var == "2018-07-01" ~ 3013,
                                       !!date_var == "2019-07-01" ~ 3378,
                                       !!date_var == "2020-04-01" ~ 0))
  
  return(x)
}


# fun sets cum_days_interval to particular value based on date value for 2000-2010 time period. I have pre-computed these 
# intervals and hard coded them into the function.
compute_cum_days_interval_2000_2010 <- function(x, date_var, cum_var){
  cum_var = enquo(cum_var)
  date_var = enquo(date_var)
  
  cum_var_name <- paste0(quo_name(cum_var))
  
  x <- x %>%
    mutate(!!cum_var_name := case_when(!!date_var == "2000-04-01" ~ 0,
                                       !!date_var == "2000-07-01" ~ 91,
                                       !!date_var == "2001-07-01" ~ 456,
                                       !!date_var == "2002-07-01" ~ 821,
                                       !!date_var == "2003-07-01" ~ 1186,
                                       !!date_var == "2004-07-01" ~ 1552,
                                       !!date_var == "2005-07-01" ~ 1917,
                                       !!date_var == "2006-07-01" ~ 2282,
                                       !!date_var == "2007-07-01" ~ 2647,
                                       !!date_var == "2008-07-01" ~ 3013,
                                       !!date_var == "2009-07-01" ~ 3378,
                                       !!date_var == "2010-04-01" ~ 0))
  
  return(x)
}

# fun sets cum_days_interval to particular value based on date value for the 1990-2000 time period. I have pre-computed these 
# intervals and hard coded them into the function.
compute_cum_days_interval_1990_2000 <- function(x, date_var, cum_var){
  cum_var = enquo(cum_var)
  date_var = enquo(date_var)
  
  cum_var_name <- paste0(quo_name(cum_var))
  
  x <- x %>%
    mutate(!!cum_var_name := case_when(!!date_var == "1990-04-01" ~ 0,
                                       !!date_var == "1990-07-01" ~ 91,
                                       !!date_var == "1991-07-01" ~ 456,
                                       !!date_var == "1992-07-01" ~ 822,
                                       !!date_var == "1993-07-01" ~ 1187,
                                       !!date_var == "1994-07-01" ~ 1552,
                                       !!date_var == "1995-07-01" ~ 1917,
                                       !!date_var == "1996-07-01" ~ 2283,
                                       !!date_var == "1997-07-01" ~ 2648,
                                       !!date_var == "1998-07-01" ~ 3013,
                                       !!date_var == "1999-07-01" ~ 3378,
                                       !!date_var == "2000-04-01" ~ 0))
  
  return(x)
}

# fun resets the cum_days_interval for April 1, YYYY2 to zero
# - this way the linear_interpolation function will just return the actual variable value for the
# April 1, YYYY2 decennial census
reset_cum_days_interval_zero <- function(x, interval_var, year){
  interval_var = enquo(interval_var)
  year = enquo(year)
  
  interval_var_name <- paste0(quo_name(interval_var))
  
  x <- x %>% 
    mutate(!!interval_var_name := replace(!!interval_var, DATAYEAR == !!year, 0))
  
  return(x)
}

# fun computes the linear interpolation for var1
# - computes the proportion of the cumulative sum of days that have passed between April 1, YYYY1 -
# date of record and the number of days in the full interval (10 years)
# - multiplies the proportion by the value of the diff_var (difference between YYYY1 and YYYY2 value 
# of variable)
# - adds that value to the var1 value for the record
linear_interpolation <- function(x, var1, diff_var, interval_var){
  var1 = enquo(var1)
  interval_var = enquo(interval_var)
  diff_var = enquo(diff_var)
  
  var1_name = paste0(quo_name(var1))

  x <- x %>%
    mutate(!!var1_name := !!var1 + ((!!interval_var / days) * (!!diff_var)))
  
  #  t <- mutate(t, CL8AA = CL8AA + ((cum_days_interval / days) * pdiff))
  return(x)
}

# fun to compute sex/age groups for CCR and CTW calculations
# - takes in a single df and computes correct sex by age cohorts
# - converts wide df to long df
# - returns long df with extra variables 
compute_age_sex_cohorts <- function(x){
  x <- x %>%
    mutate(
      male_age0_4 = CN8AA + CN8AB  + CN8AC  + CN8AD  + CN8AE,
      male_age5_9 = CN7AB,
      male_age10_14 = CN7AC,
      male_age15_19 = CN7AD + CN7AE,
      male_age20_24 = CN7AF + CN7AG + CN7AH,
      male_age25_29 = CN7AI,
      male_age30_34 = CN7AJ,
      male_age35_39 = CN7AK,
      male_age40_44 = CN7AL,
      male_age45_49 = CN7AM,
      male_age50_54 = CN7AN,
      male_age55_59 = CN7AO,
      male_age60_64 = CN7AP + CN7AQ,
      male_age65_69 = CN7AR + CN7AS,
      male_age70_74 = CN7AT,
      male_age75_79 = CN7AU,
      male_age80_84 = CN7AV,
      male_age85 = CN7AW,
      female_age0_4 = CN8AU + CN8AV + CN8AW + CN8AX + CN8AY,
      female_age5_9 = CN7AY,
      female_age10_14 = CN7AZ,
      female_age15_19 =  CN7BA + CN7BB,
      female_age20_24 =  CN7BC  + CN7BD  + CN7BE,
      female_age25_29 =  CN7BF,
      female_age30_34 =  CN7BG,
      female_age35_39 =  CN7BH,
      female_age40_44 =  CN7BI,
      female_age45_49 =  CN7BJ,
      female_age50_54 =  CN7BK,
      female_age55_59 =  CN7BL,
      female_age60_64 = CN7BM + CN7BN,
      female_age65_69 = CN7BO + CN7BP,
      female_age70_74 =  CN7BQ,
      female_age75_79 =  CN7BR,
      female_age80_84 = CN7BS,
      female_age85 = CN7BT
    ) %>%
    select(GISJOIN, DATAYEAR, male_age0_4:female_age85) %>%
    gather(var_code, n, male_age0_4:female_age85)
  
  return(x)
}

# fun to compute wide data frame for age/sex cohorts 
# - takes in single long df
# - pivots it wider so output can be used to compute CCRs
create_wide_age_cohort_df <- function(x){
  x <- x %>%
    mutate(uniq_code = paste0("y", DATAYEAR, "_", var_code)) %>%
    select(-var_code, -DATAYEAR) %>%
    pivot_wider(names_from = uniq_code, values_from = n)
  
  return(x)
  
}

# fun to compute CCRs for age/sex cohorts 
# - takes in a single df
# - computes CCRs
compute_ccr <- function(x){
  
  x <- x %>%
    mutate(ccr_male_age10_14 = y2010_male_age10_14 / y2000_male_age0_4,
           ccr_male_age15_19 = y2010_male_age15_19 / y2000_male_age5_9,
           ccr_male_age20_24 = y2010_male_age20_24 / y2000_male_age10_14,
           ccr_male_age25_29 = y2010_male_age25_29 / y2000_male_age15_19,
           ccr_male_age30_34 = y2010_male_age30_34 / y2000_male_age20_24,
           ccr_male_age35_39 = y2010_male_age35_39 / y2000_male_age25_29,
           ccr_male_age40_44 = y2010_male_age40_44 / y2000_male_age30_34,
           ccr_male_age45_49 = y2010_male_age45_49 / y2000_male_age35_39,
           ccr_male_age50_54 = y2010_male_age50_54 / y2000_male_age40_44,
           ccr_male_age55_59 = y2010_male_age55_59 / y2000_male_age45_49,
           ccr_male_age60_64 = y2010_male_age60_64 / y2000_male_age50_54,
           ccr_male_age65_69 = y2010_male_age65_69 / y2000_male_age55_59,
           ccr_male_age70_74 = y2010_male_age70_74 / y2000_male_age60_64,
           ccr_male_age75_79 = y2010_male_age75_79 / y2000_male_age65_69,
           ccr_male_age80_84 = y2010_male_age80_84 / y2000_male_age70_74,
           ccr_male_age85 = y2010_male_age85 / (y2000_male_age75_79 + y2000_male_age80_84 + y2000_male_age85),
           ccr_female_age10_14 = y2010_female_age10_14 / y2000_female_age0_4,
           ccr_female_age15_19 = y2010_female_age15_19 / y2000_female_age5_9,
           ccr_female_age20_24 = y2010_female_age20_24 / y2000_female_age10_14,
           ccr_female_age25_29 = y2010_female_age25_29 / y2000_female_age15_19,
           ccr_female_age30_34 = y2010_female_age30_34 / y2000_female_age20_24,
           ccr_female_age35_39 = y2010_female_age35_39 / y2000_female_age25_29,
           ccr_female_age40_44 = y2010_female_age40_44 / y2000_female_age30_34,
           ccr_female_age45_49 = y2010_female_age45_49 / y2000_female_age35_39,
           ccr_female_age50_54 = y2010_female_age50_54 / y2000_female_age40_44,
           ccr_female_age55_59 = y2010_female_age55_59 / y2000_female_age45_49,
           ccr_female_age60_64 = y2010_female_age60_64 / y2000_female_age50_54,
           ccr_female_age65_69 = y2010_female_age65_69 / y2000_female_age55_59,
           ccr_female_age70_74 = y2010_female_age70_74 / y2000_female_age60_64,
           ccr_female_age75_79 = y2010_female_age75_79 / y2000_female_age65_69,
           ccr_female_age80_84 = y2010_female_age80_84 / y2000_female_age70_74,
           ccr_female_age85 = y2010_female_age85 / (y2000_female_age75_79 + y2000_female_age80_84 + y2000_female_age85))
  
  return(x)
}

compute_cohort_size_flag <- function(x){
  x<- x %>%
    filter(DATAYEAR == 2010) %>%
    filter(!var_code %in% c("male_age0_4", "male_age5_9", "female_age0_4", "female_age5_9")) %>%
    mutate(ccr_cap = case_when(n < 25 ~ 1,
                               n >= 25 & n < 100 ~ 2,
                               n >= 100 ~ 0))
  
  return(x)
  
}

create_long_ccr_df <- function(x){
  x <- x %>%
    select(GISJOIN, ccr_male_age10_14:ccr_female_age85) %>%
    pivot_longer(cols = c(ccr_male_age10_14:ccr_female_age85), names_to = "var_code", values_to = "ccr") %>%
    mutate(age_code = str_remove(var_code, "ccr_"))
  
  return(x)
}

set_ccr_caps <- function(x){
  x <- x %>%
    mutate(ccr_final = case_when((ccr_cap == 1 & ccr > 1.0) ~ 1.0,
                                 (ccr_cap == 2 & ccr > 2.0) ~ 2.0,
                                 is.nan(ccr) ~ 0.0,
                                 is.infinite(ccr) ~ 2.0,
                                 (ccr_cap == 0 & ccr > 2.0) ~ 2.0,
                                 TRUE ~ ccr))
  
  return(x)
  
}

compute_ctw <- function(x){
  x <- x %>%
    mutate(ctw_male_age0_4 = (y2010_male_age0_4) / (y2010_female_age20_24 + y2010_female_age25_29 + y2010_female_age30_34 + y2010_female_age35_39 + y2010_female_age40_44),
           ctw_female_age0_4 = (y2010_female_age0_4) / (y2010_female_age20_24 + y2010_female_age25_29 + y2010_female_age30_34 + y2010_female_age35_39 + y2010_female_age40_44),
           ctw_male_age5_9 = (y2010_male_age5_9) / (y2010_female_age30_34 + y2010_female_age35_39 + y2010_female_age40_44 + y2010_female_age45_49),
           ctw_female_age5_9 = (y2010_female_age5_9) / (y2010_female_age30_34 + y2010_female_age35_39 + y2010_female_age40_44 + y2010_female_age45_49))
  
  return(x)
}

create_long_ctw_df <- function(x){
  x <- x %>%
    select(GISJOIN, ctw_male_age0_4:ctw_female_age5_9) %>%
    pivot_longer(cols = c(ctw_male_age0_4:ctw_female_age5_9), names_to = "var_code", values_to = "ctw")
  
  return(x)
}

replace_nan_inf_ctw <- function(x){
  x <- x %>%
    mutate(ctw_final = case_when(is.nan(ctw) ~ 0.0,
                                 is.infinite(ctw) ~ 2.0,
                                 TRUE ~ ctw))
  
  return(x)    
}

create_females_20_44 <- function(x){
  y <- x %>%
    mutate(recode = case_when(var_code %in% c("female_age20_24", "female_age25_29", "female_age30_34", "female_age35_39", "female_age40_44") ~ "female_age20_44")) %>%
    filter(!is.na(recode)) %>%
    group_by(GISJOIN) %>%
    summarise(n = sum(n))
  
  return(y)
}

create_females_30_49 <- function(x){
  y <- x %>%
    mutate(recode = case_when(var_code %in% c("female_age30_34", "female_age35_39", "female_age40_44", "female_age45_49") ~ "female_age30_49")) %>%
    filter(!is.na(recode)) %>%
    group_by(GISJOIN) %>%
    summarise(n = sum(n))
  
  return(y)
}