# tract_estimates_male_female_by_age_2000_2010.r
# David Van Riper
# 2018-11-20
# 
# This script reads in the Census Bureau's 2000-2010 annual county estimates and preps it.
# I keep only the necessary columns, AGEGRPs and YEARs. For testing purposes, I only keep
# total pop (AGEGRP == 99) and YEAR < 13 (this gets rid of July 1, 2010 county estimate).
# 
# Updated on 2018-11-30
# I updated this script to include multiple variables (total pop, total male, total female)
# in output df.
# 
# I convert the original df to a tidy df using gather.
# 
# Updated on 2019-04-02
# I updated this script to process total pop, sex, age, and sex by age (including age 0 and 1-4).
# 
# Updated on 2020-02-19
# I updated this script to work on our big server and to use the here package

library(tidyverse)
library(here)
library(lubridate)
library(padr)

# constants 
popest_dir <- "/pkg/popgis/labpcs/data_projects/tract_estimates/2000_2010/data/popest/"
nhgis_dir <- "/pkg/popgis/labpcs/data_projects/tract_estimates/2000_2010/data/nhgis/"
outdata_dir <- "/pkg/popgis/labpcs/data_projects/tract_estimates/2000_2010/data/outdata/"
data_dir <- "/pkg/popgis/labpcs/data_projects/tract_estimates/2000_2010/data/"

setwd("/pkg/popgis/labpcs/data_projects/tract_estimates/2000_2010/data/popest/")

#### 0. Load the cum_days_interval CSV #### 
cum_days <- read_csv(paste0(data_dir, "cumulative_days_intervals_2000_2010.csv"))

#### 1. Import CB county estimates 2000-2010 ####
# AGEGRP = 99 (total)
# 1 = 4/1/2000 resident population estimates base
# 2 = 7/1/2000 resident population estimate
# 3 = 7/1/2001 resident population estimate
# 4 = 7/1/2002 resident population estimate
# 5 = 7/1/2003 resident population estimate
# 6 = 7/1/2004 resident population estimate
# 7 = 7/1/2005 resident population estimate
# 8 = 7/1/2006 resident population estimate
# 9 = 7/1/2007 resident population estimate
# 10 = 7/1/2008 resident population estimate
# 11 = 7/1/2009 resident population estimate
# 12 = 4/1/2010 resident 2010 Census population 
# 13 = 7/1/2010 resident population estimate


# 1. find all CSV files in working directory
temp <- list.files(pattern="*.csv")

# 2. merge the CSV df into one DF
# 2019-02-21 
#  - changed to read_csv to get the correct column types at ingest
c_est <- do.call(rbind,lapply(temp, read_csv))


# 3. select out necessary columns
# select out columns from STATE to TOT_POP because that's all I need for now
c_est<- select(c_est, STATE:TOT_FEMALE)

# 4. create a GISJOIN 
c_est<- mutate(c_est, GISJOIN = paste0("G",STATE,"0",COUNTY,"0"))

# 5. Create a new date column to convert YEAR to a date 
c_est<- mutate(c_est, date_str = case_when((YEAR == 1) ~ "2000-04-01",
                                           (YEAR == 2) ~ "2000-07-01",
                                           (YEAR == 3) ~ "2001-07-01",
                                           (YEAR == 4) ~ "2002-07-01",
                                           (YEAR == 5) ~ "2003-07-01",
                                           (YEAR == 6) ~ "2004-07-01",
                                           (YEAR == 7) ~ "2005-07-01",
                                           (YEAR == 8) ~ "2006-07-01",
                                           (YEAR == 9) ~ "2007-07-01",
                                           (YEAR == 10) ~ "2008-07-01",
                                           (YEAR == 11) ~ "2009-07-01",
                                           (YEAR == 12) ~ "2010-04-01",
                                           (YEAR == 13) ~ "2010-07-01"))

# 5a. Factorize AGEGRP
c_est$AGEGRP <- factor(c_est$AGEGRP, levels=c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,99), labels = c("age0", 
                                                                                                             "age1_4",
                                                                                                             "age5_9",
                                                                                                             "age10_14",
                                                                                                             "age15_19",
                                                                                                             "age20_24",
                                                                                                             "age25_29",
                                                                                                             "age30_34",
                                                                                                             "age35_39",
                                                                                                             "age40_44",
                                                                                                             "age45_49",
                                                                                                             "age50_54",
                                                                                                             "age55_59",
                                                                                                             "age60_64",
                                                                                                             "age65_69",
                                                                                                             "age70_74",
                                                                                                             "age75_79",
                                                                                                             "age80_84",
                                                                                                             "age85",
                                                                                                             "total"))


# 6. Convert date_str to date type 
c_est<- mutate(c_est, date1 = ymd(date_str))

# 7. Keep only needed columns for gathering 
c_est <- select(c_est, AGEGRP:date1, -date_str)

# 8. Gather c_est to make it long 
c_est <- gather(c_est, var_code, n, TOT_POP:TOT_FEMALE)

# 9. Recode values in var_code to match NHGIS standardized codes 
c_est <- c_est %>%
  mutate(var_code = case_when((var_code == "TOT_POP" & AGEGRP == "total") ~ "total_pop",
                              (var_code == "TOT_MALE" & AGEGRP == "total") ~ "total_male",
                              (var_code == "TOT_FEMALE" & AGEGRP == "total") ~ "total_female",
                              (var_code == "TOT_MALE" & AGEGRP == "age0") ~ "male_age0",
                              (var_code == "TOT_MALE" & AGEGRP == "age1_4") ~ "male_age1_4",
                              (var_code == "TOT_MALE" & AGEGRP == "age5_9") ~ "male_age5_9",
                              (var_code == "TOT_MALE" & AGEGRP == "age10_14") ~ "male_age10_14",
                              (var_code == "TOT_MALE" & AGEGRP == "age15_19") ~ "male_age15_19",
                              (var_code == "TOT_MALE" & AGEGRP == "age20_24") ~ "male_age20_24",
                              (var_code == "TOT_MALE" & AGEGRP == "age25_29") ~ "male_age25_29",
                              (var_code == "TOT_MALE" & AGEGRP == "age30_34") ~ "male_age30_34",
                              (var_code == "TOT_MALE" & AGEGRP == "age35_39") ~ "male_age35_39",
                              (var_code == "TOT_MALE" & AGEGRP == "age40_44") ~ "male_age40_44",
                              (var_code == "TOT_MALE" & AGEGRP == "age45_49") ~ "male_age45_49",
                              (var_code == "TOT_MALE" & AGEGRP == "age50_54") ~ "male_age50_54",
                              (var_code == "TOT_MALE" & AGEGRP == "age55_59") ~ "male_age55_59",
                              (var_code == "TOT_MALE" & AGEGRP == "age60_64") ~ "male_age60_64",
                              (var_code == "TOT_MALE" & AGEGRP == "age65_69") ~ "male_age65_69",
                              (var_code == "TOT_MALE" & AGEGRP == "age70_74") ~ "male_age70_74",
                              (var_code == "TOT_MALE" & AGEGRP == "age75_79") ~ "male_age75_79",
                              (var_code == "TOT_MALE" & AGEGRP == "age80_84") ~ "male_age80_84",
                              (var_code == "TOT_MALE" & AGEGRP == "age85") ~ "male_age85",
                              (var_code == "TOT_FEMALE" & AGEGRP == "age0") ~ "female_age0",
                              (var_code == "TOT_FEMALE" & AGEGRP == "age1_4") ~ "female_age1_4",
                              (var_code == "TOT_FEMALE" & AGEGRP == "age5_9") ~ "female_age5_9",
                              (var_code == "TOT_FEMALE" & AGEGRP == "age10_14") ~ "female_age10_14",
                              (var_code == "TOT_FEMALE" & AGEGRP == "age15_19") ~ "female_age15_19",
                              (var_code == "TOT_FEMALE" & AGEGRP == "age20_24") ~ "female_age20_24",
                              (var_code == "TOT_FEMALE" & AGEGRP == "age25_29") ~ "female_age25_29",
                              (var_code == "TOT_FEMALE" & AGEGRP == "age30_34") ~ "female_age30_34",
                              (var_code == "TOT_FEMALE" & AGEGRP == "age35_39") ~ "female_age35_39",
                              (var_code == "TOT_FEMALE" & AGEGRP == "age40_44") ~ "female_age40_44",
                              (var_code == "TOT_FEMALE" & AGEGRP == "age45_49") ~ "female_age45_49",
                              (var_code == "TOT_FEMALE" & AGEGRP == "age50_54") ~ "female_age50_54",
                              (var_code == "TOT_FEMALE" & AGEGRP == "age55_59") ~ "female_age55_59",
                              (var_code == "TOT_FEMALE" & AGEGRP == "age60_64") ~ "female_age60_64",
                              (var_code == "TOT_FEMALE" & AGEGRP == "age65_69") ~ "female_age65_69",
                              (var_code == "TOT_FEMALE" & AGEGRP == "age70_74") ~ "female_age70_74",
                              (var_code == "TOT_FEMALE" & AGEGRP == "age75_79") ~ "female_age75_79",
                              (var_code == "TOT_FEMALE" & AGEGRP == "age80_84") ~ "female_age80_84",
                              (var_code == "TOT_FEMALE" & AGEGRP == "age85") ~ "female_age85",
                              (var_code == "TOT_POP" & AGEGRP == "age0") ~ "age0",
                              (var_code == "TOT_POP" & AGEGRP == "age1_4") ~ "age1_4",
                              (var_code == "TOT_POP" & AGEGRP == "age5_9") ~ "age5_9",
                              (var_code == "TOT_POP" & AGEGRP == "age10_14") ~ "age10_14",
                              (var_code == "TOT_POP" & AGEGRP == "age15_19") ~ "age15_19",
                              (var_code == "TOT_POP" & AGEGRP == "age20_24") ~ "age20_24",
                              (var_code == "TOT_POP" & AGEGRP == "age25_29") ~ "age25_29",
                              (var_code == "TOT_POP" & AGEGRP == "age30_34") ~ "age30_34",
                              (var_code == "TOT_POP" & AGEGRP == "age35_39") ~ "age35_39",
                              (var_code == "TOT_POP" & AGEGRP == "age40_44") ~ "age40_44",
                              (var_code == "TOT_POP" & AGEGRP == "age45_49") ~ "age45_49",
                              (var_code == "TOT_POP" & AGEGRP == "age50_54") ~ "age50_54",
                              (var_code == "TOT_POP" & AGEGRP == "age55_59") ~ "age55_59",
                              (var_code == "TOT_POP" & AGEGRP == "age60_64") ~ "age60_64",
                              (var_code == "TOT_POP" & AGEGRP == "age65_69") ~ "age65_69",
                              (var_code == "TOT_POP" & AGEGRP == "age70_74") ~ "age70_74",
                              (var_code == "TOT_POP" & AGEGRP == "age75_79") ~ "age75_79",
                              (var_code == "TOT_POP" & AGEGRP == "age80_84") ~ "age80_84",
                              (var_code == "TOT_POP" & AGEGRP == "age85") ~ "age85"))


###############
# NHGIS data prep
# 
# Read in tract and county CSVs
# Create correct variables that match county estimates

# 0. Read in standardized tract and county data
t <- read_csv(paste0(nhgis_dir, "nhgis1133_ts_geog2010_tract.csv"), guess_max = 200000)
c <- read_csv(paste0(nhgis_dir, "nhgis1133_ts_geog2010_county.csv"), guess_max = 200000)

# 1. keep 2000 and 2010 
t <- t %>%
  filter(DATAYEAR == 2000 | DATAYEAR == 2010)

c <- c %>% 
  filter(DATAYEAR == 2000 | DATAYEAR == 2010)

# 2. create necessary age groups that match pop estimates 
t <- t %>%
  mutate(
    male_age0 = CN8AA,
    male_age1_4 = CN8AB  + CN8AC  + CN8AD  + CN8AE,
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
    female_age0 = CN8AU,
    female_age1_4 = CN8AV + CN8AW + CN8AX + CN8AY,
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
  select(GISJOIN, DATAYEAR, STATE, male_age0:female_age85) %>%
  gather(var_code, n, male_age0:female_age85)

c <- c %>%
  mutate(
    male_age0 = CN8AA,
    male_age1_4 = CN8AB  + CN8AC  + CN8AD  + CN8AE,
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
    female_age0 = CN8AU,
    female_age1_4 = CN8AV + CN8AW + CN8AX + CN8AY,
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
  select(GISJOIN, DATAYEAR, STATE, male_age0:female_age85) %>%
  gather(var_code, n, male_age0:female_age85)


# 3. Create df containing males by age 
t_male <- t %>%
  filter(str_detect(var_code, "^male"))

# 4. Create df containing females by age 
t_female <- t %>%
  filter(str_detect(var_code, "^female"))

########################################################
# Step 2 - linear interpolation of tracts and counties
######################################################## 
# Updated: 2020-02-25
# I re-wrote the cumulative days function to use a case_when statement. The cumsum function operating on a difftime -> numeric cast field
# was taking forever. I think the conversion from difftime to numeric object/field was/is super slow. 

# loads functions from script
source("/pkg/popgis/labpcs/data_projects/tract_estimates/2000_2010/scripts/tidy_funs_tract_county_estimates.r")

# 0. set begin and end values, convert to dates and find number of days between them
begin_date <- ymd("2000-04-01")
end_date <- ymd("2010-04-01")

# a. set the decade interval
decade_interval <- interval(begin_date, end_date)

# b. find number of days in interval 
days <- decade_interval / days(1)

# 1. Create appropriate date fields in t_female 
t_female <- t_female %>%
  mutate(date_str = case_when((DATAYEAR == 1990) ~ "1990-04-01",
                              (DATAYEAR == 2000) ~ "2000-04-01",
                              (DATAYEAR == 2010) ~ "2010-04-01")) %>%
  mutate(date1 = ymd(date_str))

# 2. Create appropriate date fields in t_male 
t_male <- t_male %>%
  mutate(date_str = case_when((DATAYEAR == 1990) ~ "1990-04-01",
                              (DATAYEAR == 2000) ~ "2000-04-01",
                              (DATAYEAR == 2010) ~ "2010-04-01")) %>%
  mutate(date1 = ymd(date_str))

# 3. Create appropriate date fields in c
c <- c %>%
  mutate(date_str = case_when((DATAYEAR == 1990) ~ "1990-04-01",
                              (DATAYEAR == 2000) ~ "2000-04-01",
                              (DATAYEAR == 2010) ~ "2010-04-01")) %>%
  mutate(date1 = ymd(date_str))

# 4. Compute difference between 2000 and 2010 data 
t_female <- compute_diff(t_female, d2010, n, var_code, GISJOIN)
t_male <- compute_diff(t_male, d2010, n, var_code, GISJOIN)
c <- compute_diff(c, d2010, n, var_code, GISJOIN)

# 5. Add records to hold the intercensal values
t_female <- pad_months(t_female, GISJOIN, var_code, date1, "2000-04-01", "2010-04-01")
t_male <- pad_months(t_male, GISJOIN, var_code, date1, "2000-04-01", "2010-04-01")
c <- pad_months(c, GISJOIN, var_code, date1, "2000-04-01", "2010-04-01")

# 6. Join the cum_days df to t_female, t_male, and c 
t_female <- t_female %>%
  left_join(cum_days, by = c("date1" = "date"))

t_male <- t_male %>%
  left_join(cum_days, by = c("date1" = "date"))

c <- c %>%
  left_join(cum_days, by = c("date1" = "date"))

# 6. Compute cumulative sum of days_interval for each group for the 2000-2010 time period
#t_female <- compute_cum_days_interval_2000_2010(t_female, date1, cum_days_interval)
#t_male <- compute_cum_days_interval_2000_2010(t_male, date1, cum_days_interval)
#c <- compute_cum_days_interval_2000_2010(c, date1, cum_days_interval)

# 7. fill diff and CL8AA down by group 
t_female <- fill_down(t_female, GISJOIN, var_code, n)
t_male <- fill_down(t_male, GISJOIN, var_code, n)
c <- fill_down(c, GISJOIN, var_code, n)

# 8. Compute the CL8AA value using the following:
t_female <- linear_interpolation(t_female, n, n_diff, cum_days_interval)
t_male <- linear_interpolation(t_male, n, n_diff, cum_days_interval)
c <- linear_interpolation(c, n, n_diff, cum_days_interval)

########################################################
# Step 3 - adjust tract linear interpolations to sum to
# county estimates
######################################################## 
# This is the third step that actually creates the annual estimate for each tract. It first 
# processes the county data (county estimates from Census Bureau and linearly interpolated county 
# estimates). It joins the two data frames together on GISJOIN and date1, and then computes the 
# ratio between variables from each df. 
# 
# It then reduces the final county df down to the needed variables. 
# 
# Next, the script adds the county GISJOIN to each record in the tract df. It then joins the 
# county df to the tract df on the county GISJOIN and date1. 
# 
# FINALLY, the script multiplies the linearly interpolated variable by the county ratio to yield 
# the adjusted tract value. This final step would be run multiple times if we were processing 
# multiple variables. 
# 
# Updated 2018-11-30
# I updated the script to work with var_code instead of actual variable names. Looks like it works
# fine!
# 
# Updated 2020-02-25
# I updated this script for 2000-2010 to only handle the t_female and t_male dataframes because
# that's all we need for this product.

# join CB population estimates to county linear interpolation (t) on GISJOIN and date1 
c <- left_join(c, c_est, by=c("GISJOIN","date1","var_code"))

# compute ratio of county estimate to linear interpolation value 
# this step would need to be run more times if we had more than 1 variable we were estimating
c <- mutate(c, var_ratio = n.y / n.x)

# keep only needed vars for tract interpolation
# this step would need to keep more variables if we're estimating more than 1 variable
c <- select(c, GISJOIN, var_code, date1, n.x, n.y, var_ratio)

#create a GISJOIN_CTY on tract data
t_male <- mutate(t_male, GISJOIN_CTY = substr(GISJOIN, 1, 8))
t_female <- mutate(t_female, GISJOIN_CTY = substr(GISJOIN, 1, 8))

# join county data to tract for final processing 
t_male <- left_join(t_male, c, by=c("GISJOIN_CTY" = "GISJOIN", "date1"="date1", "var_code" = "var_code"))
t_female <- left_join(t_female, c, by=c("GISJOIN_CTY" = "GISJOIN", "date1"="date1", "var_code" = "var_code"))

# multiply tract linear interpolation estimate by value of county ratio 
# - this gives me a tract estimate adjusted by the ratio of the county estimate to the linearly
# interpolated county value
t_male <- mutate(t_male, n_est = n * var_ratio)
t_female <- mutate(t_female, n_est = n * var_ratio)

# fields to keep in output data
t_male <- select(t_male, GISJOIN, STATE, date1, cum_days_interval, var_code, n_diff, n.x, n.y, var_ratio, n, n_est)
t_female <- select(t_female, GISJOIN, STATE, date1, cum_days_interval, var_code, n_diff, n.x, n.y, var_ratio, n, n_est)

# write out the CSV files
write_csv(t_male, paste0(outdata_dir, "tct00_10_male.csv"))
write_csv(t_female, paste0(outdata_dir, "tct00_10_female.csv"))



