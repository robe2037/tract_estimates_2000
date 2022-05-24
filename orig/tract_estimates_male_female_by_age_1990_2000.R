# tract_estimates_male_female_by_age_1990_2000.r
# David Van Riper
# 2018-11-20
# 
# This script reads in the Census Bureau's 1990-2000 annual county estimates and preps it.
# 
# Updated on 2018-11-30
# I updated this script to include multiple variables (total pop, total male, total female)
# in output df.
# 
# I convert the original df to a tidy df using gather.
# 
# Updated on 2019-04-10
# I updated this script to process total pop, sex, age, and sex by age (including age 0 and 1-4) for 
# 1990 - 2000 datasets
# 
# Updated on 2019-05-02
# I updated this script to create only female by age and male by age tbls, which will then be 
# 'rolled-up' to sex subtotals, age subtotals, and total populations
# 
# It's marginally faster than the prior scripts because it doesn't also consider age and totals 
# 
# Updated on 2020-02-25
# Need to adapt script for Linux files and file paths - DONE on 2020-03-04
# 

library(tidyverse)
library(lubridate)
library(padr)
library(here)

# load functions from script
source("/pkg/popgis/labpcs/data_projects/tract_estimates/2000_2010/scripts/tidy_funs_tract_county_estimates.r")

# constants 
popest_dir <- "/pkg/popgis/labpcs/data_projects/tract_estimates/1990_2000/data/popest/"
nhgis_dir <- "/pkg/popgis/labpcs/data_projects/tract_estimates/1990_2000/data/nhgis/"
outdata_dir <- "/pkg/popgis/labpcs/data_projects/tract_estimates/1990_2000/data/outdata/"
data_dir <- "/pkg/popgis/labpcs/data_projects/tract_estimates/1990_2000/data/"

setwd("/pkg/popgis/labpcs/data_projects/tract_estimates/1990_2000/data/popest/")

#### 0. Load the cum_days_interval CSV #### 
cum_days <- read_csv(paste0(data_dir, "cumulative_days_intervals_1990_2000.csv"))


###### Prepare the 1990 - 1999 county estimates
# create list of individual text files to process
file_list <- paste0("stch-icen199", 0:9, ".txt") 

# create col_positions and col_types for the read_fwf function
cest_positions <-  fwf_cols(year=2, county=7,age=3,race_sex=2, ethnicity=2, pop=7)
cest_types <- c("iciiii")

# run the map_dfr command to read in all files in file_list and put in single df
cest90_99 <- map_dfr(file_list, read_fwf,col_positions = cest_positions, col_types = cest_types)

# add correct dates that the data represent
# 1. Create a new date column to convert YEAR to a date 
cest90_99 <- cest90_99 %>% mutate(date_str = case_when((year == "90") ~ "1990-07-01",
                                                       (year == "91") ~ "1991-07-01",
                                                       (year == "92") ~ "1992-07-01",
                                                       (year == "93") ~ "1993-07-01",
                                                       (year == "94") ~ "1994-07-01",
                                                       (year == "95") ~ "1995-07-01",
                                                       (year == "96") ~ "1996-07-01",
                                                       (year == "97") ~ "1997-07-01",
                                                       (year == "98") ~ "1998-07-01",
                                                       (year == "99") ~ "1999-07-01"))

# 2. Convert date_str to date type; add GISJOIN field to df
cest90_99 <- cest90_99 %>% 
  mutate(date1 = ymd(date_str)) %>%
  mutate(GISJOIN = paste0("G", str_sub(county, 1, 2), "0", str_sub(county,3,5), "0")) %>%
  select(-date_str, -year, -county)

# construct race-specific and sex-specific variables (breaking apart race_sex)
cest90_99 <- cest90_99 %>%
  mutate(sex = case_when((race_sex == 1) ~ 1,
                         (race_sex == 2) ~ 2,
                         (race_sex == 3) ~ 1,
                         (race_sex == 4) ~ 2,
                         (race_sex == 5) ~ 1,
                         (race_sex == 6) ~ 2,
                         (race_sex == 7) ~ 1,
                         (race_sex == 8) ~ 2),
         race = case_when((race_sex == 1) ~ 1,
                          (race_sex == 2) ~ 1,
                          (race_sex == 3) ~ 2,
                          (race_sex == 4) ~ 2,
                          (race_sex == 5) ~ 3,
                          (race_sex == 6) ~ 3,
                          (race_sex == 7) ~ 4,
                          (race_sex == 8) ~ 4))

# create factors for age, race, sex, ethnicity 
cest90_99$sex <- factor(cest90_99$sex, levels=c("1","2"), labels=c("Male", "Female"))
cest90_99$race <- factor(cest90_99$race, levels=c("1","2","3","4"), labels=c("White", "Black", "AIAN","Asian_PI"))
cest90_99$ethnicity <- factor(cest90_99$ethnicity, levels=c("1", "2"), labels=c("Not_Hispanic", "Hispanic"))

## creating two lists to pass into factor (easier to read code and fix later)
age_list <- paste0(0:18)
age_labels <- c("age0", "age1_4","age5_9","age10_14","age15_19","age20_24","age25_29","age30_34","age35_39","age40_44","age45_49","age50_54","age55_59","age60_64","age65_69","age70_74","age75_79","age80_84","age85")
cest90_99$age <- factor(cest90_99$age, levels=age_list, labels=age_labels)

# create a data frame with a county-year value for total population
cest90_99_tot <- cest90_99 %>%
  group_by(date1, GISJOIN) %>%
  summarize(n = sum(pop)) %>%
  mutate(var_code = "total_pop")

# create a data frame with a county-year value for each sex category
cest90_99_sex <- cest90_99 %>%
  group_by(date1, GISJOIN, sex) %>%
  summarize(n = sum(pop)) %>%
  mutate(var_code = case_when((sex == "Male") ~ "total_male",
                              (sex == "Female") ~ "total_female"))

# create a data frame with a county-year value for each age category
cest90_99_age <- cest90_99 %>%
  group_by(date1, GISJOIN, age) %>%
  summarize(n = sum(pop)) %>%
  mutate(var_code = age)

# create a data frame with a county-year value for each sex by age category
cest90_99_sex_age <- cest90_99 %>%
  group_by(date1, GISJOIN, age, sex) %>%
  summarize(n = sum(pop)) 

cest90_99_sex_age <- cest90_99_sex_age %>% mutate(var_code = case_when((sex == "Male" & age == "age0") ~ "male_age0",
                                 (sex == "Male" & age == "age1_4") ~ "male_age1_4",
                                 (sex == "Male" & age == "age5_9") ~ "male_age5_9",
                                 (sex == "Male" & age == "age10_14") ~ "male_age10_14",
                                 (sex == "Male" & age == "age15_19") ~ "male_age15_19",
                                 (sex == "Male" & age == "age20_24") ~ "male_age20_24",
                                 (sex == "Male" & age == "age25_29") ~ "male_age25_29",
                                 (sex == "Male" & age == "age30_34") ~ "male_age30_34",
                                 (sex == "Male" & age == "age35_39") ~ "male_age35_39",
                                 (sex == "Male" & age == "age40_44") ~ "male_age40_44",
                                 (sex == "Male" & age == "age45_49") ~ "male_age45_49",
                                 (sex == "Male" & age == "age50_54") ~ "male_age50_54",
                                 (sex == "Male" & age == "age55_59") ~ "male_age55_59",
                                 (sex == "Male" & age == "age60_64") ~ "male_age60_64",
                                 (sex == "Male" & age == "age65_69") ~ "male_age65_69",
                                 (sex == "Male" & age == "age70_74") ~ "male_age70_74",
                                 (sex == "Male" & age == "age75_79") ~ "male_age75_79",
                                 (sex == "Male" & age == "age80_84") ~ "male_age80_84",
                                 (sex == "Male" & age == "age85") ~ "male_age85",
                                 (sex == "Female" & age == "age0") ~ "female_age0",
                                 (sex == "Female" & age == "age1_4") ~ "female_age1_4",
                                 (sex == "Female" & age == "age5_9") ~ "female_age5_9",
                                 (sex == "Female" & age == "age10_14") ~ "female_age10_14",
                                 (sex == "Female" & age == "age15_19") ~ "female_age15_19",
                                 (sex == "Female" & age == "age20_24") ~ "female_age20_24",
                                 (sex == "Female" & age == "age25_29") ~ "female_age25_29",
                                 (sex == "Female" & age == "age30_34") ~ "female_age30_34",
                                 (sex == "Female" & age == "age35_39") ~ "female_age35_39",
                                 (sex == "Female" & age == "age40_44") ~ "female_age40_44",
                                 (sex == "Female" & age == "age45_49") ~ "female_age45_49",
                                 (sex == "Female" & age == "age50_54") ~ "female_age50_54",
                                 (sex == "Female" & age == "age55_59") ~ "female_age55_59",
                                 (sex == "Female" & age == "age60_64") ~ "female_age60_64",
                                 (sex == "Female" & age == "age65_69") ~ "female_age65_69",
                                 (sex == "Female" & age == "age70_74") ~ "female_age70_74",
                                 (sex == "Female" & age == "age75_79") ~ "female_age75_79",
                                 (sex == "Female" & age == "age80_84") ~ "female_age80_84",
                                 (sex == "Female" & age == "age85") ~ "female_age85"))


# bind the total, age, sex, and sex by age dfs by row, which creates a long data frame with a single
# var_code to differentiate each county count
cest90_99_sums <- bind_rows(cest90_99_tot, cest90_99_sex, cest90_99_age, cest90_99_sex_age) %>%
  select(-sex, -age)

###############
# NHGIS data prep
# 
# Read in tract and county CSVs
# Create correct variables that match county estimates

# 0. Read in standardized tract data and the 2000 SF1 county data
t <- read_csv(paste0(nhgis_dir, "nhgis1140_ts_geog2010_tract.csv"), guess_max = 200000)
c <- read_csv(paste0(nhgis_dir, "nhgis1141_ds146_2000_county.csv"), guess_max = 50000)

# 1. Rename year to datayear in c
c <- c %>%
  rename(DATAYEAR = YEAR)

# 2. keep 2000 and 2010 from the standardized tract data
t <- t %>%
  filter(DATAYEAR == 1990 | DATAYEAR == 2000)

# 3. Create the tract-level age by sex categories that match the county estimates
t <- t %>%
  mutate(
         male_age0 = CW6AA,
         male_age1_4 = CW6AB  + CW6AC,
         male_age5_9 = CW5AB,
         male_age10_14 = CW5AC,
         male_age15_19 = CW5AD + CW5AE,
         male_age20_24 = CW5AF + CW5AG + CW5AH,
         male_age25_29 = CW5AI,
         male_age30_34 = CW5AJ,
         male_age35_39 = CW5AK,
         male_age40_44 = CW5AL,
         male_age45_49 = CW5AM,
         male_age50_54 = CW5AN,
         male_age55_59 = CW5AO,
         male_age60_64 = CW5AP + CW5AQ,
         male_age65_69 = CW5AR,
         male_age70_74 = CW5AS,
         male_age75_79 = CW5AT,
         male_age80_84 = CW5AU,
         male_age85 = CW5AV,
         female_age0 = CW6AO,
         female_age1_4 = CW6AP + CW6AQ,
         female_age5_9 = CW5AX,
         female_age10_14 = CW5AY,
         female_age15_19 =  CW5AZ + CW5BA,
         female_age20_24 =  CW5BB  + CW5BC  + CW5BD,
         female_age25_29 =  CW5BE,
         female_age30_34 =  CW5BF,
         female_age35_39 =  CW5BG,
         female_age40_44 =  CW5BH,
         female_age45_49 =  CW5BI,
         female_age50_54 =  CW5BJ,
         female_age55_59 =  CW5BK,
         female_age60_64 = CW5BL + CW5BM,
         female_age65_69 = CW5BN,
         female_age70_74 =  CW5BO,
         female_age75_79 =  CW5BP,
         female_age80_84 = CW5BQ,
         female_age85 = CW5BR
  ) %>%
  select(GISJOIN, DATAYEAR, STATE, male_age0:female_age85) %>%
  gather(var_code, n, male_age0:female_age85)

# 4. Create the county-level age by sex categories (from SF1) that match the county estimates
c <- c %>%
  mutate(
         male_age0 = FNG001,
         male_age1_4 = FNG002 + FNG003 + FNG004 + FNG005,
         male_age5_9 = FMZ002,
         male_age10_14 = FMZ003,
         male_age15_19 = FMZ004 + FMZ005,
         male_age20_24 = FMZ006 + FMZ007 + FMZ008,
         male_age25_29 = FMZ009,
         male_age30_34 = FMZ010,
         male_age35_39 = FMZ011,
         male_age40_44 = FMZ012,
         male_age45_49 = FMZ013,
         male_age50_54 = FMZ014,
         male_age55_59 = FMZ015,
         male_age60_64 = FMZ016 + FMZ017,
         male_age65_69 = FMZ018 + FMZ019,
         male_age70_74 = FMZ020,
         male_age75_79 = FMZ021,
         male_age80_84 = FMZ022,
         male_age85 = FMZ023,
         female_age0 = FNG021,
         female_age1_4 = FNG022 + FNG023 + FNG024 + FNG025,
         female_age5_9 = FMZ025,
         female_age10_14 = FMZ026,
         female_age15_19 =  FMZ027 + FMZ028,
         female_age20_24 =  FMZ029 + FMZ030 + FMZ031,
         female_age25_29 =  FMZ032,
         female_age30_34 =  FMZ033,
         female_age35_39 =  FMZ034,
         female_age40_44 =  FMZ035,
         female_age45_49 =  FMZ036,
         female_age50_54 =  FMZ037,
         female_age55_59 =  FMZ038,
         female_age60_64 = FMZ039 + FMZ040,
         female_age65_69 = FMZ041 + FMZ042,
         female_age70_74 =  FMZ043,
         female_age75_79 =  FMZ044,
         female_age80_84 = FMZ045,
         female_age85 = FMZ046
  ) %>%
  select(GISJOIN, DATAYEAR, male_age0:female_age85) %>%
  gather(var_code, n, male_age0:female_age85) %>%
  mutate(date_str = "2000-04-01") %>%
  mutate(date1 = ymd(date_str)) %>%
  select(-date_str)

###################################################################
######## CROSSWALK prep for county adjustments
######## Need to get correct 2000 GISJOIN_CTY code on each 2010 tract
cwlk10_00 <- read_csv(paste0(nhgis_dir, "tract10_county00_crosswalk.csv"))

# 1. Create correct county and tract GISJOIN code, and remove orig vars from df
cwlk10_00 <- cwlk10_00 %>% 
  mutate(GISJOIN_CTY = paste0("G", str_sub(co00_id, 1, 2), "0", str_sub(co00_id, 3,5), "0"),
         GISJOIN = paste0("G", str_sub(t10_id, 1, 2), "0", str_sub(t10_id, 3, 5), "0", str_sub(t10_id, 6, 11))) %>% 
  select(-t10_id, -co00_id)

# 2. Join the crosswalk to the t df
t <- left_join(t, cwlk10_00, by="GISJOIN")

# Subdivide tract df into smaller parts so that the pad months function will actually finish
# males by age 
t_male <- t %>%
  filter(str_detect(var_code, "^male")) 

# females by age 
t_female <- t %>%
  filter(str_detect(var_code, "^female")) 

######## read in the adjustment dataset I created for 25 counties
## this df (pop_est1990) will be used to adjust the n's for the 25 counties in the CSV. These 25 counties are
## poorly approximated by 2010 census tracts
pop_est1990 <- read_csv(paste0(popest_dir, "county2000_poor1990_estimates.csv"))

###############
# HANDLE THE CROSSWALK ISSUES
# #############

# 1. collapse t by DATAYEAR, var_code, date1 and GISJOIN_CTY to create 1990 and  values for each county
c90_00 <- t %>%
  filter(DATAYEAR == 1990) %>%
  group_by(DATAYEAR, STATE, var_code, GISJOIN_CTY) %>% 
  summarize(n = sum(n)) %>%
  ungroup()

# 4. this version only adjusts 1990 pop
adjust_df <- pop_est1990 %>%
  mutate(ratio = pop_est / n) %>%
  select(GISJOIN_CTY, DATAYEAR, ratio)

# 5. Join pop_est1990 to c90_00 and adjust c90_00 values by ratio (if ratio isn't NA)
c90_00 <- left_join(c90_00, adjust_df, by=c("GISJOIN_CTY","DATAYEAR")) %>%
  mutate(n = ifelse(is.na(ratio), n, n * ratio)) %>%
  rename(GISJOIN = GISJOIN_CTY) %>%
  mutate(date_str = "1990-04-01") %>%
  mutate(date1 = ymd(date_str)) %>%
  select(-ratio, -date_str, -STATE)

# 6. Row bind c90_00 to the c90_99_sums dataframe
cest90_99_sums <- bind_rows(cest90_99_sums, c90_00)

# 7. Row bind c90_00 and c to create county counts from 1990 and 2000 decennials
c <- bind_rows(c90_00, c)

###################################################################
# Clean up the dfs that aren't necessary for future processing 
rm(cest90_99)
rm(cest90_99_age)
rm(cest90_99_sex)
rm(cest90_99_sex_age)
rm(cest90_99_tot)
rm(c90_00)
#rm(c)
rm(t)
rm(cwlk10_00)
rm(pop_est1990)
rm(cest_positions)
rm(adjust_df)

########################################################
# Step 2 - linear interpolation of tracts and counties
######################################################## 
# This script creates the linearly interpolated values for each tract from 2000-2010.
# 
# Updated: 2020-02-25
# I re-wrote the cumulative days function to use a case_when statement. The cumsum function operating on a difftime -> numeric cast field
# was taking forever. I think the conversion from difftime to numeric object/field was/is super slow. 
#
# For 1990 to 2000, I wrote a new function called fill_down_county to fill in the GISJOIN_CTY values 
# for each census tract-var_code combination. Otherwise, the script works the same as the one for 
# 2000 to 2010. 

# loads functions from script

# 0. set begin and end values, convert to dates and find number of days between them
begin_date <- ymd("1990-04-01")
end_date <- ymd("2000-04-01")

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


# 3. c dataframe already has date1 from the 01_1990_2000_annual_census_bureau_county_estimates_sexbyage.r script
# c <- c %>%
#   mutate(date_str = case_when((DATAYEAR == 1990) ~ "1990-04-01",
#                               (DATAYEAR == 2000) ~ "2000-04-01",
#                               (DATAYEAR == 2010) ~ "2010-04-01")) %>%
#   mutate(date1 = ymd(date_str))

# 4. Compute difference between 2000 and 2010 data 
t_female <- compute_diff(t_female, d2000, n, var_code, GISJOIN)
t_male <- compute_diff(t_male, d2000, n, var_code, GISJOIN)
c <- compute_diff(c, d2000, n, var_code, GISJOIN)

# 4. Add records to hold the intercensal values
t_female <- pad_months(t_female, GISJOIN, var_code, date1, "1990-04-01", "2000-04-01")
t_male <- pad_months(t_male, GISJOIN, var_code, date1, "1990-04-01", "2000-04-01")
c <- pad_months(c, GISJOIN, var_code, date1, "1990-04-01", "2000-04-01")

# 5. Compute cumulative sum of days_interval for each group for the 1990-2000 time period
# t_female <- compute_cum_days_interval_1990_2000(t_female, date1, cum_days_interval)
# t_male <- compute_cum_days_interval_1990_2000(t_male, date1, cum_days_interval)
# c <- compute_cum_days_interval_1990_2000(c, date1, cum_days_interval)

# 5. Join the cum_days df to t_female, t_male, and c 
t_female <- t_female %>%
  left_join(cum_days, by = c("date1" = "date"))

t_male <- t_male %>%
  left_join(cum_days, by = c("date1" = "date"))

c <- c %>%
  left_join(cum_days, by = c("date1" = "date"))
# 6. fill diff and var_code down by group 
t_female <- fill_down(t_female, GISJOIN, var_code, n)
t_male <- fill_down(t_male, GISJOIN, var_code, n)
c <- fill_down(c, GISJOIN, var_code, n)

# 7. fill GISJOIN_CTY down for all GISJOIN (tract) - var_code combos
#    - need to do this for 1990-2000 to account for the different counties that some 2010 tracts belong to
#      (e.g. Broomfield and others)
t_female <- fill_down_county(t_female, GISJOIN, var_code, GISJOIN_CTY)
t_male <- fill_down_county(t_male, GISJOIN, var_code, GISJOIN_CTY)
# c df already has GISJOIN_CTY in it for all records
#c <- fill_down(c, GISJOIN, var_code, n)

# 8. Compute the linear interpolation for all var_code using the following:
t_female <- linear_interpolation(t_female, n, n_diff, cum_days_interval)
t_male <- linear_interpolation(t_male, n, n_diff, cum_days_interval)
c <- linear_interpolation(c, n, n_diff, cum_days_interval)

########################################################
# Step 3 - adjust tract linear interpolations to sum to
# county estimates
######################################################## 
# This script computes the tract estimates  from 1990 to 2000 for census tracts, 
# adjusting a given counties tracts so they sum to county estimate.
#
# This is the third script that actually creates the annual estimate for each tract. It first 
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
# I updated this script for 1990-2000 to only handle the t_female and t_male dataframes because
# that's all we need for this product.

# join CB population estimates to county linear interpolation (t) on GISJOIN and date1 
c <- left_join(c, cest90_99_sums, by=c("GISJOIN","date1","var_code"))

# compute ratio of county estimate to linear interpolation value 
# this step would need to be run more times if we had more than 1 variable we were estimating
c <- mutate(c, var_ratio = n.y / n.x)

# keep only needed vars for tract interpolation
# this step would need to keep more variables if we're estimating more than 1 variable
c <- select(c, GISJOIN, var_code, date1, n.x, n.y, var_ratio)

# join county data to tract for final processing 
t_male <- left_join(t_male, c, by=c("GISJOIN_CTY" = "GISJOIN", "date1"="date1", "var_code" = "var_code"))
t_female <- left_join(t_female, c, by=c("GISJOIN_CTY" = "GISJOIN", "date1"="date1", "var_code" = "var_code"))

# multiply tract linear interpolation estimate by value of county ratio 
# - this gives me a tract estimate adjusted by the ratio of the county estimate to the linearly
# interpolated county value
t_male <- mutate(t_male, n_est = n * var_ratio)
t_female <- mutate(t_female, n_est = n * var_ratio)

# fields to keep in output data
# GISJOIN, STATE, var_code, n, date1, n_diff, cum_days_interval, n.x, n.y, var_ratio, n_est
# CONSIDER FILTERING OUT THE DECENNIALS FROM THE DATASET
t_male <- select(t_male, GISJOIN, STATE, date1, cum_days_interval, var_code, n_diff, n.x, n.y, var_ratio, n, n_est)
t_female <- select(t_female, GISJOIN, STATE, date1, cum_days_interval, var_code, n_diff, n.x, n.y, var_ratio, n, n_est)

# write out the CSV files
write_csv(t_male, paste0(outdata_dir, "tct90_00_male.csv"))
write_csv(t_female, paste0(outdata_dir, "tct90_00_female.csv"))
