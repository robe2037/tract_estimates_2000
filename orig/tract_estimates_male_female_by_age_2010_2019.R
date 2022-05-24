# tract_estimates_male_female_by_age_2010_2019.r
# Author: David Van Riper
# Created: 2020-08-10
# 
# This script prepares the tract estimates for 2010-2019. It reads in the vintage 2019 county estimates
# using tidycensus.
# 
# Notes to add to scripts:
# 1. We must make a few decisions that will impact the estimates:
#   - CCR caps matter a ton
#   - CTW values for cohorts with Inf values (e.g. no 0-4 or 5-9 in 20000 and greater than zero 0-4 and 
#   5-9 year olds in 2010)
#   - CTW denominators are females of child-bearing age, and we must choose what we mean by "child-bearing age"
#     - Strate et al. use females 20-44 for children 0-4 and females 30-49 for children 5-9, but those 
#       age ranges were based on Massachusetts-specific birth data

require(tidyverse)
require(lubridate)
require(padr)
require(tidycensus)

#### Source the tidy functions #### 
source("/pkg/popgis/labpcs/data_projects/tract_estimates/2000_2010/scripts/tidy_funs_tract_county_estimates.r")

# constants 
nhgis_dir <- "/pkg/popgis/labpcs/data_projects/tract_estimates/2000_2010/data/nhgis/"
outdata_dir <- "/pkg/popgis/labpcs/data_projects/tract_estimates/2000_2010/data/outdata/"

#### 1. Load in the NHGIS standardized sex/age by tract/county data ####
t <- read_csv(paste0(nhgis_dir, "nhgis1133_ts_geog2010_tract.csv"), guess_max = 200000)
c <- read_csv(paste0(nhgis_dir, "nhgis1133_ts_geog2010_county.csv"), guess_max = 200000)

#### 2. Keep only 2000 and 2010 tract and county estimates #### 
t <- t %>%
  filter(DATAYEAR == 2000 | DATAYEAR == 2010)

c <- c %>%
  filter(DATAYEAR == 2000 | DATAYEAR == 2010)

#### 2a. Bedford Independent City recodes #### 
# Recode Bedford independent city tract to Bedford county FIPS code
t <- t %>%
  mutate(GISJOIN = case_when(STATEA == "51" & COUNTYA == "515" ~ paste0("G", STATEA, "0", "019", "0", TRACTA),
                             TRUE ~ GISJOIN))

# Recode Bedford independent city FIPS code to Bedford county FIPS and collapse to create single entity 
c <- c %>%
  mutate(GISJOIN = case_when(STATEA == "51" & COUNTYA == "515" ~ paste0("G", STATEA, "0", "019", "0"),
                             TRUE ~ GISJOIN)) %>%
  pivot_longer(CL8AA:CN8BNU, names_to = "var_code", values_to = "n") %>%
  group_by(GISJOIN, DATAYEAR, var_code) %>%
  summarise(n = sum(n, na.rm = TRUE)) %>%
  pivot_wider(names_from = "var_code", values_from = "n") %>%
  ungroup()

#### 3. Creates age groups required for CCR calculations ####
t <- compute_age_sex_cohorts(t)
c <- compute_age_sex_cohorts(c)

#### 4. Create wide df where each row is a tract ####
t_wide <- create_wide_age_cohort_df(t)
c_wide <- create_wide_age_cohort_df(c)

#### 5. Compute Cohort Change Ratios for 2000-2010 by sex/age/tract #### 
# The formula for a sex-specific CCR is:
# 
# ccr_age = Population_age_2010 / Popualtion_age-10_2000
# 
# where age is the sex-specific age group in question. We do not compute CCRs for
# ages 0-4 or 5-9; instead, we will use child-to-women ratios for those age groups.
# For the top age category (85+), the denominator is the sum of the Census 2000 counts 
# of ages 75-70, 80-84, and 85+.

t_wide_ccr <- compute_ccr(t_wide)
c_wide_ccr <- compute_ccr(c_wide)

#### 6. Generate cohort size flags for each CCR cohort #### 
# we will use the cohort size flags when we set the CCR caps. If a cohort is < 25, the
# CCR for that cohort is capped to 1.0. If a cohort is >= 25 and < 100, the CCR is capped
# at 2.0. 
t_cohort_size <- compute_cohort_size_flag(t)
c_cohort_size <- compute_cohort_size_flag(c)

#### 7. Create a usable data frame from CCRs #### 
# create a long df of CCRs by cohort
# compute a new age_code for merging with the t_cohort_size df
t_long_ccr <- create_long_ccr_df(t_wide_ccr)
c_long_ccr <- create_long_ccr_df(c_wide_ccr)

#### 8. Merge cohort_size and ccr dfs and set ccr caps in place #### 
t_long_ccr <- t_long_ccr %>%
  left_join(t_cohort_size, by = c("GISJOIN" = "GISJOIN", "age_code" = "var_code")) %>%
  select(-DATAYEAR, -age_code)

c_long_ccr <- c_long_ccr %>%
  left_join(c_cohort_size, by = c("GISJOIN" = "GISJOIN", "age_code" = "var_code")) %>%
  select(-DATAYEAR, -age_code)

# set ccr caps to 1 if ccr_cap == 1 & ccr > 1.0
# set ccr caps to 2 if ccr_cap == 2 (cohort is between 25 and 100) & ccr > 2.0
# set ccr caps to 0 if is.na(ccr) - these are cohorts that were 0 in 2000 and 2010
# set ccr caps to X if is.inf(ccr) - these are cohorts that were 0 in 2000 and > 0 in 2010 ?????? - propose 2
# set ccr caps to X if ccr > 2 regardless of population size - propose 2

t_long_ccr_final <- set_ccr_caps(t_long_ccr)
c_long_ccr_final <- set_ccr_caps(c_long_ccr)

#### 9. Need to do basic analysis of CCRs #### 
# # Number of inf values in total - 2368 out of 2,337,824 (0.01% of all tract-cohort combos)
# t_long_ccr_final %>%
#   summarise(inf_count = sum(is.infinite(ccr_final)))
# 
# # Number of inf values by cohort 
# t_long_ccr_final %>%
#   group_by(var_code) %>%
#   summarise(inf_count = sum(is.infinite(ccr_final))) %>%
#   print(n = Inf)
# 
# # Combos with CCRs greater than 2.0 - 82,002 out of 2,337,824 (3% of all tract-cohort combos)
# t_long_ccr_final %>%
#   summarise(ccr_final_count = sum(ccr_final > 2.0))
# 
# # Tract-cohort combos with CCRs greater than 2.0 
# # There aren't that many in the older age cohorts with CCRs greater than 2.0
# t_long_ccr_final %>%
#   group_by(var_code) %>%
#   summarise(ccr_final_count = sum(ccr_final > 2.0)) %>%
#   print(n = Inf)
# 
# # What are typical pop counts for tract-cohorts with CCR > 2.0
# t_long_ccr_final %>%
#   filter(ccr > 2.0) %>%
#   filter(!is.infinite(ccr)) %>%
#   group_by(var_code) %>%
#   summarise(mean_n = mean(n),
#             mean_ccr = mean(ccr, na.rm = TRUE),
# #            sd_n = sd(n),
#             median_n = median(n),
#             median_ccr = median(ccr, na.rm = TRUE),
# #            min_n = min(n),
# #            max_n = max(n),
#             gt_100 = sum(n > 100)) %>%
#   print(n = Inf)
# 
# # Tract-cohort combos counts in different CCR ranges 
# cohort_ccr_counts <- t_long_ccr_final %>%
#   mutate(ccr_range = case_when(ccr_final < 1.0 ~ 1,
#                                ccr_final >= 1.0 & ccr_final < 2 ~ 2,
#                                ccr_final >= 2.0 & ccr_final < 10 ~ 3,
#                                ccr_final >= 10 & ccr_final < 100 ~ 4,
#                                ccr_final >= 100 ~ 5)) %>%
#   group_by(var_code, ccr_range) %>%
#   summarise(ccr_count = n())
# 
# # factorize ccr_ranges for plotting
# cohort_ccr_counts$ccr_range <- factor(cohort_ccr_counts$ccr_range, labels = c("Less than 1.0", "1.0-1.9", "2.0-9.9", "10.0-99.9", "Greater than 100"))
# 
# # Bar chart of counts by ccr_range category
# ggplot(data = cohort_ccr_counts, aes(x = ccr_range, y = ccr_count)) +
#   geom_bar(stat="identity") + 
#   coord_flip() +
#   facet_wrap(vars(var_code))
# 
# # Histograms of ccr2 by cohort
# ggplot(data = t_long_ccr_final, x = ccr_final) + 
#   geom_histogram() + 
#   facet_wrap(vars(var_code))


#### 10. Apply the CCRs to the 2010 counts to create 2020 estimates ####
# Create a match code in the t_long_ccr_final to join CCRs to 2010 counts
t_long_ccr_final <- t_long_ccr_final %>%
  mutate(var_code = str_remove(var_code, "ccr_")) %>%
  select(-n)

c_long_ccr_final <- c_long_ccr_final %>%
  mutate(var_code = str_remove(var_code, "ccr_")) %>%
  select(-n)

# Keep only 2010 DATAYEAR from T
t_2010 <- t %>%
  filter(DATAYEAR == 2010)

c_2010 <- c %>%
  filter(DATAYEAR == 2010)

# Join t_long_ccr_final to t_2010 on var_code and REMOVE YOUNGER AGES FOR NOW
t_2010 <- t_2010 %>%
  left_join(t_long_ccr_final, by = c("GISJOIN", "var_code")) %>%
  filter(!is.na(ccr_final))

c_2010 <- c_2010 %>%
  left_join(c_long_ccr_final, by = c("GISJOIN", "var_code")) %>%
  filter(!is.na(ccr_final))

# Compute 2020 estimates- multiply n by ccr_final and then create df with fewer vars
t_2010 <- t_2010 %>%
  mutate(n2020 = n * ccr_final) %>%
  select(GISJOIN, var_code, n2010 = n, n2020, -ccr, -ccr_final, -ccr_cap)

c_2010 <- c_2010 %>%
  mutate(n2020 = n * ccr_final) %>%
  select(GISJOIN, var_code, n2010 = n, n2020, -ccr, -ccr_final, -ccr_cap)

# Create long t_2010 df 
t_long_2010 <- t_2010 %>%
  pivot_longer(n2010:n2020, names_to = "year", values_to = "n") %>%
  mutate(DATAYEAR = case_when(year == "n2010" ~ 2010,
                              year == "n2020" ~ 2020)) %>%
  select(-year)

c_long_2010 <- c_2010 %>%
  pivot_longer(n2010:n2020, names_to = "year", values_to = "n") %>%
  mutate(DATAYEAR = case_when(year == "n2010" ~ 2010,
                              year == "n2020" ~ 2020)) %>%
  select(-year)

#### 11. Compute Child-to-Women Ratios 2000-2010 by age/tract #### 
# Child-to-women ratios are computed as follows:
# 
# CTW_male0_4 = Population_2010_male_age0_4 / Population_2010_female_age_20_44
# CTW_female0_4 = Population_2010_female_age0_4 / Population_2010_female_age_20_44
# CTW_male5_9 = Population_2010_male_age5_9 / Population_2010_female_age_30_49
# CTW_female5_9 = Population_2010_female_age5_9 / Population_2010_female_age_30_49
# 
# CTW_age0_4 = Population_2010_age0_4 / Population_2010_female_age_20_44
# CTW_age5_9 = Population_2010_age5_9 / Population_2010_female_age_30_49
# 
# where the numerator is the number of males and females in each age group. We will
# apply the overall CTW to each sex.

t_wide_ctw <- compute_ctw(t_wide_ccr)
c_wide_ctw <- compute_ctw(c_wide_ccr)

#### 12. Create a usable long data frame from the CTWs #### 
t_long_ctw <- create_long_ctw_df(t_wide_ctw)
c_long_ctw <- create_long_ctw_df(c_wide_ctw)

#### 13. Analysis of the CTWs ####
# Number of tract-cohort INF values - 12 out of 146,114
# t_long_ctw %>%
#   summarise(inf_count = sum(is.infinite(ctw)))
# 
# # Number of tract-cohort NaN values - 1222 out of 146,114
# t_long_ctw %>%
#   summarise(nan_count = sum(is.nan(ctw)))

#### 14. Set NaN and Inf values for tract-level CTWs
t_long_ctw <- replace_nan_inf_ctw(t_long_ctw)
c_long_ctw <- replace_nan_inf_ctw(c_long_ctw)

#### 15. Create a match code in the t_long_ctw and c_long_ctw ####
t_long_ctw <- t_long_ctw %>%
  mutate(var_code = str_remove(var_code, "ctw_"))

c_long_ctw <- c_long_ctw %>%
  mutate(var_code = str_remove(var_code, "ctw_"))

#### 15. Pull out the female age groups from t_long_2010 and c_long_2010 required for CTW application #### 
# Keep females age 20-44 for age 0-4
# Keep females age 30-49 for ages 5-9
t_females <- t_long_2010 %>%
  filter(DATAYEAR == 2020) %>%
  filter(var_code %in% c("female_age20_24", "female_age25_29", "female_age30_34", "female_age35_39", "female_age40_44", "female_age45_49"))

c_females <- c_long_2010 %>%
  filter(DATAYEAR == 2020) %>%
  filter(var_code %in% c("female_age20_24", "female_age25_29", "female_age30_34", "female_age35_39", "female_age40_44", "female_age45_49"))

# Recode age categories into two groups - 20-44 and 30-49
t_females_20_44 <- create_females_20_44(t_females)
t_females_30_49 <- create_females_30_49(t_females)
c_females_20_44 <- create_females_20_44(c_females)
c_females_30_49 <- create_females_30_49(c_females)

#### 16. Apply CTWs to counts in t_females* and c_females* to get 2020 estimates #### 
# a. Split apart t_long_ctw and c_long_ctw into two dfs each (one for 0-4, one for 5-9)
t_long_ctw_age0_4 <- t_long_ctw %>%
  filter(var_code == "male_age0_4" | var_code == "female_age0_4")

t_long_ctw_age5_9 <- t_long_ctw %>%
  filter(var_code == "male_age5_9" | var_code == "female_age5_9")

c_long_ctw_age0_4 <- c_long_ctw %>%
  filter(var_code == "male_age0_4" | var_code == "female_age0_4")

c_long_ctw_age5_9 <- c_long_ctw %>%
  filter(var_code == "male_age5_9" | var_code == "female_age5_9")

# b. Join the t_females_XX_YY onto CTW rates and multiply ctw * n
# - this yields the 2020 estimate for male_age0_4, male_age5_9, female_age0_4, and female_age5_9
t_long_ctw_age0_4_females_age20_44 <- t_long_ctw_age0_4 %>%
  left_join(t_females_20_44, by = "GISJOIN") %>%
  mutate(n = n * ctw_final,
         DATAYEAR = 2020) %>%
  select(-ctw, -ctw_final)

t_long_ctw_age5_9_females_age30_49 <- t_long_ctw_age5_9 %>%
  left_join(t_females_30_49, by = "GISJOIN") %>%
  mutate(n = n * ctw_final,
         DATAYEAR = 2020) %>%
  select(-ctw, -ctw_final)

c_long_ctw_age0_4_females_age20_44 <- c_long_ctw_age0_4 %>%
  left_join(c_females_20_44, by = "GISJOIN") %>%
  mutate(n = n * ctw_final,
         DATAYEAR = 2020) %>%
  select(-ctw, -ctw_final)

c_long_ctw_age5_9_females_age30_49 <- c_long_ctw_age5_9 %>%
  left_join(c_females_30_49, by = "GISJOIN") %>%
  mutate(n = n * ctw_final,
         DATAYEAR = 2020) %>%
  select(-ctw, -ctw_final)

#### 17. Extract male_age0_4, male_age5_9, female_age0_4, and female_age5_9 for 2010 from t and c dfs ####
# This step extracts 2010 counts of male_age0_4, male_age5_9, female_age0_4, and female_age5_9 from the 
# t and c dfs

t_2010_age04_59 <- t %>%
  filter(DATAYEAR == 2010) %>%
  filter(var_code %in% c("male_age0_4", "male_age5_9", "female_age0_4", "female_age5_9"))

c_2010_age04_59 <- c %>%
  filter(DATAYEAR == 2010) %>%
  filter(var_code %in% c("male_age0_4", "male_age5_9", "female_age0_4", "female_age5_9"))

#### 18. Bind t_2010_long, t_2010_age04_59, t_long_ctw_age0_4_females_age20_44, and t_long_ctw_age5_9_females_age30_49 ####
# This step generates the df with 2010 and 2020 values for each tract-sex-age combination

t_long_2010 <- bind_rows(t_long_2010, t_2010_age04_59)
t_long_2010 <- bind_rows(t_long_2010, t_long_ctw_age0_4_females_age20_44)
t_long_2010 <- bind_rows(t_long_2010, t_long_ctw_age5_9_females_age30_49)

c_long_2010 <- bind_rows(c_long_2010, c_2010_age04_59)
c_long_2010 <- bind_rows(c_long_2010, c_long_ctw_age0_4_females_age20_44)
c_long_2010 <- bind_rows(c_long_2010, c_long_ctw_age5_9_females_age30_49)


#### 19. Run the 2010 data through the prep steps for interpolation #### 
# a. set begin and end values, convert to dates and find number of days between them
begin_date <- ymd("2010-04-01")
end_date <- ymd("2019-07-01")

# b. set the decade interval
decade_interval <- interval(begin_date, end_date)

# c. find number of days in interval 
days <- decade_interval / days(1)

# # d. Create appropriate date fields in t
t_long_2010 <- t_long_2010 %>%
  mutate(date1 = ymd(ifelse(DATAYEAR == 2010, "2010-04-01", "2019-07-01")))

# mutate(date_str = case_when((DATAYEAR == 1990) ~ "1990-04-01",
#                             (DATAYEAR == 2000) ~ "2000-04-01",
#                             (DATAYEAR == 2010) ~ "2010-04-01",
#                             (DATAYEAR == 2020) ~ "2020-04-01")) %>%
# mutate(date1 = ymd(date_str))
# 
c_long_2010 <- c_long_2010 %>%
  mutate(date1 = ymd(ifelse(DATAYEAR == 2010, "2010-04-01", "2019-07-01")))

#   mutate(date_str = case_when((DATAYEAR == 1990) ~ "1990-04-01",
#                               (DATAYEAR == 2000) ~ "2000-04-01",
#                               (DATAYEAR == 2010) ~ "2010-04-01",
#                               (DATAYEAR == 2020) ~ "2020-04-01")) %>%
#   mutate(date1 = ymd(date_str)) 

# e. Create df containing males by age 
t_male <- t_long_2010 %>%
  filter(str_detect(var_code, "^male"))

# f. Create df containing females by age 
t_female <- t_long_2010 %>%
  filter(str_detect(var_code, "^female"))


t_male <- t_male %>% filter(str_detect(GISJOIN, "G0100010"))
t_female <- t_female %>% filter(str_detect(GISJOIN, "G0100010"))
c <- c_long_2010 %>% filter(GISJOIN == "G0100010")

# g. Compute difference between 2000 and 2010 data 
t_female <- compute_diff(t_female, d2010, n, var_code, GISJOIN)
t_male <- compute_diff(t_male, d2020, n, var_code, GISJOIN)
c <- compute_diff(c, d2020, n, var_code, GISJOIN)

# g. Create a custom span option for adding a specific set of months to df (all April and July months)
time_span <- 
  span_around(begin_date, interval = "month", end_shift = "10 year") %>%
  subset_span(list(mon = c(4, 7)))

# h. Use the pad_cust to add records to t_male and t_female using time_span
t_male <- t_male %>%
  pad_cust(time_span, group = c("GISJOIN", "var_code"), drop_last_spanned = FALSE) %>%
  filter(date1 == "2010-04-01" | date1 == "2020-04-01" | month(date1) == 7 )

t_female <- t_female %>%
  pad_cust(time_span, group = c("GISJOIN", "var_code"), drop_last_spanned = FALSE) %>%
  filter(date1 == "2010-04-01" |date1 == "2020-04-01" | month(date1) == 7)

c <- c %>%
  pad_cust(time_span, group = c("GISJOIN", "var_code"), drop_last_spanned = FALSE) %>%
  filter(date1 == "2010-04-01" |date1 == "2020-04-01" | month(date1) == 7)

# i. compute cumulative days interval
t_female <- compute_cum_days_interval_2010_2019(t_female, date1, cum_days_interval)
t_male <- compute_cum_days_interval_2010_2019(t_male, date1, cum_days_interval)
c <- compute_cum_days_interval_2010_2019(c, date1, cum_days_interval)

# j. fill diff and CL8AA down by group 
t_female <- fill_down(t_female, GISJOIN, var_code, n)
t_male <- fill_down(t_male, GISJOIN, var_code, n)
c <- fill_down(c, GISJOIN, var_code, n)

# k. Compute a linear interpolation of for each intercensal year value. Essentially, this function
# spreads the n_diff out over each year. 
t_female_interp <- linear_interpolation(t_female, n, n_diff, cum_days_interval)
t_male_interp <- linear_interpolation(t_male, n, n_diff, cum_days_interval)
c_interp <- linear_interpolation(c, n, n_diff, cum_days_interval)


#### Prepare 2010-2019 county estimates data #### 

#### County estimates: constants ####
popest_dir <- "/pkg/popgis/labpcs/data_projects/tract_estimates/2010_2019/data/popest/"
popest_file <- "cc-est2019-alldata.csv"

#### County estimates: Read in 2010-2019 county pop estimates file ####
c_est <- read_csv(paste0(popest_dir, popest_file))

#### County estimates: Select out required columns for further processing ####  
# select out columns from STATE to TOT_POP because that's all I need for now
c_est<- select(c_est, STATE:TOT_FEMALE)

#### County estimates: Create a GISJOIN for each record #### 
c_est<- c_est %>%
  mutate(GISJOIN = paste0("G",STATE,"0",COUNTY,"0"))

c_est <- c_est %>% filter(GISJOIN == "G0100010")

#### County estimates: Filter out YEAR == 1 from dataset ####
# We used population estimates base from 2000-2010, so we should use the same for 2010-2019. 
# The pop estimates base is YEAR == 2. YEAR == 1 represents the census 2010 population
c_est <- c_est %>%
  filter(YEAR != 1)

#### County estimates: Create a new date column to convert YEAR to a date from an integer #### 
# Add new date column and recode from integers in original data file
# Then, convert the recoded value into a Date type
c_est<- c_est %>%
  mutate(date_str = case_when((YEAR == 1) ~ "2010-04-01",
                              (YEAR == 2) ~ "2010-04-01",
                              (YEAR == 3) ~ "2010-07-01",
                              (YEAR == 4) ~ "2011-07-01",
                              (YEAR == 5) ~ "2012-07-01",
                              (YEAR == 6) ~ "2013-07-01",
                              (YEAR == 7) ~ "2014-07-01",
                              (YEAR == 8) ~ "2015-07-01",
                              (YEAR == 9) ~ "2016-07-01",
                              (YEAR == 10) ~ "2017-07-01",
                              (YEAR == 11) ~ "2018-07-01",
                              (YEAR == 12) ~ "2019-07-01"),
         date1 = ymd(date_str))


#### County estimates: Create a factor for AGEGRP variable ####
c_est$AGEGRP <- factor(c_est$AGEGRP, 
                       levels=c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18), 
                       labels = c("total",
                                  "age0_4",
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
                                  "age85"))

#### County estimates: Pivot the c_est to a long data frame ####
# Select a set of attributes and then pivot_longer
c_est <- c_est %>%
  select(AGEGRP:date1, -date_str) %>%
  pivot_longer(names_to = "var_code", values_to = "n", TOT_POP:TOT_FEMALE)

#### County estimates: Recode values in var_code to match NHGIS standardized codes #### 
c_est <- c_est %>%
  mutate(var_code = case_when((var_code == "TOT_POP" & AGEGRP == "total") ~ "total_pop",
                              (var_code == "TOT_MALE" & AGEGRP == "total") ~ "total_male",
                              (var_code == "TOT_FEMALE" & AGEGRP == "total") ~ "total_female",
                              (var_code == "TOT_MALE" & AGEGRP == "age0_4") ~ "male_age0_4",
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
                              (var_code == "TOT_FEMALE" & AGEGRP == "age0_4") ~ "female_age0_4",
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
                              (var_code == "TOT_POP" & AGEGRP == "age0_4") ~ "age0_4",
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

#### County estimates: Create final version of c_est #### 
# Drop AGEGRP from c_est
# Only keep records with var_code containing male or female
c_est <- c_est %>%
  select(-AGEGRP) %>%
  filter(str_detect(var_code, "male")) %>%
  filter(!str_detect(var_code, "total")) %>%
  arrange(GISJOIN, var_code, date1)

#### County estimates: County recodes #### 
# Recode Oglala Lakota county (G4601020) to Shannon county (G4601130) so it matches with 2010
# Recode Kusilvak Census area (G0201580) to Wade Hampton Census Area (G0202700) so it matches 2010
c_est <- c_est %>%
  mutate(GISJOIN = case_when(GISJOIN == "G4601020" ~ "G4601130",
                             GISJOIN == "G0201580" ~ "G0202700",
                             TRUE ~ GISJOIN))

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
t_male <- select(t_male, GISJOIN, GISJOIN_CTY, date1, cum_days_interval, var_code, n_diff, n.x, n.y, var_ratio, n, n_est)
t_female <- select(t_female, GISJOIN, GISJOIN_CTY, date1, cum_days_interval, var_code, n_diff, n.x, n.y, var_ratio, n, n_est)

# write out the CSV files
write_csv(t_male, paste0(outdata_dir, "tct10_19_male.csv"))
write_csv(t_female, paste0(outdata_dir, "tct10_19_female.csv"))


