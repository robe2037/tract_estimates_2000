
# -------------------------------------
#
# Read in raw MRSF data from 2010, aggregate race categories, and
# convert to long format by county, age, sex, and race.
#
# -------------------------------------

library(dplyr)
library(tidyr)
library(purrr)
library(readr)

# Read MRSF ----------------------

mrsf_2010 <- vroom::vroom(
  list.files(here::here("data", "raw", "mrsf", "2010"), full.names = TRUE)[1:2]
) %>%
  mutate(GISJOIN = paste0("G", STATE, "0", COUNTY, "0")) %>%
  relocate(GISJOIN, .after = SUMLEV)

# Format MRSF --------------------

# Crosswalks
geog_info <- mrsf_2010 %>% 
  select(SUMLEV, GISJOIN, STATE, COUNTY, STNAME, CTYNAME) %>%
  distinct()

race_recode <- set_names(c("white", "black", "aian", "asian", "nhopi", rep("multi", 26)), 1:31)
sex_recode <- set_names(c("M", "F"), 1:2)
age_recode <- set_names(c("00_04", "05_09", "10_14", "15_19", "20_24", 
                          "25_29", "30_34", "35_39", "40_44", "45_49",
                          "50_54", "55_59", "60_64", "65_69", "70_74",
                          "75_79", "80_84", "85_up"), 1:18)

# All combinations of variables to recover 0 counts 
# which were not recorded in original data.
combos <- expand(mrsf_2010, GISJOIN, SEX, AGEGRP, IMPRACE, ORIGIN) %>%
  left_join(geog_info, by = "GISJOIN")

# Aggregate to new variable groups ---

mrsf_2010_agg <- full_join(mrsf_2010, combos) %>%
  filter(STATE != "72") %>%
  mutate(RESPOP = replace_na(RESPOP, 0),
         IMPRACE = recode(IMPRACE, !!!race_recode),
         SEX = recode(SEX, !!!sex_recode),
         AGEGRP = recode(AGEGRP, !!!age_recode),
         DATAYEAR = 2010) %>%
  group_by(GISJOIN, DATAYEAR, SEX, AGEGRP, IMPRACE) %>%
  summarize(RESPOP = sum(RESPOP), .groups = "drop") %>%
  arrange(GISJOIN, SEX, AGEGRP, IMPRACE) %>%
  left_join(geog_info, by = "GISJOIN") %>%
  select(GISJOIN, STATE = STNAME, COUNTY = CTYNAME,
         STATEA = STATE, COUNTYA = COUNTY, DATAYEAR,
         SEX, AGEGRP, RACE = IMPRACE, POP = RESPOP)

# Write ------------------------------

write_csv(mrsf_2010_agg, here::here("data", "preproc", "mrsf", "mrsf_2010_agg.csv"))
