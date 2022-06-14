
library(ipumsr) # development version
library(tidyverse)

# Load data ----------------------------------

# 2000 tract level with 2000 boundaries
nhgis_tr <- read_nhgis(
  here::here("data", "extracts", "nhgis0529_csv.zip"),
  data_layer = contains("2000_tract")
)

# Variable recoding --------------------------

ds_meta <- get_nhgis_metadata(dataset = "2000_SF1a", ds_table = "NP012D")

vars <- ds_meta$variables %>%
  separate(description, into = c("RACE", "SEX", "AGEGRP"), sep = " >> ")

# Crosswalks to regroup variables for aggregation
age_recode <- set_names(
  c("00_04", "05_09", "10_14", "15_19", "15_19", "20_24", "20_24", "20_24",
    "25_29", "30_34", "35_39", "40_44", "45_49", "50_54", "55_59", 
    "60_64", "60_64", "65_69", "65_69", "70_74", "75_79", "80_84", "85_up"),
  unique(vars$AGEGRP)
)

race_recode <- set_names(
  c("white", "black", "aian", "asian", "nhopi", "other", "multi"),
  unique(vars$RACE)
)

vars <- vars %>%
  mutate(AGEGRP = recode(AGEGRP, !!!age_recode),
         SEX = str_sub(SEX, 1, 1),
         RACE = recode(RACE, !!!race_recode))

# Aggregate data to new variable levels ----

nhgis_tr_agg <- nhgis_tr %>% 
  pivot_longer(
    cols = matches(paste0("^", ds_meta$nhgis_code)),
    names_to = "nhgis_code",
    values_to = "POP"
  ) %>%
  left_join(vars) %>%
  # filter(POP != 0) %>%
  group_by(GISJOIN, STATE, STATEA, COUNTY, COUNTYA, SEX, AGEGRP, RACE) %>%
  summarize(POP = sum(POP))


# Write ------------------------------------

write_csv(nhgis_tr_agg, here::here("data", "preproc", "tract", "nhgis_tr_2000_agg.csv"))

