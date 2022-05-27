
library(dtplyr)
library(tidyverse)
library(ipumsr)

blk_2000 <- ipumsr::read_nhgis(
  here::here("data", "nhgis0487_csv.zip"), data_layer = contains("block")
)

vars <- get_nhgis_metadata(dataset = "2000_SF1b", ds_table = "NP012D")$variables %>%
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

blk_2000_dt <- blk_2000 %>% 
  pivot_longer(
    cols = matches("^FYO"),
    names_to = "nhgis_code",
    values_to = "POP"
  ) %>%
  as.data.table()

blk_2000_dt_agg <- blk_2000_dt[
  POP != 0
][
  as.data.table(vars), on = "nhgis_code"
][
  , 
  .(POP = sum(POP)), 
  by = .(GISJOIN, STATE, STATEA, COUNTY, COUNTYA, SEX, AGEGRP, RACE)
]

# Write ------------------------------------

write_csv(
  as_tibble(blk_2000_dt_agg), 
  here::here("data", "blk_agg_010.csv")
)
