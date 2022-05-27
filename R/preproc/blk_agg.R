
library(data.table)
library(tidyverse)
library(ipumsr)

extents <- get_nhgis_metadata(dataset = "2000_SF1b") %>%
  pluck("geographic_instances") %>%
  pull(name)

extent <- extents[2]

# 2000 SF1b for block-level Sex/Age/Race
fp <- define_extract_nhgis(
  description = "Block counts for tract estimate pipeline",
  datasets = "2000_SF1b",
  ds_tables = "NP012D",
  ds_geog_levels = "block",
  geographic_extents = extent
) %>% 
  submit_extract() %>%
  wait_for_extract() %>%
  download_extract(here::here("data", "extracts"))

# For old extracts:
# fp <- download_extract("nhgis:489", here::here("data","extracts"), overwrite = TRUE)

blk_2000 <- ipumsr::read_nhgis(fp)

vars <- get_nhgis_metadata(dataset = "2000_SF1b", ds_table = "NP012D") %>%
  pluck("variables") %>%
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
  as.data.table(vars), on = "nhgis_code"
][
  POP != 0
][, 
  .(POP = sum(POP)), 
  by = .(GISJOIN, STATE, STATEA, COUNTY, COUNTYA, SEX, AGEGRP, RACE)
]

# Write ------------------------------------

write_csv(
  as_tibble(blk_2000_dt_agg), 
  here::here("data", "preproc", "block", paste0("blk_agg_", extent, ".csv"))
)
