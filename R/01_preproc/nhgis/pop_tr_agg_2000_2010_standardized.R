
# -------------------------------------
#
# Read in decennial tract data for 2000 and 2010 (standardized to 2010 boundaries), 
# aggregate demographic categories, and
# convert to long format by county, age, sex, and race.
#
# -------------------------------------

library(ipumsr) # development version
library(dplyr)
library(tidyr)
library(purrr)
library(readr)
library(stringr)

# Load data ----------------------------------

nhgis_tr_2000_2010 <- read_nhgis(
  here::here("data", "extracts", "nhgis0530_csv.zip"),
  data_layer = contains("tract")
)

tsts <- c("CO7", "CO8", "CO9", "CP0", "CP1", "CP2", "CP3")

# Variable recoding --------------------------

# Get metadata for relevant tables
vars <- purrr::map_dfr(
  tsts,
  ~get_nhgis_metadata(time_series_table = .x)$time_series %>%
    mutate(name = paste0(.x, name))
) %>%
  mutate(description = str_replace(description, "65 and 69", "67 to 69")) %>% # I think this is an error in NHGIS coding in one table?
  separate(description, into = c("RACE", "SEX", "AGEGRP"), sep = " ~ ")

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

# Convert data to long format and attach variable names from metadata
nhgis_tr_2000_2010_agg <- nhgis_tr_2000_2010 %>%
  pivot_longer(
    cols = matches("^CO[0-9]|^CP[0-9]"), 
    names_to = "VAR", 
    values_to = "POP"
  ) %>%
  filter(str_count(VAR) == 5) %>% # Remove MOEs
  left_join(vars, by = c("VAR" = "name")) %>%
  group_by(GISJOIN, DATAYEAR, STATE, STATEA, COUNTY, COUNTYA, SEX, AGEGRP, RACE) %>%
  summarize(POP = sum(POP), .groups = "drop")

# Write ------------------------------------

write_csv(
  nhgis_tr_2000_2010_agg,
  here::here("data", "preproc", "tract", "nhgis_tr_2000_2010_agg_standardized.csv")
)
