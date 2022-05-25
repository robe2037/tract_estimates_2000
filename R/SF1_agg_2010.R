
library(ipumsr) # development version
library(tidyverse)

# Load data ----------------------------------

sf1_2010 <- read_nhgis(
  here::here("data", "nhgis0482_csv.zip"), 
  data_layer = contains("2010")
)

# Variable recoding --------------------------

# Get metadata for relevant tables
tables <- paste0("P12", LETTERS[1:7])

vars <- purrr::map_dfr(
  tables,
  function(t) {
    meta <- get_nhgis_metadata(dataset = "2010_SF1a", ds_table = t)
    
    variables <- meta$variables
    desc <- stringr::str_extract(meta$description, "(?<=\\().+(?=\\))")
    
    variables$description <- paste0(variables$description, ": ", desc)
    
    variables
  }
)

# Convert data to long format and attach variable names from metadata
sf1_2010_long <- sf1_2010 %>%
  pivot_longer(cols = matches("^H9"), names_to = "VAR", values_to = "POP") %>%
  left_join(vars, by = c("VAR" = "nhgis_code")) %>%
  filter(str_count(description, ":") == 2) %>% # Removes total counts
  separate(description, into = c("SEX", "AGEGRP", "RACE"), sep = ": ")

# Crosswalks to regroup variables for aggregation
age_recode <- set_names(
  c("00_04", "05_09", "10_14", "15_19", "15_19", "20_24", "20_24", "20_24",
    "25_29", "30_34", "35_39", "40_44", "45_49", "50_54", "55_59", 
    "60_64", "60_64", "65_69", "65_69", "70_74", "75_79", "80_84", "85_up"),
  unique(sf1_2010_long$AGEGRP)
)

race_recode <- set_names(
  c("white", "black", "aian", "asian", "nhopi", "other", "multi"),
  unique(sf1_2010_long$RACE)
)

# Aggregate data to new variable levels ----

sf1_2010_agg <- sf1_2010_long %>%
  mutate(AGEGRP = recode(AGEGRP, !!!age_recode),
         SEX = str_sub(SEX, 1, 1),
         RACE = recode(RACE, !!!race_recode)) %>%
  group_by(GISJOIN, YEAR, STATE, STATEA, COUNTY, COUNTYA, SEX, AGEGRP, RACE) %>%
  summarize(POP = sum(POP), .groups = "drop")

# Write ------------------------------------

write_csv(sf1_2010_agg, here::here("data", "sf1_2010_agg.csv"))
