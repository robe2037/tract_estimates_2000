
library(tidyverse)
library(ipumsr)

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
) %>% 
  filter(str_detect(description, "[0-9]")) %>%
  separate(description, into = c("SEX", "AGEGRP", "RACE"), sep = ": ")

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

var_key <- vars %>%
  mutate(AGEGRP = recode(AGEGRP, !!!age_recode),
         SEX = str_sub(SEX, 1, 1),
         RACE = recode(RACE, !!!race_recode))

# 2010 tract level with 2010 boundaries
nhgis_tr <- read_nhgis(
  here::here("data", "extracts", "nhgis0529_csv.zip"),
  data_layer = contains("2010_tract")
) %>%
  mutate(STATEA = as.character(STATEA),
         GISJOIN_CTY = str_sub(GISJOIN, 1, 8))

# 2010 MRSF
mrsf <- vroom::vroom(
  here::here("data", "preproc", "mrsf", "mrsf_2010_agg.csv")
) %>%
  rename(POP_MRSF = POP) %>%
  select(-STATE, -COUNTY)

# 2010 county level with 2010 boundaries
nhgis_cty_2010 <- vroom::vroom(
  here::here("data", "preproc", "county", "nhgis_cty_2010_agg.csv")
) %>%
  rename(POP_CTY = POP) %>%
  select(-STATE, -COUNTY) %>%
  filter(STATEA != "72") # Ignore PR? Not in MRSF.

# Calculate cty pop ratios for new race groups from MRSF
mrsf_comb <- full_join(
  mrsf, 
  nhgis_cty_2010,
  by = c("GISJOIN", "STATEA", "COUNTYA", "DATAYEAR", "SEX", "AGEGRP", "RACE")
) %>%
  mutate(MULTI_INCREASE = ifelse(RACE == "multi", POP_MRSF >= POP_CTY, NA)) %>%
  group_by(GISJOIN, SEX, AGEGRP) %>%
  mutate(MULTI_INCREASE = all(MULTI_INCREASE, na.rm = TRUE)) %>%
  ungroup() %>%
  arrange(GISJOIN, SEX, AGEGRP, RACE)

# Process ---

new <- blk_reallocate(
  nhgis_tr %>% filter(STATEA == "10"), 
  mrsf_comb, 
  var_key
) %>%
  select(GISJOIN_TR = GISJOIN_SUB, GISJOIN, STATEA, COUNTYA, DATAYEAR, SEX, AGEGRP, RACE, POP) %>%
  as_tibble() %>%
  filter(RACE != "other")

# old <- vroom::vroom(here::here("data", "preproc", "tract", "tract_mrsf_reallocation_2010.csv"))
# 
# old <- old %>%
#   filter(STATEA == "10") %>%
#   filter(RACE != "other")

