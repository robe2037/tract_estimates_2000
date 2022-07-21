
# -------------------------------------
#
# Read in decennial county data for 2010 (based on 2010 boundaries), 
# aggregate demographic categories, and
# convert to long format by county, age, sex, and race.
#
# -------------------------------------

library(ipumsr) # nhgis development version
library(tidyverse)

# Load data ----------------------------------

nhgis_cty_2010 <- read_nhgis(
  here::here("data", "extracts", "nhgis0529_csv.zip"), 
  data_layer = contains("2010_county")
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

vars <- vars %>%
  mutate(AGEGRP = recode(AGEGRP, !!!age_recode),
         SEX = str_sub(SEX, 1, 1),
         RACE = recode(RACE, !!!race_recode))

# Aggregate data to new variable levels ----

nhgis_cty_2010_agg <- nhgis_cty_2010 %>% 
  pivot_longer(
    cols = matches("^H9"),
    names_to = "nhgis_code",
    values_to = "POP"
  ) %>%
  left_join(vars) %>% 
  filter(!(is.na(SEX) & is.na(RACE) & is.na(AGEGRP))) %>%
  # filter(POP != 0) %>%
  group_by(GISJOIN, STATE, STATEA, COUNTY, COUNTYA, SEX, AGEGRP, RACE) %>%
  summarize(POP = sum(POP)) %>%
  mutate(DATAYEAR = 2010, .after = COUNTYA)

# Write ------------------------------------

write_csv(nhgis_cty_2010_agg, here::here("data", "preproc", "county", "nhgis_cty_2010_agg.csv"))
