
library(ipumsr)
library(tidyverse)

# 2010-standardized 2000 + 2010 SF1a for Sex/Age/Race
define_extract_nhgis(
  description = "County and tract counts for tract estimate pipeline: standardized",
  time_series_tables = c(
    "CO7",
    "CO8", 
    "CO9",
    "CP0", 
    "CP1", 
    "CP2", 
    "CP3"
  ),
  tst_geog_levels = c("county", "tract"),
  tst_layout = "time_by_row_layout"
) %>% 
  submit_extract() %>%
  wait_for_extract() %>%
  download_extract(here::here("data", "extracts"))

# 2000 and 2010 non-standardized boundaries at county level.
define_extract_nhgis(
  description = "County and tract counts for tract estimate pipeline",
  datasets = c("2000_SF1a", "2010_SF1a"),
  ds_tables = list(
    "NP012D",
    c("P12A", "P12B", "P12C", "P12D", "P12E", "P12F", "P12G")
  ),
  ds_geog_levels = c("county", "tract")
) %>% 
  submit_extract() %>%
  wait_for_extract() %>%
  download_extract(here::here("data", "extracts"))
