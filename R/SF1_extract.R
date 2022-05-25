
library(ipumsr)
library(tidyverse)

define_extract_nhgis(
  description = "County counts for tract estimate pipeline",
  datasets = c(
    "2000_SF1a", 
    "2010_SF1a"
  ),
  ds_tables = list(
    "NP012D",
    paste0("P12", LETTERS[1:7])
  ),
  ds_geog_levels = "county"
) %>% 
  submit_extract() %>%
  wait_for_extract() %>%
  download_extract()
