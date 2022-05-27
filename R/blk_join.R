
library(ipumsr)
library(tidyverse)

# Data -------------------------------

# 2000 MRSF
mrsf <- vroom::vroom(here::here("data", "mrsf_2000_agg.csv")) %>%
  rename(POP_MRSF = POP) %>%
  select(-STATE, -COUNTY)

# 2000 county level with 2000 boundaries
nhgis_cty_2000 <- vroom::vroom(here::here("data", "nhgis_cty_2000_agg.csv")) %>%
  rename(POP_CTY = POP) %>%
  select(-STATE, -COUNTY)

# 2010 county level with 2010 boundaries
nhgis_cty_2010 <- vroom::vroom(here::here("data", "nhgis_cty_2010_agg.csv")) %>%
  rename(POP_CTY = POP) %>%
  select(-STATE, -COUNTY)

# 2000 block level
nhgis_blk <- vroom::vroom(here::here("data", "blk_agg_010.csv")) %>%
  mutate(GISJOIN_CTY = stringr::str_sub(GISJOIN, 1, 8)) %>%
  rename(GISJOIN_BLK = GISJOIN)

# 2000-2010 block crosswalk. Currently for single state.
blk_xwalk <- ipumsr::read_nhgis(
  here::here("data", "nhgis_blk2000_blk2010_ge_01.zip")
) %>%
  mutate(
    GISJOIN_2000 = paste0(
      "G",
      str_sub(GEOID00, 1, 2),
      "0",
      str_sub(GEOID00, 3, 5),
      "0",
      str_sub(GEOID00, 6, 15)
    ),
    GISJOIN_2010 = paste0(
      "G",
      str_sub(GEOID10, 1, 2),
      "0",
      str_sub(GEOID10, 3, 5),
      "0",
      str_sub(GEOID10, 6, 15)
    )
  ) %>%
  select(-matches("GEOID"))

# Aggregate 2000 blocks to 2010 counties --------

# Calculate cty pop ratios from MRSF
mrsf_ratio <- full_join(mrsf, nhgis_cty_2000) %>%
  mutate(RATIO = POP_MRSF / POP_CTY) %>%
  filter(STATEA == "01") # Temporary for test state

# Apply cty pop ratios to 2000 block data
nhgis_blk_mrsf_adj <- left_join(
  nhgis_blk,
  mrsf_ratio, 
  by = c("GISJOIN_CTY" = "GISJOIN",
         "STATEA",
         "COUNTYA",
         "AGEGRP",
         "SEX",
         "RACE")
) %>%
  mutate(POP_ADJ = POP * RATIO)

# Sum 2010 block data to 2010 counties
agg <- left_join(
  nhgis_blk_mrsf_adj,
  blk_xwalk,
  by = c("GISJOIN_BLK" = "GISJOIN_2000")
) %>%
  mutate(POP_ADJ = POP_ADJ * WEIGHT) %>%
  mutate(GISJOIN_CTY_2010 = stringr::str_sub(GISJOIN_2010, 1, 8)) %>% # Is this necessary? We already have a GISJOIN_CTY but not sure if any blocks change county codes in the xwalk.
  group_by(GISJOIN_CTY_2010, SEX, AGEGRP, RACE) %>%
  summarize(POP_ADJ = sum(POP_ADJ, na.rm = TRUE)) %>%
  rename(GISJOIN = GISJOIN_CTY_2010)

# Get all variable combinations for 2010 counties to reattach 0 cases
mrsf_2000_2010 <- nhgis_cty_2010 %>%
  select(GISJOIN, STATEA, COUNTYA, SEX, AGEGRP, RACE) %>%
  distinct() %>%
  full_join(agg) %>%
  filter(RACE != "other") %>% # MRSF does not include other.
  mutate(POP_ADJ = replace_na(POP_ADJ, 0))

