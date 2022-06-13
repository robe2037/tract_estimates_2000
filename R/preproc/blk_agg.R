
library(data.table)
library(tidyverse)
library(ipumsr)

# Setup ------------------------------------------------------------------------

extents <- get_nhgis_metadata(dataset = "2000_SF1b") %>%
  pluck("geographic_instances") %>%
  pull(name)

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

# Wrapper for downloading and processing of block extracts
blk_to_cty_mrsf <- function(extract) {
  
  fp <- download_extract(extract) 
  
  blk_2000 <- read_nhgis(fp) %>%
    mutate(across(c(STATEA, COUNTYA), as.character))
  
  unlink(fp)
  
  # Aggregate data to new variable levels ----
  
  blk_2000_dt <- blk_2000 %>% 
    pivot_longer(
      cols = matches("^FYO"),
      names_to = "nhgis_code",
      values_to = "POP"
    ) %>%
    as.data.table()
  
  blk_2000_dt_agg <- blk_2000_dt[
    as.data.table(vars), 
    on = "nhgis_code"
  ][
    POP != 0 # Remove 0 cases to improve processing. Will add back later.
  ][, 
    .(POP = sum(POP), GISJOIN_CTY = stringr::str_sub(GISJOIN, 1, 8)), 
    by = .(GISJOIN, STATE, STATEA, COUNTY, COUNTYA, SEX, AGEGRP, RACE)
  ]
  
  # 2000 block level
  nhgis_blk <- as_tibble(blk_2000_dt_agg) %>%
    rename(GISJOIN_BLK = GISJOIN)
  
  state <- unique(nhgis_blk$STATEA)
  
  # Convert to 2010 boundaries and aggregate to create new MRSF ------------------
  
  # Data -------------------------------
  
  # 2000 MRSF
  mrsf <- vroom::vroom(
    here::here("data", "preproc", "mrsf", "mrsf_2000_agg.csv")
  ) %>%
    rename(POP_MRSF = POP) %>%
    select(-STATE, -COUNTY)
  
  # 2000 county level with 2000 boundaries
  nhgis_cty_2000 <- vroom::vroom(
    here::here("data", "preproc", "county", "nhgis_cty_2000_agg.csv")
  ) %>%
    rename(POP_CTY = POP) %>%
    select(-STATE, -COUNTY) %>%
    filter(STATEA != "72") # Ignore PR? Not in MRSF.
  
  # 2010 county level with 2010 boundaries
  nhgis_cty_2010 <- vroom::vroom(
    here::here("data", "preproc", "county", "nhgis_cty_2010_agg.csv")
  ) %>%
    rename(POP_CTY = POP) %>%
    select(-STATE, -COUNTY) %>%
    filter(STATEA != "72") # Ignore PR? Not in MRSF.
  
  # 2000-2010 block crosswalk. Currently process for single state:
  blk_xwalk <- list.files(
    "/pkg/ipums/istads/assets.nhgis.org/htdocs/crosswalks/nhgis_blk2000_blk2010_ge_state/",
    pattern = paste0(state, ".zip"),
    full.names = TRUE
  )
  
  blk_xwalk <- vroom::vroom(blk_xwalk) %>%
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
    filter(STATEA == state) # subset to state being processed
  
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
    mutate(GISJOIN_CTY_2010 = stringr::str_sub(GISJOIN_2010, 1, 8)) %>%
    group_by(GISJOIN_CTY_2010, SEX, AGEGRP, RACE) %>%
    summarize(POP_ADJ = sum(POP_ADJ, na.rm = TRUE)) %>%
    rename(GISJOIN = GISJOIN_CTY_2010)
  
  # Get all variable combinations for 2010 counties to reattach 0 cases
  mrsf_2000_2010 <- nhgis_cty_2010 %>%
    filter(STATEA == state) %>%
    select(GISJOIN, STATEA, COUNTYA, SEX, AGEGRP, RACE) %>%
    distinct() %>%
    full_join(agg) %>%
    filter(RACE != "other") %>% # MRSF does not include other.
    mutate(POP_ADJ = replace_na(POP_ADJ, 0))
  
  # Write ------------------------------------
  
  write_csv(
    mrsf_2000_2010,
    here::here(
      "data", 
      "preproc", 
      "block", 
      paste0(
        "cty_2010_blk_2000_agg_", extract$geographic_extents, ".csv")
    )
  )
  
}

# Processing -------------------------------------------------------------------

# Generate all extracts
purrr::walk(
  extents,
  ~{
    ext <- define_extract_nhgis(
      description = paste0(
        "Block counts for tract estimate pipeline. Extent: ", 
        .x
      ),
      datasets = "2000_SF1b",
      ds_tables = "NP012D",
      ds_geog_levels = "block",
      geographic_extents = .x
    )
    
    submit_extract(ext)
    
    Sys.sleep(1.5) # Avoid API limit of 60 requests per minute.
  }
)

# Get all relevant extracts
extracts <- get_recent_extracts_info_list("nhgis", length(extents))

# Download, process, and save aggregated file
purrr::walk(extracts, ~blk_to_cty_mrsf(.x))


