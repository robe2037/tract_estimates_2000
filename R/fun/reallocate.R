
library(dplyr)
library(readr)
library(tidyr)
library(purrr)
library(ipumsr)
library(data.table)

#' Reallocate race counts to MRSF categories
#' 
#' The MRSF does not include other-race counts, but decennial data do. This
#' reallocates the other race counts for a sub-county geography based on
#' their recorded other-race count for a given sex and age group category.
#' 
#' For each 2010 tract, we reallocate all "other race" persons listed in the
#' decennial data to each of the race categories listed above relative to the proportion
#' of the total reallocation population (those listed as "other race" in the
#' decennial) that was reallocated to that race class at the county level.
#' Furthermore, this is weighted by the number of "other race" counts in that block.
#' 
#' For example, if 10 people were listed as "other race" for a given county, sex, and age,
#' and 2 of these were allocated to the asian race category, then 20% of the
#' listed "other race" population for each tract within that county will be
#' added to that tract's asian population.
#' 
#' In some cases, the count for the multi-race category is smaller in the MRSF
#' than in the decennial data. Thus, there are more individuals that need to be
#' reallocated than listed in the other-race category alone.
#' 
#' In these cases, we reduce each tract's multi-race population in proportion to
#' the decrease in this group between the MRSF and decennial data for a given
#' county, sex, and age. Then, the remaining multi-race population for each tract
#' is added to that tract's other population to determine the total reallocation
#' population for that tract. The reallocation procedure then proceeds as above,
#' except for the fact that the multi-race group is not adjusted further.
#' 
#' @param dec_data Decennial data containing other-race counts that need
#'   to be reallocated. Should be in long format by sex, age, and race. Supports
#'   any sub-county geography, as long as county-level GISJOIN codes can be
#'   recreated from that geography's GISJOIN.
#' @param mrsf_data County-level MRSF data in long format by sex, age, and race
#' @param var_key Data frame in long format mapping NHGIS variable codes to
#'   their associated sex, age, and race groups
#'
#' @return Long format data frame of decennial data with no other-race counts
#'   and adjusted counts for non-other race categories
reallocate_race <- function(dec_data, mrsf_data, var_key) {
  
  county <- unique(dec_data$GISJOIN_CTY)
  
  mrsf_data <- mrsf_data %>%
    filter(GISJOIN %in% county)
  
  dec_data_cty <- dec_data %>%
    pivot_longer(
      cols = matches(paste0(var_key$nhgis_code, collapse = "|")),
      names_to = "nhgis_code",
      values_to = "POP"
    ) %>%
    as.data.table()
  
  # All GISJOIN / variable combinations
  # For recovering observations that are filtered out during processing
  dec_var_combos <- expand_grid(
    dec_data_cty %>% select(GISJOIN, GISJOIN_CTY, STATE, STATEA, COUNTY, COUNTYA) %>% distinct(),
    var_key %>% select(RACE, SEX, AGEGRP) %>% distinct()
  ) %>%
    as.data.table()
  
  nhgis_dec <- dec_data_cty[
    as.data.table(var_key),
    on = "nhgis_code"
  ][
    # Remove 0 population cases for speed since these will not affect the aggregation
    POP != 0  
  ][,
    # Aggregate population within new demographic variable groups 
    .(POP = sum(POP)),
    by = .(GISJOIN, GISJOIN_CTY, STATE, STATEA, COUNTY, COUNTYA, SEX, AGEGRP, RACE)
  ][
    # Reattach 0 population cases
    dec_var_combos,
    on = .(GISJOIN, GISJOIN_CTY, STATE, STATEA, COUNTY, COUNTYA, SEX, AGEGRP, RACE)
  ][,
    # Create county GISJOIN, recode 0 pop cases that were filtered out in last step
    `:=`(
      # GISJOIN_CTY = stringr::str_sub(GISJOIN, 1, 8),
      POP = ifelse(is.na(POP), 0, POP)
      # DATAYEAR = 2000
    )
  ][,
    # Encode each block's total OTHER and MULTI group counts as separate columns
    `:=`(
      POP_SUB_OTHER = sum(POP * as.numeric(RACE == "other")),
      POP_SUB_MULTI = sum(POP * as.numeric(RACE == "multi"))
    ),
    by = .(GISJOIN, STATE, STATEA, COUNTY, COUNTYA, SEX, AGEGRP)
  ][,
    # Rename columns for clarity
    `:=`(
      GISJOIN_SUB = GISJOIN,
      GISJOIN = GISJOIN_CTY,
      GISJOIN_CTY = NULL
    )
  ]
  
  # Multi increase ------------
  
  mrsf_multi_increase <- mrsf_data %>% 
    filter(MULTI_INCREASE)
  
  if (nrow(mrsf_multi_increase) > 0) {
    
    mrsf_multi_increase <- mrsf_multi_increase %>%
      group_by(GISJOIN, SEX, AGEGRP) %>%
      mutate(
        POP_CTY_DIFF = POP_MRSF - POP_CTY,
        PROP_CTY_DIFF = POP_CTY_DIFF / sum(POP_CTY_DIFF, na.rm = TRUE),
        PROP_CTY_DIFF = ifelse(is.nan(PROP_CTY_DIFF), 0, PROP_CTY_DIFF)
      ) %>%
      as.data.table()
    
    dec_multi_increase <- nhgis_dec[
      mrsf_multi_increase,
      on = .(GISJOIN, STATEA, COUNTYA, SEX, AGEGRP, RACE)
    ][
      POP_SUB_OTHER > 0,
      POP_SUB_NEW := POP + PROP_CTY_DIFF * POP_SUB_OTHER
    ][
      POP_SUB_OTHER == 0,
      POP_SUB_NEW := POP
    ]
    
  } else {
    
    dec_multi_increase <- data.table()
    
  }
  
  # Multi decrease ---------
  
  mrsf_multi_decrease <- mrsf_data %>% 
    group_by(GISJOIN, SEX, AGEGRP) %>%
    filter(!MULTI_INCREASE)
  
  if (nrow(mrsf_multi_decrease) > 0) {
    
    mrsf_multi_decrease <- mrsf_multi_decrease %>%
      mutate(
        POP_DIFF = POP_MRSF - POP_CTY
      ) %>%
      pivot_wider(
        names_from = RACE, 
        values_from = c(POP_MRSF, POP_CTY, POP_DIFF)
      ) %>%
      mutate(
        TO_ALLOCATE = POP_CTY_other + (POP_CTY_multi - POP_MRSF_multi),
        # MULTI_INCREASE = POP_MRSF_multi >= POP_CTY_multi,
        PROP_MULTI = POP_MRSF_multi / POP_CTY_multi
      ) %>% 
      select(
        GISJOIN, STATEA, COUNTYA, SEX, AGEGRP, TO_ALLOCATE, DATAYEAR,
        matches("POP_DIFF"),
        # POP_MRSF_multi, POP_CTY_multi,
        PROP_MULTI
      ) %>%
      pivot_longer(
        cols = matches("POP_DIFF"),
        names_to = "RACE",
        values_to = "POP_DIFF",
        names_prefix = "POP_DIFF_"
      ) %>%
      mutate(
        PROP_DIFF = POP_DIFF / TO_ALLOCATE
      ) %>%
      as.data.table()
    
    dec_multi_decrease <- nhgis_dec[
      mrsf_multi_decrease,
      on = .(GISJOIN, STATEA, COUNTYA, SEX, AGEGRP, RACE)
    ][,
      POP_ADJ_MULTI := POP_SUB_MULTI * PROP_MULTI
    ][,
      DIFF_MULTI := POP_SUB_MULTI - POP_ADJ_MULTI
    ][
      (POP_SUB_OTHER > 0 | POP_SUB_MULTI > 0) & RACE == "multi",
      POP_SUB_NEW := POP_ADJ_MULTI
    ][
      (POP_SUB_OTHER > 0 | POP_SUB_MULTI > 0) & RACE != "multi",
      POP_SUB_NEW := POP + PROP_DIFF * (POP_SUB_OTHER + DIFF_MULTI)
    ][
      POP_SUB_OTHER == 0 & POP_SUB_MULTI == 0,
      POP_SUB_NEW := POP
    ][
      order(GISJOIN_SUB, SEX, AGEGRP, RACE)
    ]
    
  } else {
    
    dec_multi_decrease <- data.table()
    
  }
  
  dec_reallocated <- rbindlist(
    list(
      dec_multi_increase,
      dec_multi_decrease
    ),
    fill = TRUE
  )[
    order(GISJOIN_SUB, SEX, AGEGRP, RACE)
  ]
  
  dec_reallocated
  
}


#' Perform basic checks on reallocation results
#' 
#' Ensures that county and sub-geography counts add up to the same value after
#' reallocation and that the number of people listed as "other race" is the same
#' as the number that are reallocated to all non-other race categories.
#'
#' @param realloc_data Reallocated data produced by reallocate_race()
#'
#' @return Returns the input and warns if a check fails
check_reallocation <- function(realloc_data) {
  
  # All var combos present for each block
  realloc_var_counts1 <- all(count(realloc_data, GISJOIN_SUB)$n == 252)
  realloc_var_counts2 <- all(count(realloc_data, GISJOIN_SUB, SEX, AGEGRP)$n == 7)
  
  # No cases where POP is reallocated if no OTHER or MULTI counts
  bad_realloc <- nrow(realloc_data %>% 
                        filter(
                          POP_SUB_OTHER == 0, 
                          POP_SUB_MULTI == 0,
                          POP_SUB_NEW != POP)
  )
  
  check_cty <- realloc_data %>%
    group_by(GISJOIN, RACE) %>%
    summarize(
      CTY_NEW = sum(POP_SUB_NEW, na.rm = TRUE),
      CTY_ORIG = sum(POP, na.rm = TRUE),
      .groups = "drop_last"
    ) %>%
    mutate(DIFF = CTY_NEW - CTY_ORIG)
  
  # Total county level original pop and reallocated pop are the same:
  tot_diff <- check_cty %>%
    summarize(TOT_DIFF = round(sum(DIFF), 7), .groups = "drop")
  
  # The number of reallocated people is equal to the number of "other"
  non_other_diff <- check_cty %>%
    filter(RACE != "other") %>%
    summarize(TOT_DIFF = -1 * round(sum(DIFF), 7), .groups = "drop")
  
  other_diff <- check_cty %>%
    filter(RACE == "other") %>%
    select(GISJOIN, DIFF)
  
  # Total pop in each block is the same after reallocation
  check_realloc <- realloc_data %>%
    group_by(GISJOIN_SUB) %>%
    summarize(tot = round(sum(POP), 10) == round(sum(POP_SUB_NEW, na.rm = TRUE), 10), .groups = "drop") %>%
    pull(tot) %>%
    all()
  
  # ID bad counties
  bad_cty <- tot_diff %>%
    filter(TOT_DIFF != 0) %>%
    pull(GISJOIN)
  
  bad_cty <- union(
    bad_cty,
    non_other_diff[non_other_diff$TOT_DIFF != other_diff$DIFF,]$GISJOIN
  )
  
  passes <- all(tot_diff$TOT_DIFF == 0) &&
    all(non_other_diff$TOT_DIFF == other_diff$DIFF) &&
    check_realloc &&
    realloc_var_counts1 &&
    realloc_var_counts2 &&
    bad_realloc == 0
  
  if (!passes) {
    warning(
      "Something didn't add up. Problems with: ",
      paste0(bad_cty, collapse = ", "),
      call. = FALSE
    )
  }
  
  realloc_data
  
}

#' Convert block-level data in 2000 to 2010 tract boundaries
#'
#' @param blk_data Reallocated data at the block level
#' @param blk_xwalk Nationwide crosswalk data for 2000 blocks to 2010 blocks
#'
#' @return Reallocated 2000 data at 2010 tract level
blk_convert_geog <- function(blk_data, blk_xwalk) {
  
  # state <- blk_data$STATEA
  
  blk_data <- blk_data[
    RACE != "other",
    .(GISJOIN_SUB, GISJOIN, STATE, STATEA, COUNTY, COUNTYA, DATAYEAR, SEX, AGEGRP, RACE, POP, POP_SUB_NEW)
  ] %>%
    as_tibble()
  
  blk_geog_merge <- left_join(
    blk_data,
    blk_xwalk,
    by = c("GISJOIN_SUB" = "GISJOIN_2000")
  ) %>%
    as.data.table()
  
  blk_geog_converted <- blk_geog_merge[
    ,
    `:=`(
      POP_SUB_NEW = POP_SUB_NEW * WEIGHT,
      GISJOIN_TR = stringr::str_sub(GISJOIN_2010, 1, 14),
      GISJOIN = str_sub(GISJOIN_2010, 1, 8),
      STATEA = str_sub(GISJOIN_2010, 2, 3),
      COUNTYA = str_sub(GISJOIN_2010, 5, 7),
      GEOGYEAR = 2010
    )
  ][,
    .(POP_ADJ = sum(POP_SUB_NEW, na.rm = TRUE)),
    by = .(GISJOIN_TR, GISJOIN, STATEA, COUNTYA, DATAYEAR, GEOGYEAR, SEX, AGEGRP, RACE)
  ]
  
  as_tibble(blk_geog_converted)
  
}

#' Load block data from a block-level NHGIS extract.
#'
#' @param extract_number Number of the extract
#'
#' @return block-level data contained in the extract
load_blk_data <- function(extract_number) {
  
  ext <- get_extract_info(paste0("nhgis:", extract_number))
  
  fp <- download_extract(
    ext,
    tempdir(),
    overwrite = TRUE
  )
  
  dat <- read_nhgis(fp) %>%
    mutate(STATEA = as.character(STATEA),
           GISJOIN_CTY = str_sub(GISJOIN, 1, 8))
  
  unlink(fp)
  
  dat
  
}

#' Load nationwide block crosswalk and calculate GISJOIN codes
#'
#' @return Crosswalk data with 2000 blocks, 2010 blocks, and weights
load_blk_xwalk <- function() {
  
  blk_xwalk <- vroom::vroom(
    here::here("data", "raw", "xwalk", "nhgis_blk2000_blk2010_ge.zip"),
    show_col_types = FALSE
  ) %>%
    as.data.table()
  
  blk_xwalk[
    ,
    `:=`(
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
      ),
      STATEA_2000 = str_sub(GEOID00, 1, 2),
      STATEA_2010 = str_sub(GEOID10, 1, 2)
    )
  ][
    ,
    .(GISJOIN_2000, GISJOIN_2010, WEIGHT, PAREA)
  ]
  
  blk_xwalk
  
}

#' Wrapper to perform entire reallocation pipeline for a set of states contained
#' in individual NHGIS extracts
#'
#' @param extract_numbers Numbers of the extracts to process. Each extract should
#'   consist of 2000 block-level data for a single state
#' @param write Logical indicating whether to write the result
#'
#' @return Reallocated 2000 data at the 2010 tract level for each of the states
#'   in the provided extracts
blk_convert_geog_batch <- function(extract_numbers, write = TRUE) {
  
  blk_xwalk <- load_blk_xwalk()
  
  # Use table metadata to create mapping of NHGIS var codes to demographic
  # categories
  var_key <- get_nhgis_metadata(dataset = "2000_SF1b", ds_table = "NP012D") %>%
    pluck("variables") %>%
    separate(description, into = c("RACE", "SEX", "AGEGRP"), sep = " >> ")
  
  age_recode <- set_names(
    c("00_04", "05_09", "10_14", "15_19", "15_19", "20_24", "20_24", "20_24",
      "25_29", "30_34", "35_39", "40_44", "45_49", "50_54", "55_59", 
      "60_64", "60_64", "65_69", "65_69", "70_74", "75_79", "80_84", "85_up"),
    unique(var_key$AGEGRP)
  )
  
  race_recode <- set_names(
    c("white", "black", "aian", "asian", "nhopi", "other", "multi"),
    unique(var_key$RACE)
  )
  
  var_key <- var_key %>%
    mutate(AGEGRP = recode(AGEGRP, !!!age_recode),
           SEX = str_sub(SEX, 1, 1),
           RACE = recode(RACE, !!!race_recode)) %>%
    as.data.table()
  
  out <- purrr::map(
    extract_numbers,
    ~{
      
      blk_data <- load_blk_data(extract_number = .x)
      
      cties <- unique(blk_data$GISJOIN_CTY)
      state <- unique(blk_data$STATEA)
      
      state_xwalk <- blk_xwalk[STATEA_2000 == state]
      
      # 2000 MRSF
      mrsf <- vroom::vroom(
        here::here("data", "preproc", "mrsf", "mrsf_2000_agg.csv"),
        show_col_types = FALSE
      ) %>%
        rename(POP_MRSF = POP) %>%
        select(-STATE, -COUNTY)  %>%
        filter(STATEA == state)
      
      # 2000 county level with 2000 boundaries
      nhgis_cty_2000 <- vroom::vroom(
        here::here("data", "preproc", "county", "nhgis_cty_2000_agg.csv"),
        show_col_types = FALSE
      ) %>%
        rename(POP_CTY = POP) %>%
        select(-STATE, -COUNTY) %>%
        filter(STATEA == state)
      
      # Calculate cty pop ratios for new race groups from MRSF
      mrsf_data <- full_join(
        mrsf, 
        nhgis_cty_2000, 
        by = c("GISJOIN", "STATEA", "COUNTYA", "DATAYEAR", "SEX", "AGEGRP", "RACE")
      ) %>%
        mutate(MULTI_INCREASE = ifelse(RACE == "multi", POP_MRSF >= POP_CTY, NA)) %>%
        group_by(GISJOIN, SEX, AGEGRP) %>%
        mutate(MULTI_INCREASE = all(MULTI_INCREASE, na.rm = TRUE)) %>%
        ungroup() %>%
        arrange(GISJOIN, SEX, AGEGRP, RACE)
      
      blk_geog_converted <- purrr::imap_dfr(
        cties,
        ~{
          message(
            paste0("Processing county ", .y, " of ", length(cties), ": ", .x)
          )
          
          blk_data %>%
            filter(GISJOIN_CTY == .x) %>%
            reallocate_race(mrsf_data, var_key) %>%
            check_reallocation() %>%
            blk_convert_geog(blk_xwalk = state_xwalk)
        }
      )
      
      # Some blocks in different counties map to same tract in 2010
      # so we can get multiple sub-counts for each 2010 tract, and should sum
      blk_geog_converted <- blk_geog_converted %>%
        group_by(GISJOIN_TR, GISJOIN, STATEA, COUNTYA, DATAYEAR, GEOGYEAR, SEX, AGEGRP, RACE) %>%
        summarize(POP_ADJ = sum(POP_ADJ), .groups = "drop")
      
      if (write) {
        vroom::vroom_write(
          blk_geog_converted,
          here::here(
            "data", "preproc", "tract", "states", glue::glue("tract_mrsf_reallocation_2000_state{state}.csv")
          ),
          delim = ","
        )
      }
      
      blk_geog_converted
      
    }
  )
  
  out
  
}
