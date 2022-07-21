
library(tidyverse)
library(ipumsr) # nhgis api dev version

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

nhgis_tr_2010 <- vroom::vroom(
  here::here("data", "preproc", "tract", "nhgis_tr_2010_agg.csv")
) %>%
  rename(POP_TRA = POP) %>%
  rename(GISJOIN_TR = GISJOIN) %>%
  mutate(GISJOIN = str_sub(GISJOIN_TR, 1, 8), .after = GISJOIN_TR) %>%
  select(-STATE, -COUNTY)

# Calculate cty pop ratios for new race groups from MRSF
mrsf_comb <- full_join(mrsf, nhgis_cty_2010) %>%
  arrange(GISJOIN, SEX, AGEGRP, RACE)

# ------------------

mrsf_tract_allocation <- function(mrsf_data, tract_data, check = FALSE) {
  
  mrsf_data <- mrsf_data %>%
    pivot_wider(
      names_from = RACE,
      values_from = c(POP_MRSF, POP_CTY)
    ) %>%
    mutate(
      MULTI_INCREASE = POP_MRSF_multi >= POP_CTY_multi
    ) %>%
    pivot_longer(
      cols = matches("^POP"),
      names_to = c("SOURCE", "RACE"),
      values_to = "POP",
      names_sep = "_(?=[a-z])"
    ) %>%
    pivot_wider(
      names_from = SOURCE,
      values_from = POP
    )
  
  test_mrsf_multi_increase <- mrsf_data %>% 
    filter(MULTI_INCREASE) %>%
    group_by(GISJOIN, SEX, AGEGRP) %>%
    mutate(
      POP_CTY_DIFF = POP_MRSF - POP_CTY,
      PROP_CTY_DIFF = POP_CTY_DIFF / sum(POP_CTY_DIFF, na.rm = TRUE),
      PROP_CTY_DIFF = ifelse(is.nan(PROP_CTY_DIFF), 0, PROP_CTY_DIFF)
    )
  
  tra_allocated_multi_increase <- tract_data %>%
    right_join(
      test_mrsf_multi_increase,
      by = c("GISJOIN", "STATEA", "COUNTYA", "DATAYEAR", "SEX", "AGEGRP", "RACE")
    ) %>%
    group_by(GISJOIN_TR, SEX, AGEGRP) %>%
    mutate(
      POP_TRA_OTHER = sum(POP_TRA * as.numeric(RACE == "other")), # Apply POP_OTHER to entire group
      POP_TRA_NEW = POP_TRA + PROP_CTY_DIFF * POP_TRA_OTHER
    )
  
  test_mrsf_multi_decrease <- mrsf_data %>% 
    filter(!MULTI_INCREASE) %>%
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
    )
  
  tra_allocated_multi_decrease <- tract_data %>%
    right_join(
      test_mrsf_multi_decrease,
      by = c("GISJOIN", "STATEA", "COUNTYA", "DATAYEAR", "SEX", "AGEGRP", "RACE")
    ) %>%
    group_by(GISJOIN_TR, SEX, AGEGRP) %>%
    pivot_wider(
      names_from = RACE,
      values_from = c(POP_DIFF, PROP_DIFF, PROP_MULTI, POP_TRA),
      names_glue = "TMP_{.value}_{RACE}"
    ) %>%
    mutate(
      POP_ADJ_MULTI = TMP_POP_TRA_multi * TMP_PROP_MULTI_multi,
      DIFF_MULTI = TMP_POP_TRA_multi - POP_ADJ_MULTI,
      POP_TRA_OTHER = TMP_POP_TRA_other
    ) %>%
    pivot_longer(
      cols = matches("^TMP"),
      names_to = c("SOURCE", "RACE"),
      values_to = "POP",
      names_sep = "_(?=[a-z])",
      names_prefix = "TMP_"
    ) %>% 
    pivot_wider(
      names_from = SOURCE,
      values_from = POP
    ) %>%
    mutate(
      POP_TRA_NEW = ifelse(
        RACE == "multi",
        POP_ADJ_MULTI,
        POP_TRA + PROP_DIFF * (POP_TRA_OTHER + DIFF_MULTI)
      )
    )
  
  combined <- bind_rows(
    tra_allocated_multi_decrease,
    tra_allocated_multi_increase
  ) 
  
  if (check) {
    
    check_cty <- combined %>% 
      group_by(GISJOIN, RACE) %>% 
      summarize(CTY_NEW = sum(POP_TRA_NEW, na.rm = TRUE), 
                CTY_ORIG = sum(POP_TRA, na.rm = TRUE),
                .groups = "drop_last") %>%
      mutate(DIFF = CTY_NEW - CTY_ORIG)
    
    tot_diff <- check_cty %>% 
      summarize(TOT_DIFF = round(sum(DIFF), 12))
    
    non_other_diff <- check_cty %>%
      filter(RACE != "other") %>%
      summarize(TOT_DIFF = -1 * sum(DIFF))
    
    other_diff <- check_cty %>%
      filter(RACE == "other") %>%
      select(GISJOIN, DIFF)
    
    # Tract totals are consistent after allocation
    check_tra <- combined %>% 
      group_by(GISJOIN_TR) %>% 
      summarize(tot = sum(POP_TRA) == sum(POP_TRA_NEW, na.rm = TRUE)) %>% 
      pull(tot)
    
    bad_cty <- tot_diff %>%
      filter(TOT_DIFF != 0) %>%
      pull(GISJOIN)
    
    bad_cty <- union(
      bad_cty, 
      non_other_diff[non_other_diff$TOT_DIFF != other_diff$DIFF,]$GISJOIN
    )
    
    checks_out <- all(tot_diff$TOT_DIFF == 0) && 
      all(non_other_diff$TOT_DIFF == other_diff$DIFF) &&
      all(check_tra)
    
    if (!checks_out) {
      warning(
        "New totals were not consistent with values to be allocated. ",
        paste0("See: ", bad_cty, collapse = ", "),
        call. = FALSE
      )
    }
    
  }
  
  combined %>%
    select(
      GISJOIN_TR, GISJOIN, STATEA, COUNTYA, DATAYEAR,
      SEX, AGEGRP, RACE, POP = POP_TRA_NEW
    ) %>%
    filter(RACE != "other") %>%
    arrange(GISJOIN, GISJOIN_TR, SEX, AGEGRP, RACE)
  
}

tictoc::tic()
mrsf_realloc <- mrsf_tract_allocation(
  mrsf_data = mrsf_comb,
  tract_data = nhgis_tr_2010,
  check = TRUE
)
tictoc::toc()

# ------------------

write_csv(mrsf_realloc, here::here("data", "preproc", "tract", "tract_mrsf_reallocation_2010.csv"))
