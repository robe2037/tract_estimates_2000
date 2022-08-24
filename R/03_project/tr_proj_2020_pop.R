
# -------------------------------------
#
# Generate 2020 population projections from 2000 and 2010 population data.
#
# For interpolation from 2010-2019, we need estimated endpoint populations for
# 2019. We use cohort change ratios (CCRs) and child-to-woman ratios (CTWs) to 
# generate projected 2020 populations from 2000 and 2010 data that have already 
# been reallocated to the MRSF race categories.
# 
# CCRs are calculated as the ratio of the population of a given cohort in a
# given decennial census to the 10-years-younger population in the previous 
# decennial census. We calculate CCRs for each tract, sex, and race group. In
# cases where counts are too small to enable the calculation of a CCR, we 
# use the CCRs calculated at the tract and sex group level. For cases where
# CCRs are still undefined, we use CCRs calculated at the county and sex group
# level. CCRs are undefined with the 10-years-younger population is 0.
#
# There are remaining cases where CCRs are still undefined even at the county
# and sex level (in special cases of Loving County, TX, and Kalawao County, HI).
# In these cases, we artificially set the undefined CCRs to 1.
#
# CCRs are not defined for age groups under 10. In these cases, we calculate
# the ratio of the 0-5 and 5-10 populations to the female mother-aged population 
# at the tract, sex, and race level. This CTW ratio is used with the projected
# 2020 mother-aged population to generate an projection for the 2020 population
# of the 0-5 and 5-10 age groups. In cases where the CTW is undefined (because
# there is no mother-aged population for a given tract, sex, and race), we
# use the CTWs calculated for tract/sex groups or county/sex groups as needed.
# (Obviously, mothers need not be the same race as children, but this restriction
# was placed to enable these calculations at the tract/sex/race groups. CTWs for
# multi-race groups may therefore be particularly affected.)
#
# We combine the 2020 population projections obtained via CCR and CTW into
# a final data source for use in interpolation to obtain annual population
# estimates.
#
# -------------------------------------

source(here::here("R", "fun", "interpolate.R"))

# Import Data ------------------------------------------------------------------

# Tract-level counts reallocated to MRSF race categories for 2000 and 2010

tr_realloc_2000 <- vroom::vroom(
  here::here("data", "preproc", "tract", "tract_mrsf_reallocation_2000.csv")
) %>%
  select(-DATAYEAR) %>%
  rename(POP_2000 = POP_ADJ)

tr_realloc_2010 <- vroom::vroom(
  here::here("data", "preproc", "tract", "tract_mrsf_reallocation_2010.csv")
) %>%
  select(-DATAYEAR) %>%
  rename(POP_2010 = POP_ADJ)

tr_realloc_pop <- full_join(
  tr_realloc_2000,
  tr_realloc_2010,
  by = c("GISJOIN_TR", "GISJOIN", "STATEA", "COUNTYA", "GEOGYEAR", "SEX", "AGEGRP", "RACE")
) %>%
  arrange(GISJOIN_TR, SEX, RACE, AGEGRP)

# Aggregate Data ---------------------------------------------------------------

# Not all CCRs can be calculated at the tract by sex/age/race level.
# In those cases, we aggregate first to the tract by sex/age level, and if
# ratios are still undefined, to the county by sex/age level.

# The final remaining undefined CCRs (only in Loving Cty TX and Kalawao)
# are artificially filled with an agnostic value of 1.

tr_realloc_pop_agg <- tr_realloc_pop %>% 
  group_by(GISJOIN_TR, GISJOIN, STATEA, COUNTYA, GEOGYEAR, SEX, AGEGRP) %>% 
  summarize(across(c(POP_2000, POP_2010), sum), .groups = "drop")

cty_realloc_pop_agg <- tr_realloc_pop %>%
  group_by(GISJOIN, STATEA, COUNTYA, GEOGYEAR, SEX, AGEGRP) %>% 
  summarize(across(c(POP_2000, POP_2010), sum), .groups = "drop")

# CCR Calculation --------------------------------------------------------------

# Calculate cohort change ratios by tract/sex/race, tract/sex, and 
# county/sex. The latter 2 are used as a fallback in cases where the first 
# cannot be calculated. (This occurs when the 2000 population for the t-10
# cohort has population 0.)

# Tract/race cases
tr_race_ccr <- tr_realloc_pop %>%
  group_by(GISJOIN_TR, SEX, RACE) %>%
  mutate(
    CCR = POP_2010 / lag_agg(POP_2000, 2),
    CCR = ifelse(!is.finite(CCR), NA, CCR)
  ) %>%
  ungroup()

# Tract cases
tr_ccr <- tr_realloc_pop_agg %>%
  group_by(GISJOIN_TR, SEX) %>%
  mutate(
    CCR = POP_2010 / lag_agg(POP_2000, 2),
    CCR = ifelse(!is.finite(CCR), NA, CCR)
  ) %>%
  ungroup() %>%
  select(-matches("^POP_MRSF"))

# County cases
cty_ccr <- cty_realloc_pop_agg %>%
  group_by(GISJOIN, SEX) %>%
  mutate(
    CCR = POP_2010 / lag_agg(POP_2000, 2),
    CCR = ifelse(!is.finite(CCR), NA, CCR)
  ) %>%
  ungroup() %>%
  select(-matches("^POP_MRSF"))

# Cap CCR values ---------------------------------------------------------------

# Fill undefined CCR values at lower aggregation levels, and cap large and small
# CCR values at predefined min/max accepted values. (From Swanson 2010: 
# https://link.springer.com/article/10.1007/s11113-009-9144-7)

# Calculate projected 2020 population for ages above 10 from final CCR values 
# and listed 2010 populations.

min_cap <- 0.98^10
max_cap <- 1.05^10

ccr_pop <- rows_patch(
  tr_race_ccr,
  tr_ccr,
  by = c("GISJOIN_TR", "GISJOIN", "STATEA", "COUNTYA",
         "GEOGYEAR", "SEX", "AGEGRP")
) %>%
  rows_patch(
    cty_ccr,
    by = c("GISJOIN", "STATEA", "COUNTYA", "GEOGYEAR", "SEX", "AGEGRP")
  ) %>%
  mutate(
    CCR = ifelse(!AGEGRP %in% c("00_04", "05_09") & is.na(CCR), 1, CCR),
    CCR_CAPPED = case_when(
      CCR < min_cap ~ min_cap,
      CCR > max_cap ~ max_cap,
      TRUE ~ CCR
    ),
    POP_2020 =  lag_agg(POP_2010, 2) * CCR_CAPPED,
  ) %>%
  select(-CCR, -CCR_CAPPED)

# Projections for 0-10 age groups ----------------------------------------------

# Convenience function to calculate female mother-age population for CTWs.
# Relevant mother population ages for children aged 0-4 is from 20-45
# Relevant mother population ages for children aged 5-9 is 30-50

# data: population data for totaling mother populations
# by: vector of columns to group by when calculating mother population totals
calculate_fem_pop <- function(data, by) {
  
  young_ages <- c("20_24", "25_29", "30_34", "35_39", "40_44")
  young_child_age <- "00_04"
  
  old_ages <- c("30_34", "35_39", "40_44", "45_49")
  old_child_age <- "05_09"
  
  dat_young <- data %>%
    filter(SEX == "F", AGEGRP %in% young_ages) %>%
    group_by(across({{ by }})) %>%
    mutate(
      POP_FEM_2010 = sum(POP_2010),
      POP_FEM_2020 = sum(POP_2020)
    ) %>%
    select({{ by }}, matches("^POP_FEM")) %>%
    ungroup() %>%
    distinct() %>%
    mutate(AGEGRP = young_child_age)
  
  dat_old <- data %>%
    filter(SEX == "F", AGEGRP %in% old_ages) %>%
    group_by(across({{ by }})) %>%
    mutate(
      POP_FEM_2010 = sum(POP_2010),
      POP_FEM_2020 = sum(POP_2020)
    ) %>%
    select({{ by }}, matches("^POP_FEM")) %>%
    ungroup() %>%
    distinct() %>%
    mutate(AGEGRP = old_child_age)
  
  bind_rows(
    dat_young,
    dat_old
  ) 
  
}

# Calculate relevant mother-aged female populations by tract/race
fempop_tr_race <- calculate_fem_pop(ccr_pop, by = c(GISJOIN_TR, GISJOIN, RACE))

# By tract
fempop_tr <- calculate_fem_pop(ccr_pop, by = c(GISJOIN_TR, GISJOIN)) %>%
  select(-POP_FEM_2020)

# By county
fempop_cty <- calculate_fem_pop(ccr_pop, by = GISJOIN) %>%
  select(-POP_FEM_2020)

# CTW calculation --------------------------------------------------------------

# Caclulate child-to-woman ratios as the proportion of children in a given
# age group to the associated total of mother-aged population.

# For values that are still undefined at tract/race level, aggregate and
# use CTWs at tract or county level as needed.

# Tract/race CTWs
tr_race_ctw <- ccr_pop %>%
  filter(AGEGRP %in% c("00_04", "05_09")) %>%
  left_join(fempop_tr_race, by = c("GISJOIN_TR", "GISJOIN", "AGEGRP", "RACE")) %>%
  mutate(
    CTW = POP_2010 / POP_FEM_2010,
    CTW = ifelse(!is.finite(CTW), NA, CTW)
  ) %>%
  ungroup() %>%
  select(-matches("^POP_MRSF"))

# Tract CTWs
tr_ctw <- tr_realloc_pop_agg %>%
  filter(AGEGRP %in% c("00_04", "05_09")) %>%
  left_join(fempop_tr, by = c("GISJOIN_TR", "GISJOIN", "AGEGRP")) %>%
  mutate(
    CTW = POP_2010 / POP_FEM_2010,
    CTW = ifelse(!is.finite(CTW), NA, CTW)
  ) %>%
  ungroup() %>%
  select(-matches("^POP_"))

# County CTWs
cty_ctw <- cty_realloc_pop_agg %>%
  filter(AGEGRP %in% c("00_04", "05_09")) %>%
  left_join(fempop_cty, by = c("GISJOIN", "AGEGRP")) %>%
  mutate(
    CTW = POP_2010 / POP_FEM_2010,
    CTW = ifelse(!is.finite(CTW), NA, CTW)
  ) %>%
  ungroup() %>%
  select(-matches("^POP_"))

# Replace undefined CTWs with CTWs for aggregated groups as needed.
ctw_final <- rows_patch(
  as_tibble(tr_race_ctw), # Bug in vctrs?
  tr_ctw,
  by = c("GISJOIN_TR", "GISJOIN", "STATEA", "COUNTYA", "GEOGYEAR", "SEX", "AGEGRP")
) %>%
  rows_patch(
    cty_ctw,
    by = c("GISJOIN", "STATEA", "COUNTYA", "GEOGYEAR", "SEX", "AGEGRP")
  ) %>%
  mutate(
    POP_2020 = POP_FEM_2020 * CTW
  ) %>%
  select(-matches("^POP_FEM"), -CTW)

# Combine CCR and CTW projections ----------------------------------------------

# Replace 2020 population projections for 0-10 year olds with values obtained 
# from CTW calculations
pop_2020_proj <- rows_patch(
    ccr_pop,
    ctw_final,
    by = c("GISJOIN_TR", "GISJOIN", "STATEA", 
           "COUNTYA", "GEOGYEAR", "SEX", "AGEGRP", "RACE")
  )

# Write ------------------------------------------------------------------------

vroom::vroom_write(
  pop_2020_proj,
  here::here("data", "preproc", "projected", "tr_realloc_proj_2020_pop.csv"),
  delim = ","
)
