
# -------------------------------------
#
# Interpolate annual estimates for sex / age / race from 2010-2019 at the tract 
# level for 2010 tract boundaries.
#
# Unlike for 2000-2010, we do not begin with 2020 counts to conduct the
# annual estimate interpolation. We first estimate the 2020 population for each
# sex / age / race group. This is done by calculating a cohort-change-ratio (CCR)
# for each age group, which is the ratio of the population in 2010 to the
# population in the 10-years-younger age group in 2000.
# 
# These CCR values are capped at specific values to prevent small counts from
# producing large CCR values that would inflate the population counts
# unreasonably.
#
# For age groups younger than 10, counts are estimated by computing a
# child-to-woman ratio. For those aged 0-4, this is the ratio of the count of
# in that age / sex / race group to the total female population in 2010 of a 
# given race from 20-44. For those aged 0-5, it is the ratio of the count in that 
# age / sex / race group to the total female population in 2010 in a given race
# group from 30-50. The 2020 total female population in each of these cases is 
# then multiplied by the CTW ratio to get an estimated 2020 population
# for the young age groups.
#
# Using these 2020 estimates, we interpolate linearly (weighted by
# number of days between estimates) fom 2010 to 2020 for each 
# tract / sex / age / race group to get race-adjusted annual
# estimates. We then adjust the interpolation to the recorded population
# estimates in the annual census estimates file. Tract counts have already been
# reallocated to the race classes present in the MRSF and annual population
# estimates files.
#
# In cases where the county-level population estimate count is 0, we coerce the
# interpolated tract estimates to 0 for all tracts within that county.
#
# For sex / age / race / date groups where the total interpolated population
# for all tracts in a county is positive and the county-level population
# estimate is also positive, we adjust each of the interpolated tract estimates
# proportionally such that the total interpolated population across all tracts
# sums to the value listed in the county-level population estimate file.
#
# For sex / age / race / date groups where the total interpolated population
# for all tracts in a county is zero, but the county-level population estimate
# value is positive, we allocate the total recorded county-level population
# to all tracts in the county proportional to the tracts' total population for
# that race (i.e. total population of a given race across all age and sex groups).
# If 0 total people are recorded for a race group, we allocate the county
# population value to tracts in proportion to their total populations.
#
# This produces tract-level annual estimates that ensure all tracts within
# counties sum to the population estimate recorded for that sex / age / race / date
# at the county level.
#
# -------------------------------------

source(here::here("R", "fun", "interpolate.R"))

# Data -------------------------------------------------------------------------

# Tract-level counts reallocated to MRSF race categories
tr_mrsf_2000 <- vroom::vroom(
  here::here("data", "preproc", "tract", "tract_mrsf_reallocation_2000.csv")
) %>%
  select(-DATAYEAR) %>%
  rename(POP_MRSF_2000 = POP_ADJ)

tr_mrsf_2010 <- vroom::vroom(
  here::here("data", "preproc", "tract", "tract_mrsf_reallocation_2010.csv")
) %>%
  select(-DATAYEAR) %>%
  rename(POP_MRSF_2010 = POP_ADJ)

tr_mrsf <- full_join(
  tr_mrsf_2000,
  tr_mrsf_2010,
  by = c("GISJOIN_TR", "GISJOIN", "STATEA", "COUNTYA", "GEOGYEAR", "SEX", "AGEGRP", "RACE")
)

# Annual population estimates
census <- vroom::vroom(
  here::here("data", "preproc", "intercensal", "intercensal_preproc_2010.csv")
)

# Days between recordings for adjusting linear interpolation
# (decennial counts are in April, annual estimates are in July)
cum_days <- count_between(
  c(unique(census$DATE), "2020-04-01"),
  include_last = TRUE
)

# Process ----------------------------------------------------------------------

# Process by state
states <- unique(census$STATE)

purrr::walk(
  states,
  function(state) {
    
    message("Processing state: ", state)
    
    # Interpolation -----------------------
    
    # Nest census data
    census_nest <- census %>%
      filter(STATE == state) %>%
      nest(DATA = c(DATE, POP)) %>%
      select(GISJOIN, AGEGRP, RACE, SEX, DATA)
    
    # Tract CCR
    mrsf_tr_ccr <- tr_mrsf %>%
      filter(STATEA == state) %>%
      arrange(GISJOIN_TR, SEX, RACE, AGEGRP) %>%
      group_by(GISJOIN_TR, SEX, RACE) %>%
      mutate(
        CCR = POP_MRSF_2010 / lag_agg(POP_MRSF_2000, 2),
        CCR_CAPPED = ccr_cap(
          POP_MRSF_2010, 
          CCR, 
          breaks = c(0, 25, 100, Inf), 
          caps = c(1, 2, 2)
        ),
        POP_2020 = POP_MRSF_2010 * CCR_CAPPED
      ) %>%
      ungroup()
    
    # Tract CTW
    femyoung_tr <- mrsf_tr_ccr %>%
      filter(SEX == "F", AGEGRP %in% c("20_24", "25_29", "30_34", "35_39", "40_44")) %>%
      group_by(GISJOIN_TR, RACE) %>%
      summarize(POP_FEM_2010 = sum(POP_MRSF_2010),
                POP_FEM_2020 = sum(POP_2020),
                .groups = "drop") %>%
      mutate(AGEGRP = "00_04")
    
    femold_tr <- mrsf_tr_ccr %>%
      filter(SEX == "F", AGEGRP %in% c("30_34", "35_39", "40_44", "45_49")) %>%
      group_by(GISJOIN_TR, RACE) %>%
      summarize(POP_FEM_2010 = sum(POP_MRSF_2010), 
                POP_FEM_2020 = sum(POP_2020),
                .groups = "drop") %>%
      mutate(AGEGRP = "05_09")
    
    fempop_tr <- bind_rows(
      femyoung_tr,
      femold_tr
    )
    
    childpop_tr <- mrsf_tr_ccr %>%
      filter(AGEGRP %in% c("00_04", "05_09")) %>%
      left_join(fempop_tr, by = c("GISJOIN_TR", "AGEGRP", "RACE")) %>%
      mutate(
        CTW = POP_MRSF_2010 / POP_FEM_2010,
        POP_2020 = POP_FEM_2020 * CTW
      )
    
    # Combine CCR and CTW estimates
    tr_final <- mrsf_tr_ccr %>%
      filter(!AGEGRP %in% c("00_04", "05_09")) %>%
      bind_rows(childpop_tr) %>%
      replace_na(list(POP_2020 = 0)) %>%
      arrange(GISJOIN_TR, SEX, AGEGRP, RACE) %>%
      select(GISJOIN_TR, GISJOIN, STATEA, COUNTYA, GEOGYEAR, SEX, AGEGRP, RACE, POP_MRSF_2010, POP_2020)
    
    tr_interp <- tr_final %>%
      mutate(
        TRA_INTERP = interp(POP_MRSF_2010, POP_2020, cum_days),
        TRA_INTERP = map(TRA_INTERP, ~.x[-length(.x)]) # We do not want to keep the 2020 estimate since annual estimates stop in 2019
      ) %>%
      left_join(census_nest, by = c("GISJOIN", "SEX", "AGEGRP", "RACE")) %>%
      unnest(cols = c(TRA_INTERP, DATA)) %>%
      group_by(GISJOIN, STATEA, COUNTYA, GEOGYEAR, SEX, AGEGRP, RACE, DATE) %>%
      mutate(CTY_INTERP = sum(TRA_INTERP)) %>%
      ungroup() %>%
      select(GISJOIN_TR, GISJOIN, COUNTYA, STATEA, GEOGYEAR, SEX, AGEGRP, RACE, DATE, TRA_INTERP, CTY_INTERP, POP)
    
    # POP == 0 -----------
    
    # When census county population is 0, we know all contained tracts must
    # also be 0, so coerce the tract-level interpolation to 0
    
    cty_zero <- tr_interp %>%
      filter(POP == 0) %>%
      mutate(POP_EST = 0)
    
    # CTY_INTERP > 0, POP > 0 -----------
    
    # When census county population is positive and total interpolated population of its
    # contained tracts is positive, adjust the tracts each such that their 
    # relative proportions are maintained but their total sum matches the recorded
    # county total.
    
    tr_pos <- tr_interp %>%
      filter(CTY_INTERP > 0, POP > 0) %>%
      mutate(POP_EST = TRA_INTERP * (POP / CTY_INTERP))
    
    # CTY_INTERP == 0, POP > 0 -----------
    
    # When census county population is positive, but all contained tracts have
    # 0 population, reallocate the county count to the tracts relative to the
    # distribution of the total population of the given race (disregarding
    # age and sex) across the county's tracts.
    #
    # If no tracts have any counts for people of that race at any age or
    # sex, reallocate using the distribution of total tract population instead.
    
    tr_race_sums <- tr_interp %>%
      group_by(GISJOIN_TR, GISJOIN, COUNTYA, STATEA, GEOGYEAR, RACE, DATE) %>%
      summarize(TOT_TRA_RACE = sum(TRA_INTERP), .groups = "drop") %>%
      group_by(GISJOIN, COUNTYA, STATEA, GEOGYEAR, RACE, DATE) %>%
      mutate(PROP_TRA_RACE = TOT_TRA_RACE / sum(TOT_TRA_RACE))
    
    tr_tot_sums <- tr_interp %>%
      group_by(GISJOIN_TR, GISJOIN, COUNTYA, STATEA, GEOGYEAR, DATE) %>%
      summarize(TOT_TRA_POP = sum(TRA_INTERP), .groups = "drop") %>%
      group_by(GISJOIN, COUNTYA, STATEA, GEOGYEAR, DATE) %>%
      mutate(PROP_TRA_POP = TOT_TRA_POP / sum(TOT_TRA_POP))
    
    tr_realloc_weights <- full_join(
      tr_race_sums, 
      tr_tot_sums, 
      by = c("GISJOIN_TR", "GISJOIN", "COUNTYA", "STATEA", "GEOGYEAR", "DATE")
    ) %>%
      mutate(TRA_REALLOC_WT = ifelse(is.nan(PROP_TRA_RACE), PROP_TRA_POP, PROP_TRA_RACE)) %>%
      select(-c(TOT_TRA_RACE, PROP_TRA_RACE, TOT_TRA_POP, PROP_TRA_POP))
    
    tr_zero <- tr_interp %>%
      filter(CTY_INTERP == 0, POP > 0) %>%
      left_join(
        tr_realloc_weights,
        by = c("GISJOIN_TR", "GISJOIN", "COUNTYA", "STATEA", "GEOGYEAR", "RACE", "DATE")
      ) %>%
      mutate(POP_EST = POP * TRA_REALLOC_WT) %>%
      select(-TRA_REALLOC_WT)
    
    # Output ---------
    
    # Recombine adjusted tract estimates into one source
    tr_interp_adj <- bind_rows(cty_zero, tr_pos, tr_zero) %>%
      arrange(GISJOIN, GISJOIN_TR, SEX, AGEGRP, RACE, DATE)
    
    # Tract estimates should sum to the annual county estimate:
    cty_sums <- tr_interp_adj %>%
      group_by(GISJOIN, SEX, AGEGRP, RACE, DATE, POP) %>%
      summarize(s = sum(POP_EST), .groups = "drop") %>%
      filter(!near(s, POP))
    
    if(nrow(cty_sums) != 0) {
      warning(
        "State ", state, " had tracts that did not sum to county totals.",
        call. = FALSE
      )
    }
    
    # All groups should be present in output:
    if(nrow(tr_interp_adj) != nrow(tr_interp)) {
      warning(
        "State ", state, " is missing tracts after adjustment.",
        " This may be because certain counties do not exist in the annual ",
        " census data.",
        call. = FALSE
      )
    }
    
    tr_interp_adj <- tr_interp_adj %>%
      select(-c(GISJOIN, TRA_INTERP, CTY_INTERP, POP)) %>%
      filter(lubridate::month(DATE) != 4) %>% # Remove decennials
      rename(GISJOIN = GISJOIN_TR) %>%
      mutate(
        TRACTA = stringr::str_sub(GISJOIN, -6),
        DATAYEAR = lubridate::year(DATE)
      ) %>%
      select(GISJOIN, COUNTYA, STATEA, TRACTA, 
             GEOGYEAR, DATAYEAR, SEX, AGEGRP, RACE, POP_EST)
    
    vroom::vroom_write(
      tr_interp_adj,
      here::here("data", "output", "interp_2010_2019", paste0("tr_interp_2010_2019_", state, ".csv")),
      delim = ","
    )
    
  }
)
