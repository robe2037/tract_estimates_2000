
# -------------------------------------
#
# Interpolate annual estimates for sex / age / race from 2000-2010 at the tract 
# level for 2010 tract boundaries.
#
# For each county / sex / age / race, we interpolate linearly (weighted by
# number of days between estimates) to get race-adjusted annual
# estimates. Then, we calculate a ratio of these counts for each year to the 
# recorded counts in the annual census estimates.
# 
# At the tract level, we conduct the same linear day-weighted interpolation
# and adjust these estimates by multiplying them with the ratios calculated at 
# the county level.
#
# This produces 2000-2010 tract-level annual estimates for race, sex, and age, using
# the race categories in the MRSF.
#
# -------------------------------------

source(here::here("R", "fun", "interpolate.R"))

# Data -------------------------------------------------------------------------

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

census <- vroom::vroom(
  here::here("data", "preproc", "intercensal", "intercensal_preproc_2000.csv")
)

census_nest <- census %>%
  nest(DATA = c(DATE, POP)) %>%
  select(GISJOIN, AGEGRP, RACE, SEX, DATA)

cum_days <- count_between(
  unique(census$DATE),
  include_last = TRUE
)

# -----------------

states <- unique(census$STATE)

purrr::walk(
  states[41],
  function(state) {
    
    message("Processing state: ", state)
    
    # Interpolation -----------------------
    
    tr_interp <- tr_mrsf %>%
      filter(STATEA == state) %>%
      mutate(TRA_INTERP = interp(POP_MRSF_2000, POP_MRSF_2010, cum_days)) %>%
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
    
    # CTY_INTERP > 0, POP > 0. Confirm this is correct approach. -----------
    
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
    
    # ---------
    
    # Recombine adjusted tract estimates into one source
    tr_interp_adj <- bind_rows(cty_zero, tr_pos, tr_zero) %>%
      arrange(GISJOIN, GISJOIN_TR, SEX, AGEGRP, RACE, DATE)
    
    # Check:
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
    
    if(nrow(tr_interp_adj) != nrow(tr_interp)) {
      warning(
        "State ", state, " is missing tracts after adjustment.",
        " This may be because certain counties do not exist in the annual ",
        " census data.",
        call. = FALSE
      )
    }
    
    tr_interp_adj <- tr_interp_adj %>%
      select(-c(TRA_INTERP, CTY_INTERP, POP)) %>%
      filter(lubridate::month(DATE) != 4) # Remove decennials
    
    vroom::vroom_write(
      tr_interp_adj,
      here::here("data", "output", "interp_2000_2010", paste0("tr_interp_2000_2010_", state, ".csv")),
      delim = ","
    )
    
  }
)
