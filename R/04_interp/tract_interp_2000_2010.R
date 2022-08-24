
# -------------------------------------
#
# Interpolate annual estimates for sex / age / race from 2000-2010 at the tract 
# level for 2010 tract boundaries.
#
# For each tract / sex / age / race, we interpolate linearly (weighted by
# number of days between estimates) fom 2000 to 2010 to get race-adjusted annual
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

# Projected decennial data reallocated to MRSF race groups
pop_proj <- vroom::vroom(
  here::here("data", "preproc", "projected", "tr_realloc_proj_2020_pop.csv")
)

# Annual population estimates
census <- vroom::vroom(
  here::here("data", "preproc", "intercensal", "intercensal_preproc_2000.csv")
)

# Days between recordings for adjusting linear interpolation
# (decennial counts are in April, annual estimates are in July)
cum_days <- count_between(
  unique(census$DATE),
  include_last = TRUE
)

# Process ----------------------------------------------------------------------

# Process by state
states <- unique(census$STATE)

purrr::walk(
  states,
  function(state) {
    
    message("Processing state: ", state)
    
    # Nest census data
    census_nest <- census %>%
      filter(STATE == state) %>%
      nest(DATA = c(DATE, POP)) %>%
      select(GISJOIN, AGEGRP, RACE, SEX, DATA)
    
    # Interpolation -----------------------
    
    tr_interp <- pop_proj %>%
      filter(STATEA == state) %>%
      mutate(TRA_INTERP = interp(POP_2000, POP_2010, cum_days)) %>%
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
      mutate(ESTIMATE = 0)
    
    # CTY_INTERP > 0, POP > 0 -----------
    
    # When census county population is positive and total interpolated population of its
    # contained tracts is positive, adjust the tracts each such that their 
    # relative proportions are maintained but their total sum matches the recorded
    # county total.
    
    tr_pos <- tr_interp %>%
      filter(CTY_INTERP > 0, POP > 0) %>%
      mutate(ESTIMATE = TRA_INTERP * (POP / CTY_INTERP))
    
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
      summarize(TOT_TRA_POP = sum(TRA_INTERP), .groups = "drop") %>%
      group_by(GISJOIN, COUNTYA, STATEA, GEOGYEAR, RACE, DATE) %>%
      mutate(WT = TOT_TRA_POP / sum(TOT_TRA_POP)) %>%
      select(-TOT_TRA_POP)
    
    tr_tot_sums <- tr_interp %>%
      group_by(GISJOIN_TR, GISJOIN, COUNTYA, STATEA, GEOGYEAR, DATE) %>%
      summarize(TOT_TRA_POP = sum(TRA_INTERP), .groups = "drop") %>%
      group_by(GISJOIN, COUNTYA, STATEA, GEOGYEAR, DATE) %>%
      mutate(WT = TOT_TRA_POP / sum(TOT_TRA_POP)) %>%
      select(-TOT_TRA_POP)
    
    tr_realloc_weights <- rows_patch(
      tr_race_sums,
      tr_tot_sums,
      by = c("GISJOIN_TR", "GISJOIN", "COUNTYA", "STATEA", "GEOGYEAR", "DATE")
    )
    
    tr_zero <- tr_interp %>%
      filter(CTY_INTERP == 0, POP > 0) %>%
      left_join(
        tr_realloc_weights,
        by = c("GISJOIN_TR", "GISJOIN", "COUNTYA", "STATEA", "GEOGYEAR", "RACE", "DATE")
      ) %>%
      mutate(ESTIMATE = POP * WT) %>%
      select(-WT)
    
    # Output ----------
    
    # Recombine adjusted tract estimates into one source
    tr_interp_adj <- bind_rows(cty_zero, tr_pos, tr_zero) %>%
      arrange(GISJOIN, GISJOIN_TR, SEX, AGEGRP, RACE, DATE)
    
    # Tract estimates should sum to the annual county estimate:
    cty_sums <- tr_interp_adj %>%
      group_by(GISJOIN, SEX, AGEGRP, RACE, DATE, POP) %>%
      summarize(s = sum(ESTIMATE), .groups = "drop") %>%
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
        DATAYEAR = lubridate::year(DATE),
        GEOID = paste0(STATEA, COUNTYA, TRACTA)
      ) %>%
      select(GISJOIN, GEOID, COUNTYA, STATEA, TRACTA, 
             GEOGYEAR, DATAYEAR, SEX, AGEGRP, RACE, ESTIMATE)
    
    vroom::vroom_write(
      tr_interp_adj,
      here::here("data", "interp", "interp_2000_2010", paste0("tr_interp_2000_2010_", state, ".csv")),
      delim = ","
    )
    
  }
)
