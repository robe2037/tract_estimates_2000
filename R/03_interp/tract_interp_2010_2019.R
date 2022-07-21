
# -------------------------------------
#
# Interpolate annual estimates for sex / age / race from 2010-2019 at the tract 
# level for 2010 tract boundaries.
#
# Unlike for 2000-2010, we do not begin with 2020 counts to conduct the
# annual estimate interpolation. We first estimate the 2020 population for each
# sex / age / race group. This is done by calculated a cohort-change-ratio (CCR)
# for each age group, which is the ratio of the population in 2010 to the
# population in the 10-years-younger age group in 2000.
#
# These CCR values are capped at specific values to prevent small counts from
# producing very large CCR values.
#
# For age groups younger than 10, counts are estimated by computing a
# child-to-woman ratio. For those aged 0-4, this is the ratio of the count of
# in that age group to the total female population in 2010 of a given race from 
# 20-44. For those aged 0-5, it is the ratio of the count in that age group to 
# the total female population in 2010 in a given race group from 30-50. 
# The 2020 total female population in each of these cases is then multiplied by 
# the CTW ratio to get an estimated 2020 population
# for the young age groups.
# 
# For each county / sex / age / race, we then interpolate linearly (weighted by
# number of days between estimates) from 2010 to the 2020 estimate to get 
# race-adjusted annual estimates. Then, we calculate a ratio of these counts 
# for each year to the recorded counts in the annual census estimates.
# 
# At the tract level, we conduct the same linear day-weighted interpolation
# and adjust these estimates by multiplying them with the ratios calculated at 
# the county level.
#
# This produces 2010-2020 tract-level annual estimates for race, sex, and age, using
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

# These values are slightly different from original MRSF because the GEOGYEAR has been changed.
cty_mrsf_2000 <- tr_mrsf_2000 %>%
  group_by(GISJOIN, STATEA, COUNTYA, GEOGYEAR, SEX, AGEGRP, RACE) %>%
  summarize(POP_MRSF_2000 = sum(POP_MRSF_2000), .groups = "drop")

# This produces same result as reading in 2010 MRSF directly as it should.
cty_mrsf_2010 <- tr_mrsf_2010 %>%
  group_by(GISJOIN, STATEA, COUNTYA, GEOGYEAR, SEX, AGEGRP, RACE) %>%
  summarize(POP_MRSF_2010 = sum(POP_MRSF_2010), .groups = "drop")

cty_mrsf <- full_join(
  cty_mrsf_2000,
  cty_mrsf_2010,
  by = c("GISJOIN", "STATEA", "COUNTYA", "GEOGYEAR", "SEX", "AGEGRP", "RACE")
)

tr_mrsf <- full_join(
  tr_mrsf_2000,
  tr_mrsf_2010,
  by = c("GISJOIN_TR", "GISJOIN", "STATEA", "COUNTYA", "GEOGYEAR", "SEX", "AGEGRP", "RACE")
)

# Annual census estimates
census <- vroom::vroom(
  here::here("data", "preproc", "intercensal", "intercensal_preproc_2010.csv")
)

cum_days <- count_between(
  unique(census$DATE),
  include_last = TRUE
)

# Process ----------------------------------------------------------------------

tictoc::tic()
tr_interp <- purrr::map_dfr(
  unique(census$STATE),
  function(state) {
    
    # CCR --------------------------------
    
    # Nest census data
    census_nest <- census %>%
      filter(STATE == state) %>%
      nest(DATA = c(DATE, POP)) %>%
      select(GISJOIN, AGEGRP, RACE, SEX, DATA)
    
    # Tract
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
    
    tr_final <- compute_ctw(mrsf_tr_ccr)
    
    # County
    mrsf_cty_ccr <- cty_mrsf %>%  
      filter(STATEA == state) %>%
      arrange(GISJOIN, SEX, RACE, AGEGRP) %>%
      group_by(GISJOIN, SEX, RACE) %>%
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
    
    cty_final <- compute_ctw(mrsf_cty_ccr)
    
    # Interpolation ------------------
    
    cty_interp <- left_join(cty_final, census_nest) %>%
      mutate(
        CTY_INTERP = interp(POP_MRSF_2010, POP_2020, cum_days),
        RATIO_2020 = map2(DATA, CTY_INTERP, ~.x$POP / .y)
      )
    
    tr_interp <- tr_final %>%
      mutate(TRA_INTERP = interp(POP_MRSF_2010, POP_2020, cum_days))
    
    tr_interp_adj <- left_join(
      tr_interp,
      cty_interp ,
      by = c("GISJOIN", "SEX", "AGEGRP", "RACE"),
      suffix = c("_TRACT", "_COUNTY")
    ) %>%
      mutate(INTERP_ADJ = map2(TRA_INTERP, RATIO_2020, ~.x * .y))# %>%
    # unnest(c(DATA, INTERP_ADJ)) %>%
    # select(GISJOIN, GISJOIN_TR, SEX, AGEGRP, RACE, DATE, INTERP_ADJ, 
    #        POP_2000_TRACT, POP_2010_TRACT, POP_2020_TRACT) %>%
    # ungroup()
    
    tr_interp_adj
    
  }
)
tictoc::toc()
