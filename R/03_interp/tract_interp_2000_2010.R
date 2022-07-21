
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

# Interpolation -----------------------

cty_interp <- left_join(cty_mrsf, census_nest) %>%
  mutate(
    CTY_INTERP = interp(POP_MRSF_2000, POP_MRSF_2010, cum_days),
    RATIO = map2(DATA, CTY_INTERP, ~ifelse(.y == 0, 0, .x$POP / .y)),
  )

tr_interp <- tr_mrsf %>%
  mutate(TR_INTERP = interp(POP_MRSF_2000, POP_MRSF_2010, cum_days))

full_interp <- left_join(
  tr_interp,
  cty_interp ,
  by = c("GISJOIN", "STATEA", "COUNTYA", "GEOGYEAR", "SEX", "AGEGRP", "RACE"),
  suffix = c("_TRACT", "_COUNTY")
)

tictoc::tic()
out <- full_interp %>%
  # filter(STATEA == "01") %>%
  mutate(INTERP_ADJ = map2(TR_INTERP, RATIO, ~.x * .y))# %>% # Ratio adjustment
  # unnest(c(DATA, INTERP_ADJ)) %>%
  # select(GISJOIN = GISJOIN_TR, STATEA, COUNTYA, GEOGYEAR, SEX, AGEGRP, RACE, DATE, INTERP_ADJ) %>%
  # ungroup()
tictoc::toc()
