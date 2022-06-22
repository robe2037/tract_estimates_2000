
source(here::here("R", "fun", "helpers.R"))

# Data -------------------------------------------------------------------------

# MRSF -----------------------

# 2010 MRSF
mrsf_2010 <- vroom::vroom(
  here::here("data", "preproc", "mrsf", "mrsf_2010_agg.csv")
)

# 2000 MRSF for 2010 boundaries produced from block-based aggregation
mrsf_2000 <- vroom::vroom(
  list.files(
    here::here("data", "preproc", "block"), 
    full.names = TRUE
  )
) %>%
  mutate(DATAYEAR = 2000) %>%
  rename(POP = POP_ADJ)

mrsf <- bind_rows(mrsf_2000, mrsf_2010) %>%
  filter(GISJOIN == "G0100010")

# SF1 ------------------------

cty <- vroom::vroom(
  here::here(
    "data", 
    "preproc", 
    "county", 
    "nhgis_cty_2000_2010_agg_standardized.csv"
  )
) %>%
  filter(GISJOIN == "G0100010")

tr <- vroom::vroom(
  here::here(
    "data", 
    "preproc", 
    "tract", 
    "nhgis_tr_2000_2010_agg_standardized.csv"
  )
) %>%
  filter(STATEA == "01", COUNTYA == "001") %>%
  mutate(GISJOIN_CTY = str_sub(GISJOIN, 1, 8))

# Race group adjustment ------------------------

# Adjust counts in tract-level race groups based on the relative counts for
# each group in the county-level MRSF vs. county-level SF1

mrsf_ratio <- full_join(
  mrsf,
  cty,
  by = c("GISJOIN", "DATAYEAR", "STATEA", "COUNTYA", "SEX", "AGEGRP", "RACE"),
  suffix = c("_MRSF", "_NHGIS")
) %>%
  mutate(RATIO = POP_MRSF / POP_NHGIS) %>%
  filter(RACE != "other") %>%
  mutate(RATIO = ifelse(is.nan(RATIO) | RATIO == Inf, 0, RATIO)) %>%
  select(GISJOIN, STATEA, COUNTYA, DATAYEAR, SEX, 
         AGEGRP, RACE, POP_MRSF, POP_NHGIS, RATIO)

mrsf_cty <- mrsf_ratio %>%
  select(-RATIO, -POP_NHGIS, -STATEA, -COUNTYA) %>%
  rename(POP = POP_MRSF) %>%
  pivot_wider(names_from = DATAYEAR, values_from = POP, names_prefix = "POP_")

mrsf_tr <- full_join(
  mrsf_ratio,
  tr, 
  by = c("GISJOIN" = "GISJOIN_CTY", "DATAYEAR", "SEX", "AGEGRP", "RACE",
         "STATEA", "COUNTYA"),
  suffix = c("_CTY", "_TR")
) %>%
  filter(RACE != "other") %>%
  mutate(POP_ADJ = POP * RATIO) %>%
  select(GISJOIN, GISJOIN_TR, DATAYEAR, SEX, AGEGRP, RACE, POP = POP_ADJ) %>%
  pivot_wider(names_from = DATAYEAR, values_from = POP, names_prefix = "POP_") %>%
  arrange(GISJOIN_TR, SEX, AGEGRP, RACE)

# Processing -----------------------------------------------------------------------

# Setup -------------------------------

# Decennial census dates
begin_date <- ymd("2000-04-01")
end_date <- ymd("2010-04-01")

dates <- c(
  begin_date, 
  seq(ymd("2000-07-01"), ymd("2009-07-01"), by = "year"), 
  end_date
)

cum_days <- count_between(
  c(begin_date, 
    seq(ymd("2000-07-01"), ymd("2009-07-01"), by = "year"), 
    end_date),
  include_last = TRUE
)

# state <- "01"

# Annual census estimates -------------

census <- vroom::vroom(
  here::here("data", "preproc", "intercensal", "intercensal_preproc_2000.csv")
)

census_nest <- census %>%
  filter(GISJOIN == "G0100010") %>%
  # filter(STATE == state) %>%
  nest(DATA = c(DATE, POP)) %>%
  select(GISJOIN, AGEGRP, RACE, SEX, DATA)

# Interpolation -----------------------

cty_interp <- left_join(mrsf_cty, census_nest) %>%
  mutate(
    CTY_INTERP = interp(POP_2000, POP_2010, cum_days),
    RATIO = map2(DATA, CTY_INTERP, ~ifelse(.y == 0, 0, .x$POP / .y)),
  )

tr_interp <- mrsf_tr %>%
  mutate(TR_INTERP = interp(POP_2000, POP_2010, cum_days))

tr_interp_adj <- left_join(
  tr_interp,
  cty_interp ,
  by = c("GISJOIN", "SEX", "AGEGRP", "RACE"),
  suffix = c("_TRACT", "_COUNTY")
) %>%
  mutate(INTERP_ADJ = map2(TR_INTERP, RATIO, ~.x * .y)) %>% # Ratio adjustment
  unnest(c(DATA, INTERP_ADJ)) %>%
  select(GISJOIN = GISJOIN_TR, SEX, AGEGRP, RACE, DATE, 
         INTERP_ADJ, POP_2000_TRACT, POP_2010_TRACT,) %>%
  ungroup()
