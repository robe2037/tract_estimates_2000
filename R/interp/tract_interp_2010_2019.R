
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
  filter(STATEA == "01")

# SF1 ------------------------

cty <- vroom::vroom(
  here::here(
    "data", 
    "preproc", 
    "county", 
    "nhgis_cty_2000_2010_agg_standardized.csv"
  )
) %>%
  filter(STATEA == "01")

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

# Processing -------------------------------------------------------------------

# Setup -------------------------------

# Census dates
begin_date <- ymd("2010-04-01")
end_date <- ymd("2019-07-01")

cum_days <- count_between(
  c(begin_date, 
    seq(ymd("2010-07-01"), ymd("2018-07-01"), by = "year"), 
    end_date),
  include_last = TRUE
)

# state <- "01"

# Annual census estimates -------------

census <- vroom::vroom(
  here::here("data", "preproc", "intercensal", "intercensal_preproc_2010.csv")
)

census_nest <- census %>%
  filter(STATE == "01") %>%
  # filter(STATE == state) %>%
  nest(DATA = c(DATE, POP)) %>%
  select(GISJOIN, AGEGRP, RACE, SEX, DATA)

# CCR --------------------------------

# Tract
mrsf_tr_ccr <- mrsf_tr %>%
  arrange(GISJOIN_TR, SEX, RACE, AGEGRP) %>%
  group_by(GISJOIN_TR, SEX, RACE) %>%
  mutate(
    CCR = POP_2010 / lag_agg(POP_2000, 2),
    CCR_CAPPED = ccr_cap(
      POP_2010, 
      CCR, 
      breaks = c(0, 25, 100, Inf), 
      caps = c(1, 2, 2)
    ),
    POP_2020 = POP_2010 * CCR_CAPPED
  ) %>%
  ungroup()

mrsf_tr_ctw <- mrsf_tr_ccr %>%
  group_by(GISJOIN_TR) %>%
  mutate(
    FEMYOUNG = SEX == "F" & AGEGRP %in% c("20_24", "25_29", "30_34", "35_39", "40_44"),
    FEMOLD = SEX == "M" & AGEGRP %in% c("30_34", "35_39", "40_44", "45_49"),
    FEMTOT_2010 = ifelse(AGEGRP == "00_04", sum(FEMYOUNG * POP_2010), sum(FEMOLD * POP_2010)),
    FEMTOT_2020 = ifelse(AGEGRP == "00_04", sum(FEMYOUNG * POP_2020, na.rm = TRUE), sum(FEMOLD * POP_2020, na.rm = TRUE))
  ) %>%
  mutate(CTW = ifelse(AGEGRP %in% c("00_04", "05_09"), POP_2010 / FEMTOT_2010, NA)) %>%
  mutate(POP_2020 = ifelse(AGEGRP %in% c("00_04", "05_09"), FEMTOT_2020 * CTW, POP_2020)) %>%
  select(GISJOIN, GISJOIN_TR, SEX, AGEGRP, RACE, CCR, CCR_CAPPED, CTW, POP_2000, POP_2010, POP_2020)

# County
mrsf_cty_ccr <- mrsf_cty %>%  
  arrange(GISJOIN, SEX, RACE, AGEGRP) %>%
  group_by(GISJOIN, SEX) %>%
  mutate(
    CCR = POP_2010 / lag_agg(POP_2000, 2),
    CCR_CAPPED = ccr_cap(
      POP_2010, 
      CCR, 
      breaks = c(0, 25, 100, Inf), 
      caps = c(1, 2, 2)
    ),
    POP_2020 = POP_2010 * CCR_CAPPED
  ) %>%
  ungroup()

mrsf_cty_ctw <- mrsf_cty_ccr %>%
  group_by(GISJOIN) %>%
  mutate(
    FEMYOUNG = SEX == "F" & AGEGRP %in% c("20_24", "25_29", "30_34", "35_39", "40_44"),
    FEMOLD = SEX == "F" & AGEGRP %in% c("30_34", "35_39", "40_44", "45_49"),
    FEMTOT_2010 = ifelse(AGEGRP == "00_04", sum(FEMYOUNG * POP_2010), sum(FEMOLD * POP_2010)),
    FEMTOT_2020 = ifelse(AGEGRP == "00_04", sum(FEMYOUNG * POP_2020, na.rm = TRUE), sum(FEMOLD * POP_2020, na.rm = TRUE))
  ) %>%
  mutate(CTW = ifelse(AGEGRP %in% c("00_04", "05_09"), POP_2010 / FEMTOT_2010, NA)) %>%
  mutate(POP_2020 = ifelse(AGEGRP %in% c("00_04", "05_09"), FEMTOT_2020 * CTW, POP_2020)) %>%
  select(GISJOIN, SEX, AGEGRP, RACE, CCR, CCR_CAPPED, CTW, POP_2000, POP_2010, POP_2020)

# Census --------------------------

cty_interp <- left_join(mrsf_cty_ctw, census_nest) %>%
  mutate(
    CTY_INTERP = interp(POP_2010, POP_2020, cum_days),
    RATIO_2020 = map2(DATA, CTY_INTERP, ~.x$POP / .y)
  )

tr_interp <- mrsf_tr_ctw %>%
  mutate(TRA_INTERP = interp(POP_2010, POP_2020, cum_days))

tr_interp_adj <- left_join(
  tr_interp,
  cty_interp ,
  by = c("GISJOIN", "SEX", "AGEGRP", "RACE"),
  suffix = c("_TRACT", "_COUNTY")
) %>%
  mutate(INTERP_ADJ = map2(TRA_INTERP, RATIO_2020, ~.x * .y)) %>%
  unnest(c(DATA, INTERP_ADJ)) %>%
  select(GISJOIN, GISJOIN_TR, SEX, AGEGRP, RACE, DATE, INTERP_ADJ, 
         POP_2000_TRACT, POP_2010_TRACT, POP_2020_TRACT) %>%
  ungroup()

tmp <- tr_interp_adj %>% 
  nest(data = c(DATE, INTERP_ADJ))

