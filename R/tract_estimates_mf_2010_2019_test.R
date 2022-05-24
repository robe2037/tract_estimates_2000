
source(here::here("R", "helperxs.R"))

# Setup ------------------------------------------------------------------------

begin_date <- ymd("2010-04-01")
end_date <- ymd("2019-07-01")

cum_days <- count_between(
  c(begin_date, seq(ymd("2010-07-01"), ymd("2018-07-01"), by = "year"), end_date),
  include_last = TRUE
)

# Census county estimates ------------------------------------------------------

census <- vroom::vroom(
  "/pkg/popgis/labpcs/data_projects/tract_estimates/2010_2019/data/popest/cc-est2019-alldata.csv"
) %>%
  select(STATE:TOT_FEMALE) %>%
  mutate(GISJOIN = paste0("G", STATE, "0", COUNTY, "0"))

yr_map <- tibble::tibble(
  CODE = 1:12,
  DATE = lubridate::ymd("2010-04-01",
                        "2010-04-01",
                        "2010-07-01",
                        "2011-07-01",
                        "2012-07-01",
                        "2013-07-01",
                        "2014-07-01",
                        "2015-07-01",
                        "2016-07-01",
                        "2017-07-01",
                        "2018-07-01",
                        "2019-07-01")
)

age_map <- tibble::tibble(
  levels = 0:18, 
  AGE = c("total",
          "age0_4",
          "age5_9",
          "age10_14",
          "age15_19",
          "age20_24",
          "age25_29",
          "age30_34",
          "age35_39",
          "age40_44",
          "age45_49",
          "age50_54",
          "age55_59",
          "age60_64",
          "age65_69",
          "age70_74",
          "age75_79",
          "age80_84",
          "age85")
)

census_long <- left_join(census, yr_map, by = c("YEAR" = "CODE")) %>%
  filter(YEAR != 1, AGEGRP != 0) %>% # out of scope
  left_join(age_map, by = c("AGEGRP" = "levels")) %>%
  select(GISJOIN, DATE, AGE, TOT_MALE, TOT_FEMALE) %>%
  pivot_longer(cols = matches("^TOT"), 
               names_to = "SEX", 
               values_to = "POP", 
               names_prefix = "TOT_",
               names_transform = list(SEX = tolower)) %>%
  mutate(AGE = str_sub(AGE, 4)) %>%
  arrange(GISJOIN, DATE)

# NHGIS data -------------------------------------------------------------------

tr <- vroom::vroom("/pkg/popgis/labpcs/data_projects/tract_estimates/2000_2010/data/nhgis/nhgis1133_ts_geog2010_tract.csv")
cty <- vroom::vroom("/pkg/popgis/labpcs/data_projects/tract_estimates/2000_2010/data/nhgis/nhgis1133_ts_geog2010_tract.csv")

nhgis_long <- purrr::map(
  list(tr, cty),
  ~.x %>%
    filter(DATAYEAR == 2000 | DATAYEAR == 2010) %>%
    mutate(
      # male_age0 = CN8AA, # This differs from 2000-2010
      male_age0_4 = CN8AA + CN8AB  + CN8AC  + CN8AD  + CN8AE,
      male_age5_9 = CN7AB,
      male_age10_14 = CN7AC,
      male_age15_19 = CN7AD + CN7AE,
      male_age20_24 = CN7AF + CN7AG + CN7AH,
      male_age25_29 = CN7AI,
      male_age30_34 = CN7AJ,
      male_age35_39 = CN7AK,
      male_age40_44 = CN7AL,
      male_age45_49 = CN7AM,
      male_age50_54 = CN7AN,
      male_age55_59 = CN7AO,
      male_age60_64 = CN7AP + CN7AQ,
      male_age65_69 = CN7AR + CN7AS,
      male_age70_74 = CN7AT,
      male_age75_79 = CN7AU,
      male_age80_84 = CN7AV,
      male_age85 = CN7AW,
      # female_age0 = CN8AU, # This differs from 2000-2010
      female_age0_4 = CN8AU + CN8AV + CN8AW + CN8AX + CN8AY,
      female_age5_9 = CN7AY,
      female_age10_14 = CN7AZ,
      female_age15_19 =  CN7BA + CN7BB,
      female_age20_24 =  CN7BC  + CN7BD  + CN7BE,
      female_age25_29 =  CN7BF,
      female_age30_34 =  CN7BG,
      female_age35_39 =  CN7BH,
      female_age40_44 =  CN7BI,
      female_age45_49 =  CN7BJ,
      female_age50_54 =  CN7BK,
      female_age55_59 =  CN7BL,
      female_age60_64 = CN7BM + CN7BN,
      female_age65_69 = CN7BO + CN7BP,
      female_age70_74 =  CN7BQ,
      female_age75_79 =  CN7BR,
      female_age80_84 = CN7BS,
      female_age85 = CN7BT
    ) %>%
    select(GISJOIN, DATAYEAR, male_age0_4:female_age85) %>%
    pivot_longer(-c(GISJOIN, DATAYEAR), names_to = "VARIABLE", values_to = "POP") %>%
    pivot_wider(names_from = DATAYEAR, values_from = POP, names_prefix = "POP_") %>%
    mutate(SEX = str_extract(VARIABLE, "[^_]+"),
           AGE = str_extract(VARIABLE, "(?<=age).+$")) %>%
    select(GISJOIN, SEX, AGE, POP_2000, POP_2010)
)

nhgis_tract <- nhgis_long[[1]] %>% 
  mutate(GISJOIN_CTY = substr(GISJOIN, 1, 8)) %>%
  relocate(GISJOIN_CTY, .after = GISJOIN)

nhgis_county <- nhgis_long[[2]]

# This will ultimately need to be incorporated:

#### 2a. Bedford Independent City recodes #### 
# Recode Bedford independent city tract to Bedford county FIPS code
# t <- t %>%
#   mutate(GISJOIN = case_when(STATEA == "51" & COUNTYA == "515" ~ paste0("G", STATEA, "0", "019", "0", TRACTA),
#                              TRUE ~ GISJOIN))
# 
# # Recode Bedford independent city FIPS code to Bedford county FIPS and collapse to create single entity 
# c <- c %>%
#   mutate(GISJOIN = case_when(STATEA == "51" & COUNTYA == "515" ~ paste0("G", STATEA, "0", "019", "0"),
#                              TRUE ~ GISJOIN)) %>%
#   pivot_longer(CL8AA:CN8BNU, names_to = "var_code", values_to = "n") %>%
#   group_by(GISJOIN, DATAYEAR, var_code) %>%
#   summarise(n = sum(n, na.rm = TRUE)) %>%
#   pivot_wider(names_from = "var_code", values_from = "n") %>%
#   ungroup()

# Processing -------------------------------------------------------------------

# Tract
nhgis_tract_ccr <- nhgis_tract %>%
  group_by(GISJOIN, SEX) %>%
  mutate(
    CCR = POP_2010 / lag_agg(POP_2000, 2),
    CCR_CAPPED = ccr_cap(
      POP_2010, 
      CCR, 
      breaks = c(0, 25, 100, Inf), 
      caps = c(1, 2, 2)
    ),
    POP_2019 = POP_2010 * CCR_CAPPED
  ) %>%
  ungroup()

nhgis_tract <- nhgis_tract_ccr %>%
  group_by(GISJOIN) %>%
  mutate(
    FEMYOUNG = SEX == "female" & AGE %in% c("20_24", "25_29", "30_34", "35_39", "40_44"),
    FEMOLD = SEX == "female" & AGE %in% c("30_34", "35_39", "40_44", "45_49"),
    FEMTOT_2010 = ifelse(AGE == "0_4", sum(FEMYOUNG * POP_2010), sum(FEMOLD * POP_2010)),
    FEMTOT_2019 = ifelse(AGE == "0_4", sum(FEMYOUNG * POP_2019, na.rm = TRUE), sum(FEMOLD * POP_2019, na.rm = TRUE))
  ) %>%
  mutate(CTW = ifelse(AGE %in% c("0_4", "5_9"), POP_2010 / FEMTOT_2010, NA)) %>%
  mutate(POP_2019 = ifelse(AGE %in% c("0_4", "5_9"), FEMTOT_2019 * CTW, POP_2019)) %>%
  select(GISJOIN, GISJOIN_CTY, SEX, AGE, CCR, CCR_CAPPED, CTW, POP_2000, POP_2010, POP_2019)

# County
nhgis_county_ccr <- nhgis_county %>%
  group_by(GISJOIN, SEX) %>%
  mutate(
    CCR = POP_2010 / lag_agg(POP_2000, 2),
    CCR_CAPPED = ccr_cap(
      POP_2010, 
      CCR, 
      breaks = c(0, 25, 100, Inf), 
      caps = c(1, 2, 2)
    ),
    POP_2019 = POP_2010 * CCR_CAPPED
  ) %>%
  ungroup()

nhgis_county <- nhgis_county_ccr %>%
  group_by(GISJOIN) %>%
  mutate(
    FEMYOUNG = SEX == "female" & AGE %in% c("20_24", "25_29", "30_34", "35_39", "40_44"),
    FEMOLD = SEX == "female" & AGE %in% c("30_34", "35_39", "40_44", "45_49"),
    FEMTOT_2010 = ifelse(AGE == "0_4", sum(FEMYOUNG * POP_2010), sum(FEMOLD * POP_2010)),
    FEMTOT_2019 = ifelse(AGE == "0_4", sum(FEMYOUNG * POP_2019, na.rm = TRUE), sum(FEMOLD * POP_2019, na.rm = TRUE))
  ) %>%
  mutate(CTW = ifelse(AGE %in% c("0_4", "5_9"), POP_2010 / FEMTOT_2010, NA)) %>%
  mutate(POP_2019 = ifelse(AGE %in% c("0_4", "5_9"), FEMTOT_2019 * CTW, POP_2019)) %>%
  select(GISJOIN, SEX, AGE, CCR, CCR_CAPPED, CTW, POP_2000, POP_2010, POP_2019)

census_nest <- census_long %>%
  nest(DATA = c(DATE, POP))

cty_interp <- left_join(nhgis_county, census_nest) %>%
  mutate(
    CTY_INTERP = interp(POP_2010, POP_2019, cum_days),
    RATIO_2019 = map2(DATA, CTY_INTERP, ~.x$POP / .y)
  )

tra_interp <- nhgis_tract %>%
  mutate(TRA_INTERP = interp(POP_2010, POP_2019, cum_days))

tra_interp_adj <- left_join(
  tra_interp,
  cty_interp ,
  by = c("GISJOIN_CTY" = "GISJOIN", "SEX", "AGE"),
  suffix = c("_TRACT", "_COUNTY")
) %>%
  mutate(INTERP_ADJ = map2(TRA_INTERP, RATIO_2019, ~.x * .y)) %>%
  unnest(c(DATA, INTERP_ADJ)) %>%
  select(GISJOIN, SEX, AGE, DATE, INTERP_ADJ, POP_2000_TRACT, POP_2010_TRACT, POP_2019_TRACT)

tra_interp_adj %>% 
  nest(data = c(DATE, INTERP_ADJ, POP_2000_TRACT, POP_2010_TRACT, POP_2019_TRACT))
