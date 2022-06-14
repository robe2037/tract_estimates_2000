
source(here::here("R", "fun", "helpers.R"))

# Setup ------------------------------------------------------------------------

# set begin and end values, convert to dates and find number of days between them
begin_date <- ymd("2000-04-01")
end_date <- ymd("2010-04-01")

dates <- c(
  begin_date, 
  seq(ymd("2000-07-01"), ymd("2009-07-01"), by = "year"), 
  end_date
)

cum_days <- count_between(
  c(begin_date, seq(ymd("2000-07-01"), ymd("2009-07-01"), by = "year"), end_date),
  include_last = TRUE
)

# Census county estimates ------------------------------------------------------

temp <- list.files(
  here::here("data", "raw", "intercensal", "2000"),
  pattern = "*.csv", 
  full.names = TRUE
)

census <- vroom::vroom(temp) %>%
  select(STATE:TOT_FEMALE) %>%
  mutate(GISJOIN = paste0("G", STATE, "0", COUNTY, "0"))

yr_map <- tibble::tibble(
  CODE = 1:13,
  DATE = lubridate::ymd("2000-04-01",
                        "2000-07-01",
                        "2001-07-01",
                        "2002-07-01",
                        "2003-07-01",
                        "2004-07-01",
                        "2005-07-01",
                        "2006-07-01",
                        "2007-07-01",
                        "2008-07-01",
                        "2009-07-01",
                        "2010-04-01",
                        "2010-07-01")
)

age_map <- tibble::tibble(
  levels = c(0:18, 99), 
  AGE = c("age0",  
          "age1_4",
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
          "age85",
          "total")
)

census_long <- left_join(census, yr_map, by = c("YEAR" = "CODE")) %>%
  filter(DATE != ymd("2010-07-01"), AGEGRP != 99) %>% # out of scope
  left_join(age_map, by = c("AGEGRP" = "levels")) %>%
  select(GISJOIN, DATE, AGE, TOT_MALE, TOT_FEMALE) %>%
  pivot_longer(cols = matches("^TOT"), 
               names_to = "VARIABLE", 
               values_to = "POP", 
               names_prefix = "TOT_") %>%
  mutate(VARIABLE = tolower(paste(VARIABLE, AGE, sep = "_"))) %>%
  arrange(GISJOIN, DATE)

# NHGIS data -------------------------------------------------------------------

tr <- vroom::vroom("/pkg/popgis/labpcs/data_projects/tract_estimates/2000_2010/data/nhgis/nhgis1133_ts_geog2010_tract.csv")
cty <- vroom::vroom("/pkg/popgis/labpcs/data_projects/tract_estimates/2000_2010/data/nhgis/nhgis1133_ts_geog2010_tract.csv")

nhgis_long <- purrr::map(
  list(tr, cty),
  ~.x %>%
    filter(DATAYEAR == 2000 | DATAYEAR == 2010) %>%
    mutate(
      male_age0 = CN8AA,
      male_age1_4 = CN8AB  + CN8AC  + CN8AD  + CN8AE,
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
      female_age0 = CN8AU,
      female_age1_4 = CN8AV + CN8AW + CN8AX + CN8AY,
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
    select(GISJOIN, DATAYEAR, male_age0:female_age85) %>%
    pivot_longer(-c(GISJOIN, DATAYEAR), names_to = "VARIABLE", values_to = "POP") %>%
    pivot_wider(names_from = DATAYEAR, values_from = POP, names_prefix = "POP_")
)

nhgis_tract <- nhgis_long[[1]] %>% mutate(GISJOIN_CTY = substr(GISJOIN, 1, 8))
nhgis_county <- nhgis_long[[2]]

# Processing -------------------------------------------------------------------

census_nest <- census_long %>%
  select(-AGE) %>%
  nest(DATA = c(DATE, POP))

cty_interp <- left_join(nhgis_county, census_nest) %>%
  mutate(
    CTY_INTERP = interp(POP_2000, POP_2010, cum_days),
    RATIO = map2(DATA, CTY_INTERP, ~.x$POP / .y)
  )

tra_interp <- nhgis_tract %>%
  mutate(TRA_INTERP = interp(POP_2000, POP_2010, cum_days))

tra_interp_adj <- left_join(
  tra_interp,
  cty_interp ,
  by = c("GISJOIN_CTY" = "GISJOIN", "VARIABLE"),
  suffix = c("_TRACT", "_COUNTY")
) %>%
  mutate(INTERP_ADJ = map2(TRA_INTERP, RATIO, ~.x * .y)) %>% # Ratio adjustment
  unnest(c(DATA, INTERP_ADJ)) %>%
  select(GISJOIN, VARIABLE, DATE, INTERP_ADJ, POP_2000_TRACT, POP_2010_TRACT)

tra_interp_adj %>% 
  nest(data = c(DATE, INTERP_ADJ, POP_2000_TRACT, POP_2010_TRACT))
