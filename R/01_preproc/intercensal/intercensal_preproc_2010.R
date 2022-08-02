
# -------------------------------------
#
# Read in raw annual census estimate data from 2010-2019 and 
# convert to long format by county, date, age, sex, and race.
#
# -------------------------------------

library(tidyverse)

census <- vroom::vroom(
  here::here("data", "raw", "intercensal", "2010", "cc-est2019-alldata.csv")
) %>%
  mutate(
    GISJOIN = paste0("G", STATE, "0", COUNTY, "0"),
    # Recode counties whose FIPS is different in MRSF
    GISJOIN = case_when(
      GISJOIN == "G4601020" ~ "G4601130",
      GISJOIN == "G0201580" ~ "G0202700",
      TRUE ~ GISJOIN
    ),
    STATE = str_sub(GISJOIN, 2, 3),
    COUNTY = str_sub(GISJOIN, 5, 7)
  )

race_recode <- set_names(
  c("white", "black", "aian", "asian", "nhopi", "multi"),
  c("WA", "BA", "IA", "AA", "NA", "TOM")
)

yr_map <- tibble::tibble(
  YEAR = 1:12,
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
  AGEGRP = 0:18, 
  AGE = c("total", "00_04", "05_09", "10_14", "15_19", "20_24",
          "25_29", "30_34", "35_39", "40_44", "45_49", "50_54", "55_59", 
          "60_64", "65_69", "70_74", "75_79", "80_84", "85_up")
)

census_long <- census %>%
  filter(AGEGRP != 0, YEAR != 1) %>%
  left_join(yr_map, by = "YEAR") %>%
  left_join(age_map, by = "AGEGRP") %>%
  select(
    -c(SUMLEV, 
       STNAME, 
       CTYNAME, 
       YEAR,
       AGEGRP,
       contains("TOT"), 
       matches("^NH"), 
       matches("H"),
       matches("C_"))
  ) %>%
  pivot_longer(
    cols = contains("MALE"),
    names_to = c("RACE", "SEX"),
    names_sep = "_",
    values_to = "POP"
  ) %>%
  mutate(
    RACE = recode(RACE, !!!race_recode),
    SEX = str_sub(SEX, 1, 1)
  ) %>%
  rename(AGEGRP = AGE) %>%
  relocate(GISJOIN, .before = STATE) %>%
  arrange(GISJOIN, DATE, SEX, AGEGRP, RACE)

write_csv(
  census_long,
  here::here("data", "preproc", "intercensal", "intercensal_preproc_2010.csv")
)
