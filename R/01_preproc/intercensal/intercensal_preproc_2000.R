
# -------------------------------------
#
# Read in raw annual census estimate data from 2000-2010 and 
# convert to long format by county, date, age, sex, and race.
#
# -------------------------------------

library(tidyverse)

fp <- list.files(
  here::here("data", "raw", "intercensal", "2000"),
  pattern = "*.csv", 
  full.names = TRUE
)

race_recode <- set_names(
  c("white", "black", "aian", "asian", "nhopi", "multi"),
  c("WA", "BA", "IA", "AA", "NA", "TOM")
)

# Aggregate first two groups for consistency with other data sources.
age_map <- tibble::tibble(
  AGEGRP = c(0:18, 99),
  AGE = c("00_04", "00_04", "05_09", "10_14", "15_19", "20_24",
    "25_29", "30_34", "35_39", "40_44", "45_49", "50_54", "55_59", 
    "60_64", "65_69", "70_74", "75_79", "80_84", "85_up", "total")
  
)

yr_map <- tibble::tibble(
  YEAR = 1:13,
  DATE = c("2000-04-01",
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

census_long <- vroom::vroom(fp) %>%
  mutate(GISJOIN = paste0("G", STATE, "0", COUNTY, "0")) %>%
  filter(AGEGRP != 99, YEAR != 13) %>%
  left_join(yr_map, by = "YEAR") %>%
  left_join(age_map, by = "AGEGRP") %>%
  select(
    -c(SUMLEV, 
       YEAR, 
       AGEGRP,
       STNAME, 
       CTYNAME, 
       contains("TOT"), 
       matches("^NH"), 
       matches("H"))
  ) %>%
  pivot_longer(
    cols = contains("MALE"),
    names_to = c("RACE", "SEX"),
    names_sep = "_"
  ) %>%
  mutate(
    RACE = recode(RACE, !!!race_recode),
    SEX = str_sub(SEX, 1, 1)
  ) %>%
  rename(AGEGRP = AGE, POP = value) %>%
  group_by(GISJOIN, DATE, AGEGRP, RACE, SEX) %>% 
  summarize(POP = sum(POP), .groups = "drop") %>%
  arrange(GISJOIN, DATE, SEX, AGEGRP, RACE)

write_csv(
  census_long,
  here::here("data", "preproc", "intercensal", "intercensal_preproc_2000.csv")
)
