
# -------------------------------------
#
# Reallocate 2010 decennial data to new race categories using the 2010
# MRSF. The new race categories are:
#
# White, Black, Asian, American Indian / Alaska Native, Native Hawaiian /
# Pacific Islander, Multiple races
#
# Decennial data also includes "Other race" -- we use the 2000 MRSF to reallocate
# those counted as "other race" to the categories listed above.
#
# Because data are already based on 2010 boundaries, we can process directly
# at the tract level.
#
# For each 2010 tract, we reallocate all "other race" persons listed in the
# decennial data to each of the race categories listed above relative to the proportion
# of the total reallocation population (those listed as "other race" in the
# decennial) that was reallocated to that race class at the county level.
# Furthermore, this is weighted by the number of "other race" counts in that block.
#
# For example, if 10 people were listed as "other race" for a given county, sex, and age,
# and 2 of these were allocated to the asian race category, then 20% of the
# listed "other race" population for each tract within that county will be 
# added to that tract's asian population.
#
# In some cases, the count for the multi-race category is smaller in the MRSF
# than in the decennial data. Thus, there are more individuals that need to be
# reallocated than listed in the other-race category alone.
#
# In these cases, we reduce each tract's multi-race population in proportion to
# the decrease in this group between the MRSF and decennial data for a given
# county, sex, and age. Then, the difference in multi-race population for each tract
# is added to that tract's other population to determine the total reallocation
# population for that tract. The reallocation procedure then proceeds as above,
# except for the fact that the multi-race group is not adjusted further.
#
# -------------------------------------

source(here::here("R", "fun", "reallocate.R"))

# Get metadata for relevant tables
tables <- paste0("P12", LETTERS[1:7])

vars <- purrr::map_dfr(
  tables,
  function(t) {
    meta <- get_nhgis_metadata(dataset = "2010_SF1a", ds_table = t)
    
    variables <- meta$variables
    desc <- stringr::str_extract(meta$description, "(?<=\\().+(?=\\))")
    
    variables$description <- paste0(variables$description, ": ", desc)
    
    variables
  }
) %>% 
  filter(str_detect(description, "[0-9]")) %>%
  separate(description, into = c("SEX", "AGEGRP", "RACE"), sep = ": ")

# Crosswalks to regroup variables for aggregation
age_recode <- set_names(
  c("00_04", "05_09", "10_14", "15_19", "15_19", "20_24", "20_24", "20_24",
    "25_29", "30_34", "35_39", "40_44", "45_49", "50_54", "55_59", 
    "60_64", "60_64", "65_69", "65_69", "70_74", "75_79", "80_84", "85_up"),
  unique(vars$AGEGRP)
)

race_recode <- set_names(
  c("white", "black", "aian", "asian", "nhopi", "other", "multi"),
  unique(vars$RACE)
)

var_key <- vars %>%
  mutate(AGEGRP = recode(AGEGRP, !!!age_recode),
         SEX = str_sub(SEX, 1, 1),
         RACE = recode(RACE, !!!race_recode))

# 2010 tract level with 2010 boundaries
nhgis_tr <- read_nhgis(
  here::here("data", "extracts", "nhgis0529_csv.zip"),
  data_layer = contains("2010_tract")
) %>%
  mutate(STATEA = as.character(STATEA),
         GISJOIN_CTY = str_sub(GISJOIN, 1, 8))

# 2010 MRSF
mrsf <- vroom::vroom(
  here::here("data", "preproc", "mrsf", "mrsf_2010_agg.csv")
) %>%
  rename(POP_MRSF = POP) %>%
  select(-STATE, -COUNTY)

# 2010 county level with 2010 boundaries
nhgis_cty_2010 <- vroom::vroom(
  here::here("data", "preproc", "decennial", "county", "nhgis_cty_2010_agg.csv")
) %>%
  rename(POP_CTY = POP) %>%
  select(-STATE, -COUNTY) %>%
  filter(STATEA != "72") # Ignore PR? Not in MRSF.

# Calculate cty pop ratios for new race groups from MRSF
mrsf_comb <- full_join(
  mrsf, 
  nhgis_cty_2010,
  by = c("GISJOIN", "STATEA", "COUNTYA", "DATAYEAR", "SEX", "AGEGRP", "RACE")
) %>%
  mutate(MULTI_INCREASE = ifelse(RACE == "multi", POP_MRSF >= POP_CTY, NA)) %>%
  group_by(GISJOIN, SEX, AGEGRP) %>%
  mutate(MULTI_INCREASE = all(MULTI_INCREASE, na.rm = TRUE)) %>%
  ungroup() %>%
  arrange(GISJOIN, SEX, AGEGRP, RACE)

# Process --------------

tr_realloc <- reallocate_race(
  nhgis_tr, 
  mrsf_comb, 
  var_key
) %>%
  check_reallocation() %>%
  mutate(GEOGYEAR = 2010) %>%
  select(GISJOIN_TR = GISJOIN_SUB, GISJOIN, STATEA, COUNTYA, DATAYEAR, GEOGYEAR, SEX, AGEGRP, RACE, POP_ADJ = POP_SUB_NEW) %>%
  as_tibble() %>%
  filter(RACE != "other") %>%
  # Recode Bedford City into Bedford County for consistency with annual estimates
  mutate(
    GISJOIN_TR = case_when(
      GISJOIN == "G5105150" ~ "G5100190050100", 
      TRUE ~ GISJOIN_TR
    ),
    GISJOIN = str_sub(GISJOIN_TR, 1, 8),
    COUNTYA = str_sub(GISJOIN_TR, 5, 7)
  )

# Write finalized file
vroom::vroom_write(
  tr_realloc,
  here::here("data", "realloc", "tract_mrsf_reallocation_2010.csv"),
  delim = ","
)
