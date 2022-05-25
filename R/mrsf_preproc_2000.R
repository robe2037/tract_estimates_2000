
library(tidyverse)

# Read MRSF ----------------------

mrsf_dir <- "/pkg/popgis/labpcs/data_projects/tract_estimates/1990_2000/data/popest/modified_race/"

fp <- list.files(mrsf_dir, pattern = ".txt", full.names = TRUE)

mrsf_2000 <- read_fwf(fp, fwf_widths(c(2, 3, 1, 2, rep(8, 124), 2, NA)))

# Format MRSF --------------------

hisp <- c("hisp", "nonhisp")
sex <- c("M", "F")
race <- c("white", "black", "aian", "asian", "nhopi", paste0(rep("multi", 26), 1:26))

# Detailed race, if needed.
# race <- c("WHITE", "BLACK", "AIAN", "ASIAN", "NHOPI", "WHITE_BLACK", 
#           "WHITE_AIAN", "WHITE_ASIAN", "WHITE_NHOPI", "BLACK_AIAN", 
#           "BLACK_ASIAN", "BLACK_NHOPI", "AIAN_ASIAN", "AIAN_NHOPI", 
#           "ASIAN_NHOPI", "WHITE_BLACK_AIAN", "WHITE_BLACK_ASIAN", 
#           "WHITE_BLACK_NHOPI", "WHITE_AIAN_ASIAN", "WHITE_AIAN_NHOPI", 
#           "WHITE_ASIAN_NHOPI", "BLACK_AIAN_ASIAN", "BLACK_AIAN_NHOPI", 
#           "BLACK_ASIAN_NHOPI", "AIAN_ASIAN_NHOPI", "WHITE_BLACK_AIAN_ASIAN", 
#           "WHITE_BLACK_AIAN_NHOPI", "WHITE_BLACK_ASIAN_NHOPI", 
#           "WHITE_AIAN_ASIAN_NHOPI", "BLACK_AIAN_ASIAN_NHOPI", 
#           "WHITE_BLACK_AIAN_ASIAN_NHOPI")

age_recode <- set_names(
  c("00_04", "00_04", "05_09", "10_14", "15_19", 
    "20_24", "25_29", "30_34", "35_39", "40_44", 
    "45_49", "50_54", "55_59", "60_64", "65_69", 
    "70_74", "75_79", "80_84", "85_up"), 
  1:19
)

col_names <- expand_grid(hisp, sex, race) %>%
  rowwise() %>%
  mutate(VAR = paste(hisp, sex, race, sep = "_")) %>%
  pull(VAR)

colnames(mrsf_2000) <- c("STATEA", "COUNTYA", "BLANK", "AGEGRP", col_names, 
                         "STATE", "COUNTY")

# Aggregate to new variable groups ---

mrsf_2000_agg <- mrsf_2000 %>%
  select(-BLANK) %>%
  filter(!is.na(COUNTYA)) %>% # Remove state totals(?)
  mutate(
    AGEGRP = recode(AGEGRP, !!!age_recode),
    GISJOIN = paste0("G", STATEA, "0", COUNTYA, "0")
  ) %>%
  pivot_longer(
    -c(GISJOIN, AGEGRP, STATE, STATEA, COUNTY, COUNTYA), 
    names_to = "VAR", 
    values_to = "POP"
  ) %>%
  mutate(
    HISP =  str_extract(VAR, "[^_]+"),
    SEX = str_extract(VAR, "(?<=_)[A-Z](?=_)"),
    RACE = str_extract(str_extract(VAR, "[^_]+$"), "[^0-9]+"),
    YEAR = 2000
  ) %>%
  select(-VAR) %>%
  group_by(GISJOIN, STATE, COUNTY, STATEA, COUNTYA, YEAR, SEX, AGEGRP, RACE) %>%
  summarize(POP = sum(POP), .groups = "drop")

# Write ------------------------------

write_csv(mrsf_2000_agg, here::here("data", "mrsf_2000_agg.csv"))

