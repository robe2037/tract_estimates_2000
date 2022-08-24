
# -------------------------------------
#
# Combine 2000-2010 and 2010-2019 interpolated tract estimates
# into single time series from 2000-2019.
#
# Each state is contained in its own file.
#
# -------------------------------------

library(dplyr)

# Data -------------------------------------------------------------------------

# County information for joining county/state names to output
cties <-  vroom::vroom(
  here::here("data", "preproc", "county", "nhgis_cty_2010_agg.csv")
) %>%
  select(STATEA, STATE, COUNTYA, COUNTY) %>%
  distinct() %>%
  filter(STATEA != "72")

states <- unique(cties$STATEA)

# Process ----------------------------------------------------------------------

# Combine 2000-2010 and 2010-2019 time series, attach state/county names,
# round estimates, and write final state-level files.

purrr::walk(
  states,
  function(state) {
    
    message("Processing state: ", state)
    
    interp_2000 <- vroom::vroom(
      here::here("data", "interp", "interp_2000_2010", glue::glue("tr_interp_2000_2010_{state}.csv")),
      col_types = "cccccddcccd"
    )
    
    interp_2010 <- vroom::vroom(
      here::here("data", "interp", "interp_2010_2019", glue::glue("tr_interp_2010_2019_{state}.csv")),
      col_types = "cccccddcccd"
    )
    
    mismatch <- anti_join(
      interp_2000 %>% select(-ESTIMATE, -DATAYEAR) %>% distinct(), 
      interp_2010 %>% select(-ESTIMATE, -DATAYEAR) %>% distinct(),
      by = c("GISJOIN", "COUNTYA", "STATEA", "TRACTA", "GEOGYEAR", "SEX", "AGEGRP", "RACE")
    )
    
    if (nrow(interp_2000) != nrow(interp_2010) | nrow(mismatch) > 0) {
      warning(
        "Grouping variables are not consistent between 2000 and 2010 files",
        "for state: ", state,
        call. = FALSE
      )
    }
    
    interp <- bind_rows(interp_2000, interp_2010) %>%
      mutate(ESTIMATE = round(ESTIMATE, 4)) %>%
      left_join(cties, by = c("STATEA", "COUNTYA")) %>%
      select(GISJOIN, GEOID, STATEA, STATE, COUNTYA, COUNTY, TRACTA, GEOGYEAR,
             DATAYEAR, SEX, AGEGRP, RACE, ESTIMATE) %>%
      arrange(GISJOIN, SEX, AGEGRP, RACE, DATAYEAR)
    
    if (!all(complete.cases(interp))) {
      warning(
        "Some observations have missing values in state: ", state,
        call. = FALSE
      )
    }
    
    vroom::vroom_write(
      interp,
      here::here("data", "popest", glue::glue("tr_popest_2000_2019_{state}.csv")),
      delim = ","
    )
    
  }
)
