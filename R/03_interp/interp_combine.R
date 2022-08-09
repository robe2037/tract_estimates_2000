
library(dplyr)

nhgis_cty <- vroom::vroom(
  here::here("data", "preproc", "county", "nhgis_cty_2010_agg.csv")
)

cties <- nhgis_cty %>%
  select(STATEA, STATE, COUNTYA, COUNTY) %>%
  distinct() %>%
  filter(STATEA != "72")

states <- unique(cties$STATEA)

purrr::walk(
  states,
  function(state) {
    
    message("Processing state: ", state)
    
    interp_2000 <- vroom::vroom(
      here::here("data", "output", "interp_2000_2010", glue::glue("tr_interp_2000_2010_{state}.csv")),
      col_types = "ccccddcccd"
    )
    
    interp_2010 <- vroom::vroom(
      here::here("data", "output", "interp_2010_2019", glue::glue("tr_interp_2010_2019_{state}.csv")),
      col_types = "ccccddcccd"
    )
    
    mismatch <- anti_join(
      interp_2000 %>% select(-POP_EST, -DATAYEAR) %>% distinct(), 
      interp_2010 %>% select(-POP_EST, -DATAYEAR) %>% distinct(),
      by = c("GISJOIN", "COUNTYA", "STATEA", "TRACTA", "GEOGYEAR", "SEX", "AGEGRP", "RACE")
    )
    
    if (nrow(interp_2000) != nrow(interp_2010) | nrow(mismatch) > 0) {
      warning(
        "Grouping variables are not consistent between 2000 and 2010 files",
        "for state: ", state,
        call. = FALSE
      )
    }
    
    interp <- bind_rows(
      interp_2000,
      interp_2010
    ) %>%
      mutate(POP_EST = round(POP_EST, 2)) %>%
      left_join(cties, by = c("STATEA", "COUNTYA")) %>%
      select(GISJOIN, STATEA, STATE, COUNTYA, COUNTY, TRACTA, GEOGYEAR,
             DATAYEAR, SEX, AGEGRP, RACE, POP_EST) %>%
      arrange(GISJOIN, SEX, AGEGRP, RACE, DATAYEAR)
    
    if (!all(complete.cases(interp))) {
      warning(
        "Some observations have missing values in state: ", state,
        call. = FALSE
      )
    }
    
    vroom::vroom_write(
      interp,
      here::here("data", "output", "interp", glue::glue("tr_interp_2000_2019_{state}.csv")),
      delim = ","
    )
    
  }
)
