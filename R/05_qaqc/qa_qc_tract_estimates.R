
# -------------------------------------
#
# Assess the quality of the tract estimates
#
# -------------------------------------

#require(vroom)
require(ggplot2)
require(dplyr)
require(purrr)
library(cli, lib.loc = "/usr/local/lib/R/site-library")
require(RColorBrewer)
require(tidycensus)

## ---- Get ACS data for comparisons ----
acs2019 <- get_acs(geography = "tract", state = c("27", "36", "46"), year = 2019, table = "B01001A")
acs2010 <- get_acs(geography = "tract", state = c("27", "36", "46"), year = 2010, table = "B01001A")
dec2010 <- get_decennial(geography = "tract", state = c("27", "36", "46"), county = c("053", "123", "047", "061", "011"), year = 2010, table = "P012A") %>%
  rename(estimate = value)

## ---- Prep decennial data ---- 
# Recode ages to be consistent with ACS data 
dec2010 <- dec2010 %>%
  filter(variable %in% c("P012A008", "P012A009", "P012A010", "P012A011", "P012A012",
                         "P012A032", "P012A033", "P012A034", "P012A035", "P012A036")) %>%
  mutate(variable = case_when(variable %in% c("P012A008", "P012A009", "P012A010") ~ "B01001A_008",
                              variable == "P012A011" ~ "B01001A_009",
                              variable == "P012A012" ~ "B01001A_010",
                              variable %in% c("P012A032", "P012A033", "P012A034") ~ "B01001A_023",
                              variable == "P012A035" ~ "B01001A_024",
                              variable == "P012A036" ~ "B01001A_025")) %>%
  group_by(GEOID, NAME, variable) %>%
  summarise(estimate = sum(estimate))



acs <- bind_rows("2010acs" = acs2010, "2010dec" = dec2010, "2019acs" = acs2019, .id = "year") %>%
  mutate(GISJOIN = paste0("G", str_sub(GEOID, 1, 2), "0", str_sub(GEOID, 3, 5), "0", str_sub(GEOID, 6, 11))) %>%
  arrange(GISJOIN, GEOID, variable)

## ---- Subset ACS data ----
acs_sel <- acs %>%
  filter(GISJOIN %in% tracts) %>%
  filter(variable %in% c("B01001A_008", "B01001A_009", "B01001A_010", "B01001A_023", "B01001A_024", "B01001A_025")) %>%
  mutate(RACE = "white",
         SEX = case_when(variable %in% c("B01001A_008", "B01001A_009", "B01001A_010") ~ "M",
                         TRUE ~ "F"),
         AGEGRP = case_when(variable %in% c("B01001A_008",  "B01001A_023") ~ "20_24",
                            variable %in% c("B01001A_009",  "B01001A_024") ~ "25_29",
                            variable %in% c("B01001A_010",  "B01001A_025") ~ "30_34"),
         DATAYEAR = case_when(year %in% c("2010acs", "2010dec") ~ 2010,
                              TRUE ~ 2019))

## ---- Load data ----
# Create list of state CSVs to load
fp <- c("/pkg/ipums/personal/finn/nhgis/projects/tract_estimates_2000/data/popest//tr_popest_2000_2019_27.csv",
           "/pkg/ipums/personal/finn/nhgis/projects/tract_estimates_2000/data/popest//tr_popest_2000_2019_46.csv",
           "/pkg/ipums/personal/finn/nhgis/projects/tract_estimates_2000/data/popest//tr_popest_2000_2019_36.csv")

df <- vroom::vroom(fp)

## ---- Restrict to one tract ----
# df_sel <- df %>%
#   filter(GISJOIN == "G2701230030300")
# 
# df_sel %>%
#   filter(SEX == "F") %>%
#   filter(RACE != "white") %>%
#   ggplot(aes(x = DATAYEAR, y = ESTIMATE, color = RACE )) +
#   geom_line() +
#   facet_wrap("AGEGRP", nrow = 3, ncol = 6) + 
#   scale_color_brewer(palette="Set1")
# 
# df_sel %>%
#   filter(SEX == "M") %>%
#   ggplot(aes(x = DATAYEAR, y = ESTIMATE, color = RACE )) +
#   geom_line() +
#   facet_wrap("AGEGRP", nrow = 3, ncol = 6) + 
#   scale_color_brewer(palette="Set1")

## ---- Plot function ----
plot_estimates_allracegroups <- function(tract){
  
  df %>%
    filter(GISJOIN == tract) %>%
    filter(SEX == "F") %>%
    ggplot(aes(x = DATAYEAR, y = ESTIMATE, color = RACE )) +
    geom_line() +
    geom_point(data = filter(acs_sel, GISJOIN == tract & SEX == "F"), aes(x = DATAYEAR, y = estimate, color = year)) + 
    facet_wrap("AGEGRP", nrow = 3, ncol = 6) + 
    scale_color_brewer(palette="Set1") + 
    ggtitle(paste(tract, "- Females"))
  
  ggsave(paste0("plots/", tract, "_allrace_f.png"), width = 10, height = 7, units = "in")
  
  df %>%
    filter(GISJOIN == tract) %>%
    filter(SEX == "M") %>%
    ggplot(aes(x = DATAYEAR, y = ESTIMATE, color = RACE )) +
    geom_line() +
    geom_point(data = filter(acs_sel, GISJOIN == tract & SEX == "M"), aes(x = DATAYEAR, y = estimate, color = year)) + 
    facet_wrap("AGEGRP", nrow = 3, ncol = 6) + 
    scale_color_brewer(palette="Set1") + 
    ggtitle(paste(tract, "- Males"))
  
  ggsave(paste0("plots/", tract, "_allrace_m.png"), width = 10, height = 7, units = "in")
  
}

plot_estimates_nonwhite <- function(tract){
  
  df %>%
    filter(GISJOIN == tract) %>%
    filter(SEX == "F") %>%
    filter(RACE != "white") %>%
    ggplot(aes(x = DATAYEAR, y = ESTIMATE, color = RACE )) +
    geom_line() +
    facet_wrap("AGEGRP", nrow = 3, ncol = 6) + 
    scale_color_brewer(palette="Set1") + 
    ggtitle(paste(tract, "- Females"))
  
  ggsave(paste0("plots/", tract, "_nonwhite_f.png"), width = 10, height = 7, units = "in")
  
  df %>%
    filter(GISJOIN == tract) %>%
    filter(SEX == "M") %>%
    filter(RACE != "white") %>%
    ggplot(aes(x = DATAYEAR, y = ESTIMATE, color = RACE )) +
    geom_line() +
    facet_wrap("AGEGRP", nrow = 3, ncol = 6) + 
    scale_color_brewer(palette="Set1") + 
    ggtitle(paste(tract, "- Males"))
  
  ggsave(paste0("plots/", tract, "_nonwhite_m.png"), width = 10, height = 7, units = "in")
  
}

## ---- Create vector of tract IDs to plot ----
tracts <- c("G2701230030300", "G2700530106200", "G4600110959000", "G3600610008400", "G3600470018100", "G3600610022800")

map(tracts, ~ plot_estimates_allracegroups(.x))
map(tracts, ~ plot_estimates_nonwhite(.x))




