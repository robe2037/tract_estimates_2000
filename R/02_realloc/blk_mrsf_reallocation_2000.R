
# -------------------------------------
#
# Reallocate 2000 decennial data to new race categories using the 2000
# MRSF. The new race categories are:
#
# White, Black, Asian, American Indian / Alaska Native, Native Hawaiian /
# Pacific Islander, Multiple races
#
# Decennial data also includes "Other race" -- we use the 2000 MRSF to reallocate
# those counted as "other race" to the categories listed above.
#
# We must also standardize the counts in these new categories to 2010 tract 
# boundaries. To do this, we:
#
# 1. Reallocate 2000 block level data to new race categories
# 2. Convert reallocated data to 2010 block boundaries using block to block
#    crosswalk.
# 3. Aggregate block-level counts to 2010 tracts
#
# Step 1 is accomplished by comparing the county-level counts in the decennial
# data and the MRSF for each county, sex, and age group. The MRSF does not
# include counts for an "Other race" category, while the decennial data do.
# Since these counts have been reallocated to the race categories listed above
# in the MRSF, those categories necessarily will have equal or larger counts
# than their counterparts in the decennial data.
#
# Therefore, for each 2000 block, we reallocate all "other race" persons listed in the
# decennial data to each of the race categories listed above relative to the proportion
# of the total reallocation population (those listed as "other race" in the
# decennial) that was reallocated to that race class at the county level.
# Furthermore, this is weighted by the number of "other race" counts in that block.
#
# For example, if 10 people were listed as "other race" for a given county, sex, and age,
# and 2 of these were allocated to the asian race category, then 20% of the
# listed "other race" population for each block within that county will be 
# added to that block's asian population.
#
# In some cases, the count for the multi-race category is smaller in the MRSF
# than in the decennial data. Thus, there are more individuals that need to be
# reallocated than listed in the other-race category alone.
#
# In these cases, we reduce each block's multi-race population in proportion to
# the decrease in this group between the MRSF and decennial data for a given
# county, sex, and age. Then, the remaining multi-race population for each block
# is added to that block's other population to determine the total reallocation
# population for that block. The reallocation procedure then proceeds as above,
# except for the fact that the multi-race group is not adjusted further.
#
# At this point we convert these adjusted counts to 2010 block boundaries,
# and aggregate those counts to the 2010 tract level.
#
# To make processing of block-level data feasible, we do this procedure by state
# and county. This script assumes the availability of block-level NHGIS extracts
# to enable this.
#
# -------------------------------------

source(here::here("R", "fun", "reallocate.R"))

# Process ----------------

# Must have block-level extracts created for each state. Can retrieve with
# ipumsr API client.

# Process by extract number for a single state of block-level sex/age/race counts
#nos <- unique(get_recent_extracts_info_list("nhgis")$number)
blk_convert_geog_batch(extract_numbers = nos)

# Aggregate --------------

# Aggregate identical 2010 tract ids that are present in multiple states because
# of boundary changes between 2000 and 2010
tmp <- vroom::vroom(
  list.files(here::here("data", "preproc", "tract", "states"), full.names = TRUE)
) %>% 
  group_by(GISJOIN_TR, GISJOIN, STATEA, COUNTYA, DATAYEAR, GEOGYEAR, SEX, AGEGRP, RACE) %>%
  summarize(POP_ADJ = sum(POP_ADJ), .groups = "drop")

# Write finalized file
vroom::vroom_write(
  tmp,
  here::here("data", "preproc", "tract", "tract_mrsf_reallocation_2000.csv"),
  delim = ","
)


