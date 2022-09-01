
# Tract Estimate Time Series for Sex/Age/Race: 2000-2019

This repository contains code to produce tract-level annual population estimates for age by sex by race demographic groups for 2000-2019.

## Data Sources

The processing pipeline uses data from the following sources:

* Decennial census data
  * 2000 Summary File (SF) 1 for counties and census blocks (DVR: we use block data from 2000 - we stopped using tracts, correct?)
  * 2010 Summary File (SF) 1 for counties and census tracts
* Population estimates data 
  * Data provided for 2000-2010 (intercensal estimates) and 2010-2019 (vintage 2019)
  * Has no "some other race" category
* Modified Race Summary File (MRSF) data
  * Provided for 2000 and 2010
  * Has no "some other race" category

Raw, intermediate, and output data files are not pushed to GitHub, but are assumed to exist in the `/data` directory in the local repository. 

The raw formatted data for annual population estimates and MRSF files are stored in `/data/raw`. Decennial data are obtained from NHGIS and stored in compressed extract files in `/data/extracts`. Code for generating these extracts is given in `/R/00_extract/data_extract.R`

## Processing pipeline

### 1. Preprocessing / Data Formatting

Raw files for all three sources are converted to a consistent long format by geography, sex, age, and race. 

Preprocessing scripts for all data sources are found in `/R/01_preproc`. Preprocessed files are stored in `/data/preproc`.

### 2. Race Category Reallocation

The decennial data include a "some other race" category when reporting counts of persons by race, but this category is not included in the MRSF or annual population estimates. The MRSF and annual population estimates use the Office of Management and Budget (OMB) [standard race and ethnicity categories](https://orwh.od.nih.gov/toolkit/other-relevant-federal-policies/OMB-standards). "Some other race" is not a standard OMB category but is used by the Census Bureau on the decennial questionnaires. The MRSF is created by allocating all persons who identify as "some other race" to one of the OMB standard race categories or to the "Two or more race" category (note that the OMB standard allows individuals to select one or more races). MAYBE ADD A SENTENCE SUMMARIZING HOW THE ALLOCATION IS DONE FOR MRSF. The annual population estimates data starts with the MRSF counts; therefore, the annual population estimates also excludes the "some other race" category.

We reconcile this discrepancy by reallocating the "some other race" counts in the decennial data to the race categories present in the other sources. The final categories are:

* White alone
* Black alone
* Asian alone
* American Indian / Alaska Native alone
* Native Hawaiian / Pacific Islander alone
* Two or more races (DVR: just editing to make consistent with census terminology)

To conduct this reallocation, we compare the county-level counts in the SF1 data to the counts in the MRSF for each sex, age, and race group. The MRSF counts for each of the race categories listed above will reflect the original counts plus the number of persons identifying as "some other race" who were reclassified to one of the above categories in the creation of the MRSF. The reallocation approach differs slightly for the 2010 and 2000 data.

#### 2010 Data

For each tract within a county, we reallocate all "some other race"" persons (for a given age and sex) listed in that tract's SF1 data to each of the MRSF race categories in the same proportion as was present for each race (DVR: we use the given age and sex here as well? May be worth adding that parenthetical again for clarity) for the county to which that tract belongs.

For instance, if 50 "some other race"" persons (for a given age and sex) were recorded in the county-level decennial data and the MRSF counts show an increase in 20 people listed as White alone, 20 people listed as Black alone, and 10 people listed as Asian alone, then 40% of each tract's "some other race"" count would be reallocated to that tract's White alone and Black alone counts, and the remaining 20% would be reallocated to that tract's Asian alone count.

In general, the count of persons listed as multiple races in the decennial data is equal to or larger than the count of persons listed as multiple races in the MRSF. In these cases, we treat the multiple-race category similarly to the single-race categories, reallocating "some other race" counts to it proportional to the increase in the multi-race count at the county level.

However, in some cases, the count for the multi-race category is smaller in the MRSF than in the SF1 data (in addition to "some other race"" records, multi-race records may be reclassified to single-race categories in the process of making the MRSF file). In these cases, there are more individuals that need to be reallocated than listed solely in the "some other race" category. Yet, we cannot exactly how many people need to be reallocated, since the counts in the MRSF's multi-race category reflect both reclassifications of multi-race individuals to single-race categories as well as the reclassification of "some other race" individuals to the multi-race category.

In these cases, we first reduce each tract's multi-race population (for a given age and sex) in proportion to the decrease in multi-race population at the county level between the SF1 and MRSF data. The amount of decrease in the multi-race population count is then added to the SF1 "some other race" count to determine the tract's final count for reallocation. The reallocation process then proceeds as described above, except that the final reallocation count is only distributed among single-race categories, not the multi-race category (which has already been adjusted).

#### 2000 Data

The 2000 SF1 data require the same processing as above, but also need to be standardized to 2010 tract boundaries. Therefore, for 2000, we first reallocate the SF1 race categories at the _block_, rather than tract level (still using reallocation population proportions from the county containing each block).

We then convert 2000 block counts onto 2010 blocks using the [2000 block to 2010 block crosswalk](https://www.nhgis.org/geographic-crosswalks#download-from-blocks) provided by NHGIS. We then aggregate these counts from the 2010 blocks to 2010 tracts to obtain reallocated 2000 data for 2010 tract boundaries. A zipped copy of the crosswalk is stored in `/data/raw/xwalk`

Reallocation scripts are found in `/R/02_realloc`, and intermediate reallocated data are stored in `/data/realloc`. 2000 data are batch processed by state, with state-level output stored in `/data/realloc/states`.

### 3. 2020 Population Projection

Producing annual estimates requires interpolating between starting and ending decennial counts, but 2020 data for the age by sex by race breakdown have not been released at the time this was written. Therefore, we must generate projected 2020 population counts for sex by age by race at the tract level. We follow the Hamilton-Perry method described in [Swanson (2010)](https://link.springer.com/article/10.1007/s11113-009-9144-7) to generate these projections.

We use 2000 and 2010 decennial counts to generate cohort change ratios (CCRs) for each sex by race group. CCRs are calculated as the ratio of the population of a given age group in a given decennial census to the 10-years-younger population in the previous decennial census.

In cases where counts are too small to enable the calculation of a CCR (CCRs are undefined when any cohort's starting population in 2000 is 0), we aggregate to the tract by sex level and calculate CCRs for each tract by sex cohort. For cases where CCRs are still undefined, we use CCRs calculated at the county by sex level. There are a small number of remaining cases where CCRs are still undefined even at the county by sex level (special cases of Loving County, TX, and Kalawao County, HI). In these cases, we artificially set the undefined CCRs to 1.

CCRs for small-population areas have the potential to be highly variable. Per Swanson (2010), we place a floor of 0.82 (2% reduction rate over 10 years) and a ceiling of 1.63 (5% growth rate over 10 years) on all CCRs. Any CCRs lower than the floor value or higher than the ceiling value are replaced with the floor or ceiling value, respectively.

CCRs are not defined for age groups under 10. In these cases, we calculate the ratio of the 0-5 year-old and 5-10 year-old populations to the female mother-aged population at the tract by sex by race level. This child-to-woman (CTW) ratio is multiplied by the projected 2020 mother-aged population (obtained using CCRs) to generate an projection for the 2020 population of the 0-5 and 5-10 age groups. 

For projecting the population of children aged 0-4, the eligible mother population is the total population of all females of the given race between ages 20-45. For projecting the population of children aged 5-10, the eligible mother population is the total population of all females of the given race between 30-50. While mothers obviously do not need to be the same race as children, this restriction was necessary to enable the calculation of approximate rates at the sex by age by race level.

In cases where the CTW is undefined (because there is no mother-aged population for a given tract, sex, and race), we use the CTWs calculated for tract by sex groups or, if still undefined, county by sex groups. 

We combine the 2020 population projections obtained via CCR and CTW into a final data source for use in interpolation to obtain annual population estimates. Projection scripts are found in `/R/03_project` and intermediate data are stored in `/data/projected`.

### 4. Annual Estimate Interpolation

Finally, we generate annual estimates at the tract level by interpolating between either the 2000 and 2010 decennial counts or the 2010 and projected 2020 counts for each sex by age by race group.

For 2000-2010, we linearly interpolate between the 2000 and 2010 population for each tract, accounting for the fact that annual estimates are released on July 1 of each year while decennial estimates are released on April 1.

The interpolated values for all tracts in a given county will not necessarily sum to match the recorded annual estimate for that county. Therefore, we adjust all tracts within a county upward or downward to ensure that county-level counts are consistent with the counts of their contained tracts. This also causes tract-level estimates to follow the general trajectory of the county-level trend.

In some cases, the annual estimates for a county suggest that some persons of a given sex, age, and race group were counted in that county, but no tracts within that county have any counts for that group after interpolation. In these cases, we allocate the county-level counts for a given sex, age, and race group to the tracts relative to the distribution of the total population of that race (disregarding sex and age) across the county's tracts

In the cases where no persons of a given race were recorded at _any_ sex and age group, we use the total population of each tract within the county to re-distribute the recorded county-level count.

For 2010-2020, we use the projected 2020 population as our ending point for the interpolation and proceed as described above. However, we discard estimates after 2019, which is the latest year for which annual county-level estimates are available. 

Decennial counts are not included in the output files, even in years where both a decennial and annual estimate are recorded. Therefore, data listed as originating from 2000 represent the estimated tract-level _annual estimate_ for 2000, not the count recorded in the 2000 decennial census.

Scripts for annual estimate interpolation are found in `/R/04_interp`, with single-decade data stored in `/data/interp`

We combine the 2000 to 2010 and 2010 to 2019 time series for each tract into a single time series spanning 2000 to 2019. Final output data are provided by state and found in `/data/popest`.

### Notes

This code uses the development version of `ipumsr` (0.5.0.9000) for access to the NHGIS API R client. This code is actively in development at the time this data processing pipeline was created, and future changes may impact parts of this repository. This functionality is used primarily in creating data extracts for decennial data.

### References

Swanson, D. A., Schlottmann, A., & Schmidt, B. (2010). Forecasting the population of census tracts by age and sex: An example of the Hamilton–Perry method in action. Population Research and Policy Review, 29(1), 47-63.
