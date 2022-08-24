
# Tract Estimate Time Series for Sex/Age/Race: 2000-2019

This repository contains code to produce tract-level annual population estimates for age by sex by race demographic groups for 2000-2019.

## Data Sources

The processing pipeline uses data from the following sources:

* Decennial data
  * Decennial data at both county and tract level from 2000 and 2010 SF1 tables
* Intercensal data
  * Annual population estimates are provided by the census for 2000-2010 and 2010-2019 at the county level
  * Population estimates are released on July first of each year
* Modified Race Summary File (MRSF) data
  * The MRSF files provide decennial population data in adjusted race categories consistent with those used in the annual estimates
  
The raw formatted data for annual estimates and MRSF files are stored in `/data/raw` (not posted to GitHub).

Decennial data are obtained from NHGIS and stored in compressed extract files in `/data/extracts`.

## Pipeline

### Preprocessing

Raw files for all three sources are converted to a consistent long format. Preprocessed files are stored in `/data/preproc`.

### Race Category Reallocation

The decennial data include an "Other race" category when counting persons by race, but this category is not included in the MRSF or annual county estimates. We reconcile this discrepancy by reallocated the listed counts in the "Some other race" category to the race categories present in the other sources:

* White
* Black
* Asian
* American Indian / Alaska Native
* Native Hawaiian / Pacific Islander
* Multiple races

To conduct this reallocation, we compare the county-level counts for sex by age by race between the decennial and MRSF data. The MRSF does not include counts for an "Other race" category, while the decennial data do. Since these counts have been reallocated to the race categories listed above in the MRSF, those categories necessarily will have equal or larger counts than their counterparts in the decennial data.

#### 2010 Data

For each tract within a county, we reallocate all "other race" persons listed in that tract's decennial data to each of the MRSF race categories in the same proportion as was present for each race for the county to which that tract belongs.

For instance, if 50 people were listed as "other race" in the county-level decennial data, and the MRSF reflected an increase in 20 people listed as "White", 20 people listed as "Black", and 10 people listed as "Asian", then 40% of each tract's "other race" count would be reallocated to that tract's "White" and "Black" counts, and the remaining 20% would be reallocated to that tract's "Asian" count.

In general, the count of persons listed as multiple races in the decennial data is equal or larger than the count of persons listed as multiple races in the MRSF. In these cases, we treat the multiple-race category similarly to any of the single-race categories, reallocaating "other race" counts to it proportionally to the increase in that category at the county level.

However, in some cases, the count for the multi-race category is smaller in the MRSF than in the decennial data, as these records are occasionally reclassified to single-race categories in the process of making the MRSF file as well. Therefore, there are more individuals that need to be reallocated than listed in the other-race category alone, but it is not possible to determine exactly how many people need to be reallocated.

In these cases, we first reduce each tract's multi-race population in proportion to the decrease in multi-race population at the county level between the decennial and MRSF data. The amount of decrease in the multi-race population count is then added to the listed "other race" count to determine the total reallocation population for that tract. The reallocation process then proceeds as described above, except that the total reallocation population is only distributed to the single-race counts, not the multi-race count (which has already been adjusted).

(Note that all the above operations are conducted at the sex by age by race level, so the reallocation is conducted as described for every sex, age, and race group intersection.)

#### 2000 Data

The 2000 data are reallocated as described for 2010. However, 2000 tract data need to be standardized to 2010 tract boundaries. Therefore, for 2000, we first reallocate the decennial race categories at the _block_ level (still using reallocation population proportions from the county containing each block).

We then convert 2000 block counts to 2010 block counts using the 2000 block to 2010 block crosswalk provided by NHGIS. At this point, we aggregate the counts at the 2010 block level to 2010 tracts to obtain reallocated 2000 data for 2010 tract boundaries.

Reallocated data are found in `/data/realloc`. 2000 data are batch processed by state, with intermediate output stored in `/data/realloc/states`.

### 2020 Population Projection

The final interpolation step requires an ending estimate for counts of persons by sex by age by race at the tract level. Since 2020 data for this breakdown have not been released at the time this was written, we generate projected 2020 counts following the Hamilton-Perry method described in [Swanson (2010)](https://link.springer.com/article/10.1007/s11113-009-9144-7).

We use 2000 and 2010 decennial counts to generate cohort change ratios (CCRs). CCRs are calculated as the ratio of the population of a given cohort in a given decennial census to the 10-years-younger population in the previous decennial census. We calculate CCRs for each tract, sex, and race group. 

In cases where counts are too small to enable the calculation of a CCR (CCRs are undefined when any cohort's starting population in 2000 is 0), we aggregate to the tract by sex level and calculate CCRs for each tract by sex cohort. For cases where CCRs are still undefined, we use CCRs calculated at the county and sex group level. There are a small number of remaining cases where CCRs are still undefined even at the county and sex level (in special cases of Loving County, TX, and Kalawao County, HI). In these cases, we artificially set the undefined CCRs to 1.

CCRs for small-population areas have the potential to be highly variable. Per Swanson (2010), we place a floor of 0.82 (2% reduction rate over 10 years) and a ceiling of 1.63 (5% growth rate over 10 years) on all CCRs.

CCRs are not defined for age groups under 10. In these cases, we calculate the ratio of the 0-5 year-old and 5-10 year-old populations to the female mother-aged population at the tract, sex, and race level. This child-to-woman (CTW) ratio is multiplied by the projected 2020 mother-aged population (obtained using CCRs) to generate an projection for the 2020 population of the 0-5 and 5-10 age groups. 

In cases where the CTW is undefined (because there is no mother-aged population for a given tract, sex, and race), we use the CTWs calculated for tract by sex groups or county by sex groups as needed. 

Note that mothers obviously need not be the same race as children, but this restriction was placed to enable these calculations at the sex by age by race level.

We combine the 2020 population projections obtained via CCR and CTW into a final data source for use in interpolation to obtain annual population estimates. These data are stored in `/data/projected`.

### Annual Estimate Interpolation

Finally, we generate annual estimates at the tract level by interpolating between the adjacent decennial counts for each sex by age by race group.

For 2000-2010, we linearly interpolate between the 2000 and 2010 population for each tract, accounting for the fact that annual estimates are released on July 1 of each year while decennial estimates are released on April 1.

The interpolated values for all tracts in a given county will not necessarily sum to match the recorded annual estimate for that county. Therefore, we adjust all tracts within a county upward or downward to ensure that county-level counts are consistent with the counts of their contained tracts. This also causes tract-level estimates to follow the general trajectory of the county-level trend.

In some cases, the annual estimates for a county suggest that some persons of a given sex, age, and race group were counted in that county, but no tracts within that county have any interpolated population. In these cases, we allocate the county-level counts for a given sex and age group to the race groups listed above relative to the distribution of _total_ population (disregarding sex and age) of each race across the county's tracts.

In the cases where no persons of a given race were recorded at any sex and age group, we use the total population of each tract within the county to re-distribute the recorded county-level count.

For 2010-2020, we use the projected 2020 population as our ending point for the interpolation and proceed as described above. However, we discard estimates after 2019, which is the latest year for which annual county-level estimates are available. Similarly, decennial data are not included in the output files, even in years where both a decennial and annual estimate are recorded. Therefore, data listed as originating from 2000 represent the estimated tract-level annual estimate for 2000, not a value obtained from the 2000 decennial census.

### Notes

This code uses the development version of `ipumsr` (0.5.0.9000) for access to the NHGIS API R client. This code is actively in development at the time this data processing pipeline was created, and future changes may impact parts of this repository. This functionality is used primarily in creating data extracts for decennial data.

### References

Swanson, D. A., Schlottmann, A., & Schmidt, B. (2010). Forecasting the population of census tracts by age and sex: An example of the Hamilton–Perry method in action. Population Research and Policy Review, 29(1), 47-63.
