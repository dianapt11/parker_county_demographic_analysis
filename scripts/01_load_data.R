# ===================================================
# 01_load_data
# Purpose: load raw spatial data and census data
# Location: Parker County, Texas
# ===================================================

# Load libraries
library(sf)
library(tigris)
library(tidycensus)
library(tidyverse)
library(terra)
library(ggplot2)
library(exactextractr)

# options for tigris
options(tigris_use_cache = TRUE) #cache downloaded shapefiles
options(tigris_class = "sf") # default output of sf objects

#========================================

# Download boundaries

# Parker County FIPS
## find codes
data("fips_codes")

fips_codes %>%
  filter(
    grepl("Texas", state_name, ignore.case = TRUE),
    grepl("Parker", county, ignore.case = TRUE)
  )

state_fips <- "48" # Texas
county_fips <- "367" # Parker County

## County Boundary
parker_county <- counties(
  state = state_fips,
  year = 2022
) %>%
  filter(COUNTYFP == county_fips)

## census tracts
tracts_2022 <- tracts(
  state = state_fips, 
  county = county_fips, 
  year = 2022
  )

## check
st_crs(parker_county)
st_crs(tracts_2022)
plot(st_geometry(parker_county), col = NA, border ="black")
plot(st_geometry(tracts_2022), add = TRUE, border = "blue")

## save raw boundary data
saveRDS(parker_county, "data/raw/parker_county_boundary.rds")
saveRDS(tracts_2022, "data/raw/tracts_2022.rds")

#========================================

# Load and Pull in ACS 5-year data

## analysis years of interest
acs_years <- c(2014, 2019, 2024)

## ACS demographic variables of interest
acs_vars <- c(
  # population & income
  total_pop = "B01003_001",
  med_income = "B19013_001",
  
  ### race/ethinicity
  white_pop = "B02001_002",
  black_pop = "B02001_003",
  asian_pop = "B02001_005",
  hispanic_pop = "B03003_003",
  
  ### sex
  male_pop = "B01001_002",
  female_pop = "B01001_026",
  
  ### school enrollment (K-12) approx by sex
  school_enroll_male = "B14001_002",
  school_enroll_female= "B14002_026",
  
  ### age: under 18
  male_u18_1 = "B01001_003",
  male_u18_2 = "B01001_004",
  male_u18_3 = "B01001_005",
  male_u18_4 = "B01001_006",
  female_u18_1 = "B01001_027",
  female_u18_2 = "B01001_028",
  female_u18_3 = "B01001_029",
  female_u18_4 = "B01001_030",
  
  ### age: 65+
  male_65_1 = "B01001_020",
  male_65_2 = "B01001_021",
  male_65_3 = "B01001_022",
  male_65_4 = "B01001_023",
  male_65_5 = "B01001_024",
  male_65_6 = "B01001_025",
  female_65_1 = "B01001_044",
  female_65_2 = "B01001_045",
  female_65_3 = "B01001_046",
  female_65_4 = "B01001_047",
  female_65_5 = "B01001_048",
  female_65_6 = "B01001_049"
)

#========================================

## Function to pull ACS data for a given year
get_acs_year <- function(year, state_fips, county_fips, acs_vars){
  
  # pull tracts for the year
  tracts_year <- tracts(
    state = state_fips,
    county = county_fips,
    year = year
  )
  
  # pull ACS data for the year
  acs <- get_acs(
    geography = "tract",
    state = state_fips,
    county = county_fips,
    variables = acs_vars,
    year = year,
    survey = "acs5",
    geometry = FALSE
  )
    
  # transform to wide format
    acs_wide <- acs %>%
      select(GEOID, variable, estimate) %>%
      pivot_wider(
      names_from = variable, 
      values_from = estimate, 
      values_fill = NA
      )

  # Join ACS data to tracts for that year to ensure a complete panel
    tracts_year %>%
      left_join(acs_wide, by = "GEOID") %>%
      mutate(year = year)
}

#========================================

# Pull ACS for all years and bind
acs_data_all <- lapply(acs_years, function(y) {
  get_acs_year(y, state_fips, county_fips, acs_vars)
}) %>% bind_rows()

# check
head(acs_data_all)
acs_data_all %>% count(year)
#========================================

# join ACS data to geometry
tracts_2024_with_acs <- acs_data_all %>% filter(year == 2024)

#========================================
# Save raw ACS data
saveRDS(acs_data_all, "data/raw/acs_2014_2019_2024.rds")
saveRDS(tracts_2024_with_acs, "data/raw/tracts_2024_with_acs.rds")
