# ===================================================
# 02_clean_data
# Purpose: Clean and preprocess Parker County Data Sets
# ===================================================

#Load libraries
library(sf)
library(dplyr)
library(tidyr)

# ===================================================

# Load raw data
tracts_2014 <-readRDS("data/raw/acs_2014_2019_2024.rds") %>%
  filter(year == 2014)

tracts_2019 <-readRDS("data/raw/acs_2014_2019_2024.rds") %>%
  filter(year == 2019)

tracts_2024 <-readRDS("data/raw/acs_2014_2019_2024.rds") %>%
  filter(year == 2024)

# combine all years into 1 dataset
tracts_raw <- bind_rows(tracts_2014, tracts_2019, tracts_2024)
glimpse(tracts_raw)
sum(is.na(tracts_raw))

# ===================================================

# clean and derive demographic indicators

tracts_clean <- tracts_raw %>%
  
  # replace NAs with 0 for all numeric ACS counts
  mutate(across(
    c(total_pop, male_pop, female_pop, school_enroll_male, 
      school_enroll_female, starts_with("male_u18"), starts_with("female_u18"),
      starts_with("male_65"), starts_with("female_65"), white_pop, black_pop,
      asian_pop, hispanic_pop),
    ~replace_na(.x, 0)
  )) %>%
  
  # derived age groups & school enroll total
  mutate(
    pop_under_18 = male_u18_1 + male_u18_2 + male_u18_3 + male_u18_4 +
      female_u18_1 + female_u18_2 + female_u18_3 + female_u18_4,
    
    pop_65_plus = male_65_1 + male_65_2 + male_65_3 + male_65_4 +
      male_65_5 + male_65_6 + female_65_1 + female_65_2 + female_65_3 +
      female_65_4 + female_65_5 + female_65_6,
    
    pop_18_84 = total_pop - pop_under_18 - pop_65_plus,
    
    # total school enrollment
    school_enroll_total = school_enroll_male + school_enroll_female
  ) %>%
  
  # derive percentages
  mutate(
    pct_white = ifelse(total_pop > 0, white_pop / total_pop * 100, NA),
    pct_black = ifelse(total_pop > 0, black_pop / total_pop * 100, NA),
    pct_asian = ifelse(total_pop > 0, asian_pop / total_pop * 100, NA),
    pct_hispanic = ifelse(total_pop > 0, hispanic_pop / total_pop * 100, NA),
    
    pct_under_18 = ifelse(total_pop > 0, pop_under_18 / total_pop * 100, NA),
    pct_65_plus = ifelse(total_pop > 0, pop_65_plus / total_pop * 100, NA),
    pct_18_64 = ifelse(total_pop > 0, pop_18_84 / total_pop * 100, NA),
    
    pct_enrolled = ifelse(total_pop > 0, 
                          school_enroll_total / total_pop * 100, NA),
    # clean median income
    med_income = ifelse(med_income <= 0, NA, med_income)
  ) %>%
  
  # keep only useful variables
  select(
    GEOID, year, geometry, total_pop, med_income, pop_under_18, pop_18_84,
    pop_65_plus, school_enroll_total, starts_with("pct")
  )
# ===================================================

# save cleaned data
saveRDS(tracts_clean, "data/processed/tracts_clean_no_geom.rds")

# ===================================================

# attach geometry safely
tracts_clean <-readRDS("data/processed/tracts_clean_no_geom.rds")

# load geometry source
tracts_geom <- readRDS("data/raw/tracts_2024_with_acs.rds") %>%
  select(GEOID, GEOIDFQ, geometry)

# drop geometry form clean, so its not sf during join
tracts_clean_df <- tracts_clean %>%
  st_drop_geometry() 

tracts_clean_joined <- tracts_clean_df %>%
  left_join(
    tracts_geom %>% st_drop_geometry(),
    by = "GEOID"
  )

# attach geometry back properly
tracts_clean <- tracts_clean_joined %>%
  left_join(
    tracts_geom %>% select(GEOID, geometry),
    by = "GEOID"
  ) %>%
  st_as_sf()

# ===================================================

# check
glimpse(tracts_clean)
nrow(tracts_clean)
summary(tracts_clean$med_income)
sum(is.na(tracts_clean))

# check number of tracts per year
tracts_clean %>%
  st_drop_geometry() %>%
  group_by(year) %>%
  summarise(n_tracts = n_distinct(GEOID))
# ===================================================

saveRDS(tracts_clean, "data/processed/tracts_clean.rds")