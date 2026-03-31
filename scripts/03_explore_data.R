# ===================================================
# 03_explore_data
# Purpose: Explore Parker County tract-level demographic data
# ===================================================

# load libraries & data
library(sf)
library(dplyr)
library(ggplot2)
library(tidyr)

tracts_clean <- readRDS("data/processed/tracts_clean.rds")

glimpse(tracts_clean)

# ===================================================
# Structure checks

## confirm number of tracts per year (tract year may change over time)
tracts_clean %>%
  st_drop_geometry() %>%
  count(year)

## check for duplicate GEOIDs within year
tracts_clean %>%
  st_drop_geometry() %>%
  count(year, GEOIDFQ) %>%
  filter(n > 1)

## check missing values
na_summary <- tracts_clean %>%
  st_drop_geometry() %>%
  summarise(across(everything(), ~sum(is.na(.)))) %>%
  pivot_longer(
    cols = everything(), 
    names_to = "variable", 
    values_to = "num_missing")
print(na_summary)

### note: Some GEOIDFQ values are NA after the join. We are keeping the dataset as-is because we have GEOID and geometry and do not plan to merge external Census tables. All mapping and tract-level analysis will use GEOID.

# ===================================================

# summary statistics // How much do tracts differ?
summary_stats <- tracts_clean %>%
  st_drop_geometry() %>%
  select(total_pop, med_income, pop_under_18, pop_18_84, pop_65_plus,
         school_enroll_total, starts_with("pct")) %>%
  summarise(across(everything(), list(
    mean = ~ mean(., na.rm = TRUE),
    sd = ~ sd(., na.rm = TRUE),
    min = ~ min(., na.rm = TRUE),
    max = ~ max(., na.rm = TRUE)
  )))
print(summary_stats)

### notes:
# 1. Population: tracts vary a lot in size
# 2. Median Income: large spread indicates big differences in wealth in tracts
# 3. Age Comp: mix family sizes and an elderly population
# 4. School Enroll: tracks roughly with total population and younger cohorts
# 5. Racial Comp: Parker County is larger white with a smaller minority population
# 6. Age Pct: confirms a working-age majority with a modest elderly share
# 7. School Enroll Pct: indicates enrollment rates vary

# ===================================================

# Temporal trend scan
## calculate average values of important demographic variables across years
## will help identify which variables are increasing, decreasing, or stable over time

trend_scan <- tracts_clean %>%
  st_drop_geometry() %>%
  group_by(year) %>%
  summarise(
    total_pop_avg = mean(total_pop, na.rm = TRUE),
    med_income_avg = mean(med_income, na.rm = TRUE),
    
    pct_under_18_avg = mean(pct_under_18, na.rm = TRUE),
    pct_18_64_avg = mean(pct_18_64, na.rm = TRUE),
    pct_65_plus_avg = mean(pct_65_plus, na.rm = TRUE),
    
    pct_white_avg = mean(pct_white, na.rm = TRUE),
    pct_black_avg = mean(pct_black, na.rm = TRUE),
    pct_asian_avg = mean(pct_asian, na.rm = TRUE),
    pct_hispanic_avg = mean(pct_hispanic, na.rm = TRUE),
    
    pct_enrolled_avg = mean(pct_enrolled, na.rm = TRUE),
    .groups = "drop"
    )
print(trend_scan)

# save results as a csv file
write.csv(trend_scan, "outputs/trend_scan.csv", row.names = FALSE)

### note: tract level avgs.; outliers exist; 2024 has more tracts
# - population rises from 2014-2019, but then drops in 2024,this is likely due to new tracts
# - median income looks to have a steady increase over time
# - age pct under 18 & 18-64 have slight decrease while % 65+ increase
# - race % white decrease, black stable, asian & hispanic slight increase
# - school enroll stable %

# ===================================================

# Explore trends over time via line plot
## convert to long format for plotting
trend_long <- trend_scan %>%
  pivot_longer(cols = -year, names_to = "variable", values_to = "value")

temp_trend_plot <- ggplot(trend_long, aes(x = year, y = value)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  facet_wrap(~ variable, scales = "free_y") +
  theme_minimal() +
  labs(title = "Temporal Trends in Parker County Demographics",
       x = "year",
       y = "average value")
temp_trend_plot
# ===================================================

# Explore total population by year
county_pop <- tracts_clean %>%
  st_drop_geometry() %>%
  group_by(year) %>%
  summarise(total_population = sum(total_pop, na.rm = TRUE))

total_pop_plot <- ggplot(
  county_pop, aes(x = year, y = total_population)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  theme_minimal() +
  labs(
    title = "Total Population Growth in Parker County (2014–2024)",
    x = "Year",
    y = "Total Population"
  )
total_pop_plot

# ===================================================

# Explore racial composition over time
race_long <- tracts_clean %>%
  st_drop_geometry() %>%
  pivot_longer(
    cols = c(pct_white, pct_black, pct_asian, pct_hispanic),
    names_to = "race",
    values_to = "percent"
  )
# clean up race labels for nicer facets
race_long <- race_long %>%
  mutate(race = recode(race,
                       pct_white = "White", pct_black = "Black",
                       pct_asian = "Asian", pct_hispanic = "Hispanic"))
# boxplot
race_box_plot <- ggplot(race_long, aes(x = factor(year), y = percent)) +
  geom_boxplot(fill = "blue", alpha = 0.7) +
  facet_wrap(~ race, scales = "free_y") +
  theme_minimal() +
  labs(
    title = "Racial Composition by Tract Over Time in Parker County",
    x = "Year",
    y = "% Population"
  )
race_box_plot

### note:
# - The White population remains the majority across all tracts and years, with medians  generally above 80%, but there is a visible decrease in 2024, suggesting growing racial diversity in some tracts.

# - The Hispanic population shows a consistent increase in median percentage over time, rising from roughly 10% in 2014 to around 15-18% in 2024. The interquartile range also widens, indicating increased variation across tracts.

# - The Black population remains relatively low overall with median values around 1-1.5%, but there are outlier tracts where the percentage is noticeably higher.

# - The Asian population is the smallest group in terms of median percentage but shows slight growth in some tracts by 2024.

# ===================================================

# Explore distribution of median income over time by tract
med_income_plot <-ggplot(
  tracts_clean, aes(x = factor(year), y = med_income)) +
  geom_boxplot(fill = "forestgreen", alpha = 0.7) +
  geom_jitter(width = 0.1, color = "orange", size = 1.5, alpha = 0.7) +
  theme_minimal() +
  labs(
    title = "Distribution of Median Income by Tract Over Time",
    x = "Year",
    y = "Median Income ($)"
  )
med_income_plot

### note:
#### Jitter shows individual tract values to highlight variability and outliers without overcrowding
#### Result insights:
## Median income appears to increase overall from 2014 to 2024, with wider spread and some high-income outliers emerging in 2024. This suggests growing income disparities between tracts over time.

# ===================================================

# Explore age composition over time

# reshape age percentages for boxplots 
age_long <- tracts_clean %>%
  st_drop_geometry() %>%
  select(year, GEOID, pct_under_18, pct_18_64, pct_65_plus) %>%
  pivot_longer(
    cols = -c(year, GEOID),
    names_to = "age_group",
    values_to = "percent") %>%
  mutate(age_group = recode(age_group,
                            pct_under_18 = "Under 18",
                            pct_18_64 = "18-64",
                            pct_65_plus = "65+"))
# box plot by age group
age_box_plot <- ggplot(age_long, aes(x = factor(year), y = percent)) +
  geom_boxplot(fill = "lightblue", alpha = 0.7) +
  facet_wrap(~ age_group, scales = "free_y") +
  theme_minimal() +
  labs(
    title = "Age Composition by Tract Over Time",
    x = "Year",
    y = "% of Population"
  )
age_box_plot

# line plot for countywide average for each age group
age_trend <- tracts_clean %>%
  st_drop_geometry() %>%
  group_by(year) %>%
  summarise(
    pct_under_18_avg = mean(pct_under_18, na.rm = TRUE),
    pct_18_64_avg = mean(pct_18_64, na.rm = TRUE),
    pct_65_plus_avg = mean(pct_65_plus, na.rm = TRUE),
    .groups = "drop"
  ) %>%
 pivot_longer(cols = -year, names_to = "age_group", values_to = "percent") %>%
  mutate(age_group = recode(age_group,
                            pct_under_18_avg = "Under 18",
                            pct_18_64_avg = "18-64",
                            pct_65_plus_avg = "65+"))

age_line_plot <- ggplot(
  age_trend, aes(x = year, y = percent, color = age_group)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  theme_minimal() +
  labs(
    title = "Countywide Average Age Composition Over Time",
    x = "Year",
    y = "% of Population",
    color = "Age Group")
age_line_plot

### note:
# Boxplot: 
# - shows distribution of age group pcts within tracts for each year
# - highlights variability and shifts in age composition across tracts
# Lineplot:
# - shows countywide average pcts of each group over time
# Results:
## - 18-64 group remains the largest but shows slight fluctuations
## - 65+ group shows a gradual increase, indicating an aging population
## - under 18 group shows a slight decrease or stabilization

# ===================================================

# Save important plots

plot_list <- list(
  temp_trend_plot = temp_trend_plot,
  total_pop_plot = total_pop_plot,
  race_box_plot = race_box_plot,
  med_income_plot = med_income_plot,
  age_box_plot = age_box_plot,
  age_line_plot = age_line_plot
)

for(name in names(plot_list)) {
  ggsave(
    filename = paste0("outputs/", name, ".png"),
    plot = plot_list[[name]],
    width = 8,
    height = 6,
    dpi = 300
  )
}


