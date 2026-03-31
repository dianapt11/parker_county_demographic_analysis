# ===================================================
# 04_maps
# Purpose: Spatial Visualization & Change Mapping
# ===================================================

# load libraries
library(tidyverse) # data wrangling
library(sf) # spatial data handling
library(scales) # to format labels
library(patchwork) # to combine maps side by side
library(tigris) # get Census TIGER/LINE shapefiles
library(ggspatial) # north arrow & scale bar

options(tigris_use_cache = TRUE) # cache TIGER download; speed up repeated runs

# set theme for maps
theme_set(theme_void()) # remove gridlines and axis clutter

# Load clean data
tracts_clean <- readRDS("data/processed/tracts_clean.rds")

# ===================================================

# Create context layers for mapping (County, Highways, Cities)

### Use 2024 TIGER/Line shapefiles for boundary consistency with 2024 ACS tract geometry

## Parker county boundary
parker_county <- counties(
  state = "TX",
  year = 2024,
  class = "sf"
) %>%
  filter(NAME == "Parker")

## Major Roads (TIGER roads layer)
roads_tx <- roads(
  state = "TX",
  county = "Parker",
  year = 2024,
  class = "sf"
)

## Filter major highways
highways_sf <- roads_tx %>%
  filter(grepl("I-|US ", FULLNAME))

## Cities
places_tx <- places(
  state = "TX", 
  year = 2024, 
  class = "sf")

## clip to Parker County
places_sf <- st_intersection(places_tx, parker_county)

## keep only major cities to reduce clutter
places_sf <- places_sf %>%
  filter(NAME %in% 
           c("Weatherford", "Aledo", "Hudson Oaks", "Springtown",
             "Garner", "Dennis", "Millsap"))

# ===================================================
# Create change variables

## drop geometry for change calculations
tracts_wide <- tracts_clean %>%
  st_drop_geometry() %>%
  select(GEOID, year, total_pop, med_income, pct_hispanic) %>%
  pivot_wider(names_from = year, 
              values_from = c(total_pop, med_income, pct_hispanic),
              names_sep = "_" )

## create change variables
tracts_change <- tracts_wide %>%
  mutate(
    pop_change = total_pop_2024 - total_pop_2014,
    income_change = med_income_2024 - med_income_2014, # absolute $ change
    hisp_change = pct_hispanic_2024 - pct_hispanic_2014
  )

## join back to geometry (2024 geometry used as base)
  # NOTE:
  # Change maps use 2024 tract geometry as spatial reference.
  # All years are retained in tracts_clean; no tracts were removed for change calculation.
tracts_map <- tracts_clean %>%
  filter(year == 2024) %>%
  select(GEOID, geometry) %>%
  left_join(tracts_change, by = "GEOID")

# Project all spatial layers to projected CRS to improve centroid accuracy and remove st warnings

target_crs <- 5070  # NAD83 / Conus Albers Equal Area

## check CRS matches tract data from tracts_map
tracts_map <- st_transform(tracts_map, target_crs)
parker_county <- st_transform(parker_county, target_crs)
highways_sf <- st_transform(highways_sf, target_crs)
places_sf <- st_transform(places_sf, target_crs)

## create dots for cities on map; point geom now in meters instead of lat/lon
places_centroids <- st_centroid(st_geometry((places_sf)))

## create nudge columns in data so they are placed nicely on maps
places_sf <- places_sf %>%
  mutate(
    nudge_x_val = case_when(
      NAME == "Weatherford" ~ -8000,
      NAME == "Dennis" ~ 200,
      NAME == "Millsap" ~ 200,
      NAME == "Garner" ~ 200,
      NAME == "Springtown" ~ 200,
      NAME == "Hudson Oaks" ~ 900,
      NAME == "Aledo" ~ 200,
      TRUE ~ 0
    ),
    nudge_y_val = case_when(
      NAME == "Weatherford" ~ -5050,
      NAME == "Dennis" ~ 2000,
      NAME == "Millsap" ~ 2000,
      NAME == "Garner" ~ 2000,
      NAME == "Springtown" ~ 2000,
      NAME == "Hudson Oaks" ~ 2000,
      NAME == "Aledo" ~ 2000,
      TRUE ~ 0
    ) )

# ===================================================
# Create Maps

## Income Change Map

income_change_map <- ggplot() +
  geom_sf(data = tracts_map, aes(fill = income_change),
          color = "white", size = 0.2) +
  geom_sf(data = parker_county, fill = NA, color = "black", size = 1) +
  geom_sf(data = highways_sf, color = "gray35", size = 0.5) +
  geom_sf_text(data = places_sf, aes(label = NAME), 
               size = 3, fontface = "bold", 
               nudge_x = places_sf$nudge_x_val,
               nudge_y = places_sf$nudge_y_val,
               check_overlap = TRUE) +
  geom_sf(data = places_centroids, 
          color = "black", size = 2.5, shape = 21, 
          fill = "white", stroke = 0.5 ) +
  annotation_scale(location = "bl", width_hint = 0.3) +
  annotation_north_arrow(
    location = "tl", which_north = "true",
    style = north_arrow_fancy_orienteering()
  ) +
  scale_fill_gradient2(
    low = "#3B6FB6", mid = "white", high = "#B23A48",
    midpoint = 0, na.value = "lightgray",
    labels = scales:: dollar_format(), 
    name = "Income Change (USD)\n(0 = No Change)") +
  coord_sf(datum = NA) +
  labs(
    title = "Median Household Income Change",
    subtitle = "Parker County, Texas Census Tracts (2014-2024)",
    fill = "Income Change\n(USD)",
    caption = "Note: Gray areas indicate where income could not be calculated 
    because the tract did not exist in 2014/2019.
    Source: American Community Survey (ACS 5-Year Estimates)"
  ) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, size = 11),
    plot.caption = element_text(hjust = 0.5, size = 10, face = "italic")
  )
income_change_map
# ===================================================

## Side by Side Median Income Maps (2014 vs. 2019 vs. 2024)

### filter by year
income_2014 <- tracts_clean %>% filter(year == 2014)
income_2019 <- tracts_clean %>% filter(year == 2019)
income_2024 <- tracts_clean %>% filter(year == 2024)

### ensure CRS match
income_2014 <- st_transform(income_2014, st_crs(tracts_map))
income_2019 <- st_transform(income_2019, st_crs(tracts_map))
income_2024 <- st_transform(income_2024, st_crs(tracts_map))

### find max income values across both years for consistent scale
max_income <- max(c(income_2014$med_income, income_2019$med_income,
                    income_2024$med_income), 
                  na.rm = TRUE)

# 2014 map
map_income_2014 <- ggplot() +
  geom_sf(data = income_2014, aes(fill = med_income), color = "white", 
          size = 0.2) +
  geom_sf(data = parker_county, fill = NA, color = "black", size = 1) +
  geom_sf(data = highways_sf, color = "gray35", size = 0.5) +
  geom_sf_text(data = places_sf, aes(label = NAME), 
             size = 3, fontface = "bold", 
             nudge_x = places_sf$nudge_x_val,
             nudge_y = places_sf$nudge_y_val,
             check_overlap = TRUE) +
  geom_sf(data = places_centroids, 
          color = "black", size = 2.5, shape = 21, 
          fill = "white", stroke = 0.5 ) +
  annotation_scale(location = "bl", width_hint = 0.3) +
  annotation_north_arrow(
    location = "tl", which_north = "true",
    style = north_arrow_fancy_orienteering()
  ) +
  scale_fill_viridis_c(limits = c(0, max_income), na.value = "gray",
                       labels = scales::dollar_format()) +
  labs(
    title = "Parker County Median Income by Tract (2014)", fill = "Income",
    caption = "Note: White areas represent tracts created after 2014 under 2024
    tract boundaries and therefore have no income estimates for that year. 
    Source: American Community Survey (ACS) 5-Year Estimates"
  )  +
  theme(
    plot.caption = element_text(hjust = 0.5, size = 10, face = "italic"),
    plot.title = element_text(hjust = 0.6, size = 12, face = "bold")
  ) 
map_income_2014

# 2019 map
map_income_2019 <- ggplot() +
  geom_sf(data = income_2019, aes(fill = med_income), color = "black", 
          size = 0.2) +
  geom_sf(data = parker_county, fill = NA, color = "black", size = 1) +
  geom_sf(data = highways_sf, color = "gray35", size = 0.5) +
  geom_sf_text(data = places_sf, aes(label = NAME), 
               size = 3, fontface = "bold", 
               nudge_x = places_sf$nudge_x_val,
               nudge_y = places_sf$nudge_y_val,
               check_overlap = TRUE) +
  geom_sf(data = places_centroids, 
          color = "black", size = 2.5, shape = 21, 
          fill = "white", stroke = 0.5 ) +
  annotation_scale(location = "bl", width_hint = 0.3) +
  annotation_north_arrow(
    location = "tl", which_north = "true",
    style = north_arrow_fancy_orienteering()
  ) +
  scale_fill_viridis_c(limits = c(0, max_income), na.value = "gray",
                       labels = scales::dollar_format()) +
  labs(
    title = "Parker County Median Income by Tract (2019)", fill = "Income",
    caption = "Note: White areas represent tracts created after 2019 under 2024
    tract boundaries and therefore have no income estimates for that year. 
    Source: American Community Survey (ACS) 5-Year Estimates"
  )  +
  theme(
    plot.caption = element_text(hjust = 0.5, size = 10, face = "italic"),
    plot.title = element_text(hjust = 0.6, size = 12, face = "bold")
  )
map_income_2019

# 2024 map
map_income_2024 <- ggplot() +
  geom_sf(data = income_2024, aes(fill = med_income), color = "white", 
          size = 0.2) +
  geom_sf(data = parker_county, fill = NA, color = "black", size = 1) +
  geom_sf(data = highways_sf, color = "gray35", size = 0.5) +
  geom_sf_text(data = places_sf, aes(label = NAME), 
               size = 3, fontface = "bold", 
               nudge_x = places_sf$nudge_x_val,
               nudge_y = places_sf$nudge_y_val,
               check_overlap = TRUE) +
  geom_sf(data = places_centroids, 
          color = "black", size = 2.5, shape = 21, 
          fill = "white", stroke = 0.5 ) +
  annotation_scale(location = "bl", width_hint = 0.3) +
  annotation_north_arrow(
    location = "tl", which_north = "true",
    style = north_arrow_fancy_orienteering()
  ) +
  scale_fill_viridis_c(limits = c(0, max_income), na.value = "gray",
                       labels = scales::dollar_format()) +
  labs(
    title = "Parker County Median Income by Tract (2024)", fill = "Income",
    caption = "Source: American Community Survey (ACS) 5-Year Estimates"
  )  +
  theme(
    plot.caption = element_text(hjust = 0.5, size = 10, face = "italic"),
    plot.title = element_text(hjust = 0.6, size = 12, face = "bold")
  )
map_income_2024

### combine side by side
#map_income_2014 <- map_income_2014 +labs(caption = NULL)
#map_income_2019 <- map_income_2019 +labs(caption = NULL)
#map_income_2024 <- map_income_2024 +labs(caption = NULL)

#income_side_by_side <- map_income_2014 + 
  #map_income_2019 + map_income_2024 +
  #plot_annotation(
    #title = "Parker County Median Income Change by Tract",
    #caption = "Note: White areas indicate census tracts created after 2019 
    #under 2024 tract boundaries. 
    #Source: American Community Survey (ACS) 5-Year Estimates",
    #theme = theme(
      #plot.caption = element_text(hjust = 0.5, size = 10, face = "italic"),
      #plot.title = element_text(hjust = 0.5, size = 12, face = "bold")
    #)
  #)
#income_side_by_side

# ===================================================

## Population Change Map

# filter by year
pop_2014 <- tracts_clean %>% filter(year == 2014)
pop_2019 <- tracts_clean %>% filter(year == 2019)
pop_2024 <- tracts_clean %>% filter(year == 2024)

# ensure CRS matches geometry
pop_2014 <- st_transform(pop_2014, st_crs(tracts_map))
pop_2019 <- st_transform(pop_2019, st_crs(tracts_map))
pop_2024 <- st_transform(pop_2024, st_crs(tracts_map))

# find max population across years for consistent fill scale
max_pop <- max(c(pop_2014$total_pop, pop_2019$total_pop, pop_2024$total_pop),
               na.rm = TRUE)

# 2014 population map
map_pop_2014 <- ggplot() +
  geom_sf(data = pop_2014, aes(fill = total_pop),
          color = "white", size = 0.2) +
  geom_sf(data = parker_county, fill = NA, color = "black", size = 1) +
  geom_sf(data = highways_sf, color = "gray35", size = 0.5) +
  geom_sf_text(data = places_sf, aes(label = NAME), 
               size = 3, fontface = "bold", 
               nudge_x = places_sf$nudge_x_val,
               nudge_y = places_sf$nudge_y_val,
               check_overlap = TRUE) +
  geom_sf(data = places_centroids, 
          color = "black", size = 2.5, shape = 21, 
          fill = "white", stroke = 0.5 ) +
  annotation_scale(location = "bl", width_hint = 0.3) +
  annotation_north_arrow(
    location = "tl", which_north = "true",
    style = north_arrow_fancy_orienteering()
  ) +
  scale_fill_viridis_c(limits = c(0, max_pop), na.value = "gray",
                       labels = scales::comma_format(),
                       option = "plasma") +
  labs(
    title = "Total Population by Tract", fill = "Population Count",
    subtitle = "Parker County 2014",
    caption = "Note: White areas represent tracts created after 2019
    under 2024 tract boundaries and therefore have no population
    estimates for that year. 
    Source: American Community Survey (ACS) 5-Year Estimates"
  )  +
  theme(
    plot.caption = element_text(hjust = 0.5, 
                                size = 10, face = "italic"),
    plot.title = element_text(hjust = 0.5, size = 12, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, size = 10, face = "bold")
  )
map_pop_2014

# 2019 population map
map_pop_2019 <- ggplot() +
  geom_sf(data = pop_2019, aes(fill = total_pop),
          color = "white", size = 0.2) +
  geom_sf(data = parker_county, fill = NA, color = "black", size = 1) +
  geom_sf(data = highways_sf, color = "gray35", size = 0.5) +
  geom_sf_text(data = places_sf, aes(label = NAME), 
               size = 3, fontface = "bold", 
               nudge_x = places_sf$nudge_x_val,
               nudge_y = places_sf$nudge_y_val,
               check_overlap = TRUE) +
  geom_sf(data = places_centroids, 
          color = "black", size = 2.5, shape = 21, 
          fill = "white", stroke = 0.5 ) +
  annotation_scale(location = "bl", width_hint = 0.3) +
  annotation_north_arrow(
    location = "tl", which_north = "true",
    style = north_arrow_fancy_orienteering()
  ) +
  scale_fill_viridis_c(limits = c(0, max_pop), na.value = "gray",
                       labels = scales::comma_format(),
                       option = "plasma") +
  labs(
    title = "Total Population by Tract", fill = "Population Count",
    subtitle = "Parker County 2019",
    caption = "Note: White areas represent tracts created after 2019
    under 2024 tract boundaries and therefore have no population
    estimates for that year. 
    Source: American Community Survey (ACS) 5-Year Estimates"
  )  +
  theme(
    plot.caption = element_text(hjust = 0.5, 
                                size = 10, face = "italic"),
    plot.title = element_text(hjust = 0.5, size = 12, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, size = 10, face = "bold")
  )
map_pop_2019

# 2024 population map
map_pop_2024 <- ggplot() +
  geom_sf(data = pop_2024, aes(fill = total_pop),
          color = "white", size = 0.2) +
  geom_sf(data = parker_county, fill = NA, color = "black", size = 1) +
  geom_sf(data = highways_sf, color = "gray35", size = 0.5) +
  geom_sf_text(data = places_sf, aes(label = NAME), 
               size = 3, fontface = "bold", 
               nudge_x = places_sf$nudge_x_val,
               nudge_y = places_sf$nudge_y_val,
               check_overlap = TRUE) +
  geom_sf(data = places_centroids, 
          color = "black", size = 2.5, shape = 21, 
          fill = "white", stroke = 0.5 ) +
  annotation_scale(location = "bl", width_hint = 0.3) +
  annotation_north_arrow(
    location = "tl", which_north = "true",
    style = north_arrow_fancy_orienteering()
  ) +
  scale_fill_viridis_c(limits = c(0, max_pop), na.value = "gray",
                       labels = scales::comma_format(),
                       option = "plasma") +
  labs(
    title = "Total Population by Tract", fill = "Population Count",
    subtitle = "Parker County 2024",
    caption = "Note: White areas represent tracts created after 2019
    under 2024 tract boundaries and therefore have no population
    estimates for that year. 
    Source: American Community Survey (ACS) 5-Year Estimates"
  )  +
  theme(
    plot.caption = element_text(hjust = 0.5, 
                                size = 10, face = "italic"),
    plot.title = element_text(hjust = 0.5, size = 12, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, size = 10, face = "bold")
  )
map_pop_2024

# side by side population
#map_pop_2014 <- map_pop_2014 +labs(caption = NULL)
#map_pop_2019 <- map_pop_2019 +labs(caption = NULL)
#map_pop_2024 <- map_pop_2024 +labs(caption = NULL)

#pop_side_by_side <- map_pop_2014 + map_pop_2019 + map_pop_2024 +
  #plot_annotation(
    #title = "Total Population by Tract",
    #subtitle = "Parker County, Texas",
    #caption = "NOTE: White areas indicate census tracts created after 2019 under 2024 tract boundaries. Source: American Community Survey (ACS) 5-Year Estimates",
    #theme = theme(
      #plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
      #plot.subtitle = element_text(hjust = 0.5, size = 12, 
                                   #face = "bold"),
      #plot.caption = element_text(hjust = 0.5, size = 9, 
                                  #face = "italic")
    #)
  #)
#pop_side_by_side

# ===================================================

## Hispanic Population % Change Map
hisp_change_map <- ggplot() +
  geom_sf(data = tracts_map, aes(fill = hisp_change), color = "white",
          size = 0.2) +
  geom_sf(data = parker_county, fill = NA, color = "black", size = 1) +
  geom_sf(data = highways_sf, color = "gray35", size = 0.5) +
  geom_sf_text(data = places_sf, aes(label = NAME), 
               size = 3, fontface = "bold", 
               nudge_x = places_sf$nudge_x_val,
               nudge_y = places_sf$nudge_y_val,
               check_overlap = TRUE) +
  geom_sf(data = places_centroids, 
          color = "black", size = 2.5, shape = 21, 
          fill = "white", stroke = 0.5 ) +
  annotation_scale(location = "bl", width_hint = 0.3) +
  annotation_north_arrow(
    location = "tl", which_north = "true",
    style = north_arrow_fancy_orienteering()
  ) +
  scale_fill_gradient2(low = "#3B6FB6", mid = "white", high = "#B23A48",
                       midpoint = 0, na.value = "gray",
                       labels = scales::percent_format(scale = 1)) +
  labs(title = "Change in Hispanic Population Share (2014-2024)",
       fill = "Percentage Point Change",
       caption = "Note: Gray areas indicate missing or suppressed data, often 
       due to census tract boundary changes between 2014 and 2024 or 
       insufficient data for reliable estimates.
    Source: American Community Survey (ACS 5-Year Estimates)") +
  theme(
  plot.title = element_text(hjust = 0.5, size = 12, face = "bold"),
  plot.caption = element_text(hjust = 0.5, size = 9, 
                              face = "italic"))
hisp_change_map

# ===================================================

## Income Change Distribution Histogram
income_change_plot <- tracts_map %>%
  st_drop_geometry() %>%
  ggplot(aes(x = income_change)) +
  geom_histogram(fill = "blue", color = "white", bins = 6) +
  scale_x_continuous(labels = scales::dollar_format()) +
  labs(title = "Distribution of Income Change by Tract (2014-2024)",
    x = "Income Change (USD)",
    y = "Number of Tracts") +
  theme_minimal(base_size = 12)
income_change_plot

# ===================================================

# Save map outputs

map_list <- list(
  income_change_map = income_change_map,
  map_pop_2014 = map_pop_2014,
  map_pop_2019 = map_pop_2019,
  map_pop_2024 = map_pop_2024,
  map_income_2014 = map_income_2014,
  map_income_2019 = map_income_2019,
  map_income_2024 = map_income_2024,
  hisp_change_map = hisp_change_map,
  income_change_plot = income_change_plot
)

for(name in names(map_list)) {
  ggsave(
    filename = paste0("outputs/", name, ".png"),
    plot = map_list[[name]],
    width = 8,
    height = 6,
    dpi = 300
  )
}

# One huge portfolio layout to view all visuals created
portfolio_layout <- 
  (map_pop_2014 + map_pop_2019 + map_pop_2024) / # row 1
  (map_income_2014 + map_income_2019 + map_income_2024) / # row 2
  (income_change_map + hisp_change_map) / # row 3
  income_change_plot + # row 4
  plot_layout(heights = c(2, 2, 2, 1))

ggsave("outputs/portfolio_layout.png", plot = portfolio_layout,
       width = 14, height = 12, dpi = 300)