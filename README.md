# Spatial Demographic Change in Parker County, Texas (2014 - 2024)

A tract-level analysis of income, population growth, and demographic change using American Community Survey (ACS) data.

---

## Overview
This project examines how Parker County has changed between 2014 and 2024.
The analysis focuses on:
- Income growth
- Population expansion
- Demographic shifts
- Spatial patterns across census tracts
The goal is to understand where growth is happening and how it differs across the county.

## Key Visuals
![Population Change Map](outputs/map_pop_2024.png)
![Median Income 2024](outputs/map_income_2024.png)
![Hispanic Population Change](outputs/hisp_change_map.png)

## Methods
- Pulled ACS 5-year data (2014, 2019, 2024) using the API
- Cleaned and standardized tract-level data
- Conducted exploratory data analysis (EDA)
- Created spatial maps and visualizations in R

## Tools
- R / RStudio
- tidyverse
- tidycensus
- sf
- tigris
- ggplot2
- ggspatial

## Outputs
- Full Report (PDF): report/parker_county_analysis_report.pdf
- Maps & visualizations: outputs/
- Trend summary data: outputs/trend_scan.csv

## Project structure:
```
parker_county_analysis/
│
├── data/
│   ├── raw/          # (ignored)
│   └── processed/
│
├── outputs/          # maps + figures
├── scripts/          # analysis scripts (01–04)
├── report/           # RMarkdown + final PDF
└── README.md
```

## Notes:
    - Raw data files are excluded from this repository
    - Some tract boundaries change over time; 2024 geometry is used for consistency
