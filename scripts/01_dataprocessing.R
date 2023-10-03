# SETUP ----

## 1. Load required packages ----
library(here)
library(tidyverse)
library(janitor)
library(sf)

## 2. Set working directory ----
here::i_am("scripts/01_dataprocessing.R")

## 3. Run Util.R ----
source(here::here("scripts", "util.R"))

## 4. Create necessary directories ----
dir.create(here::here("derived_data"), showWarnings = FALSE)

# 01 MERGING DATA ----

## 1. Load in all data ----
hvi_data <- read_csv(here::here("source_data", "hvi_data.csv")) %>% as.data.frame()
househeat <- read_csv(here::here("source_data", "house_heating_final.csv")) %>% as.data.frame()
houseincome <- read_csv(here::here("source_data", "house_income_final.csv")) %>% as.data.frame()
coverage <- read_csv(here::here("source_data", "coverage.csv")) %>% as.data.frame()

## 2. Merge datasets ----

### Function to merge datasets ----
merge_datasets <- function(main_df, df_to_merge) {
  merged_df <- left_join(main_df, df_to_merge, by = "GEOID")
  return(merged_df)
}

coverage$GEOID <- as.character(coverage$GEOID)
hvi_data$GEOID <- as.character(hvi_data$GEOID)
househeat$GEOID <- as.character(househeat$GEOID)
houseincome$GEOID <- as.character(houseincome$GEOID)


merged_df <- merge_datasets(coverage, househeat)
merged_df <- merge_datasets(merged_df, houseincome)
merged_df <- merge_datasets(merged_df, hvi_data)
merged_df <- janitor::clean_names(merged_df)
merged_df <- merged_df %>%
  mutate_all(~ifelse(. == "#DIV/0!" | . == "<Null>" | . == "-", NA, .))

### Remove duplicate columns  ----
merged_df <- merged_df[, !duplicated(colnames(merged_df))]
merged_df <- merged_df %>% select (-c(name, name_y, objectid,shape)) %>% rename(name = name_x)
merged_df <- merged_df %>% select(geoid,state, name, everything()) %>% arrange(state, county, name)


### Rename cols  ----
colnames(merged_df) <- c(
  "geoid", "state", "name", "area", "coverage", 
  "occupied_housing_units", "utility_gas", "utility_gas_pct", 
  "bottled_gas", "bottled_gas_pct", "electricity", "electricity_pct", 
  "fuel_oil", "fuel_oil_pct", "coal", "coal_pct", 
  "other_fuels", "other_fuels_pct", "no_fuel", "no_fuel_pct", "occupied_housing_units_y",
  "median_income", "objectid_y", "county", 
  "low_surfacetemp_summer", "high_surfacetemp_summer", "avg_surfacetemp_summer", "tree_canopy_pct", 
  "lack_tree_canopy_pct", "impervious_surfaces_pct", "population", "population_65_pct", 
  "population_white_pct", "population_minority_pct", "males_under_5", "females_under_5", 
  "youth_population", "population_under_5_pct", "population_below_poverty_pct", 
  "vacant_houses_pct", "education_below_hs_pct", "houses_no_vehicle_pct", 
  "houses_no_internet_pct", "population_disability_pct", "houses_single_occupancy_pct", 
  "limited_english_ability_pct", "dependent_age_groups_pct", 
  "high_temp_summer_standardized", "avg_temp_summer_standardized", "tree_canopy_standardized", 
  "lack_tree_canopy_standardized", "impervious_surfaces_standardized", "population_standardized", 
  "houses_no_internet_standardized", "houses_no_vehicle_standardized", "vacant_houses_standardized", 
  "education_below_hs_standardized", "population_65_standardized", "population_minority_standardized", 
  "population_under_5_standardized", "population_below_poverty_standardized", "population_disability_standardized", 
  "houses_single_occupancy_standardized", "limited_english_ability_standardized", "shape_x", "shape_y",
  "night_air_temp", "afternoon_air_temp", "morning_air_temp"
)

### Drop redundant _x and _y variables ----
drop_vars <- grep("_x$|_y$", colnames(merged_df), value = TRUE)
merged_df <- merged_df[, !(colnames(merged_df) %in% drop_vars)]

### Order columns in a meaningful order ----
cols_order <- c(
  # Identifiers and general info
  "geoid", "state", "name", "area", "county",
  
  # Housing and utilities
  "occupied_housing_units", "vacant_houses_pct", "vacant_houses_standardized", 
  "houses_single_occupancy_pct", "houses_single_occupancy_standardized",
  "utility_gas", "utility_gas_pct", 
  "bottled_gas", "bottled_gas_pct", 
  "electricity", "electricity_pct", 
  "fuel_oil", "fuel_oil_pct", 
  "coal", "coal_pct", 
  "other_fuels", "other_fuels_pct", 
  "no_fuel", "no_fuel_pct",
  
  # Income and poverty
  "median_income", "population_below_poverty_pct", "population_below_poverty_standardized", 
  "education_below_hs_pct", "education_below_hs_standardized",
  
  # Population demographics
  "population", "population_standardized", 
  "population_65_pct", "population_65_standardized", 
  "population_white_pct", "population_minority_pct", "population_minority_standardized",
  "males_under_5", "females_under_5", 
  "youth_population", "population_under_5_pct", "population_under_5_standardized",
  "population_disability_pct", "population_disability_standardized", 
  "limited_english_ability_pct", "limited_english_ability_standardized", 
  "dependent_age_groups_pct",
  
  # Internet, vehicles, and other amenities
  "houses_no_internet_pct", "houses_no_internet_standardized", 
  "houses_no_vehicle_pct", "houses_no_vehicle_standardized",
  
  # Surface and canopy data
  "tree_canopy_pct", "tree_canopy_standardized", 
  "lack_tree_canopy_pct", "lack_tree_canopy_standardized", 
  "impervious_surfaces_pct", "impervious_surfaces_standardized",
  
  # Temperature data
  "low_surfacetemp_summer", "high_surfacetemp_summer", "avg_surfacetemp_summer",
  "high_temp_summer_standardized", "avg_temp_summer_standardized", 
  "night_air_temp", "afternoon_air_temp", "morning_air_temp"
)

merged_df <- merged_df[, cols_order]


### Drop all standardized columns ----
standardized_vars <- grep("_standardized$", colnames(merged_df), value = TRUE)
merged_df <- merged_df[, !(colnames(merged_df) %in% standardized_vars)]


### Save dataframe in derived_data folder ----
rm(hvi_data, househeat, houseincome, coverage)
write_csv(merged_df, here::here("derived_data", "merged_data.csv"))




