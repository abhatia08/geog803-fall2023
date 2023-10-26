# SETUP ----

## 1. Load required packages ----
library(here)
library(tidyverse)
library(janitor)
library(sf)
library(magrittr)

## 2. Set working directory ----
here::i_am("scripts/01_dataprocessing.R")

## 3. Run Util.R ----
source(here::here("scripts", "util.R"))

## 4. Create necessary directories ----
dir.create(here::here("derived_data"), showWarnings = FALSE)

# 01 MERGING DATA ----

## 1. Load in all data ----
hvi_data <- read.csv(here::here("source_data", "hvi_data.csv")) %>% as.data.frame()
househeat <- read.csv(here::here("source_data", "house_heating_final.csv")) %>% as.data.frame()
houseincome <- read.csv(here::here("source_data", "house_income_final.csv")) %>% as.data.frame()
coverage <- read.csv(here::here("source_data", "coverage.csv")) %>% as.data.frame()


## 2. Merge datasets ----

### Function to merge datasets 
merge_datasets <- function(main_df, df_to_merge) {
  merged_df <- dplyr::left_join(main_df, df_to_merge, by = "GEOID")
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
  dplyr::mutate_all(~ifelse(. == "#DIV/0!" | . == "<Null>" | . == "-", NA, .))

#### Remove duplicate columns  
merged_df <- merged_df[, !duplicated(colnames(merged_df))]
merged_df <- merged_df %>% dplyr::select (-c(name, name_y, objectid,shape)) %>% dplyr::rename(name = name_x)
merged_df <- merged_df %>% dplyr::select(geoid,state, name, everything()) %>% dplyr::arrange(state, county, name)


#### Rename cols  
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

#### Drop redundant _x and _y variables 
drop_vars <- grep("_x$|_y$", colnames(merged_df), value = TRUE)
merged_df <- merged_df[, !(colnames(merged_df) %in% drop_vars)]

#### Order columns in a meaningful order 
cols_order <- c(
  # Identifiers and general info
  "geoid", "state", "name", "area", "county", "coverage",
  
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

### Formatting columns 
pct_cols <- grep("_pct$", colnames(merged_df), value = TRUE)
merged_df[pct_cols] <- merged_df[pct_cols] %>% 
  dplyr::mutate_all(~as.numeric(gsub("[^0-9.]", "", .))) %>%
  dplyr::mutate_all(~ifelse(!is.na(.) & max(., na.rm = TRUE) <= 1, . * 100, .)) %>% 
  dplyr::mutate_all(~round(., 1)) 
merged_df <- merged_df %>% 
  dplyr::mutate(
    median_income = dplyr::case_when(
      median_income == "250000+" ~ "250,000 and over",
      as.numeric(median_income) < 15000 ~ "Under 15,000",
      as.numeric(median_income) >= 15000 & as.numeric(median_income) <= 74999 ~ "15,000 to 74,999",
      as.numeric(median_income) >= 75000 & as.numeric(median_income) <= 149999 ~ "75,000 to 149,999",
      as.numeric(median_income) >= 150000 & as.numeric(median_income) <= 249999 ~ "150,000 to 249,999",
      TRUE ~ as.character(median_income) 
    )
  )


#### Drop all standardized columns 
standardized_vars <-
  grep("_standardized$", colnames(merged_df), value = TRUE)
merged_df <-
  merged_df[,!(colnames(merged_df) %in% standardized_vars)]

### Drop non-pct columns
merged_df <-
  merged_df %>% dplyr::select(
    -c(
      occupied_housing_units,
      utility_gas,
      bottled_gas,
      electricity,
      fuel_oil,
      coal,
      other_fuels,
      no_fuel
    )
  )

### Drop missing cols
merged_df <-
  merged_df %>% dplyr::filter(!is.na(population_white_pct)) %>% dplyr::filter(coverage > 50)

#### Add column for City Name 
merged_df$city <- NA

for (x in 1:5502){
     if (merged_df$county[x] == "Los Angeles County"){
       merged_df$city[x] <- as.character("Los Angeles, CA")
     } 
  if (merged_df$county[x] == "San Diego County"){
    merged_df$city[x] <- as.character("San Diego, CA")
  }
  if (merged_df$county[x] == "San Francisco County"){
    merged_df$city[x] <- as.character("San Francisco, CA")
  }
  if (merged_df$county[x] == "Boulder County"){
    merged_df$city[x] <- as.character("Boulder, CO")
  }
  if (merged_df$county[x] == "Broward County"){
    merged_df$city[x] <- as.character("Fort Lauderdale, FL")
  }
  if (merged_df$county[x] == "Duval County"){
    merged_df$city[x] <- as.character("Jacksonville, FL")
  }
  if (merged_df$county[x] == "Palm Beach County"){
    merged_df$city[x] <- as.character("Palm Beach, FL")
  }
  if (merged_df$county[x] == "DeKalb County"){
    merged_df$city[x] <- as.character("Atlanta, GA")
  }
  if (merged_df$county[x] == "Fulton County"){
    merged_df$city[x] <- as.character("Atlanta, GA")
  }
  if (merged_df$county[x] == "Ada County"){
    merged_df$city[x] <- as.character("Boise, ID")
  }
  if (merged_df$county[x] == "Canyon County"){
    merged_df$city[x] <- as.character("Boise, ID")
  }
  if (merged_df$county[x] == "Clark County"){
    merged_df$city[x] <- as.character("Louisville, KY")
  }
  if (merged_df$county[x] == "Wayne County"){
    merged_df$city[x] <- as.character("Richmond, IN")
  }
  if (merged_df$county[x] == "Anne Arundel County"){
    merged_df$city[x] <- as.character("Annapolis, MD")
  }
  if (merged_df$county[x] == "Baltimore County"){
    merged_df$city[x] <- as.character("Baltimore, MD")
  }
  if (merged_df$county[x] == "Baltimore city"){
    merged_df$city[x] <- as.character("Baltimore, MD")
  }
  if (merged_df$county[x] == "Montgomery County"){
    merged_df$city[x] <- as.character("Rockville, MD")
  }
  if (merged_df$county[x] == "Middlesex County"){
    merged_df$city[x] <- as.character("Boston, MA")
  }
  if (merged_df$county[x] == "Norfolk County"){
    merged_df$city[x] <- as.character("Boston, MA")
  }
  if (merged_df$county[x] == "Suffolk County"){
    merged_df$city[x] <- as.character("Boston, MA")
  }
  if (merged_df$county[x] == "Worcester County"){
    merged_df$city[x] <- as.character("Boston, MA")
  }
  if (merged_df$county[x] == "Jackson County"){
    merged_df$city[x] <- as.character("Kansas City, MO")
  }
  if (merged_df$state[x] == "Nevada"){
    merged_df$city[x] <- as.character("Las Vegas, NV")
  }
  if (merged_df$county[x] == "Essex County"){
    merged_df$city[x] <- as.character("New York City, NY")
  }
  if (merged_df$county[x] == "Hudson County"){
    merged_df$city[x] <- as.character("New York City, NY")
  }
  if (merged_df$county[x] == "Union County"){
    merged_df$city[x] <- as.character("New York City, NY")
  }
  if (merged_df$county[x] == "Bronx County"){
    merged_df$city[x] <- as.character("New York City, NY")
  }
  if (merged_df$county[x] == "Doña Ana County"){
    merged_df$city[x] <- as.character("El Paso, TX")
  }
  if (merged_df$county[x] == "New York County"){
    merged_df$city[x] <- as.character("New York City, NY")
  }
  if (merged_df$county[x] == "Westchester County"){
    merged_df$city[x] <- as.character("New York City, NY")
  }
  if (merged_df$county[x] == "Durham County"){
    merged_df$city[x] <- as.character("Durham, NC")
  }
  if (merged_df$county[x] == "Wake County"){
    merged_df$city[x] <- as.character("Raleigh, NC")
  }
  if (merged_df$county[x] == "Franklin County"){
    merged_df$city[x] <- as.character("Columbus, OH")
  }
  if (merged_df$county[x] == "Delaware County"){
    merged_df$city[x] <- as.character("Columbus, OH")
  }
  if (merged_df$county[x] == "Fairfield County"){
    merged_df$city[x] <- as.character("Columbus, OH")
  }
  if (merged_df$county[x] == "Hamilton County"){
    merged_df$city[x] <- as.character("Cinncinati, OH")
  }
  if (merged_df$county[x] == "Multnomah County"){
    merged_df$city[x] <- as.character("Portland, OR")
  }
  if (merged_df$county[x] == "Charleston County"){
    merged_df$city[x] <- as.character("Charleston, SC")
  }
  if (merged_df$county[x] == "Lexington County"){
    merged_df$city[x] <- as.character("Columbia, SC")
  }
  if (merged_df$county[x] == "Richland County"){
    merged_df$city[x] <- as.character("Columbia, SC")
  }
  if (merged_df$county[x] == "Davidson County"){
    merged_df$city[x] <- as.character("Nashville, TN")
  }
  if (merged_df$county[x] == "Knox County"){
    merged_df$city[x] <- as.character("Knoxville, TN")
  }
  if (merged_df$county[x] == "Arlington County"){
    merged_df$city[x] <- as.character("Arlington, VA")
  }
  if (merged_df$county[x] == "Charlottesville city"){
    merged_df$city[x] <- as.character("Charlottesville, VA")
  }
  if (merged_df$county[x] == "Albemarle County"){
    merged_df$city[x] <- as.character("Charlottesville, VA")
  }
  if (merged_df$county[x] == "Harrisonburg city"){
    merged_df$city[x] <- as.character("Harrisonburg, VA")
  }
  if (merged_df$county[x] == "Henrico County"){
    merged_df$city[x] <- as.character("Richmond, VA")
  }
  if (merged_df$county[x] == "Lynchburg city"){
    merged_df$city[x] <- as.character("Lynchburg, VA")
  }
  if (merged_df$county[x] == "Petersburg city"){
    merged_df$city[x] <- as.character("Richmond, VA")
  }
  if (merged_df$county[x] == "Richmond city"){
    merged_df$city[x] <- as.character("Richmond, VA")
  }
  if (merged_df$county[x] == "Prince Edward County"){
    merged_df$city[x] <- as.character("Richmond, VA")
  }
  if (merged_df$county[x] == "Roanoke city"){
    merged_df$city[x] <- as.character("Roanoke, VA")
  }
  if (merged_df$county[x] == "Rockingham County"){
    merged_df$city[x] <- as.character("Harrisonburg, VA")
  }
  if (merged_df$county[x] == "Salem city"){
    merged_df$city[x] <- as.character("Roanoke, VA")
  }
  if (merged_df$county[x] == "Washington County"){
    merged_df$city[x] <- as.character("Abingdon, VA")
  }
  if (merged_df$county[x] == "Winchester city"){
    merged_df$city[x] <- as.character("Winchester, VA")
  }
  if (merged_df$county[x] == "King County"){
    merged_df$city[x] <- as.character("Seattle, WA")
  }
  if (merged_df$county[x] == "Spokane County"){
    merged_df$city[x] <- as.character("Spokane, WA")
  }
  if (merged_df$county[x] == "Milwaukee County"){
    merged_df$city[x] <- as.character("Milwaukee, WI")
  }}



### Add column for Climate Zone
merged_df$climate_zone <- NA

for (x in 1:5502){
  y <- merged_df$city[x]
  if (y == "Los Angeles, CA" || y == "San Diego, CA" || y == "San Francisco, CA" || y == "Jacksonville, FL" || 
      y == "Palm Beach, FL" || y == "Atlanta, GA" || y == "Louisville, KY" || y == "Annapolis, MD" || 
      y == "Baltimore, MD" || y == "Rockville, MD" || y == "Kansas City, MO" || y == "New York City, NY" || 
      y == "Durham, NC" || y == "Raleigh, NC" || y == "Cinncinati, OH" || y == "Portland, OR" || 
      y == "Charleston, SC" || y == "Columbia, SC" || y == "Nashville, TN" || y == "Knoxville, TN" || 
      y == "Charlottesville, VA" || y == "Arlington, VA" || y == "Harrisonburg, VA" || y == "Richmond, VA" || 
      y == "Lynchburg, VA" || y == "Roanoke, VA" || y == "Abingdon, VA" || y == "Winchester, VA" || 
      y == "Seattle, WA" || y == "Spokane, WA"){
    merged_df$climate_zone[x] <- as.character("Temperate")
  }
  if (y == "Boise, ID" || y == "Las Vegas, NV" || y == "El Paso, TX"){
    merged_df$climate_zone[x] <- as.character("Arid")
  }
  if (y == "Boulder, CO" || y == "Richmond, IN" || y == "Boston, MA" || y == "Columbus, OH" || 
      y == "Milwaukee, WI"){
    merged_df$climate_zone[x] <- as.character("Snow")
  }
  if (y == "Fort Lauderdale, FL"){
    merged_df$climate_zone[x] <- as.character("Equatorial")
  }} 



#### Add column for Region 
merged_df$region <- NA

for (x in 1:5502){
  y <- merged_df$city[x]
  if (y == "Boston, MA" || y == "New York City, NY"){
    merged_df$region[x] <- as.character("North")
  }
  if (y == "Los Angeles, CA" || y == "San Diego, CA" || y == "San Francisco, CA" || y == "Boise, ID" || 
      y == "Las Vegas, NV" || y == "Boulder, CO" || y == "Portland, OR" ||  y == "Seattle, WA" || 
      y == "Spokane, WA"){
    merged_df$region[x] <- as.character("West")
  }
  if (y == "Richmond, IN" || y == "Columbus, OH" || 
      y == "Milwaukee, WI" || y == "Kansas City, MO" || y == "Cinncinati, OH"){
    merged_df$region[x] <- as.character("Midwest")
  }
  if (y == "Jacksonville, FL" || y == "Fort Lauderdale, FL" || y == "Palm Beach, FL" || 
      y == "Atlanta, GA" || y == "El Paso, TX" || y == "Louisville, KY" || y == "Annapolis, MD" || 
      y == "Baltimore, MD" || y == "Rockville, MD" || y == "Durham, NC" || y == "Raleigh, NC" || 
      y == "Charleston, SC" || y == "Columbia, SC" ||  y == "Nashville, TN" || y == "Knoxville, TN" || 
      y == "Charlottesville, VA" || y == "Arlington, VA" || y == "Harrisonburg, VA" || 
      y == "Richmond, VA" || y == "Lynchburg, VA" || y == "Roanoke, VA" || y == "Abingdon, VA" || 
      y == "Winchester, VA"){
    merged_df$region[x] <- as.character("South")
  }} 


## 3. Save dataframe in derived_data folder ----
rm(hvi_data, househeat, houseincome, coverage)
write_csv(merged_df, here::here("derived_data", "merged_data.csv"))



# 02 TRACT DATASET ----

### 1. Avg by race ----
tract_df <- merged_df %>%
  mutate(
    minority_night_air_temp = night_air_temp * (population_minority_pct / 100),
    white_night_air_temp = night_air_temp * (population_white_pct / 100),
    minority_afternoon_air_temp = afternoon_air_temp * (population_minority_pct / 100),
    white_afternoon_air_temp = afternoon_air_temp * (population_white_pct / 100),
    minority_morning_air_temp = morning_air_temp * (population_minority_pct / 100),
    white_morning_air_temp = morning_air_temp * (population_white_pct / 100)
  ) %>%
  select(
    geoid,
    minority_night_air_temp,
    white_night_air_temp,
    minority_afternoon_air_temp,
    white_afternoon_air_temp,
    minority_morning_air_temp,
    white_morning_air_temp
  )

### 2. Avg by population_under_5 ----
tract_df <- tract_df %>%
  left_join(
    merged_df %>%
      mutate(
        under_5_night_air_temp = night_air_temp * (population_under_5_pct / 100),
        under_5_afternoon_air_temp = afternoon_air_temp * (population_under_5_pct / 100),
        under_5_morning_air_temp = morning_air_temp * (population_under_5_pct / 100),
        over_5_night_air_temp = night_air_temp * ((100 - population_under_5_pct) / 100),
        over_5_afternoon_air_temp = afternoon_air_temp * ((100 - population_under_5_pct) / 100),
        over_5_morning_air_temp = morning_air_temp * ((100 - population_under_5_pct) / 100)
      ) %>%
      select(
        geoid,
        under_5_night_air_temp,
        under_5_afternoon_air_temp,
        under_5_morning_air_temp,
        over_5_night_air_temp,
        over_5_afternoon_air_temp,
        over_5_morning_air_temp
      ),
    by = "geoid"
  )


### 3. Dependant population ----
tract_df <- tract_df %>%
  left_join(
    merged_df %>%
      mutate(
        dependant_night_air_temp = night_air_temp * (dependent_age_groups_pct / 100),
        dependant_afternoon_air_temp = afternoon_air_temp * (dependent_age_groups_pct / 100),
        dependant_morning_air_temp = morning_air_temp * (dependent_age_groups_pct / 100),
        non_dependant_night_air_temp = night_air_temp * ((100 - dependent_age_groups_pct) / 100),
        non_dependant_afternoon_air_temp = afternoon_air_temp * ((100 - dependent_age_groups_pct) / 100),
        non_dependant_morning_air_temp = morning_air_temp * ((100 - dependent_age_groups_pct) / 100)
      ) %>%
      select(geoid, dependant_night_air_temp, dependant_afternoon_air_temp, dependant_morning_air_temp,
             non_dependant_night_air_temp, non_dependant_afternoon_air_temp, non_dependant_morning_air_temp),
    by = "geoid"
  )

### 4. Population w disability ----
tract_df <- tract_df %>%
  left_join(
    merged_df %>%
      mutate(
        disability_night_air_temp = night_air_temp * (population_disability_pct / 100),
        disability_afternoon_air_temp = afternoon_air_temp * (population_disability_pct / 100),
        disability_morning_air_temp = morning_air_temp * (population_disability_pct / 100),
        no_disability_night_air_temp = night_air_temp * ((100 - population_disability_pct) / 100),
        no_disability_afternoon_air_temp = afternoon_air_temp * ((100 - population_disability_pct) / 100),
        no_disability_morning_air_temp = morning_air_temp * ((100 - population_disability_pct) / 100)
      ) %>%
      select(
        geoid,
        disability_night_air_temp, disability_afternoon_air_temp, disability_morning_air_temp,
        no_disability_night_air_temp, no_disability_afternoon_air_temp, no_disability_morning_air_temp
      ),
    by = "geoid"
  )

### 5 Append climate zone ----
tract_df <- tract_df %>%
  left_join(
    merged_df %>%
      select(geoid, region, climate_zone),  # Select the columns you want to join
    by = "geoid"
  )

### 5 Append population ----
tract_df <- tract_df %>%
  left_join(
    merged_df %>%
      select(geoid, population),  # Select the columns you want to join
    by = "geoid"
  )

### 6. Save dataset ----
write_csv(tract_df, here::here("derived_data", "tract_data.csv"))


# 03. CITY DATASET ----

### 1. Avg by race ----
city_df <- merged_df %>%
  group_by(city) %>%
  summarise(
    minority_night_air_temp = sum(night_air_temp * (population_minority_pct / 100) * population, na.rm = TRUE) / sum((population_minority_pct / 100) * population, na.rm = TRUE),
    white_night_air_temp = sum(night_air_temp * (population_white_pct / 100) * population, na.rm = TRUE) / sum((population_white_pct / 100) * population, na.rm = TRUE),
    minority_afternoon_air_temp = sum(afternoon_air_temp * (population_minority_pct / 100) * population, na.rm = TRUE) / sum((population_minority_pct / 100) * population, na.rm = TRUE),
    white_afternoon_air_temp = sum(afternoon_air_temp * (population_white_pct / 100) * population, na.rm = TRUE) / sum((population_white_pct / 100) * population, na.rm = TRUE),
    minority_morning_air_temp = sum(morning_air_temp * (population_minority_pct / 100) * population, na.rm = TRUE) / sum((population_minority_pct / 100) * population, na.rm = TRUE),
    white_morning_air_temp = sum(morning_air_temp * (population_white_pct / 100) * population, na.rm = TRUE) / sum((population_white_pct / 100) * population, na.rm = TRUE)
  ) %>%
  ungroup()

## 2. Avg by population_under_5 ----
city_df <- city_df %>%
  left_join(
    merged_df %>%
      group_by(city) %>%
      summarise(
        under_5_night_air_temp = sum(night_air_temp * (population_under_5_pct / 100) * population, na.rm = TRUE) / sum((population_under_5_pct / 100) * population, na.rm = TRUE),
        under_5_afternoon_air_temp = sum(afternoon_air_temp * (population_under_5_pct / 100) * population, na.rm = TRUE) / sum((population_under_5_pct / 100) * population, na.rm = TRUE),
        under_5_morning_air_temp = sum(morning_air_temp * (population_under_5_pct / 100) * population, na.rm = TRUE) / sum((population_under_5_pct / 100) * population, na.rm = TRUE),
        over_5_night_air_temp = sum(night_air_temp * ((100 - population_under_5_pct) / 100) * population, na.rm = TRUE) / sum(((100 - population_under_5_pct) / 100) * population, na.rm = TRUE),
        over_5_afternoon_air_temp = sum(afternoon_air_temp * ((100 - population_under_5_pct) / 100) * population, na.rm = TRUE) / sum(((100 - population_under_5_pct) / 100) * population, na.rm = TRUE),
        over_5_morning_air_temp = sum(morning_air_temp * ((100 - population_under_5_pct) / 100) * population, na.rm = TRUE) / sum(((100 - population_under_5_pct) / 100) * population, na.rm = TRUE)
      ) %>%
      ungroup(),
    by = "city"
  )

### 3. Avg by dependant population ----
city_df <- city_df %>%
  left_join(
    merged_df %>%
      group_by(city) %>%
      summarise(
        dependant_night_air_temp = sum(night_air_temp * (dependent_age_groups_pct / 100) * population, na.rm = TRUE) / sum((dependent_age_groups_pct / 100) * population, na.rm = TRUE),
        dependant_afternoon_air_temp = sum(afternoon_air_temp * (dependent_age_groups_pct / 100) * population, na.rm = TRUE) / sum((dependent_age_groups_pct / 100) * population, na.rm = TRUE),
        dependant_morning_air_temp = sum(morning_air_temp * (dependent_age_groups_pct / 100) * population, na.rm = TRUE) / sum((dependent_age_groups_pct / 100) * population, na.rm = TRUE),
        non_dependant_night_air_temp = sum(night_air_temp * ((100 - dependent_age_groups_pct) / 100) * population, na.rm = TRUE) / sum(((100 - dependent_age_groups_pct) / 100) * population, na.rm = TRUE),
        non_dependant_afternoon_air_temp = sum(afternoon_air_temp * ((100 - dependent_age_groups_pct) / 100) * population, na.rm = TRUE) / sum(((100 - dependent_age_groups_pct) / 100) * population, na.rm = TRUE),
        non_dependant_morning_air_temp = sum(morning_air_temp * ((100 - dependent_age_groups_pct) / 100) * population, na.rm = TRUE) / sum(((100 - dependent_age_groups_pct) / 100) * population, na.rm = TRUE)
      ) %>%
      ungroup(),
    by = "city"
  )

### 4. Avg by population_disability ----
city_df <- city_df %>%
  left_join(
    merged_df %>%
      group_by(city) %>%
      summarise(
        disability_night_air_temp = sum(night_air_temp * (population_disability_pct / 100) * population, na.rm = TRUE) / sum((population_disability_pct / 100) * population, na.rm = TRUE),
        disability_afternoon_air_temp = sum(afternoon_air_temp * (population_disability_pct / 100) * population, na.rm = TRUE) / sum((population_disability_pct / 100) * population, na.rm = TRUE),
        disability_morning_air_temp = sum(morning_air_temp * (population_disability_pct / 100) * population, na.rm = TRUE) / sum((population_disability_pct / 100) * population, na.rm = TRUE),
        no_disability_night_air_temp = sum(night_air_temp * ((100 - population_disability_pct) / 100) * population, na.rm = TRUE) / sum(((100 - population_disability_pct) / 100) * population, na.rm = TRUE),
        no_disability_afternoon_air_temp = sum(afternoon_air_temp * ((100 - population_disability_pct) / 100) * population, na.rm = TRUE) / sum(((100 - population_disability_pct) / 100) * population, na.rm = TRUE),
        no_disability_morning_air_temp = sum(morning_air_temp * ((100 - population_disability_pct) / 100) * population, na.rm = TRUE) / sum(((100 - population_disability_pct) / 100) * population, na.rm = TRUE)
      ) %>%
      ungroup(),
    by = "city"
  )

### 5. Append climate zone ----
city_df <- city_df %>%
  left_join(
    merged_df %>%
      select(city, region, climate_zone),  # Select the columns you want to join
    by = "city"
  )

### 6. Append population ----
population_by_city <- merged_df %>%
  group_by(city) %>%
  summarise(total_population = sum(population, na.rm = TRUE)) %>%
  ungroup()

city_df <- city_df %>%
  left_join(population_by_city, by = "city") %>%
  distinct()

### 7. Save dataset ----
write_csv(city_df, here::here("derived_data", "city_data.csv"))
