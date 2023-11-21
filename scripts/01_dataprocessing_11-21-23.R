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


## load in new dataset
data <- read.csv(here::here("derived_data", "finaltractsREVISED.csv")) %>% as.data.frame()


#### Order columns in a meaningful order 
data <- select(data, "GEOID", "NAME", "State", "County", "climate_zone",
               "Tract_Anomaly", "Low.Summer.Average.Land.Surface.Temperature..F.", 
               "High.Summer.Average.Land.Surface.Temperature..F.", "Summer.Average.Land.Surface.Temperature..F.",
               "Total.Population", "Percent.of.Population.that.is.White.alone..Non.Hispanic",
               "Percent.of.Population.that.is.Minority", "Percent.of.Population.whose.Income.is.Below.Poverty.Level")





# 02 TRACT DATASET ----

### 1. Avg by race ----
tract_df <- data %>%
  mutate(
    minority_air_temp = Tract_Anomaly * (Percent.of.Population.that.is.Minority / 100),
    white_air_temp = Tract_Anomaly * (Percent.of.Population.that.is.White.alone..Non.Hispanic / 100),
  ) %>%
  select(
    GEOID,
    minority_air_temp,
    white_air_temp,
    climate_zone
  )


### Save dataset ----
write_csv(tract_df, here::here("derived_data", "tract_data_11-21-23.csv"))






###########################################################################################3
### 2. Avg by population_under_5 ----
tract_df <- tract_df %>%
  left_join(
    merged_df %>%
      mutate(
        under_5_night_air_temp = night_air_temp * (population_under_5_pct / 100),
        over_5_night_air_temp = night_air_temp * ((100 - population_under_5_pct) / 100),
      ) %>%
      select(
        geoid,
        under_5_night_air_temp,
        over_5_night_air_temp,
      ),
    by = "geoid"
  )


### 3. Dependant population ----
tract_df <- tract_df %>%
  left_join(
    merged_df %>%
      mutate(
        dependant_night_air_temp = night_air_temp * (dependent_age_groups_pct / 100),
        non_dependant_night_air_temp = night_air_temp * ((100 - dependent_age_groups_pct) / 100)
      ) %>%
      select(geoid, dependant_night_air_temp,
             non_dependant_night_air_temp),
    by = "geoid"
  )

### 4. Population w disability ----
tract_df <- tract_df %>%
  left_join(
    merged_df %>%
      mutate(
        disability_night_air_temp = night_air_temp * (population_disability_pct / 100),
        no_disability_night_air_temp = night_air_temp * ((100 - population_disability_pct) / 100),
      ) %>%
      select(
        geoid,
        disability_night_air_temp,
        no_disability_night_air_temp
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
      select(geoid, population),  
    by = "geoid"
  )

merged_df <- merged_df %>% mutate(pop_minority = population*(population_minority_pct/100))

tract_df <- tract_df %>%
  left_join(
    merged_df %>%
      group_by(geoid) %>%
      summarise(
        total_population_minority = sum(pop_minority, na.rm = TRUE)
      ) %>% 
      ungroup(),
    by = "geoid"
  ) %>%
  rename(
    population_minority = total_population_minority
  ) 

### 6. Save dataset ----
write_csv(tract_df, here::here("derived_data", "tract_data.csv"))


