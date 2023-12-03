# SETUP ----

## 1. Load required packages ----
library(here)
library(tidyverse)
library(janitor)
library(tidycensus)

## 2. Set working directory ----
here::i_am("scripts/01_dataprocessing.R")

## 3. Run Util.R ----
source(here::here("scripts", "util.R"))

## 4. Create necessary directories ----
dir.create(here::here("derived_data"), showWarnings = FALSE)

# 01 PROCESSING ----

## 1. Load and process data ----
data <-
  read.csv(here::here("source_data", "tract_data.csv")) %>%
  as.data.frame() %>% 
  filter(PERCENTAGE >=50) %>%
  select(
    "GEOID",
    "NAME",
    "State",
    "County",
    "climate_zone",
    "Tract_Anomaly",
    "Low.Summer.Average.Land.Surface.Temperature..F.",
    "High.Summer.Average.Land.Surface.Temperature..F.",
    "Summer.Average.Land.Surface.Temperature..F.",
    "Total.Population",
    "Percent.of.Population.that.is.White.alone..Non.Hispanic",
    "Percent.of.Population.that.is.Minority",
    "PERCENTAGE",
    "City_Name"
  ) %>%
  janitor::clean_names() %>%
  filter(!is.na(geoid))

## 2. Mutate and subset ----
tract_df <- data %>%
  mutate(
    geoid = as.character(geoid),
    afternoon_air_temp = tract_anomaly,
    population_minority = total_population * (percent_of_population_that_is_minority / 100),
    population = total_population,
    pop_white = afternoon_air_temp * (percent_of_population_that_is_white_alone_non_hispanic / 100),
    minority_afternoon_air_temp = afternoon_air_temp * (percent_of_population_that_is_minority / 100),
    white_afternoon_air_temp = afternoon_air_temp * (percent_of_population_that_is_white_alone_non_hispanic / 100),
    climate_zone = forcats::as_factor(stringr::str_to_title(climate_zone)),
    city = paste0(city_name, ", ", state)
  ) %>%
  select(
    c(
      geoid,
      city,
      minority_afternoon_air_temp,
      white_afternoon_air_temp,
      climate_zone,
      population,
      population_minority
    )
  ) %>% filter(population > 0)

## 3. Save dataset ----
write_csv(tract_df,
          here::here("derived_data", "tract_data.csv"))

