# SETUP ----

## 1. Load required packages ----
library(here)
library(tidyverse)
library(janitor)
library(sf)

## 2. Set working directory ----
here::i_am("scripts/02_analysis.R")

## 3. Run Util.R ----
source(here::here("scripts", "util.R"))

## 4. Create necessary directories ----
dir.create(here::here("derived_data"), showWarnings = FALSE)

## 5. Load data ----
if (!file.exists(here::here("derived_data", "merged_data.csv"))) {
  source(here::here("scripts", "01_dataprocessing.R"))
} else {
  df <- read_csv(here::here("derived_data", "merged_data.csv"))
}


# 02 DESCRIPTIVE STATS ----

## Read in tract_boundaries ----
tract_boundaries <- sf::st_read(here::here("source_data", "Tract_boundaries", "cb_2019_us_tract_500k.shp")) %>%
  sf::st_transform(crs = 4326) 

## Merge tract boundaries with df
df <- df %>% mutate(geoid = as.character(geoid)) 
df_shp <- tract_boundaries %>% dplyr::left_join(df, by = c("GEOID" = "geoid"))

# Drop tract_boundaries
rm(tract_boundaries)


