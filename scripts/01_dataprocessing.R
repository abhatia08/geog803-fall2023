# SETUP ----

## 1. Load required packages ----
library(here)
library(plyr)
library(data.table)
library(tidyverse)
library(fs)
library(janitor)
library(raster)
library(rgdal)
library(sf)

## 2. Declare `here`
here::i_am("scripts/01_dataprocessing.R")

## 3. Run Util.R ----
source(here::here("scripts", "util.R"))

## 4. Create necessary directories ----
ensure_directory(here::here("derived_data"))

# 01 MERGING DATA ----

## 1. Load in all data ----

### HVI data ----
hvi_data <- read_csv(here::here("source_data", "heat_vulnerability", "hvi_data.csv")) %>% as.data.frame()

### Census data ----
true_census <- st_read(here::here("source_data", "spatial_tables", "true_cenusTracts.shp")) %>% as.data.frame()

### Housing_heating data ----
househeat <- read_csv(here::here("source_data", "housing_heating", "ACSST5Y2021.S2504-Data.csv")) %>% as.data.frame()

### Housing_income data ----
houseincome <- read_csv(here::here("source_data", "housing_income_costs", "ACSST5Y2021.S2503-Data.csv")) %>% as.data.frame()

### Age race sex data ----
agesexrace <- read_csv(here::here("source_data", "age_sex_race", "ACSDP5Y2021.DP05-Data.csv")) %>% as.data.frame()

## 2. Merge datasets ----

### Clean and keep only relevant GEOIDs ----
process_and_filter_data <- function(df, hvi_data) {
  # Split GEOID
  df$GEOID <- as.character(str_extract(df$GEO_ID, "(?<=US)\\d+"))
  
  # Prepare for merging
  names(df) <- df[1,]
  df <- df[-1,]
  names(df)[ncol(df)] <- "GEOID"
  
  # Filter rows based on GEOIDs in hvi_data
  valid_geoids <- unique(hvi_data$GEOID)
  df <- df[df$GEOID %in% valid_geoids, ]
  df <- janitor::clean_names(df)
  
  return(df)
}

### Function to keep columns that start with "estimate_" and "GEOID"
filter_estimate_columns <- function(df) {
  cols_to_keep <- c("geoid", grep("^estimate_", names(df), value = TRUE))
  return(df[, cols_to_keep])
}


### Process datasets ----
agesexrace <- process_and_filter_data(agesexrace, hvi_data) %>% clean_names
househeat <- process_and_filter_data(househeat, hvi_data) %>% clean_names
houseincome <- process_and_filter_data(houseincome, hvi_data) %>% clean_names

agesexrace <- filter_estimate_columns(agesexrace)
househeat <- filter_estimate_columns(househeat)
houseincome <- filter_estimate_columns(houseincome)

### Merge datasets ----
merge_datasets <- function(main_df, df_to_merge) {
  merged_df <- left_join(main_df, df_to_merge, by = "geoid")
  return(merged_df)
}

hvi_data$GEOID <- as.character(hvi_data$GEOID) 
hvi_data <- janitor::clean_names(hvi_data)
merged_df <- left_join(hvi_data, true_census, by = c("geoid" = "FIPS"))
merged_df <- merge_datasets(merged_df, agesexrace)
merged_df <- merge_datasets(merged_df, househeat)
merged_df <- merge_datasets(merged_df, houseincome)

## Save dataframe in derived_data folder ----
write_csv(merged_df, here::here("derived_data", "merged_data.csv"))

