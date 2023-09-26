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
hvi_data <- read_csv(here::here("source_data", "heat_vulnerability", "hvi_data.csv")) %>% as.data.frame()
# true_census <- st_read(here::here("source_data", "spatial_tables", "true_cenusTracts.shp"))
househeat <- read_csv(here::here("source_data", "spatial_tables", "house_heating_final.csv")) %>% as.data.frame()
houseincome <- read_csv(here::here("source_data", "spatial_tables", "house_income_final.csv")) %>% as.data.frame()

## 2. Merge datasets ----

### Function to merge datasets ----
merge_datasets <- function(main_df, df_to_merge) {
  merged_df <- left_join(main_df, df_to_merge, by = "GEOID")
  return(merged_df)
}

hvi_data$GEOID <- as.character(hvi_data$GEOID)
househeat$GEOID <- as.character(househeat$GEOID)
houseincome$GEOID <- as.character(houseincome$GEOID)

merged_df <- merge_datasets(hvi_data, househeat)
merged_df <- merge_datasets(merged_df, houseincome)
merged_df <- janitor::clean_names(merged_df)


### Remove duplicate columns
merged_df <- merged_df[, !duplicated(colnames(merged_df))]

### Save dataframe in derived_data folder ----
write_csv(merged_df, here::here("derived_data", "merged_data.csv"))




