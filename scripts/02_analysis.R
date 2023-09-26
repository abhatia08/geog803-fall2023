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
ensure_directory(here::here("derived_data"))
ensure_directory(here::here("figures"))
ensure_directory(here::here("figures", "bivariate"))
ensure_directory(here::here("figures", "univariate"))

## 5. Load data ----
data_path <- here("derived_data", "merged_data.csv")
if (!file.exists(data_path)) {
  source(here("scripts", "01_dataprocessing.R"))
}
df <- read_csv(data_path) %>% as.data.frame()


# 02 ANALYSIS ----

# # Subset for North Carolina
# df <- df %>% filter(state == "Virginia")


