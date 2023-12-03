# SETUP ----

## 1. Load required packages ----
library(here)
library(tidyverse)
library(ggExtra)
library(patchwork)
library(viridis)

## 2. Set working directory ----
here::i_am("scripts/03_figures.R")

## 3. Run Util.R ----
source(here::here("scripts", "util.R"))

## 4. Create necessary directories ----
ensure_directory(here::here("derived_data"))
ensure_directory(here::here("figures"))
ensure_directory(here::here("figures", "maps"))

## 5. Load data ----
tract_df <- read_csv(here("derived_data", "tract_data.csv"))


# 03. FIGURES ----

### a. Overall Boxplot ----
# Parameters 
names <- c("People of Color", "Non-Hispanic White")
cols = c("#5e9db3", "#e2b108")
par(family = 'serif')

# Combined plot layout (comment it out to save individual)
layout_matrix <- matrix(c(1, 1, 2, 3, 4,5), nrow = 3, byrow = TRUE)
layout(layout_matrix)

# par(mfrow = c(1, 1)) # Comment this back in to save individual plots

# Plot the overall boxplot
boxplot(
  tract_df[, 2:3],
  main = "All Tracts",
  ylab = "Deviation from Mean Temperature (°F)",
  outline = FALSE,
  names = names,
  col = cols
)

### b. Boxplot by climate zone ----
temperate <- filter(tract_df, climate_zone == "Temperate")
dry <- filter(tract_df, climate_zone == "Dry")
tropical <- filter(tract_df, climate_zone == "Tropical")
continental <- filter(tract_df, climate_zone == "Continental")

# par(mfrow = c(2, 2)) # Comment this back in to save individual plots

# Plot each boxplot in its own panel with the climate zone as the title
boxplot(
  temperate[, 2:3],
  main = "Temperate Zone",
  ylab = "Deviation from Mean Temperature (°F)",
  outline = FALSE,
  names = names,
  col = cols
)

boxplot(
  dry[, 2:3],
  main = "Dry Zone",
  ylab = "Deviation from Mean Temperature (°F)",
  outline = FALSE,
  names = names,
  col = cols
)

boxplot(
  tropical[, 2:3],
  main = "Tropical Zone",
  ylab = "Deviation from Mean Temperature (°F)",
  outline = FALSE,
  names = names,
  col = cols
)

boxplot(
  continental[, 2:3],
  main = "Continental",
  ylab = "Deviation from Mean Temperature (°F)",
  outline = FALSE,
  names = names,
  col = cols
)
