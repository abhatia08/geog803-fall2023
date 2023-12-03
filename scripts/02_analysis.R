# SETUP ----

## 1. Load required packages ----
library(here)
library(tidyverse)
library(janitor)
library(gt)
library(gtsummary)
library(ejanalysis) # Note: Installation requires dependencies: https://rdrr.io/github/ejanalysis/ejanalysis/f/README.md
library(survey)
library(infer)

## 2. Set working directory ----
here::i_am("scripts/02_analysis.R")

## 3. Run Util.R ----
source(here::here("scripts", "util.R"))

## 4. Create necessary directories ----
ensure_directory(here::here("derived_data"))

## 5. Load data ----
data_path <- here("derived_data", "tract_data.csv")
if (!file.exists(data_path)) {
  source(here("scripts", "01_dataprocessing.R"))
}

tract_df <-
  read_csv(here("derived_data", "tract_data.csv")) %>% as_tibble()

## Relevel the factor 'climate_zone' in the 'tract_df' data frame
tract_df <- tract_df %>% 
  mutate(climate_zone = forcats::fct_relevel(climate_zone, "Temperate", "Dry", "Continental", "Tropical"))

# 02 ANALYSIS ----

## 1. Difference-in-means ----

# Calculate weights
tract_df$weight_minority <-
  tract_df$population_minority / tract_df$population
tract_df$weight_white <- 1 - tract_df$weight_minority

tract_df <- tract_df %>%
  mutate(
    population_white = population - population_minority,
    total_air_temp = (minority_afternoon_air_temp * population_minority + white_afternoon_air_temp * population_white) / population
  )

### a. Weighted means and SD by climate zone----
climate_zone_diff_weighted <- tract_df %>%
  group_by(climate_zone) %>%
  summarise(
    weighted_mean_minority = sum(minority_afternoon_air_temp * population_minority, na.rm = TRUE) / sum(population_minority, na.rm = TRUE),
    sd_minority = sqrt(sum(population_minority * (minority_afternoon_air_temp - weighted_mean_minority)^2, na.rm = TRUE) / sum(population_minority, na.rm = TRUE)),
    weighted_mean_white = sum(white_afternoon_air_temp * population_white, na.rm = TRUE) / sum(population_white, na.rm = TRUE),
    sd_white = sqrt(sum(population_white * (white_afternoon_air_temp - weighted_mean_white)^2, na.rm = TRUE) / sum(population_white, na.rm = TRUE)),
    weighted_mean_total = mean(total_air_temp, na.rm = TRUE),
    weighted_sd_total = sqrt(sum(population * (total_air_temp - weighted_mean_total)^2, na.rm = TRUE) / sum(population, na.rm = TRUE)),
    weighted_mean_difference = weighted_mean_minority - weighted_mean_white
  )


### b. Weighted means and SD Total----
total_diff_weighted <- tract_df %>%
  summarise(
    weighted_mean_minority = sum(minority_afternoon_air_temp * population_minority, na.rm = TRUE) / sum(population_minority, na.rm = TRUE),
    sd_minority = sqrt(sum(population_minority * (minority_afternoon_air_temp - weighted_mean_minority)^2, na.rm = TRUE) / sum(population_minority, na.rm = TRUE)),
    weighted_mean_white = sum(white_afternoon_air_temp * population_white, na.rm = TRUE) / sum(population_white, na.rm = TRUE),
    sd_white = sqrt(sum(population_white * (white_afternoon_air_temp - weighted_mean_white)^2, na.rm = TRUE) / sum(population_white, na.rm = TRUE)),
    weighted_mean_total = mean(total_air_temp, na.rm = TRUE),
    weighted_sd_total = sqrt(sum(population * (total_air_temp - weighted_mean_total)^2, na.rm = TRUE) / sum(population, na.rm = TRUE)),
    weighted_mean_difference = weighted_mean_minority - weighted_mean_white
  )


### c. Difference in means SE by climate zone ----
# Loop through each climate zone for SEs
climate_zones <- c("Temperate", "Dry", "Continental","Tropical")
for (zone in climate_zones) {
  data_zone <- filter(tract_df, climate_zone == zone)
  perform_analysis(data_zone, zone)
}

### d. Difference in means SE Total ----
# Bootstrap for Standard Deviation
n_boot <- 1000
boot_results <- numeric(n_boot)
for (i in 1:n_boot) {
  boot_sample <- tract_df[sample(nrow(tract_df), replace = TRUE), ]
  boot_results[i] <- calc_weighted_mean_diff(boot_sample)
}
bootstrap_sd <- sd(boot_results)

# Permutation Test for T-Test
n_perm <- 1000
perm_results <- numeric(n_perm)
obs_diff <- calc_weighted_mean_diff(tract_df)
for (i in 1:n_perm) {
  shuffled_minority <- sample(tract_df$population_minority)
  tract_df$population_minority <- shuffled_minority
  perm_results[i] <- calc_weighted_mean_diff(tract_df)
}
p_value <- mean(abs(perm_results) >= abs(obs_diff))

# Print results
cat("Bootstrap SD of Weighted Mean Difference:", bootstrap_sd, "\n")
cat("Permutation Test P-Value:", p_value, "\n")




## 2. Kolm-Pollack Index ----

### a. Total KP index ----
KolmPollak(tract_df$total_air_temp,
           bigbadx = TRUE,
           na.rm = TRUE)

KolmPollak(tract_df$minority_afternoon_air_temp,
           bigbadx = TRUE,
           na.rm = TRUE)

KolmPollak(tract_df$white_afternoon_air_temp,
           bigbadx = TRUE,
           na.rm = TRUE)

### b. KP index by climate ----
# Looping through each climate zone and applying KolmPollak function
results <- tract_df %>%
  group_by(climate_zone) %>%
  summarise(
    KolmPollak_total = KolmPollak(total_air_temp, bigbadx = TRUE, na.rm = TRUE),
    KolmPollak_minority = KolmPollak(minority_afternoon_air_temp, bigbadx = TRUE, na.rm = TRUE),
    KolmPollak_white = KolmPollak(white_afternoon_air_temp, bigbadx = TRUE, na.rm = TRUE)
  )

results$KolmPollak_meandiff <- results$KolmPollak_minority - results$KolmPollak_white

# Print the results
print(results)

### c. SD for White KP index ----
n_boot <- 1000
boot_results <- numeric(n_boot)
cat("SDs for NH White Kolm Pollack by Climate:", "\n")
for (zone in climate_zones) {
  data_zone <- filter(tract_df, climate_zone == zone)
  for (i in 1:n_boot) {
    boot_sample <- data_zone[sample(nrow(data_zone), replace = TRUE),]
    boot_results[i] <- KolmPollak(boot_sample$white_afternoon_air_temp,
                                  bigbadx = TRUE,
                                  na.rm = TRUE)
  }
  bootstrap_sd <- sd(boot_results)
  cat("Climate Zone:", zone, "\n")
  cat("Bootstrapped SD:", bootstrap_sd, "\n", "\n")
}

### d. SD for Minority KP index ----
n_boot <- 1000
boot_results <- numeric(n_boot)
cat("SDs for Minority Kolm Pollack by Climate:", "\n")
for (zone in climate_zones) {
  data_zone <- filter(tract_df, climate_zone == zone)
  for (i in 1:n_boot) {
    boot_sample <- data_zone[sample(nrow(data_zone), replace = TRUE),]
    boot_results[i] <- KolmPollak(boot_sample$minority_afternoon_air_temp,
                                  bigbadx = TRUE,
                                  na.rm = TRUE)
  }
  bootstrap_sd <- sd(boot_results)

  cat("Climate Zone:", zone, "\n")
  cat("Bootstrapped SD:", bootstrap_sd, "\n", "\n")
}

### e. SD for Total KP index ----
n_boot <- 1000
boot_results <- numeric(n_boot)
cat("SDs for Total Kolm Pollack by Climate:", "\n")
for (zone in climate_zones) {
  data_zone <- filter(tract_df, climate_zone == zone)
  for (i in 1:n_boot) {
    boot_sample <- data_zone[sample(nrow(data_zone), replace = TRUE),]
    boot_results[i] <- KolmPollak(boot_sample$total_air_temp,
                                  bigbadx = TRUE,
                                  na.rm = TRUE)
  }
  bootstrap_sd <- sd(boot_results)

  cat("Climate Zone:", zone, "\n")
  cat("Bootstrapped SD:", bootstrap_sd, "\n", "\n")
}

### f. SD for Total KP index ----
n_boot <- 1000
boot_results <- numeric(n_boot)
for (i in 1:n_boot) {
  boot_sample <- tract_df[sample(nrow(tract_df), replace = TRUE), ]
  boot_results[i] <- KolmPollak(boot_sample$total_air_temp,
                                bigbadx = TRUE,
                                na.rm = TRUE)
}
bootstrap_sd <- sd(boot_results)
cat("SDs for Total Kolm Pollack:", "\n")
cat("Bootstrapped SD:", bootstrap_sd, "\n", "\n")

### g. SD for Minority KP index ----
n_boot <- 1000
boot_results <- numeric(n_boot)
for (i in 1:n_boot) {
  boot_sample <- tract_df[sample(nrow(tract_df), replace = TRUE), ]
  boot_results[i] <- KolmPollak(boot_sample$minority_afternoon_air_temp,
                                bigbadx = TRUE,
                                na.rm = TRUE)
}
bootstrap_sd <- sd(boot_results)
cat("SDs for Minority Kolm Pollack:", "\n")
cat("Bootstrapped SD:", bootstrap_sd, "\n", "\n")

### e. SD for White KP index ----
n_boot <- 1000
boot_results <- numeric(n_boot)
for (i in 1:n_boot) {
  boot_sample <- tract_df[sample(nrow(tract_df), replace = TRUE), ]
  boot_results[i] <- KolmPollak(boot_sample$white_afternoon_air_temp,
                                bigbadx = TRUE,
                                na.rm = TRUE)
}
bootstrap_sd <- sd(boot_results)
cat("SDs for White Kolm Pollack:", "\n")
cat("Bootstrapped SD:", bootstrap_sd, "\n", "\n")

### Difference in KP index Bootstrapped SE ----
for (zone in climate_zones) {
  data_zone <- filter(tract_df, climate_zone == zone)
  perform_analysis_kolm(data_zone, zone)
}

## Total SE 
# Bootstrap for Standard Deviation
n_boot <- 1000
boot_results <- numeric(n_boot)
for (i in 1:n_boot) {
  boot_sample <- tract_df[sample(nrow(tract_df), replace = TRUE), ]
  boot_results[i] <- calc_kp_diff(boot_sample)
}
bootstrap_sd <- sd(boot_results)

# Permutation Test for T-Test
n_perm <- 1000
perm_results <- numeric(n_perm)
obs_diff <- calc_kp_diff(tract_df)
for (i in 1:n_perm) {
  shuffled_minority <- sample(tract_df$population_minority)
  tract_df$population_minority <- shuffled_minority
  perm_results[i] <- calc_kp_diff(tract_df)
}
p_value <- mean(abs(perm_results) >= abs(obs_diff))

# Print results
cat("Bootstrap SD of Weighted Mean Difference:", bootstrap_sd, "\n")
cat("Permutation Test P-Value:", p_value, "\n")