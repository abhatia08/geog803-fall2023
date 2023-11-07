# SETUP ----

## 1. Load required packages ----
library(here)
library(tidyverse)
library(janitor)
library(sf)
library(gt)
library(gtsummary)
library(ejanalysis) # Note: Installation requires dependencies: https://rdrr.io/github/ejanalysis/ejanalysis/f/README.md
library(survey)

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
merged_df <- read_csv(data_path) %>% as.data.frame()
city_df <- read_csv(here("derived_data", "city_data.csv"))
tract_df <- read_csv(here("derived_data", "tract_data.csv"))
county_df <- read_csv(here("derived_data", "county_data.csv"))

## 6. Temporary handling of NA ----
city_df <- city_df %>% select(c(city, minority_night_air_temp, white_night_air_temp, minority_afternoon_air_temp, white_afternoon_air_temp, minority_morning_air_temp, white_morning_air_temp, region, climate_zone, population))
tract_df <- tract_df %>% select(c(geoid, minority_night_air_temp, white_night_air_temp, minority_afternoon_air_temp, white_afternoon_air_temp, minority_morning_air_temp, white_morning_air_temp, region, climate_zone, population))
county_df <- county_df %>% select(c(county, minority_night_air_temp, white_night_air_temp, minority_afternoon_air_temp, white_afternoon_air_temp, minority_morning_air_temp, white_morning_air_temp, region, climate_zone, population, population_minority))

# 02 ANALYSIS ----

#######################################

# TABLE 1 WITH COUNTY ----
# Calculate weights
tract_df$weight_minority <- tract_df$population_minority / tract_df$population
tract_df$weight_white <- 1 - tract_df$weight_minority

## 1. Morning ----
time_of_day <- "morning"
compute_stats(tract_df, time_of_day)
calculate_weighted_t_test(tract_df, time_of_day)
calculate_weighted_t_test_by_climate(tract_df, time_of_day)

## 2. Afternoon ----
time_of_day <- "afternoon"
compute_stats(tract_df, time_of_day)
calculate_weighted_t_test(tract_df, time_of_day)
calculate_weighted_t_test_by_climate(tract_df, time_of_day)

## 3. Night ----
time_of_day <- "night"
compute_stats(tract_df, time_of_day)
calculate_weighted_t_test(tract_df, time_of_day)
calculate_weighted_t_test_by_climate(tract_df, time_of_day)


## 4. Kolm-Pollack ----
KolmPollak(tract_df$minority_afternoon_air_temp, bigbadx = TRUE, na.rm = TRUE)
KolmPollak(tract_df$white_afternoon_air_temp, bigbadx = TRUE, na.rm = TRUE)

?KolmPollak

KolmPollak(tract_df$minority_night_air_temp, bigbadx = TRUE)
KolmPollak(tract_df$white_night_air_temp, bigbadx = TRUE)

KolmPollak(tract_df$minority_morning_air_temp, bigbadx = TRUE)
KolmPollak(tract_df$white_morning_air_temp, bigbadx = TRUE)

## Kolm-Pollack


######## 
# ATTIC ----

## 1. Basic Descriptives ----
# # Formatting
# df <- df %>%
#   mutate(median_income = factor(
#     median_income,
#     levels = c(
#       "Under 15,000",
#       "15,000 to 74,999",
#       "75,000 to 149,999",
#       "150,000 to 249,999",
#       "250,000 and over"
#     )
#   ))
# 
# # Filter the dataframe to keep only the desired columns
# selected_vars <-
#   c(
#     "vacant_houses_pct",
#     "houses_single_occupancy_pct",
#     "utility_gas_pct",
#     "bottled_gas_pct",
#     "electricity_pct",
#     "fuel_oil_pct",
#     "coal_pct",
#     "other_fuels_pct",
#     "no_fuel_pct",
#     # "median_income",
#     "population_below_poverty_pct",
#     "education_below_hs_pct",
#     "population",
#     "population_65_pct",
#     "population_white_pct",
#     "population_minority_pct",
#     "population_disability_pct",
#     "tree_canopy_pct",
#     "morning_air_temp",
#     "afternoon_air_temp",
#     "night_air_temp",
#     "houses_no_internet_pct",
#     "houses_no_vehicle_pct",
#     "impervious_surfaces_pct"
#   )
# 
# # Filter the dataframe to keep only the selected variables
# df_selected <- df %>%
#   select(all_of(selected_vars))
# 
# ### b. Correlation Coefficients ----
# 
# # Ensure the dataframe contains the selected variables and the temperature variables
# df_selected <- df %>%
#   select(all_of(
#     c(
#       selected_vars,
#       "night_air_temp",
#       "afternoon_air_temp",
#       "morning_air_temp"
#     )
#   ))
# 
# # Function to calculate correlations for the three temperature variables
# calculate_correlation <- function(var_name) {
#   df_selected %>%
#     summarise(
#       night_air_temp_cor = cor(.data[[var_name]], night_air_temp, use = "complete.obs"),
#       afternoon_air_temp_cor = cor(.data[[var_name]], afternoon_air_temp, use = "complete.obs"),
#       morning_air_temp_cor = cor(.data[[var_name]], morning_air_temp, use = "complete.obs")
#     )
# }
# 
# # Calculate correlations
# correlation_df <-
#   map_dfr(
#     setdiff(
#       selected_vars,
#       c("night_air_temp", "afternoon_air_temp", "morning_air_temp")
#     ),
#     ~ calculate_correlation(.x) %>%
#       add_column(variable = .x, .before = 1)
#   )
# 
# 

## 2. Table 1 ----

# ### 1. Computing averages and SD by strata
# # Ensure climate_zone and population are correctly formatted
# tract_df$climate_zone <- as.factor(tract_df$climate_zone)
# tract_df$population <- as.numeric(tract_df$population)
# 
# # Separate the data based on time of day
# night_data <- tract_df %>%
#   dplyr::select(dplyr::contains("night"), "climate_zone", "population")
# 
# afternoon_data <- tract_df %>%
#   dplyr::select(dplyr::contains("afternoon"), "climate_zone", "population")
# 
# morning_data <- tract_df %>%
#   dplyr::select(dplyr::contains("morning"), "climate_zone", "population")
# 
# # Calculate population weighted means and standard deviations
# ## Night
# night_results <-
#   calculate_weighted_means(night_data, "night_air_temp")
# night_sd_results <-
#   calculate_weighted_sd(night_data, "night_air_temp")
# night_final <- cbind(night_results, night_sd_results[, -1])
# colnames(night_final)[2:5] <-
#   paste0(colnames(night_final)[2:5], "_mean")
# colnames(night_final)[6:9] <-
#   paste0(colnames(night_final)[6:9], "_sd")
# night_final <- night_final %>%  janitor::clean_names() %>%
#   select(variable, arid_mean, arid_sd, equatorial_mean, equatorial_sd, snow_mean, snow_sd, temperate_mean, temperate_sd)
# 
# ## Afternoon
# afternoon_results <-
#   calculate_weighted_means(afternoon_data, "afternoon_air_temp")
# afternoon_sd_results <-
#   calculate_weighted_sd(afternoon_data, "afternoon_air_temp")
# afternoon_final <-
#   cbind(afternoon_results, afternoon_sd_results[, -1])
# colnames(afternoon_final)[2:5] <-
#   paste0(colnames(afternoon_final)[2:5], "_mean")
# colnames(afternoon_final)[6:9] <-
#   paste0(colnames(afternoon_final)[6:9], "_sd")
# afternoon_final <- afternoon_final %>%  janitor::clean_names() %>%
#   select(variable, arid_mean, arid_sd, equatorial_mean, equatorial_sd, snow_mean, snow_sd, temperate_mean, temperate_sd)
# ## Morning
# morning_results <-
#   calculate_weighted_means(morning_data, "morning_air_temp")
# morning_sd_results <-
#   calculate_weighted_sd(morning_data, "morning_air_temp")
# morning_final <- cbind(morning_results, morning_sd_results[, -1])
# colnames(morning_final)[2:5] <-
#   paste0(colnames(morning_final)[2:5], "_mean")
# colnames(morning_final)[6:9] <-
#   paste0(colnames(morning_final)[6:9], "_sd")
# morning_final <- morning_final %>%  janitor::clean_names() %>%
#   select(variable, arid_mean, arid_sd, equatorial_mean, equatorial_sd, snow_mean, snow_sd, temperate_mean, temperate_sd)
# 
# 
# # Difference in means (morning)
# 
# ## ARID
# arid_data <- tract_df %>%
#   filter(climate_zone == "Arid") %>%
#   select(minority_morning_air_temp, white_morning_air_temp, population, geoid)
# 
# Arid_design <- svydesign(ids = ~geoid, weights = ~population, data = arid_data)
# 
# mean_minority <- svymean(~minority_morning_air_temp, arid_design, na.rm = TRUE)
# sd_minority <- svyvar(~minority_morning_air_temp, arid_design, na.rm = TRUE)
# mean_white <- svymean(~white_morning_air_temp, arid_design, na.rm = TRUE)
# sd_white <- svyvar(~white_morning_air_temp, arid_design, na.rm = TRUE)
# 
# mean_diff <- coef(mean_minority) - coef(mean_white)
# se_diff <- sqrt(coef(sd_minority)/sum(arid_data$population) + coef(sd_white)/sum(arid_data$population))
# 
# t_stat <- mean_diff / se_diff
# p_value <- 2 * pt(-abs(t_stat), df = sum(arid_data$population) - 1)
# 
# # Print the results
# cat("Climate Zone: Arid\n")
# cat("Time of Day: Morning\n")
# cat(sprintf("Population-Weighted Mean (Minority): %.3f\n", coef(mean_minority)))
# cat(sprintf("Population-Weighted SD (Minority): %.3f\n", sqrt(coef(sd_minority))))
# cat(sprintf("Population-Weighted Mean (White): %.3f\n", coef(mean_white)))
# cat(sprintf("Population-Weighted SD (White): %.3f\n", sqrt(coef(sd_white))))
# cat(sprintf("Population-Weighted Mean Difference: %.3f\n", mean_diff))
# cat(sprintf("Clustered SE: %.3f\n", se_diff))
# cat(sprintf("P-Value: %.3f\n", p_value))
# 
# ## SNOW
# # Select data for Snow climate zone
# snow_data <- tract_df %>%
#   filter(climate_zone == "Snow") %>%
#   select(minority_morning_air_temp, white_morning_air_temp, population, geoid)
# 
# # Create a survey design object
# snow_design <- svydesign(ids = ~geoid, weights = ~population, data = snow_data)
# 
# # Calculate population-weighted means and standard deviations
# mean_minority_snow <- svymean(~minority_morning_air_temp, snow_design, na.rm = TRUE)
# sd_minority_snow <- svyvar(~minority_morning_air_temp, snow_design, na.rm = TRUE)
# mean_white_snow <- svymean(~white_morning_air_temp, snow_design, na.rm = TRUE)
# sd_white_snow <- svyvar(~white_morning_air_temp, snow_design, na.rm = TRUE)
# 
# # Calculate the difference in means and its standard error
# mean_diff_snow <- coef(mean_minority_snow) - coef(mean_white_snow)
# se_diff_snow <- sqrt(coef(sd_minority_snow)/sum(snow_data$population) + coef(sd_white_snow)/sum(snow_data$population))
# 
# # Perform a t-test with clustered standard errors
# t_stat_snow <- mean_diff_snow / se_diff_snow
# p_value_snow <- 2 * pt(-abs(t_stat_snow), df = sum(snow_data$population) - 1)
# 
# # Print the results
# cat("Climate Zone: Snow\n")
# cat("Time of Day: Morning\n")
# cat(sprintf("Population-Weighted Mean (Minority): %.3f\n", coef(mean_minority_snow)))
# cat(sprintf("Population-Weighted SD (Minority): %.3f\n", sqrt(coef(sd_minority_snow))))
# cat(sprintf("Population-Weighted Mean (White): %.3f\n", coef(mean_white_snow)))
# cat(sprintf("Population-Weighted SD (White): %.3f\n", sqrt(coef(sd_white_snow))))
# cat(sprintf("Population-Weighted Mean Difference: %.3f\n", mean_diff_snow))
# cat(sprintf("Clustered SE: %.3f\n", se_diff_snow))
# cat(sprintf("P-Value: %.3f\n", p_value_snow))
# 
# ## Equatorial
# # Select data for Equatorial climate zone
# equatorial_data <- tract_df %>%
#   filter(climate_zone == "Equatorial") %>%
#   select(minority_morning_air_temp, white_morning_air_temp, population, geoid)
# 
# # Create a survey design object
# equatorial_design <- svydesign(ids = ~geoid, weights = ~population, data = equatorial_data)
# 
# # Calculate population-weighted means and standard deviations
# mean_minority_equatorial <- svymean(~minority_morning_air_temp, equatorial_design, na.rm = TRUE)
# sd_minority_equatorial <- svyvar(~minority_morning_air_temp, equatorial_design, na.rm = TRUE)
# mean_white_equatorial <- svymean(~white_morning_air_temp, equatorial_design, na.rm = TRUE)
# sd_white_equatorial <- svyvar(~white_morning_air_temp, equatorial_design, na.rm = TRUE)
# 
# # Calculate the difference in means and its standard error
# mean_diff_equatorial <- coef(mean_minority_equatorial) - coef(mean_white_equatorial)
# se_diff_equatorial <- sqrt(coef(sd_minority_equatorial)/sum(equatorial_data$population) + coef(sd_white_equatorial)/sum(equatorial_data$population))
# 
# # Perform a t-test with clustered standard errors
# t_stat_equatorial <- mean_diff_equatorial / se_diff_equatorial
# p_value_equatorial <- 2 * pt(-abs(t_stat_equatorial), df = sum(equatorial_data$population) - 1)
# 
# # Print the results
# cat("Climate Zone: Equatorial\n")
# cat("Time of Day: Morning\n")
# cat(sprintf("Population-Weighted Mean (Minority): %.3f\n", coef(mean_minority_equatorial)))
# cat(sprintf("Population-Weighted SD (Minority): %.3f\n", sqrt(coef(sd_minority_equatorial))))
# cat(sprintf("Population-Weighted Mean (White): %.3f\n", coef(mean_white_equatorial)))
# cat(sprintf("Population-Weighted SD (White): %.3f\n", sqrt(coef(sd_white_equatorial))))
# cat(sprintf("Population-Weighted Mean Difference: %.3f\n", mean_diff_equatorial))
# cat(sprintf("Clustered SE: %.3f\n", se_diff_equatorial))
# cat(sprintf("P-Value: %.3f\n", p_value_equatorial))
# 
# ## TEMPERATURE
# # Select data for Temperate climate zone
# temperate_data <- tract_df %>%
#   filter(climate_zone == "Temperate") %>%
#   select(minority_morning_air_temp, white_morning_air_temp, population, geoid)
# 
# # Create a survey design object
# temperate_design <- svydesign(ids = ~geoid, weights = ~population, data = temperate_data)
# 
# # Calculate population-weighted means and standard deviations
# mean_minority_temperate <- svymean(~minority_morning_air_temp, temperate_design, na.rm = TRUE)
# sd_minority_temperate <- svyvar(~minority_morning_air_temp, temperate_design, na.rm = TRUE)
# mean_white_temperate <- svymean(~white_morning_air_temp, temperate_design, na.rm = TRUE)
# sd_white_temperate <- svyvar(~white_morning_air_temp, temperate_design, na.rm = TRUE)
# 
# # Calculate the difference in means and its standard error
# mean_diff_temperate <- coef(mean_minority_temperate) - coef(mean_white_temperate)
# se_diff_temperate <- sqrt(coef(sd_minority_temperate)/sum(temperate_data$population) + coef(sd_white_temperate)/sum(temperate_data$population))
# 
# # Perform a t-test with clustered standard errors
# t_stat_temperate <- mean_diff_temperate / se_diff_temperate
# p_value_temperate <- 2 * pt(-abs(t_stat_temperate), df = sum(temperate_data$population) - 1)
# 
# # Print the results
# cat("Climate Zone: Temperate\n")
# cat("Time of Day: Morning\n")
# cat(sprintf("Population-Weighted Mean (Minority): %.3f\n", coef(mean_minority_temperate)))
# cat(sprintf("Population-Weighted SD (Minority): %.3f\n", sqrt(coef(sd_minority_temperate))))
# cat(sprintf("Population-Weighted Mean (White): %.3f\n", coef(mean_white_temperate)))
# cat(sprintf("Population-Weighted SD (White): %.3f\n", sqrt(coef(sd_white_temperate))))
# cat(sprintf("Population-Weighted Mean Difference: %.3f\n", mean_diff_temperate))
# cat(sprintf("Clustered SE: %.3f\n", se_diff_temperate))
# cat(sprintf("P-Value: %.3f\n", p_value_temperate))
# 
# ## OVERALL
# # Select data
# overall_data <- tract_df %>%
#   select(minority_morning_air_temp, white_morning_air_temp, population, geoid)
# 
# # Create a survey design object
# overall_design <- svydesign(ids = ~geoid, weights = ~population, data = overall_data)
# 
# # Calculate population-weighted means and standard deviations
# mean_minority_overall <- svymean(~minority_morning_air_temp, overall_design, na.rm = TRUE)
# sd_minority_overall <- svyvar(~minority_morning_air_temp, overall_design, na.rm = TRUE)
# mean_white_overall <- svymean(~white_morning_air_temp, overall_design, na.rm = TRUE)
# sd_white_overall <- svyvar(~white_morning_air_temp, overall_design, na.rm = TRUE)
# 
# # Calculate the difference in means and its standard error
# mean_diff_overall <- coef(mean_minority_overall) - coef(mean_white_overall)
# se_diff_overall <- sqrt(coef(sd_minority_overall)/sum(overall_data$population) + coef(sd_white_overall)/sum(overall_data$population))
# 
# # Perform a t-test with clustered standard errors
# t_stat_overall <- mean_diff_overall / se_diff_overall
# p_value_overall <- 2 * pt(-abs(t_stat_overall), df = sum(overall_data$population) - 1)
# 
# # Print the results
# cat("Overall (not by climate zone)\n")
# cat("Time of Day: Morning\n")
# cat(sprintf("Population-Weighted Mean (Minority): %.3f\n", coef(mean_minority_overall)))
# cat(sprintf("Population-Weighted SD (Minority): %.3f\n", sqrt(coef(sd_minority_overall))))
# cat(sprintf("Population-Weighted Mean (White): %.3f\n", coef(mean_white_overall)))
# cat(sprintf("Population-Weighted SD (White): %.3f\n", sqrt(coef(sd_white_overall))))
# cat(sprintf("Population-Weighted Mean Difference: %.3f\n", mean_diff_overall))
# cat(sprintf("Clustered SE: %.3f\n", se_diff_overall))
# cat(sprintf("P-Value: %.3f\n", p_value_overall))
# 


