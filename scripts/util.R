## Load Libraries
library(tidyverse)
library(rlang)
library(janitor)
library(survey)
library(ejanalysis)

# ENSURE DIRECTORY ----
## Create directory if it doesn't exist
ensure_directory <- function(directory) {
  if (!dir.exists(directory)) {
    dir.create(directory)
    
  }
}


# Weighted Means ----
# Function to calculate weighted means
calculate_weighted_means <- function(data, time_of_day) {
  results <- data %>%
    group_by(climate_zone) %>%
    summarise(across(ends_with(time_of_day),
                     ~ round(
                       sum(. * population, na.rm = TRUE) / sum(population, na.rm = TRUE),
                       3
                     ),
                     .names = "mean_{.col}"),
              .groups = 'drop')
  
  # Transpose the dataframe, set the first row as column names, and remove the time_of_day row
  results_transposed <- as.data.frame(t(results))
  colnames(results_transposed) <- results_transposed[1,]
  results_transposed <- results_transposed[-1, , drop = FALSE]
  
  # Remove the prefix and suffix from the row names
  new_row_names <- gsub("^mean_", "", rownames(results_transposed))
  new_row_names <-
    gsub(paste0("_", time_of_day, "$"), "", new_row_names)
  rownames(results_transposed) <- new_row_names
  
  results_transposed$variable <- rownames(results_transposed)
  rownames(results_transposed) <- NULL
  
  results_transposed <-
    results_transposed %>% select(c(variable), everything())
  return(results_transposed)
}

# Function to calculate weighted standard deviations
calculate_weighted_sd <- function(data, time_of_day) {
  means <- calculate_weighted_means(data, time_of_day)
  
  results_sd <- data %>%
    group_by(climate_zone) %>%
    summarise(across(ends_with(time_of_day),
                     ~ round(sqrt(
                       sum(population * (. - mean(.)) ^ 2, na.rm = TRUE) / sum(population, na.rm = TRUE)
                     ), 3),
                     .names = "sd_{.col}"),
              .groups = 'drop')
  
  # Transpose the dataframe, set the first row as column names, and remove the time_of_day row
  results_sd_transposed <- as.data.frame(t(results_sd))
  colnames(results_sd_transposed) <- results_sd_transposed[1,]
  results_sd_transposed <-
    results_sd_transposed[-1, , drop = FALSE]
  
  # Remove the prefix and suffix from the row names
  new_row_names <-
    gsub("^sd_", "", rownames(results_sd_transposed))
  new_row_names <-
    gsub(paste0("_", time_of_day, "$"), "", new_row_names)
  rownames(results_sd_transposed) <- new_row_names
  
  results_sd_transposed$variable <-
    rownames(results_sd_transposed)
  rownames(results_sd_transposed) <- NULL
  
  results_sd_transposed <-
    results_sd_transposed %>% select(c(variable), everything())
  return(results_sd_transposed)
}



compute_stats <- function(data, time_of_day, climate_zone = NULL) {
  # Ensure climate_zone and population are correctly formatted
  data$climate_zone <- as.factor(data$climate_zone)
  data$population <- as.numeric(data$population)
  
    if (time_of_day != "tract_df") {
    # Calculate weights
    data$weight_minority <- data$population_minority / data$population
    data$weight_white <- 1 - data$weight_minority
    } else {
      
      data$weight_minority <- 1
      data$weight_white <- 1 
    }
  
  # Construct variable names based on time of day
  temp_var <- paste0(time_of_day, "_air_temp")
  temp_var_minority <- paste0("minority_", time_of_day, "_air_temp")
  temp_var_white <- paste0("white_", time_of_day, "_air_temp")
  
  # Filter data based on climate zone (if provided)
  filtered_data <- data %>%
    select(dplyr::contains(temp_var), "climate_zone", "population") %>%
    filter(if (!is.null(climate_zone))
      climate_zone == climate_zone
      else
        TRUE)
  
  # Calculate population weighted means and standard deviations
  results <- calculate_weighted_means(filtered_data, temp_var)
  sd_results <- calculate_weighted_sd(filtered_data, temp_var)
  
  # Combine results and format output
  final_results <- cbind(results, sd_results[, -1])
  colnames(final_results)[2:5] <-
    paste0(colnames(final_results)[2:5], "_mean")
  colnames(final_results)[6:9] <-
    paste0(colnames(final_results)[6:9], "_sd")
  final_results <- final_results %>%
    janitor::clean_names() %>%
    select(
      variable,
      dry_mean,
      dry_sd,
      tropical_mean,
      tropical_sd,
      continental_mean,
      continental_sd,
      temperate_mean,
      temperate_sd
    )

  # Output 1
  cat("Stratified by Climate Zone:\n")
  print(final_results)
  
  # Calculate totals by climate zone including standard deviations
  totals_by_climate_zone <- data %>%
    group_by(climate_zone) %>%
    summarize(
      total_population = sum(population),
      total_temp_white = sum(get(temp_var_white) * population) / sum(population),
      total_temp_white_sd = sd(get(temp_var_white) * population, na.rm = TRUE),
      total_temp_minority = sum(get(temp_var_minority) * population) / sum(population),
      total_temp_minority_sd = sd(get(temp_var_minority) * population, na.rm = TRUE),
      total_temp = sum((
        get(temp_var_white) + get(temp_var_minority)
      ) / 2 * population) / sum(population),
      total_temp_sd = ifelse(n() == 1, 0, sd((population / sum(population)) * (get(temp_var_white) + get(temp_var_minority)) / 2, na.rm = TRUE
      )),
      .groups = 'drop'
    )
  
  # Output 2
  cat("\nTotals by Climate Zone:\n")
  totals_by_climate_zone %>%
    select(climate_zone, total_population, total_temp, total_temp_sd) %>%
    print()
  
  # Calculate total white temp standard deviation across climate zones
  mean_white_temp <-
    sum(data[[temp_var_white]] * data$population) / sum(data$population)
  sum_squared_diff <-
    sum(data$population * (data[[temp_var_white]] - mean_white_temp) ^ 2)
  total_white_temp_sd <-
    sqrt(sum_squared_diff / sum(data$population))
  
  # Calculate total minority temp standard deviation across climate zones
  mean_minority_temp <-
    sum(data[[temp_var_minority]] * data$population) / sum(data$population)
  sum_squared_diff <-
    sum(data$population * (data[[temp_var_minority]] - mean_minority_temp) ^
          2)
  total_minority_temp_sd <-
    sqrt(sum_squared_diff / sum(data$population))
  
  # Calculate totals across climate zones
  totals_across_climate_zones <- data %>%
    summarize(
      # total_population = sum(population),
      total_white_temp = mean_white_temp,
      total_white_temp_sd = total_white_temp_sd,
      total_minority_temp = mean_minority_temp,
      total_minority_temp_sd = total_minority_temp_sd
    )
  
  # Output 3: Totals for Minority and White
  cat("\nTotals for Minority and White:\n")
  print(totals_across_climate_zones)
  
  # Calculate total weighted mean and SD for Temperature, Ungrouped
  total_population <- sum(data$population, na.rm = TRUE)
  
  # Calculate weighted mean
  weighted_sum_minority <-
    sum(data[[temp_var_minority]] * data$population_minority, na.rm = TRUE)
  weighted_sum_white <-
    sum(data[[temp_var_white]] * (data$population - data$population_minority), na.rm = TRUE)
  total_weighted_mean_temp <-
    (weighted_sum_minority + weighted_sum_white) / total_population
  
  # Calculate weighted standard deviation
  deviation_squared_minority <-
    (data[[temp_var_minority]] - total_weighted_mean_temp) ^ 2
  deviation_squared_white <-
    (data[[temp_var_white]] - total_weighted_mean_temp) ^ 2
  weighted_deviation_squared_sum_minority <-
    sum(deviation_squared_minority * data$population_minority,
        na.rm = TRUE)
  weighted_deviation_squared_sum_white <-
    sum(deviation_squared_white * (data$population - data$population_minority),
        na.rm = TRUE)
  total_weighted_sd_temp <-
    sqrt((
      weighted_deviation_squared_sum_minority + weighted_deviation_squared_sum_white
    ) / total_population
    )
  
  # Output 4: Total Weighted Mean and SD for Temperature, Ungrouped
  cat("\nTotal Weighted Mean and SD for Temperature, Ungrouped:\n")
  cat("Total Weighted Mean:", round(total_weighted_mean_temp,3), "\n")
  cat("Total Weighted SD:", round(total_weighted_sd_temp,3), "\n")
  
}


calculate_weighted_t_test <- function(data, time_of_day) {
  # Ensure the population is correctly formatted
  data$population <- as.numeric(data$population)

  # Construct variable names based on time of day
  temp_var_minority <- paste0("minority_", time_of_day, "_air_temp")
  temp_var_white <- paste0("white_", time_of_day, "_air_temp")

  # Ensure the selected columns exist in the data
  if (!all(c(temp_var_minority, temp_var_white, "population", "geoid") %in% colnames(data))) {
    cat("One or more of the required columns do not exist in the data.\n")
    return()
  }

  # Select relevant columns
  analysis_data <- data %>%
    select(all_of(c(temp_var_minority, temp_var_white, "population", "geoid")))

  # Create a survey design object
  design <-
    svydesign(ids = ~ geoid,
              weights = ~ population,
              data = analysis_data)

  # Calculate population-weighted means and standard deviations
  mean_minority <- svymean( ~get(temp_var_minority), design, na.rm = TRUE)
  sd_minority <- svyvar( ~get(temp_var_minority), design, na.rm = TRUE)
  mean_white <- svymean( ~get(temp_var_white), design, na.rm = TRUE)
  sd_white <- svyvar( ~get(temp_var_white), design, na.rm = TRUE)

  # Calculate the difference in means and its standard error
  mean_diff <- coef(mean_minority) - coef(mean_white)
  se_diff <-
    sqrt(
      coef(sd_minority) / sum(analysis_data$population) + coef(sd_white) / sum(analysis_data$population)
    )

  # Perform a t-test with clustered standard errors
  t_stat <- mean_diff / se_diff
  p_value <-
    2 * pt(-abs(t_stat), df = sum(analysis_data$population) - 1)

  # Print the results
  cat("Time of Day:", time_of_day, "\n")
  cat(sprintf("Population-Weighted Mean Difference: %.3f\n", mean_diff))
  cat(sprintf("Population-Weighted SE Difference: %.3f\n", se_diff))
  cat(sprintf("P-Value: %.3f\n", p_value), "\n\n")

}



calculate_weighted_t_test_by_climate <- function(data, time_of_day) {
  # Ensure the population is correctly formatted
  data$population <- as.numeric(data$population)
  
  # Construct variable names based on time of day
  temp_var_minority <- paste0("minority_", time_of_day, "_air_temp")
  temp_var_white <- paste0("white_", time_of_day, "_air_temp")
  
  # Ensure the selected columns exist in the data
  if (!all(c(temp_var_minority, temp_var_white, "population", "geoid", "climate_zone") %in% colnames(data))) {
    cat("One or more of the required columns do not exist in the data.\n")
    return()
  }
  
  # Loop through each unique climate zone
  for(zone in unique(data$climate_zone)) {
    cat("Climate Zone:", zone, "\n")
    
    # Subset data for the current climate zone
    subset_data <- data[data$climate_zone == zone,]
    
    # If only one PSU, perform unweighted t-test
    if (length(unique(subset_data$geoid)) == 1) {
      if (length(na.omit(subset_data[[temp_var_minority]])) < 2 || length(na.omit(subset_data[[temp_var_white]])) < 2) {
        cat("Not enough observations for unweighted t-test.\n\n")
        next
      }
      t_test_result <- t.test(subset_data[[temp_var_minority]], subset_data[[temp_var_white]])
      cat("Unweighted T-Test due to single PSU:\n")
      cat(sprintf("T-Statistic: %.3f\n", t_test_result$statistic))
      cat(sprintf("P-Value: %.3f\n", t_test_result$p.value), "\n\n")
      next
    }
    
    # Select relevant columns
    analysis_data <- subset_data %>%
      select(all_of(c(temp_var_minority, temp_var_white, "population", "geoid")))
    
    # Create a survey design object
    design <-
      svydesign(ids = ~ geoid,
                weights = ~ population,
                data = analysis_data)
    
    # Calculate population-weighted means and standard deviations
    mean_minority <- svymean( ~get(temp_var_minority), design, na.rm = TRUE)
    sd_minority <- svyvar( ~get(temp_var_minority), design, na.rm = TRUE)
    mean_white <- svymean( ~get(temp_var_white), design, na.rm = TRUE)
    sd_white <- svyvar( ~get(temp_var_white), design, na.rm = TRUE)
    
    # Calculate the difference in means and its standard error
    mean_diff <- coef(mean_minority) - coef(mean_white)
    se_diff <-
      sqrt(
        coef(sd_minority) / sum(analysis_data$population) + coef(sd_white) / sum(analysis_data$population)
      )
    
    # Perform a t-test with clustered standard errors
    t_stat <- mean_diff / se_diff
    p_value <-
      2 * pt(-abs(t_stat), df = sum(analysis_data$population) - 1)
    
    # Print the results
    cat("Time of Day:", time_of_day, "\n")
     cat(sprintf("Population-Weighted Mean Difference: %.3f\n", mean_diff))
    cat(sprintf("Clustered SE: %.3f\n", se_diff))
    cat(sprintf("P-Value: %.3f\n", p_value), "\n\n")
  }
}



# Function to perform bootstrap and permutation test for means 
perform_analysis <- function(data_zone, zone_name) {
  # Bootstrap for Standard Deviation
  n_boot <- 1000
  boot_results <- numeric(n_boot)
  for (i in 1:n_boot) {
    boot_sample <- data_zone[sample(nrow(data_zone), replace = TRUE), ]
    boot_results[i] <- calc_weighted_mean_diff(boot_sample)
  }
  bootstrap_sd <- sd(boot_results)
  
  # Permutation Test for T-Test
  n_perm <- 1000
  perm_results <- numeric(n_perm)
  obs_diff <- calc_weighted_mean_diff(data_zone)
  for (i in 1:n_perm) {
    shuffled_minority <- sample(data_zone$population_minority)
    data_zone$population_minority <- shuffled_minority
    perm_results[i] <- calc_weighted_mean_diff(data_zone)
  }
  p_value <- mean(abs(perm_results) >= abs(obs_diff))
  
  # Print results
  cat(zone_name, "Zone\n")
  cat("Bootstrap SD of Weighted Mean Difference:", bootstrap_sd, "\n")
  cat("Permutation Test P-Value:", p_value, "\n\n")
}



perform_analysis_kolm <- function(data_zone, zone_name) {
  # Bootstrap for Standard Deviation
  n_boot <- 1000
  boot_results <- numeric(n_boot)
  for (i in 1:n_boot) {
    boot_sample <- data_zone[sample(nrow(data_zone), replace = TRUE), ]
    boot_results[i] <- calc_kp_diff(boot_sample)
  }
  bootstrap_sd <- sd(boot_results)
  
  # Permutation Test for T-Test
  n_perm <- 1000
  perm_results <- numeric(n_perm)
  obs_diff <- calc_kp_diff(data_zone)
  for (i in 1:n_perm) {
    shuffled_minority <- sample(data_zone$population_minority)
    data_zone$population_minority <- shuffled_minority
    perm_results[i] <- calc_kp_diff(data_zone)
  }
  p_value <- mean(abs(perm_results) >= abs(obs_diff))
  
  # Print results
  cat(zone_name, "Zone\n")
  cat("Bootstrap SD of Weighted Mean Difference:", bootstrap_sd, "\n")
  cat("Permutation Test P-Value:", p_value, "\n\n")
}


# Function to calculate weighted mean difference
calc_weighted_mean_diff <- function(df) {
  mean_minority <- sum(df$minority_afternoon_air_temp * df$population_minority) / sum(df$population_minority)
  mean_white <- sum(df$white_afternoon_air_temp * (df$population - df$population_minority)) / sum(df$population - df$population_minority)
  mean_minority - mean_white
}
# Function to calculate KolmPollack  difference
calc_kp_diff <- function(df) {
  kp_minority <- KolmPollak(df$minority_afternoon_air_temp,
                            bigbadx = TRUE,
                            na.rm = TRUE)
  kp_white <- KolmPollak(df$white_afternoon_air_temp,
                         bigbadx = TRUE,
                         na.rm = TRUE)
  kp_minority - kp_white
}