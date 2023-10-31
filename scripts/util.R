## Load Libraries
library(tidyverse)
library(rlang)
library(patchwork)
library(janitor)
library(survey)
library(sandwich)

# ENSURE DIRECTORY ----
## Create directory if it doesn't exist
ensure_directory <- function(directory) {
  if (!dir.exists(directory)) {
    dir.create(directory)
    
  }
}

## Univariate plots
create_map <- function(data, variable, title, palette = "plasma") {
  # Check if the variable exists in the data
  if (!variable %in% names(data)) {
    stop(paste("Variable", variable, "not found in the data."))
  }
  
  # Check if the variable is continuous or discrete
  is_continuous <-
    is.numeric(data[[variable]]) && !is.factor(data[[variable]])
  
  # Create the map
  p <- ggplot() +
    geom_sf(data = data,
            aes_string(fill = variable),
            color = NA) +
    theme_void() +
    theme(legend.position = "bottom") +
    labs(title = title) +
    geom_sf(
      data = data,
      fill = NA,
      color = "black",
      size = 0.1
    ) +
    facet_wrap( ~ state, ncol = 3)
  
  # Apply the appropriate color scale
  if (is_continuous) {
    p <-
      p + scale_fill_viridis_c(option = palette,
                               direction = -1,
                               name = title)
  } else {
    p <-
      p + scale_fill_viridis_d(option = palette,
                               direction = -1,
                               name = title)
  }
  
  return(p)
}

# KDensity Plots (Figure 1)
plot_temperature_distributions <-
  function(data,
           temp_var_prefix_1,
           temp_var_prefix_2) {
    p1 <- ggplot(data) +
      geom_density(aes_string(
        x = paste0(temp_var_prefix_1, "_night_air_temp"),
        fill = "'Group 1'"
      ), alpha = 0.5) +
      geom_density(aes_string(
        x = paste0(temp_var_prefix_2, "_night_air_temp"),
        fill = "'Group 2'"
      ), alpha = 0.5) +
      labs(
        title = "Density Plot of Night Air Temperature",
        x = "Night Air Temperature",
        y = "Density",
        fill = NULL
      ) +
      theme_minimal() +
      theme(legend.position = "none")
    
    p2 <- ggplot(data) +
      geom_density(aes_string(
        x = paste0(temp_var_prefix_1, "_afternoon_air_temp"),
        fill = "'Group 1'"
      ), alpha = 0.5) +
      geom_density(aes_string(
        x = paste0(temp_var_prefix_2, "_afternoon_air_temp"),
        fill = "'Group 2'"
      ), alpha = 0.5) +
      labs(
        title = "Density Plot of Afternoon Air Temperature",
        x = "Afternoon Air Temperature",
        y = "Density",
        fill = NULL
      ) +
      theme_minimal() +
      theme(legend.position = "none")
    
    p3 <- ggplot(data) +
      geom_density(aes_string(
        x = paste0(temp_var_prefix_1, "_morning_air_temp"),
        fill = "'Group 1'"
      ), alpha = 0.5) +
      geom_density(aes_string(
        x = paste0(temp_var_prefix_2, "_morning_air_temp"),
        fill = "'Group 2'"
      ), alpha = 0.5) +
      labs(
        title = "Density Plot of Morning Air Temperature",
        x = "Morning Air Temperature",
        y = "Density",
        fill = NULL
      ) +
      theme_minimal() +
      theme(legend.position = "none")
    
    p_combined <- p3 / p2 / p1
    return(p_combined)
  }




## Kolm-Pollack ----
# functions from: https://github.com/urutau-nz/kolmpollak-R/blob/master/R/kolmpollak.Rhttps://github.com/urutau-nz/kolmpollak-R/blob/master/R/kolmpollak.R

ede <- function(a,
                epsilon = NULL,
                kappa = NULL,
                weights = NULL)
{
  if (is.null(kappa)) {
    if (is.null(epsilon)) {
      stop("you must provide either an epsilon or kappa aversion parameter")
    }
    kappa <- calc_kappa(a, epsilon, weights)
  }
  if (is.null(weights)) {
    ede_sum <- sum(exp(a * -kappa))
    N <- length(a)
  } else{
    ede_sum <- sum(exp(a * -kappa) * weights)
    N <- sum(weights)
  }
  (-1 / kappa) * log(ede_sum / N)
}

index <- function(a,
                  epsilon = NULL,
                  kappa = NULL,
                  weights = NULL)
{
  if (is.null(weights)) {
    x_mean <- mean(a)
  } else{
    x_mean <- sum(a * weights) / sum(weights)
  }
  ede(a,
      epsilon = epsilon,
      kappa = kappa,
      weights = weights) - x_mean
}

calc_kappa <- function(a, epsilon, weights = NULL)
{
  if (is.null(weights)) {
    x_sum <- sum(a)
    x_sq_sum <- sum(a ** 2)
  } else {
    x_sum <- sum(a * weights)
    x_sq_sum <- sum((a ** 2) * weights)
  }
  epsilon * (x_sum / x_sq_sum)
}

# Correlations ----
# Function to calculate correlations for the three temperature variables
calculate_correlation <- function(var_name) {
  df_selected %>%
    summarise(
      night_air_temp_cor = cor(.data[[var_name]], night_air_temp, use = "complete.obs"),
      afternoon_air_temp_cor = cor(.data[[var_name]], afternoon_air_temp, use = "complete.obs"),
      morning_air_temp_cor = cor(.data[[var_name]], morning_air_temp, use = "complete.obs")
    )
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
  
  # Calculate weights
  data$weight_minority <- data$population_minority / data$population
  data$weight_white <- 1 - data$weight_minority
  
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
      arid_mean,
      arid_sd,
      equatorial_mean,
      equatorial_sd,
      snow_mean,
      snow_sd,
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
      total_population = sum(population),
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
  if (!all(c(temp_var_minority, temp_var_white, "population", "county") %in% colnames(data))) {
    cat("One or more of the required columns do not exist in the data.\n")
    return()
  }
  
  # Select relevant columns
  analysis_data <- data %>%
    select(all_of(c(temp_var_minority, temp_var_white, "population", "county")))
  
  # Create a survey design object
  design <-
    svydesign(ids = ~ county,
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
  cat(sprintf(
    "Population-Weighted Mean (Minority): %.3f\n",
    coef(mean_minority)
  ))
  cat(sprintf("Population-Weighted SD (Minority): %.3f\n", sqrt(coef(sd_minority))))
  cat(sprintf(
    "Population-Weighted Mean (White): %.3f\n",
    coef(mean_white)
  ))
  cat(sprintf("Population-Weighted SD (White): %.3f\n", sqrt(coef(sd_white))))
  cat(sprintf("Population-Weighted Mean Difference: %.3f\n", mean_diff))
  cat(sprintf("Clustered SE: %.3f\n", se_diff))
  cat(sprintf("T-Statistic: %.3f\n", t_stat))
  cat(sprintf("P-Value: %.3f\n", p_value), "\n\n")
}


