## Load Libraries
library(tidyverse)
library(rlang)
library(patchwork)
library(janitor)
library(survey)

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

# # Weighted Means ----
# # Function to calculate weighted means
# calculate_weighted_means <- function(data, time_of_day) {
#   results <- data %>%
#     group_by(climate_zone) %>%
#     summarise(across(ends_with(time_of_day),
#                      ~ round(
#                        sum(. * population, na.rm = TRUE) / sum(population, na.rm = TRUE),
#                        3
#                      ),
#                      .names = "mean_{.col}"),
#               .groups = 'drop')
#
# # Calculate the total population-weighted mean
# total <- summarise(data,
#                    across(ends_with(time_of_day),
#                           ~ round(
#                             sum(. * population, na.rm = TRUE) / sum(population, na.rm = TRUE),
#                             3
#                           ),
#                           .names = "mean_{.col}")) %>%
#   mutate(climate_zone = "Total")
#
# # Combine the results
# results <- bind_rows(total, results)
#
#   # Transpose the dataframe, set the first row as column names, and remove the time_of_day row
#   results_transposed <- as.data.frame(t(results))
#   colnames(results_transposed) <- results_transposed[1,]
#   results_transposed <- results_transposed[-1, , drop = FALSE]
#
#   # Remove the prefix and suffix from the row names
#   new_row_names <- gsub("^mean_", "", rownames(results_transposed))
#   new_row_names <-
#     gsub(paste0("_", time_of_day, "$"), "", new_row_names)
#   rownames(results_transposed) <- new_row_names
#
#   results_transposed$variable <- rownames(results_transposed)
#   rownames(results_transposed) <- NULL
#
#   results_transposed <-
#     results_transposed %>% select(c(variable), everything())
#   return(results_transposed)
# }
#
# # Function to calculate weighted standard deviations
# calculate_weighted_sd <- function(data, time_of_day) {
#   means <- calculate_weighted_means(data, time_of_day)
#
#   results_sd <- data %>%
#     group_by(climate_zone) %>%
#     summarise(across(ends_with(time_of_day),
#                      ~ {
#                        variable_name <- sub(paste0("_", time_of_day, "$"), "", .col)
#                        mean_value <- means[means$climate_zone == as.character(.by_group), paste0("mean_", variable_name)]
#                        population_weighted_sd <- sqrt(sum(population * (. - mean_value) ^ 2, na.rm = TRUE) / sum(population, na.rm = TRUE))
#                        round(population_weighted_sd, 3)
#                      },
#                      .names = "sd_{.col}"),
#               .groups = 'drop') %>%
#     ungroup()
#   #
# # Calculate the total population-weighted standard deviation
# total_sd <- summarise(data,
#                       across(ends_with(time_of_day),
#                              ~ {
#                                variable_name <- sub(paste0("_", time_of_day, "$"), "", .col)
#                                mean_value <- means[means$climate_zone == "Total", paste0("mean_", variable_name)]
#                                population_weighted_sd <- sqrt(sum(population * (. - mean_value) ^ 2, na.rm = TRUE) / sum(population, na.rm = TRUE))
#                                round(population_weighted_sd, 3)
#                              },
#                              .names = "sd_{.col}")) %>%
#   mutate(climate_zone = "Total")
#
# # Combine the results
# results_sd <- bind_rows(total_sd, results_sd)
#   # Transpose the dataframe, set the first row as column names, and remove the time_of_day row
#   results_sd_transposed <- as.data.frame(t(results_sd))
#   colnames(results_sd_transposed) <- results_sd_transposed[1,]
#   results_sd_transposed <-
#     results_sd_transposed[-1, , drop = FALSE]
#
#   # Remove the prefix and suffix from the row names
#   new_row_names <-
#     gsub("^sd_", "", rownames(results_sd_transposed))
#   new_row_names <-
#     gsub(paste0("_", time_of_day, "$"), "", new_row_names)
#   rownames(results_sd_transposed) <- new_row_names
#
#   results_sd_transposed$variable <-
#     rownames(results_sd_transposed)
#   rownames(results_sd_transposed) <- NULL
#
#   results_sd_transposed <-
#     results_sd_transposed %>% select(c(variable), everything())
#   return(results_sd_transposed)
# }

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

# T-tests ----
library(dplyr)
library(tidyr)
library(survey)

# Define the function
calculate_weighted_t_test <-
  function(data,
           climate_zone,
           time_of_day,
           term1,
           term2) {
    # Generate column names
    col1 <- paste0(term1, time_of_day, "_air_temp")
    col2 <- paste0(term2, time_of_day, "_air_temp")
    
    # Select the appropriate data based on the climate zone
    selected_data <- if (climate_zone == "all") {
      data
    } else {
      data %>% filter(climate_zone == climate_zone)
    }
    
    # Check if there are enough levels to proceed
    if (length(unique(selected_data$geoid)) < 2) {
      cat("Not enough levels to perform the analysis. Need at least 2 different geoids.\n")
      return()
    }
    
    # Ensure the selected columns exist in the data
    if (!all(c(col1, col2) %in% colnames(selected_data))) {
      cat("One or more of the specified columns do not exist in the data.\n")
      return()
    }
    
    # Select relevant columns
    analysis_data <- selected_data %>%
      select(all_of(c(col1, col2, "population", "geoid")))
    
    # Select relevant columns
    analysis_data <- selected_data %>%
      select(
        paste0(term1, time_of_day, "_air_temp"),
        paste0(term2, time_of_day, "_air_temp"),
        population,
        geoid
      ) %>%
      rename(term1 = starts_with(term1),
             term2 = starts_with(term2))
    
    # Create a survey design object
    design <-
      svydesign(ids = ~ geoid,
                weights = ~ population,
                data = analysis_data)
    
    # Calculate population-weighted means and standard deviations
    mean_term1 <- svymean( ~ term1, design, na.rm = TRUE)
    sd_term1 <- svyvar( ~ term1, design, na.rm = TRUE)
    mean_term2 <- svymean( ~ term2, design, na.rm = TRUE)
    sd_term2 <- svyvar( ~ term2, design, na.rm = TRUE)
    
    # Calculate the difference in means and its standard error
    mean_diff <- coef(mean_term1) - coef(mean_term2)
    se_diff <-
      sqrt(
        coef(sd_term1) / sum(analysis_data$population) + coef(sd_term2) / sum(analysis_data$population)
      )
    
    # Perform a t-test with clustered standard errors
    t_stat <- mean_diff / se_diff
    p_value <-
      2 * pt(-abs(t_stat), df = sum(analysis_data$population) - 1)
    
    # Print the results
    cat("Climate Zone:",
        ifelse(climate_zone == "all", "Total", climate_zone),
        "\n")
    cat("Time of Day:", time_of_day, "\n")
    cat(sprintf(
      "Population-Weighted Mean (%s): %.3f\n",
      term1,
      coef(mean_term1)
    ))
    cat(sprintf("Population-Weighted SD (%s): %.3f\n", term1, sqrt(coef(sd_term1))))
    cat(sprintf(
      "Population-Weighted Mean (%s): %.3f\n",
      term2,
      coef(mean_term2)
    ))
    cat(sprintf("Population-Weighted SD (%s): %.3f\n", term2, sqrt(coef(sd_term2))))
    cat(sprintf("Population-Weighted Mean Difference: %.3f\n", mean_diff))
    cat(sprintf("Clustered SE: %.3f\n", se_diff))
    cat(sprintf("T-Statistic: %.3f\n", t_stat))
    cat(sprintf("P-Value: %.3f\n", p_value), "\n\n")
  }
