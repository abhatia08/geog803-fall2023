# SETUP ----

## 1. Load required packages ----
library(here)
library(tidyverse)
library(janitor)
library(sf)
library(gtsummary)

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

## 1. Basic Descriptives ----

### a. Generate a table 1 ----

# Formatting

df <- df %>%
  mutate(median_income = factor(
    median_income,
    levels = c(
      "Under 15,000",
      "15,000 to 74,999",
      "75,000 to 149,999",
      "150,000 to 249,999",
      "250,000 and over"
    )
  ))

# Filter the dataframe to keep only the desired columns
selected_vars <- c("vacant_houses_pct", "houses_single_occupancy_pct", "utility_gas_pct", 
                   "bottled_gas_pct", "electricity_pct", "fuel_oil_pct", "coal_pct", 
                   "other_fuels_pct", "no_fuel_pct", "median_income", 
                   "population_below_poverty_pct", "education_below_hs_pct", "population", 
                   "population_65_pct", "population_white_pct", "population_minority_pct", "population_disability_pct", "tree_canopy_pct", )

df_selected <- df %>% select(all_of(selected_vars))

# Calculate majority counts and percentages for each variable
majority_stats <- lapply(selected_vars, function(var) {
  n_majority <- sum(df[[var]] > 50, na.rm = TRUE)
  n_total <- length(df[[var]])
  percent_majority <- (n_majority / n_total) * 100
  return(list(count = n_majority, percentage = percent_majority))
})

# Convert the list to a dataframe
majority_df <- as.data.frame(do.call(rbind, majority_stats))
row.names(majority_df) <- selected_vars

# Add a formatted percentage column
majority_df$percentage_formatted <- sprintf("%d (%.1f%%)", majority_df$count, majority_df$percentage)

# View the dataframe
print(majority_df)

# If you want to further format this dataframe into a table, you can use the `gt` or `kable` packages.

# Descriptive statistics table for the selected columns in dataframe "df"
table_df <- df %>%
  tbl_summary(
    by = NULL,
    statistic = list(
      all_of(selected_vars) ~ majority_fun
    ),
    label = list(
      vacant_houses_pct = "Percent of Tracts with Majority Vacant Houses",
      houses_single_occupancy_pct = "Percent of Tracts with Majority Single Occupancy Houses",
      utility_gas_pct = "Percent of Tracts with Majority Utility Gas Houses",
      bottled_gas_pct = "Percent of Tracts with Majority Bottled Gas Houses",
      electricity_pct = "Percent of Tracts with Majority Electricity Houses",
      fuel_oil_pct = "Percent of Tracts with Majority Fuel Oil Houses",
      coal_pct = "Percent of Tracts with Majority Coal Houses",
      other_fuels_pct = "Percent of Tracts with Majority Other Fuels Houses",
      no_fuel_pct = "Percent of Tracts with Majority No Fuel Houses",
      median_income = "Percent of Tracts with Majority Median Income",
      population_below_poverty_pct = "Percent of Tracts with Majority Population Below Poverty",
      education_below_hs_pct = "Percent of Tracts with Majority Population with Education Below High School",
      population = "Percent of Tracts with Majority Population",
      population_65_pct = "Percent of Tracts with Majority Population Aged 65 and Above",
      population_white_pct = "Percent of Tracts with Majority White Population",
      population_minority_pct = "Percent of Tracts with Majority Minority Population"
    ),
    missing = "no"
  ) %>%
  modify_caption("**Characteristics of sampled tracts**")



## 2. Assocations ----


## Kolm-Pollack


# Clean the median_income column
df$median_income <-
  as.numeric(gsub("[^0-9.]", "", df$median_income))

# Process percentage columns
pct_cols <- grep("_pct$", colnames(df), value = TRUE)
for (col in pct_cols) {
  if (max(df[[col]], na.rm = TRUE) <= 1) {
    df[[col]] <- round(df[[col]] * 100, 1)
  } else {
    df[[col]] <- round(df[[col]], 1)
  }
}

# Create a binary column for white and non-white groups
threshold <- 50
df$group <-
  ifelse(df$population_white_pct > threshold, "white", "non_white")

# Compute Kolm-Pollack Inequality Index
## Compute mean temperature for each group
group_means <- df %>%
  group_by(group) %>%
  summarise(mean_temp = mean(night_air_temp, na.rm = TRUE))

print(group_means)

# Compute average absolute deviation from the mean for each group
group_deviation <- df %>%
  left_join(group_means, by = "group") %>%
  group_by(group) %>%
  summarise(avg_abs_deviation = mean(abs(night_air_temp - mean_temp), na.rm = TRUE))

print(group_deviation)

# Compute Kolm-Pollack index for each group
indices <- group_deviation %>%
  left_join(group_means, by = "group") %>%
  mutate(kolm_pollack_index = avg_abs_deviation / mean_temp) %>%
  select(group, kolm_pollack_index)

print(indices)

# Comparison
difference <- diff(indices$kolm_pollack_index)
print(paste(
  "Difference in Kolm-Pollack indices between groups:",
  round(difference, 4)
))

## P val
# Define the compute_kolm_pollack function
compute_kolm_pollack <- function(data, temp_col) {
  mean_temp <- mean(data[[temp_col]], na.rm = TRUE)
  avg_abs_deviation <-
    mean(abs(data[[temp_col]] - mean_temp), na.rm = TRUE)
  return(avg_abs_deviation / mean_temp)
}

# Compute the observed difference
observed_difference <- diff(
  df %>%
    group_by(group) %>%
    summarise(kolm_pollack_index = compute_kolm_pollack(., "night_air_temp"))$kolm_pollack_index
)

# Permutation test
n_permutations <- 10000
permuted_differences <- numeric(n_permutations)

for (i in 1:n_permutations) {
  permuted_data <- df %>%
    mutate(group = sample(group, size = n(), replace = FALSE))
  
  permuted_indices <- permuted_data %>%
    group_by(group) %>%
    summarise(kolm_pollack_index = compute_kolm_pollack(., "night_air_temp"))
  
  permuted_differences[i] <-
    diff(permuted_indices$kolm_pollack_index)
}

# Compute p-value
p_value <-
  mean(abs(permuted_differences) >= abs(observed_difference))

print(paste("p-value:", p_value))


## 3. PCA ----
# Subset for North Carolina
df <- df %>% filter(state == "North Carolina")


# 1. PCA on the dataframe
# Exclude non-numeric columns and the "air temperature" column
df_numeric <-
  df %>% select(
    -c(
      geoid,
      state,
      name,
      county,
      night_mean_air_temperature,
      morning_mean_air_temperature,
      afternoon_mean_air_temperature
    )
  ) %>% select_if(is.numeric)

# Scale the data
df_scaled <- scale(df_numeric)

# Perform PCA
pca_result <- PCA(df_scaled, graph = FALSE)

# 2. Examine correlations between variables in each PC
biplot(pca_result$eig, pca_result$var, cex = 0.7)

# 3. Correlation of each PC with "air temperature"
# Extract the scores of the principal components
pc_scores <- as.data.frame(pca_result$ind$coord)

# Compute the correlation with "air temperature"
correlations <-
  sapply(pc_scores, function(pc)
    cor(pc, df$afternoon_mean_air_temperature))

# Print the correlations
print(correlations)


# 1. Scree plot
eig.val <- pca_result$eig

eig.val <- eig.val %>%
  as.data.frame() %>%
  rownames_to_column("Principal Component") %>% janitor::clean_names()
eig.val$principal_component <-
  as.numeric(str_remove(eig.val$principal_component, "comp "))

ggplot(data = eig.val, aes(x = principal_component, y = eigenvalue)) +
  geom_line() +
  geom_point(size = 4) +
  ggtitle("Scree plot") +
  xlab("Principal Component") +
  ylab("Eigenvalue") +
  theme_minimal()
