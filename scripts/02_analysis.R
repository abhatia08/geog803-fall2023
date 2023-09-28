# SETUP ----

## 1. Load required packages ----
library(here)
library(tidyverse)
library(janitor)
library(sf)
library(FactoMineR)


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

eig.val<- eig.val %>%
  as.data.frame() %>%
  rownames_to_column("Principal Component") %>% janitor::clean_names() 
eig.val$principal_component <- as.numeric(str_remove(eig.val$principal_component, "comp "))

ggplot(data = eig.val, aes(x = principal_component, y = eigenvalue)) +
  geom_line() +
  geom_point(size = 4) +
  ggtitle("Scree plot") +
  xlab("Principal Component") +
  ylab("Eigenvalue") +
  theme_minimal()


