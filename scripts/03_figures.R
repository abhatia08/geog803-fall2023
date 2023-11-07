# SETUP ----

## 1. Load required packages ----
library(here)
library(tidyverse)
library(janitor)
library(sf)
library(ggExtra)
library(maps)
library(patchwork)
library(viridis)
library(cowplot)
library(biscale)

## 2. Set working directory ----
here::i_am("scripts/03_figures.R")

## 3. Run Util.R ----
source(here::here("scripts", "util.R"))

## 4. Create necessary directories ----
ensure_directory(here::here("derived_data"))
ensure_directory(here::here("figures"))
ensure_directory(here::here("figures", "maps"))

## 5. Load data ----
data_path <- here("derived_data", "merged_data.csv")
if (!file.exists(data_path)) {
  source(here("scripts", "01_dataprocessing.R"))
}
df <- read_csv(data_path) %>% as.data.frame()
city_df <- read_csv(here("derived_data", "city_data.csv"))
tract_df <- read_csv(here("derived_data", "tract_data.csv"))


# 03. FIGURES ----
## 1. Figure 1- Kernel Density ----
## Diagnostic, the latter is "blue"
plot_temperature_distributions(city_df, "under_5", "over_5")

plot_temperature_distributions(city_df, "minority", "white")

plot_temperature_distributions(tract_df, "disability", "no_disability")

plot_temperature_distributions(tract_df, "disability", "no_disability")



## 2. Maps ----
### Create merged spatial file ----
tract_boundaries <-
  st_read(here(
    "source_data",
    "cb_2019_us_tract_500k.shp"
  )) %>%
  st_transform(crs = 4326)

df <- df %>% mutate(geoid = as.character(geoid))
df_shp <- left_join(tract_boundaries, df, by = c("GEOID" = "geoid"))
rm(tract_boundaries)

# Subset for North Carolina
# df_shp <- df_shp %>% filter(state == "North Carolina")
# df <- df %>% filter(state == "North Carolina")

### Univariate map ----
create_map(
  df_shp,
  "coverage",
  "Vacant Houses PCT"
)

### Create Bivariate Plots using biscale ----
data <-
  bi_class(
    df_shp,
    x = population,
    y = percent_tree_canopy,
    style = "quantile",
    dim = 3
  )

map <- ggplot() +
  geom_sf(data = data, mapping = aes(fill = bi_class), color = "black", size = 0.1, show.legend = FALSE) +
  bi_scale_fill(pal = "GrPink", dim = 3) +
  labs(
    title = "Surface Temp and Tree cover",
  ) +
  bi_theme() +
  theme(plot.title = element_text(size = 15))


legend <- bi_legend(pal = "GrPink2",
                    dim = 3,
                    xlab = "Higher Temp ",
                    ylab = "Higher Tree Cover ",
                    size = 7)

finalPlot <- ggdraw() +
  draw_plot(map, 0, 0, 1, 1) +
  draw_plot(legend, 0.2, 0, 0.2, 0.2)

finalPlot
