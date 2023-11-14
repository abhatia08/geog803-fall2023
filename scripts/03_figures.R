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


## Overall Boxplots
names <- c("Minority population", "White population")

boxplot(tract_df[,2:3], xlab = "All Tracts", ylab = "Night Temperature (F)", outline=FALSE, names=names, col = c("lightblue","lightgreen")) 
boxplot(tract_df[,4:5], xlab = "All Tracts", ylab = "Afternoon Temperature (F)", outline=FALSE, names=names, col = c("lightblue","lightgreen")) 
boxplot(tract_df[,6:7], xlab = "All Tracts", ylab = "Morning Temperature (F)", outline=FALSE, names=names, col = c("lightblue","lightgreen")) # morning



## split by climate zone

plot_temperature_distributions(tract_df, "minority", "white")

temperate <- filter(tract_df, climate_zone=="Temperate")
arid <- filter(tract_df, climate_zone=="Arid")
snow <- filter(tract_df, climate_zone=="Snow")
equatorial <- filter(tract_df, climate_zone=="Equatorial")

plot_temperature_distributions(temperate, "minority", "white")
plot_temperature_distributions(arid, "minority", "white")
plot_temperature_distributions(snow, "minority", "white")
plot_temperature_distributions(equatorial, "minority", "white")


## Boxplots
boxplot(temperate[,2:3], xlab = "Temperate", ylab = "Night Temperature (F)", outline=FALSE, names=names, col = c("lightblue","lightgreen")) # night
boxplot(temperate[,4:5], xlab = "Temperate", ylab = "Afternoon Temperature (F)", outline=FALSE, names=names, col = c("lightblue","lightgreen")) # afternoon
boxplot(temperate[,6:7], xlab = "Temperate", ylab = "Morning Temperature (F)", outline=FALSE, names=names, col = c("lightblue","lightgreen")) # morning

boxplot(arid[,2:3], xlab = "Arid", ylab = "Night Temperature (F)", outline=FALSE, names=names, col = c("lightblue","lightgreen")) # night
boxplot(arid[,4:5], xlab = "Arid", ylab = "Afternoon Temperature (F)", outline=FALSE, names=names, col = c("lightblue","lightgreen")) # afternoon
boxplot(arid[,6:7], xlab = "Arid", ylab = "Morning Temperature (F)", outline=FALSE, names=names, col = c("lightblue","lightgreen")) # morning

boxplot(snow[,2:3], xlab = "Snow", ylab = "Night Temperature (F)", outline=FALSE, names=names, col = c("lightblue","lightgreen")) # night
boxplot(snow[,4:5], xlab = "Snow", ylab = "Afternoon Temperature (F)", outline=FALSE, names=names, col = c("lightblue","lightgreen")) # afternoon
boxplot(snow[,6:7], xlab = "Snow", ylab = "Morning Temperature (F)", outline=FALSE, names=names, col = c("lightblue","lightgreen")) # morning

boxplot(equatorial[,2:3], xlab = "Equatorial", ylab = "Night Temperature (F)", outline=FALSE, names=names, col = c("lightblue","lightgreen")) # night
boxplot(equatorial[,4:5], xlab = "Equatorial", ylab = "Afternoon Temperature (F)", outline=FALSE, names=names, col = c("lightblue","lightgreen")) # afternoon
boxplot(equatorial[,6:7], xlab = "Equatorial", ylab = "Morning Temperature (F)", outline=FALSE, names=names, col = c("lightblue","lightgreen")) # morning



## split by region

south <- filter(tract_df, region=="South")
west <- filter(tract_df, region=="West")
midwest <- filter(tract_df, region=="Midwest")
north <- filter(tract_df, region=="North")

plot_temperature_distributions(south, "minority", "white")
plot_temperature_distributions(west, "minority", "white")
plot_temperature_distributions(midwest, "minority", "white")
plot_temperature_distributions(north, "minority", "white")


## Boxplots
boxplot(south[,2:3], xlab = "South", ylab = "Night Temperature (F)", outline=FALSE, names=names, col = c("lightblue","lightgreen")) # night
boxplot(south[,4:5], xlab = "South", ylab = "Afternoon Temperature (F)", outline=FALSE, names=names, col = c("lightblue","lightgreen")) # afternoon
boxplot(south[,6:7], xlab = "South", ylab = "Morning Temperature (F)", outline=FALSE, names=names, col = c("lightblue","lightgreen")) # morning

boxplot(west[,2:3], xlab = "West", ylab = "Night Temperature (F)", outline=FALSE, names=names, col = c("lightblue","lightgreen")) # night
boxplot(west[,4:5], xlab = "West", ylab = "Afternoon Temperature (F)", outline=FALSE, names=names, col = c("lightblue","lightgreen")) # afternoon
boxplot(west[,6:7], xlab = "West", ylab = "Morning Temperature (F)", outline=FALSE, names=names, col = c("lightblue","lightgreen")) # morning

boxplot(midwest[,2:3], xlab = "Midwest", ylab = "Night Temperature (F)", outline=FALSE, names=names, col = c("lightblue","lightgreen")) # night
boxplot(midwest[,4:5], xlab = "Midwest", ylab = "Afternoon Temperature (F)", outline=FALSE, names=names, col = c("lightblue","lightgreen")) # afternoon
boxplot(midwest[,6:7], xlab = "Midwest", ylab = "Morning Temperature (F)", outline=FALSE, names=names, col = c("lightblue","lightgreen")) # morning

boxplot(north[,2:3], xlab = "North", ylab = "Night Temperature (F)", outline=FALSE, names=names, col = c("lightblue","lightgreen")) # night
boxplot(north[,4:5], xlab = "North", ylab = "Afternoon Temperature (F)", outline=FALSE, names=names, col = c("lightblue","lightgreen")) # afternoon
boxplot(north[,6:7], xlab = "North", ylab = "Morning Temperature (F)", outline=FALSE, names=names, col = c("lightblue","lightgreen")) # morning




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
