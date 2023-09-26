## Load Libraries
library(tidyverse)
library(rlang)



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
  if(!variable %in% names(data)) {
    stop(paste("Variable", variable, "not found in the data."))
  }
  
  # Check if the variable is continuous or discrete
  is_continuous <- is.numeric(data[[variable]]) && !is.factor(data[[variable]])
  
  # Create the map
  p <- ggplot() +
    geom_sf(data = data, aes_string(fill = variable), color = NA) +
    theme_void() +
    theme(legend.position = "bottom") +
    labs(title = title) +
    geom_sf(data = data, fill = NA, color = "black", size = 0.1)
  
  # Apply the appropriate color scale
  if(is_continuous) {
    p <- p + scale_fill_viridis_c(option = palette, direction = -1, name = title)
  } else {
    p <- p + scale_fill_viridis_d(option = palette, direction = -1, name = title)
  }
  
  return(p)
}
