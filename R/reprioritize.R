# ==========================================================================================================================================
## Script Name: Create Reprioritization Maps
# Author: Grace Legris, Research Data Analyst
# Date: 01/30/25
# Purpose: Creates and saves final reprioritization maps using composite scores.
# ==========================================================================================================================================

create_reprioritization_map <- function(state_name, shp_dir, outputs_dir, itn_dir, prioritize_wards, map_theme) {

  library(sf)
  library(dplyr)
  library(ggplot2)
  library(readxl)
  library(stringr)

  # Define file paths
  state_shp_path <- file.path(shp_dir, state_name, paste0(state_name, "_State.shp"))
  variables_path <- file.path(outputs_dir, "Final Extractions", paste0(tolower(state_name), "_plus.csv"))
  rankings_path <- file.path(outputs_dir, "rankings", paste0(state_name, "_rankings.csv"))
  itn_data_path <- file.path(itn_dir, paste0("pbi_distribution_", state_name, ".xlsx"))

  # Load shapefile
  state_shp <- st_read(state_shp_path)

  # Load and clean variables
  state_variables <- read.csv(variables_path) %>%
    distinct(WardCode, .keep_all = TRUE) %>%
    dplyr::select(X, WardName, urbanPercentage, WardCode) %>%
    mutate(
      classification_20 = ifelse(urbanPercentage > 20, "Urban", "Rural"),
      classification_30 = ifelse(urbanPercentage > 30, "Urban", "Rural"),
      classification_50 = ifelse(urbanPercentage > 50, "Urban", "Rural"),
      classification_75 = ifelse(urbanPercentage > 75, "Urban", "Rural")
    )

  # Read in rankings
  state_ranks <- read.csv(rankings_path) %>%
    dplyr::mutate(WardName = str_trim(WardName), ranks = str_trim(ranks))

  # Read and clean ITN data
  state_itn_data <- readxl::read_excel(itn_data_path, sheet = 3) %>%
    rename(population = N_FamilyMembers, Ward = AdminLevel3) %>%
    dplyr::select(population, Ward) %>%
    group_by(Ward) %>%
    summarise(Population = sum(population, na.rm = TRUE))

  # Merge datasets
  combined_wards <- left_join(state_variables, state_ranks, by = "WardName")
  combined_wards2 <- left_join(combined_wards, state_itn_data, by = c("WardName" = "Ward"))

  # Prioritize wards
  prioritized_wards_20 <- prioritize_wards(combined_wards2, "Population", "ranks", "classification_20", "WardName", 30)
  prioritized_wards_30 <- prioritize_wards(combined_wards2, "Population", "ranks", "classification_30", "WardName", 30)

  # Create risk map
  risk_map <- ggplot() +
    geom_sf(data = state_shp %>% left_join(combined_wards2, by = "WardName"), aes(geometry = geometry, fill = as.numeric(ranks))) +
    scale_fill_gradient(na.value = "grey") +
    labs(title = paste("Risk Map in", state_name)) +
    map_theme()

  # Create reprioritization map
  reprioritization_map <- ggplot() +
    geom_sf(data = state_shp %>% left_join(prioritized_wards_20, by = c("WardName" = "SelectedWards")),
            aes(geometry = geometry, fill = ifelse(is.na(WardPopulation), "Not Reprioritized", "Reprioritized"))) +
    scale_fill_manual(values = c("Not Reprioritized" = "red", "Reprioritized" = "green"), name = "Status") +
    labs(title = paste("Reprioritization Scenario 1 -", state_name),
         caption = "Wards selected for reprioritization based on composite scores and urban classification.") +
    map_theme()

  return(list(risk_map = risk_map, reprioritization_map = reprioritization_map))
}

# Example usage:
maps <- create_reprioritization_map("Delta", StateShpDir, OutputsDir, ITNDir, prioritize_wards, map_theme)
maps$risk_map  # Display risk map
maps$reprioritization_map  # Display reprioritization map
