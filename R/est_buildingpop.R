# ==========================================================================================================================================
## Script Name: Estimate Buildings and Population
# Author: Grace Legris, Research Data Analyst
# Date: 01/21/25
# Purpose: Calculates counts of residential buildings within a given shapefile and population estimates based on user-specified household sizes.
# ==========================================================================================================================================

est_buildingpop <- function(building_data_path, settlement_data_path, shapefile_path, household_size, state, landuse_filter) {

  # read in and process settlement blocks (residential areas)
  settlement_blocks <- st_read(settlement_data_path) %>%
    filter(state == state, landuse == landuse_filter) %>%
    st_transform(crs = 4326) %>%
    st_make_valid()

  # read in shapefile
  study_area_shp <- st_read(shapefile_path) %>%
    st_transform(crs = 4326)

  # read in building footprints
  building_data <- st_read(building_data_path) %>%
    st_transform(crs = 4326)

  # spatial join: assign buildings to wards/regions in the shapefile
  joined_data <- st_join(building_data, study_area_shp, join = st_within)

  # count total and residential buildings per ward
  total_buildings <- joined_data %>%
    st_drop_geometry() %>%
    group_by(WardName) %>%
    summarise(total_buildings = n_distinct(id), .groups = "drop")

  residential_buildings <- st_join(joined_data, settlement_blocks, join = st_within) %>%
    filter(landuse == landuse_filter) %>%
    st_drop_geometry() %>%
    group_by(WardName) %>%
    summarise(residential_buildings = n_distinct(id), .groups = "drop")

  # combine total and residential building counts
  building_counts <- left_join(total_buildings, residential_buildings, by = "WardName") %>%
    mutate(residential_buildings = replace_na(residential_buildings, 0),
           population_estimate = residential_buildings * household_size)

  # summarize overall population estimate
  total_population <- sum(building_counts$population_estimate)

  # visualize results
  ggplot(building_counts, aes(x = WardName, y = residential_buildings)) +
    geom_bar(stat = "identity", fill = "steelblue") +
    theme_manuscript() +
    labs(title = "Residential Building Counts by Ward",
         x = "Ward Name", y = "Residential Buildings") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))

  # return processed data and total population
  return(list(
    building_counts = building_counts,
    total_population = total_population
  ))
}
