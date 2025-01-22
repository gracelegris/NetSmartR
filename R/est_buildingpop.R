# ==========================================================================================================================================
## Script Name: Estimate Buildings and Population
# Author: Grace Legris, Research Data Analyst
# Date: 01/22/25
# Purpose: Calculates counts of residential buildings within a given shapefile and population estimates based on user-specified household sizes.
# ==========================================================================================================================================

theme_manuscript <- function(){
  theme_bw() +
    theme(panel.border = element_rect(colour = "black", fill=NA, size=0.5),
          plot.title = element_text(hjust = 0.5),
          axis.text.x = element_text(size = 12, color = "black"),
          axis.text.y = element_text(size = 12, color = "black"),
          axis.title.x = element_text(size = 12),
          axis.title.y = element_text(size =12),
          legend.title=element_text(size=12, colour = 'black'),
          legend.text =element_text(size = 12, colour = 'black'),
          legend.key.height = unit(1, "cm"))
}

est_buildingpop <- function(building_data_path, settlement_data_path, shapefile_path, household_size, state, landuse_filter) {

  # read in settlement blocks data
  settlement_blocks <- st_read(settlement_data_path)

  # get unique land use filters
  landuse_filters <- unique(settlement_blocks$landuse)

  # check that user specified a land use filter
  if (is.null(landuse_filter) || landuse_filter == "") {
    stop(paste0(
      "ERROR: Please specify a land use filter from the following list: ",
      paste(landuse_filters, collapse = ", ")
    ))
  }

  # ensure all geometries are valid
  settlement_blocks <- settlement_blocks %>%
    st_make_valid()

  # identify and handle invalid geometries (if any remain)
  invalid_geometries <- settlement_blocks[!st_is_valid(settlement_blocks), ]

  if (nrow(invalid_geometries) > 0) {
    warning(paste(nrow(invalid_geometries), "invalid geometries detected. Removing them."))
    settlement_blocks <- settlement_blocks[st_is_valid(settlement_blocks), ]
  }

  # filter and transform settlement blocks for the specified state and land use
  settlement_blocks <- settlement_blocks %>%
    dplyr::filter(state == state, landuse == landuse_filter) %>%
    st_transform(crs = 4326)

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

  message("Total building counts in ", state, ":")
  print(kable(building_counts, format = "simple"))
  message("Total population in ", state, ": ", total_population)

  # return processed data and total population
  return(list(
    building_counts = building_counts,
    total_population = total_population
  ))
}
