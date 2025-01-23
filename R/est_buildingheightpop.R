# ==========================================================================================================================================
## Script Name: Estimate Building Heights and Population
# Author: Grace Legris, Research Data Analyst
# Date: 01/23/25
# Purpose: Calculates number of households per building and population estimates based on building height.
# ==========================================================================================================================================

est_buildingheightpop <- function(building_data_path, settlement_data_path, ward_shapefile_path, height_raster_path, output_csv_path, urban_wards,
                                  part_urban_wards, low_risk_wards, ward, state) {

  # read in settlement blocks, building data, and ward shapefile and transform to same crs
  message("Loading settlement blocks for ", ward, ", ", state)
  settlement_blocks <- st_read(settlement_data_path) %>%
    st_transform(crs = 4326) %>%
    st_make_valid(settlement_blocks)
  message("Loading building footprints for ", ward, ", ", state)
  building_data <- st_read(building_data_path)  %>%
    st_transform(crs = 4326) %>%
    st_make_valid(settlement_blocks)
  message("Loading shapefile for ", ward, ", ", state)
  ward_shp <- st_read(ward_shapefile_path)  %>%
    st_transform(crs = 4326) %>%
    st_make_valid(settlement_blocks)

  # read in building height raster data
  message("Loading building height raster for ", ward, ", ", state)
  building_heights <- raster(height_raster_path)

  # filter residential buildings
  residential_buildings <- st_join(building_data, settlement_blocks, join = st_within) %>%
    filter(landuse == "Residential") %>%
    filter(st_geometry_type(.) == "POLYGON")

  # extract mean building heights
  building_heights <- exactextractr::exact_extract(building_heights, residential_buildings)
  mean_heights <- lapply(building_heights, function(df) mean(df$value, na.rm = TRUE))
  residential_buildings$building_height <- unlist(mean_heights)

  # set values for building height to likely household size per building
  household_sizes = list(
    "1.8-3.5" = 1,
    "3.5-7.0" = 1.5,
    "7.0-10.5" = 2,
    "10.5-14.0" = 2.5
  )

  # filter and calculate resident households
  filtered_buildings <- residential_buildings %>%
    filter(building_height >= 1.8, building_height <= 14.0) %>%
    mutate(resident_households = case_when(
      building_height >= 1.8 & building_height < 3.5 ~ household_sizes[["1.8-3.5"]],
      building_height >= 3.5 & building_height < 7.0 ~ household_sizes[["3.5-7.0"]],
      building_height >= 7.0 & building_height < 10.5 ~ household_sizes[["7.0-10.5"]],
      building_height >= 10.5 & building_height < 14.0 ~ household_sizes[["10.5-14.0"]]
    ))

  # summarize households by ward
  household_counts <- filtered_buildings %>%
    group_by(WardName) %>%
    summarise(total_households = sum(resident_households, na.rm = TRUE)) %>%
    mutate(total_households = ceiling(total_households))

  # calculate population estimates
  population_data <- household_counts %>%
    mutate(WardType = case_when(
      WardName %in% urban_wards ~ "Urban",
      WardName %in% part_urban_wards ~ "Part-Urban",
      TRUE ~ "Rural"
    )) %>%
    mutate(pop_estimate5 = total_households * 5,
           pop_estimate4 = total_households * 4,
           pop_estimate3 = total_households * 3) %>%
    mutate(pop_estimate_uneven = case_when(
      WardType == "Urban" ~ total_households * 3,
      WardType == "Rural" ~ total_households * 5,
      WardType == "Part-Urban" ~ total_households * 4
    )) %>%
    mutate(WardGroup = ifelse(WardName %in% low_risk_wards, WardName, "Rest of Wards")) %>%
    group_by(WardGroup) %>%
    summarize(pop_estimate5 = sum(pop_estimate5),
              pop_estimate3 = sum(pop_estimate3),
              pop_estimate4 = sum(pop_estimate4),
              pop_estimate_uneven = sum(pop_estimate_uneven)) %>%
    pivot_longer(cols = starts_with("pop_estimate"),
                 names_to = "EstimateType",
                 values_to = "Population") %>%
    group_by(EstimateType) %>%
    mutate(percentage = (Population / sum(Population)) * 100,
           label = paste0(Population, " - ", round(percentage, 2), "%"))

  # save summary to csv
  message("Saving summary .csv file to: ", output_csv_path)
  write.csv(filtered_buildings, output_csv_path, row.names = FALSE)

  # create bar plot
  population_plot <- ggplot(population_data, aes(x = EstimateType, y = Population, fill = WardGroup)) +
    geom_bar(stat = "identity") +
    scale_fill_manual(values = c("Rest of Wards" = "coral",
                                 "Urban" = "pink",
                                 "Part-Urban" = "lightyellow")) +
    labs(title = "Population Estimates by Ward Group",
         x = "Estimate Type",
         y = "Population",
         fill = "Ward Group") +
    geom_text(aes(label = label), position = position_stack(vjust = 0.5)) +
    theme_manuscript()

  return(population_plot)
}
