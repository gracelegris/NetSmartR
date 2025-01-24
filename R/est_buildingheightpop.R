# ==========================================================================================================================================
## Script Name: Estimate Building Heights and Population
# Author: Grace Legris, Research Data Analyst
# Date: 01/23/25
# Purpose: Calculates number of households per building and population estimates based on building height.
# ==========================================================================================================================================

est_buildingheightpop <- function(building_data_path, settlement_data_path, ward_shapefile_path, height_raster_path, output_dir, urban_wards,
                                  part_urban_wards, low_risk_wards, ward, state) {

  ## -----------------------------------------------------------------------------------------------------------------------------------------
  ### Read in Data
  ## -----------------------------------------------------------------------------------------------------------------------------------------

  # read in settlement blocks, building data, and ward shapefile and transform to same crs
  message("Loading settlement blocks for ", ward, ", ", state)
  # read in settlement blocks data
  settlement_blocks <- st_read(settlement_data_path) %>%
    filter(state == state, landuse == 'Residential')

  # ensure all geometries are valid
  settlement_blocks <- st_transform(settlement_blocks, crs = 4326)
  settlement_blocks <- st_make_valid(settlement_blocks)

  # # identify and handle invalid geometries (if any remain)
  # invalid_geometries <- settlement_blocks[!st_is_valid(settlement_blocks), ]
  #
  # if (nrow(invalid_geometries) > 0) {
  #   warning(paste(nrow(invalid_geometries), "invalid geometries detected. Removing them."))
  #   settlement_blocks <- settlement_blocks[st_is_valid(settlement_blocks), ]
  # }

  message("Loading building footprints for ", ward, ", ", state)
  building_data <- st_read(building_data_path)  %>%
    st_transform(crs = 4326)

  message("Loading shapefile for ", ward, ", ", state)
  ward_shp <- st_read(ward_shapefile_path)  %>%
    st_transform(crs = 4326)

  # filter residential buildings
  residential_buildings <- st_join(building_data, settlement_blocks, join = st_within) %>%
    filter(landuse == "Residential") %>%
    filter(st_geometry_type(.) == "POLYGON")

  ## -----------------------------------------------------------------------------------------------------------------------------------------
  ### Read in raster building height data, process, and save as .csv for future use
  ## -----------------------------------------------------------------------------------------------------------------------------------------

  message("Loading building height raster for ", ward, ", ", state)
  height_raster <- raster(height_raster_path)

  # extract mean building height in specified ward
  mean_building_height <- raster::extract(height_raster, ward_shp, fun = mean, df = T)

  # extract individual building heights
  individual_heights <- exactextractr::exact_extract(height_raster, residential_buildings)

  # calculate mean building height
  mean_height <- lapply(individual_heights, function(df) {
    mean(df$value, na.rm = TRUE)
  })

  mean_height_df <- data.frame(mean_height = unlist(mean_height))

  # add mean heights to a new var in residential buildings df
  residential_buildings$building_height <- mean_height_df$mean_height

  height_summary <- residential_buildings %>%
    dplyr::select(id, FID, WardName, building_height) %>%
    st_drop_geometry()

  write.csv(height_summary, file.path(output_dir, paste0(ward, "_buildings.csv")))

  ## -----------------------------------------------------------------------------------------------------------------------------------------
  ### Read in saved building height .csv
  ## -----------------------------------------------------------------------------------------------------------------------------------------
  building_heights <- read.csv(file.path(output_dir, paste0(ward, "_buildings.csv")))

  # convert height to likely household number per building
  building_story_count <- building_heights %>%
    filter(building_heights >= 1.8) %>%  # remove uninhabitable buildings less than one story (8015)
    filter(building_heights <= 14) %>%  # with average story height of 3.5m, remove buildings > 16m (3) or > 14m / 4 stories (7)
    mutate(resident_households = case_when(
      building_heights >= 1.8 & building_heights < 3.5 ~ 1,
      building_heights >= 3.5 & building_heights < 7.0 ~ 1.5,
      building_heights >= 7.0 & building_heights < 10.5 ~ 2,
      building_heights >= 10.5 & building_heights < 14.0 ~ 2.5,
    ))

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

  # filter and calculate the estimated number of resident households based on building height
  filtered_buildings <- residential_buildings %>%
    filter(building_height >= 1.8, building_height <= 14.0) %>%
    mutate(resident_households = case_when(
      building_height >= 1.8 & building_height < 3.5 ~ household_sizes[["1.8-3.5"]],
      building_height >= 3.5 & building_height < 7.0 ~ household_sizes[["3.5-7.0"]],
      building_height >= 7.0 & building_height < 10.5 ~ household_sizes[["7.0-10.5"]],
      building_height >= 10.5 & building_height < 14.0 ~ household_sizes[["10.5-14.0"]]
    ))

  # summarize households counts
  household_counts <- filtered_buildings %>%
    #group_by(WardName) %>%
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
  message("Saving summary .csv file to: ", output_dir)
  write.csv(filtered_buildings, output_dir, row.names = FALSE)

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
