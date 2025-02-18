# ==========================================================================================================================================
## Script Name: Estimate Building Heights and Population
# Author: Grace Legris, Research Data Analyst
# Date: 01/23/25
# Purpose: Calculates number of households per building and population estimates based on building height.
# ==========================================================================================================================================

#' Estimate Building Heights, Household Counts, and Population
#'
#' This function calculates the number of households per building and provides population estimates
#' based on building height data. It reads spatial data for settlement blocks, building footprints,
#' and a ward boundary shapefile, then uses a building height raster to estimate the number of stories
#' (and thereby households) per building. Population estimates are generated based on multiple household
#' size scenarios.
#'
#' @param building_data_path A character string specifying the file path to the building footprints dataset.
#'   The dataset should be readable by the \code{sf} package.
#' @param settlement_data_path A character string specifying the file path to the settlement blocks dataset.
#'   This dataset is expected to contain a \code{state} column and a \code{landuse} column.
#' @param ward_shapefile_path A character string specifying the file path to the ward boundary shapefile.
#' @param height_raster_path A character string specifying the file path to the raster dataset containing building heights.
#' @param output_dir A character string specifying the directory where any output files (e.g., CSVs) may be saved.
#' @param urban_wards A character vector of ward names that are classified as urban.
#' @param part_urban_wards A character vector of ward names that are classified as partially urban.
#' @param low_risk_wards A character vector of ward names that are considered low risk.
#' @param ward A character string indicating the ward being processed (used for messaging).
#' @param state A character string indicating the state being processed (used for filtering and messaging).
#'
#' @return This function returns a ggplot2 object (\code{p1}) representing a bar plot of population estimates
#'   by different household size scenarios. The plot shows the estimated population per ward group, along with labels
#'   indicating both the absolute population and its percentage contribution.
#'
#' @details The function performs the following steps:
#' \enumerate{
#'   \item \strong{Data Loading and Preprocessing:}
#'     \itemize{
#'       \item Reads settlement blocks, building footprints, and the ward shapefile, and transforms all data to EPSG:4326.
#'       \item Filters settlement blocks for the specified state and only retains those with \code{landuse} equal to "Residential".
#'       \item Ensures geometries are valid; any invalid geometries are removed with a warning.
#'     }
#'   \item \strong{Spatial Joins:}
#'     \itemize{
#'       \item Performs a spatial join to assign building footprints to the settlement blocks.
#'       \item Joins ward information (i.e., \code{WardName}) from the ward shapefile to the residential buildings.
#'     }
#'   \item \strong{Building Height Extraction:}
#'     \itemize{
#'       \item Loads a building height raster and extracts both the mean height for the ward and individual building heights
#'             using \code{raster::extract} and \code{exactextractr::exact_extract}.
#'       \item Adds the calculated mean building height to each residential building.
#'     }
#'   \item \strong{Household and Population Estimation:}
#'     \itemize{
#'       \item Filters buildings to remove uninhabitable structures (e.g., buildings shorter than 1.8 m or taller than 14 m).
#'       \item Estimates the number of households per building based on building height thresholds:
#'             - 1.8 m to <3.5 m: 1 household
#'             - 3.5 m to <7.0 m: 1.5 households
#'             - 7.0 m to <10.5 m: 2 households
#'             - 10.5 m to <14.0 m: 2.5 households
#'       \item Aggregates household counts at the ward level and computes population estimates using various multipliers
#'             (e.g., 3, 4, or 5 persons per household) and an "uneven" estimate that varies by ward type.
#'       \item Groups wards into categories based on risk (using the provided \code{low_risk_wards}) for comparison.
#'     }
#'   \item \strong{Visualization:}
#'     \itemize{
#'       \item Creates a bar plot using \code{ggplot2} to display population estimates per ward group.
#'       \item The plot is styled using a custom manuscript theme (\code{theme_manuscript}).
#'     }
#' }
#'
#' @examples
#' \dontrun{
#' # Example usage:
#' plot_result <- est_buildingheightpop(
#'   building_data_path = "path/to/building_data.geojson",
#'   settlement_data_path = "path/to/settlement_blocks.geojson",
#'   ward_shapefile_path = "path/to/ward_shapefile.shp",
#'   height_raster_path = "path/to/height_raster.tif",
#'   output_dir = "path/to/output_directory",
#'   urban_wards = c("WardA", "WardB"),
#'   part_urban_wards = c("WardC"),
#'   low_risk_wards = c("WardD"),
#'   ward = "ExampleWard",
#'   state = "ExampleState"
#' )
#'
#' # To display the plot:
#' print(plot_result)
#' }
#'
#' @import sf
#' @import dplyr
#' @import raster
#' @import exactextractr
#' @import ggplot2
#' @import tidyr
#' @importFrom stats filter
#' @export
est_buildingheightpop <- function(building_data_path, settlement_data_path, ward_shapefile_path, height_raster_path, output_dir, urban_wards,
                                  part_urban_wards, low_risk_wards, ward, state) {

  ## -----------------------------------------------------------------------------------------------------------------------------------------
  ### Read in Data
  ## -----------------------------------------------------------------------------------------------------------------------------------------

  # read in settlement blocks, building data, and ward shapefile and transform to same crs
  #message("Loading settlement blocks for ", ward, ", ", state)
  # read in settlement blocks data
  message("Loading settlement blocks data for ", ward, ", ", state)
  settlement_blocks <- st_read(settlement_data_path) %>%
    filter(state == state, landuse == 'Residential')

  # ensure all geometries are valid
  settlement_blocks <- st_transform(settlement_blocks, crs = 4326)
  settlement_blocks <- st_make_valid(settlement_blocks)

  # identify and handle invalid geometries (if any remain)
  invalid_geometries <- settlement_blocks[!st_is_valid(settlement_blocks), ]

  if (nrow(invalid_geometries) > 0) {
    warning(paste(nrow(invalid_geometries), "invalid geometries detected. Removing them."))
    settlement_blocks <- settlement_blocks[st_is_valid(settlement_blocks), ]
  }

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

  # spatial join to transfer WardName from shapefile to residential_buildings
  residential_buildings <- st_join(residential_buildings, ward_shp["WardName"], join = st_within)

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

  #write.csv(height_summary, file.path(output_dir, paste0(ward, "_buildings.csv")))

  ## -----------------------------------------------------------------------------------------------------------------------------------------
  ### Read in saved building height .csv
  ## -----------------------------------------------------------------------------------------------------------------------------------------
  #building_heights <- read.csv(file.path("/Users/grace/Urban Malaria Proj Dropbox/urban_malaria/data/nigeria/Raster_files/GoogleBuildings2_5/Asaba/asaba_buildings.csv"))

  # convert height to likely household number per building
  building_story_count <- height_summary %>%
    dplyr::filter(building_height >= 1.8) %>%  # remove uninhabitable buildings less than one story (8015)
    dplyr::filter(building_height <= 14) %>%  # with average story height of 3.5m, remove buildings > 16m (3) or > 14m / 4 stories (7)
    mutate(resident_households = case_when(
      building_height >= 1.8 & building_height < 3.5 ~ 1,
      building_height >= 3.5 & building_height < 7.0 ~ 1.5,
      building_height >= 7.0 & building_height < 10.5 ~ 2,
      building_height >= 10.5 & building_height < 14.0 ~ 2.5,
    ))

  # count households in each ward
  ward_household_counts <- building_story_count %>%
    group_by(WardName) %>%
    summarise(total_households = sum(resident_households, na.rm = TRUE)) %>%
    mutate(total_households = ceiling(total_households)) # round up

  plot_all <- ward_household_counts %>%
    mutate(WardType = case_when(
      WardName %in% urban_wards ~ "Urban", WardName %in% part_urban_wards ~ "Part-Urban",
      TRUE ~ "Rural")) %>%
    mutate(pop_estimate5 = total_households * 5,
           pop_estimate4 = total_households * 4,
           pop_estimate3 = total_households * 3 ) %>% #different estimates per household
    mutate(pop_estimateuneven = case_when(
      WardType == "Urban" ~ total_households * 3,
      WardType == "Rural" ~ total_households * 5,
      WardType == "Part-Urban" ~ total_households * 4)) %>% #uneven population
    mutate(WardGroup = ifelse(WardName %in% low_risk_wards1, WardName, "Rest of Asaba")) %>% #change low_risk_asaba1 to low_risk_asaba2 for different scenarios
    group_by(WardGroup) %>%
    summarize(pop_estimate5 = sum(pop_estimate5),
              pop_estimate3 = sum(pop_estimate3),
              pop_estimate4 = sum(pop_estimate4),
              pop_estimate_uneven = sum(pop_estimateuneven)) %>%
    pivot_longer(cols = starts_with("pop_estimate"),
                 names_to = "EstimateType",
                 values_to = "Population") %>%
    group_by(EstimateType) %>%
    mutate(percentage = (Population / sum(Population)) * 100) %>%
    mutate(label = paste0(Population, " (", round(percentage, 2), "%)")) %>%
    mutate(EstimateType = recode(EstimateType,
                                 "pop_estimate_uneven" = "Estimate Uneven",
                                 "pop_estimate3" = "Estimate 3",
                                 "pop_estimate4" = "Estimate 4",
                                 "pop_estimate5" = "Estimate 5"
    ))

  # create plot
  p1 <- ggplot(plot_all, aes(x = EstimateType, y = Population, fill = WardGroup))+
    geom_bar(stat = "identity")+
    scale_fill_manual(values = c("Rest of Asaba" = "coral", "Umuagu" = "pink", "Umuezei" = "lightyellow",
                                 "Ugbomanta" = "lightblue", "West End" = "lightgreen"))+
    labs(title = "Population Estimates in Asaba with Building Height (Scenario 1)",
         x = "Household sizes",
         y = "Population",
         fill = "Ward Group")+
    geom_text(aes(label = label), position = position_stack(vjust = 0.5), color = "black", size = 3) +
    theme_manuscript()
  p1

}
