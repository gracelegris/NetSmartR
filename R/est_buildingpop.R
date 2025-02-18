# ==========================================================================================================================================
## Script Name: Estimate Buildings and Population
# Author: Grace Legris, Research Data Analyst
# Date: 01/22/25
# Purpose: Calculates counts of residential buildings within a given shapefile and population estimates based on user-specified household sizes.
# ==========================================================================================================================================

#' Manuscript Map Theme
#'
#' This function creates and returns a ggplot2 theme suitable for manuscript-style figures.
#' The theme includes a white background, black borders, and adjusted text sizes and colors for improved clarity.
#'
#' @return A ggplot2 theme object that can be applied to ggplot figures.
#'
#' @details The theme sets:
#' \itemize{
#'   \item A white background with a black panel border.
#'   \item Centered plot titles.
#'   \item Axis text and titles with specified sizes and colors.
#'   \item Customized legend text and key dimensions.
#' }
#'
#' @examples
#' \dontrun{
#' library(ggplot2)
#' p <- ggplot(mtcars, aes(x = wt, y = mpg)) +
#'   geom_point() +
#'   theme_manuscript() +
#'   labs(title = "Manuscript Style Plot")
#' print(p)
#' }
#'
#' @export
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

#' Estimate Residential Building Counts and Population
#'
#' This function calculates the number of residential buildings and estimates the population within a study area.
#' It reads spatial data for building footprints, settlement blocks, and the study area, filters the settlement blocks
#' based on a specified land use type, and then performs spatial joins to count the total and residential buildings.
#' Population estimates are derived by multiplying the number of residential buildings by a user-specified household size.
#'
#' @param building_data_path A character string specifying the file path to the building footprints dataset (supported by \code{sf}).
#' @param settlement_data_path A character string specifying the file path to the settlement blocks dataset (supported by \code{sf}).
#' @param shapefile_path A character string specifying the file path to the study area shapefile (supported by \code{sf}).
#' @param household_size A numeric value representing the average household size, used to estimate the population.
#' @param city A character string representing the name of the city (used for labeling output messages).
#' @param state A character string representing the state name (used for filtering settlement blocks and labeling).
#' @param landuse_filter A character string specifying the land use type to filter the settlement blocks (e.g., "Residential").
#'
#' @return A list containing:
#'   \itemize{
#'     \item \code{building_counts}: A data frame with the following columns:
#'       \itemize{
#'         \item \code{WardName}: Name of the ward.
#'         \item \code{total_buildings}: Total number of distinct building footprints within the ward.
#'         \item \code{residential_buildings}: Count of residential buildings (filtered by the specified land use).
#'         \item \code{population_estimate}: Estimated population for the ward (residential buildings multiplied by \code{household_size}).
#'       }
#'     \item \code{total_population}: A numeric value representing the overall population estimate for the study area.
#'   }
#'
#' @details The function performs the following steps:
#' \enumerate{
#'   \item Reads the settlement blocks, study area, and building footprints using the \code{sf} package.
#'   \item Validates and, if necessary, fixes invalid geometries in the settlement blocks.
#'   \item Filters the settlement blocks for the specified state and land use type. If no valid \code{landuse_filter}
#'         is provided, the function stops with an error listing the available land use types.
#'   \item Transforms all spatial data to EPSG:4326.
#'   \item Joins the building footprints with the study area to determine which buildings fall within the study area.
#'   \item Counts the total number of buildings and, via a further spatial join with the filtered settlement blocks,
#'         counts the number of residential buildings.
#'   \item Estimates the population for each ward by multiplying the number of residential buildings by the provided
#'         \code{household_size}, and calculates the overall population.
#'   \item Outputs a summary table and messages with the total counts and population estimates.
#' }
#'
#' @examples
#' \dontrun{
#' result <- est_buildingpop(
#'   building_data_path = "path/to/buildings.geojson",
#'   settlement_data_path = "path/to/settlement_blocks.geojson",
#'   shapefile_path = "path/to/study_area.shp",
#'   household_size = 5,
#'   city = "Example City",
#'   state = "Example State",
#'   landuse_filter = "Residential"
#' )
#'
#' # View building counts and estimated total population
#' print(result$building_counts)
#' print(result$total_population)
#' }
#'
#' @import sf
#' @import dplyr
#' @importFrom tidyr replace_na
#' @import knitr
#' @export
est_buildingpop <- function(building_data_path, settlement_data_path, shapefile_path, household_size, city, state, landuse_filter) {

  # read in settlement blocks data
  settlement_blocks <- st_read(settlement_data_path)

  # read in settlement blocks data from Google Drive (not working - says shapefile could be corrupted)
  # shp_id <- "17UQtuCzDTTFiV69-r8TVmbNAr-5ZXYTm"
  # dbf_id <- "1PppXg66v020ylIt2X22e_fkRTjUYsZkO"
  # prj_id <- "1UqnzhS6aHJiKeiUkktXbXsH9tNLi-E_x"
  # shx_id <- "1sckPapY2hfrlepyIQ2V-szGMdOGi4pvY"
  #
  # download_gdrive <- function(file_id, destfile) {
  #   drive_url <- paste0("https://drive.google.com/uc?export=download&id=", file_id)
  #   response <- GET(drive_url, write_disk(destfile, overwrite = TRUE))
  #   if (response$status_code != 200) {
  #     stop(paste("Failed to download file:", destfile))
  #   }
  # }
  #
  # # Create a temp directory to store files
  # temp_dir <- tempfile()
  # dir.create(temp_dir)
  #
  # # Download files
  # download_gdrive(shp_id, file.path(temp_dir, "data.shp"))
  # download_gdrive(dbf_id, file.path(temp_dir, "data.dbf"))
  # download_gdrive(prj_id, file.path(temp_dir, "data.prj"))
  # download_gdrive(shx_id, file.path(temp_dir, "data.shx"))
  #
  # # Read shapefile using sf
  # shp_path <- file.path(temp_dir, "data.shp")
  # settlement_blocks <- st_read(shp_path)
  #
  # # Print first few rows
  # print(head(settlement_blocks))

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
  # ggplot(building_counts, aes(x = WardName, y = residential_buildings)) +
  #   geom_bar(stat = "identity", fill = "steelblue") +
  #   theme_manuscript() +
  #   labs(title = "Residential Building Counts by Ward",
  #        x = "Ward Name", y = "Residential Buildings") +
  #   theme(axis.text.x = element_text(angle = 45, hjust = 1))

  message("Total building counts in ", city, ", ", state, ":")
  print(kable(building_counts, format = "simple"))
  message("Total population in ", city, ", ", state, ": ", total_population)

  # return processed data and total population
  return(list(
    building_counts = building_counts,
    total_population = total_population
  ))
}
