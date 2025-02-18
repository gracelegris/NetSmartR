# ==========================================================================================================================================
## Script Name: Map Creator
# Author: Grace Legris, Research Data Analyst
# Date: 01/10/25
# Purpose: Plots a map of the user-specified state, ward, or LGA. Function reads in a shapefile from a Google Drive
# link, which contains boundaries for states, wards, and LGAs.
# ==========================================================================================================================================

# directories
Drive <- gsub("OneDrive|Documents", "", Sys.getenv("HOME"))
DriveDir <- file.path(Drive, "Urban Malaria Proj Dropbox", "urban_malaria")
output_dir <- file.path(DriveDir, "projects/urban_microstratification/NetSmartR/outputs")

map_theme <- function(){
  theme(axis.text.x = ggplot2::element_blank(),
        axis.text.y = ggplot2::element_blank(),
        axis.ticks = ggplot2::element_blank(),
        rect = ggplot2::element_blank(),
        plot.background = ggplot2::element_rect(fill = "white", colour = NA),
        plot.title = element_text(hjust = 0.5),
        legend.title=element_text(size=8, colour = 'black', hjust = 0.5),
        legend.text =element_text(size = 8, colour = 'black'),
        legend.key.height = unit(0.65, "cm"))
}

# ==========================================================================================================================================
## Function to load the shapefile and plot the map for a specified state, ward, or LGA (using name or code)
# ==========================================================================================================================================

#' Generate a Map for a Specified Area
#'
#' This function downloads a shapefile from Google Drive containing boundaries for Nigerian states, wards, or LGAs,
#' filters the data based on user-specified criteria (state code, state name, ward codes, ward names, LGA codes, or LGA names),
#' and generates a map of the selected area. The map is then saved as a PDF file in the specified output directory.
#'
#' @param map_name A character string representing the name of the map. This name is used in the map title and the output file name.
#' @param state_code A character string specifying the state code. Only one of \code{state_code} or \code{state_name} should be provided.
#' @param state_name A character string specifying the state name. Only one of \code{state_code} or \code{state_name} should be provided.
#' @param ward_codes A character vector specifying one or more ward codes. Only one of \code{ward_codes} or \code{ward_names} should be provided.
#' @param ward_names A character vector specifying one or more ward names. Only one of \code{ward_codes} or \code{ward_names} should be provided.
#' @param lga_codes A character vector specifying one or more Local Government Area (LGA) codes. If specified, neither \code{ward_codes} nor \code{ward_names} should be provided.
#' @param lga_names A character vector specifying one or more Local Government Area (LGA) names. If specified, neither \code{ward_codes} nor \code{ward_names} should be provided.
#' @param output_dir A character string specifying the directory where the output PDF map will be saved.
#'
#' @return Invisibly returns \code{NULL}. The function primarily generates and saves a PDF file containing the map.
#'
#' @details The function operates in several steps:
#' \enumerate{
#'   \item \strong{Input Validation:} Ensures that mutually exclusive parameters (e.g., \code{state_code} vs. \code{state_name} or \code{ward_codes} vs. \code{ward_names}) are not both provided.
#'   \item \strong{Downloading Shapefiles:} Based on the specified parameters, the function downloads the appropriate shapefile from Google Drive using a provided URL.
#'   \item \strong{Filtering Data:} The downloaded shapefile is filtered to retain only the geometries matching the specified state, ward, or LGA.
#'   \item \strong{Map Generation:} A map is created using \code{ggplot2} with a custom theme defined by the \code{map_theme} function.
#'   \item \strong{Saving the Map:} The map is saved as a PDF file in the \code{output_dir}, with a filename that incorporates \code{map_name} and the type of area.
#' }
#'
#' @note The function relies on an active internet connection to download shapefiles from Google Drive and uses temporary files to store the downloaded data.
#'
#' @examples
#' \dontrun{
#' # Example: Generate a map for a state using its code
#' generate_map(
#'   map_name = "Kano_Map",
#'   state_code = "KN",
#'   output_dir = "path/to/output/directory"
#' )
#'
#' # Example: Generate a map for specific wards using ward codes
#' generate_map(
#'   map_name = "Wards_Map",
#'   ward_codes = c("W001", "W002", "W003"),
#'   output_dir = "path/to/output/directory"
#' )
#'
#' # Example: Generate a map for a specific LGA using its name
#' generate_map(
#'   map_name = "LGA_Map",
#'   lga_names = c("LGA1"),
#'   output_dir = "path/to/output/directory"
#' )
#' }
#'
#' @import dplyr
#' @import ggplot2
#' @import sf
#' @export
#'
generate_map <- function(map_name = NULL, state_code = NULL, state_name = NULL, ward_codes = NULL, ward_names = NULL, lga_codes = NULL, lga_names = NULL, output_dir) {
  ## -------------------------------------------------------------------------
  ### Validate inputs
  ## -------------------------------------------------------------------------

  # ensure only one state option is provided
  if (!is.null(state_code) && !is.null(state_name)) {
    stop("ERROR: Please specify either state_code or state_name, not both.")
  }

  # ensure multiple LGAs or wards are valid inputs
  if (!is.null(ward_codes) && !is.null(ward_names)) {
    stop("ERROR: Please specify either ward_codes or ward_names, not both.")
  }
  if (!is.null(lga_codes) && (!is.null(ward_codes) || !is.null(ward_names))) {
    stop("ERROR: Specify LGAs or wards, but not both.")
  }

  # ensure only one state is specified if provided
  if (!is.null(state_code) && length(state_code) > 1) {
    stop("ERROR: You can specify only one state_code.")
  }
  if (!is.null(state_name) && length(state_name) > 1) {
    stop("ERROR: You can specify only one state_name.")
  }

  ## -------------------------------------------------------------------------
  ### Load shapefiles from Google Drive and filter for area of interest
  ## -------------------------------------------------------------------------

  message("Downloading shapefile from Google Drive...")

  if (!is.null(ward_codes)) {
    ward_url <- "https://drive.google.com/uc?id=1PCmCqI9or66cdgfPaCYndv7XLz-hqG4s&export=download"
    temp_file <- tempfile(fileext = ".geojson")
    download.file(ward_url, temp_file, mode = "wb")
    ward_geo_data <- st_read(temp_file)
    area_shp <- ward_geo_data %>% dplyr::filter(ward_code %in% ward_codes)
    type <- "Wards"
  } else if (!is.null(ward_names)) {
    ward_url <- "https://drive.google.com/uc?id=1PCmCqI9or66cdgfPaCYndv7XLz-hqG4s&export=download"
    temp_file <- tempfile(fileext = ".geojson")
    download.file(ward_url, temp_file, mode = "wb")
    ward_geo_data <- st_read(temp_file)
    area_shp <- ward_geo_data %>% dplyr::filter(ward_name %in% ward_names)
    type <- "Wards"
  } else if (!is.null(lga_codes)) {
    lga_url <- "https://drive.google.com/uc?id=16xEg2kzFW2J8L3q3dfTZmykuZhxG59jV&export=download"
    temp_file <- tempfile(fileext = ".geojson")
    download.file(lga_url, temp_file, mode = "wb")
    lga_geo_data <- st_read(temp_file)
    area_shp <- lga_geo_data %>% dplyr::filter(lga_code %in% lga_codes)
    type <- "LGAs"
  } else if (!is.null(lga_names)) {
    lga_url <- "https://drive.google.com/uc?id=16xEg2kzFW2J8L3q3dfTZmykuZhxG59jV&export=download"
    temp_file <- tempfile(fileext = ".geojson")
    download.file(lga_url, temp_file, mode = "wb")
    lga_geo_data <- st_read(temp_file)
    area_shp <- lga_geo_data %>% dplyr::filter(lga_name %in% lga_names)
    type <- "LGAs"
  } else if (!is.null(state_code)) {
    state_url <- "https://drive.google.com/uc?id=174JOHNbQZCxoxD2diqjjSLquyzVUK3MY&export=download"
    temp_file <- tempfile(fileext = ".geojson")
    download.file(state_url, temp_file, mode = "wb")
    state_geo_data <- st_read(temp_file)
    area_shp <- state_geo_data %>% dplyr::filter(state_code == state_code)
    type <- "State"
  } else if (!is.null(state_name)) {
    state_url <- "https://drive.google.com/uc?id=174JOHNbQZCxoxD2diqjjSLquyzVUK3MY&export=download"
    temp_file <- tempfile(fileext = ".geojson")
    download.file(state_url, temp_file, mode = "wb")
    state_geo_data <- st_read(temp_file)
    area_shp <- state_geo_data %>% dplyr::filter(state_name == state_name)
    type <- "State"
  }

  # check if the filtered shapefile is empty
  if (nrow(area_shp) == 0) {
    stop("ERROR: No matching areas found for the specified input.")
  }

  ## -------------------------------------------------------------------------
  ### Plot the selected areas
  ## -------------------------------------------------------------------------

  message("Generating map...")

  area_map <- ggplot() +
    geom_sf(data = area_shp, aes(geometry = geometry), fill = "#D1E5F4", color = "black", size = 0.2) +
    labs(
      title = paste("Map of ", map_name, " ", type, ",", " Nigeria", sep = ""),
      x = NULL, y = NULL
    ) +
    map_theme()

  # save the plot to the output directory
  plot_path <- file.path(output_dir, paste0(map_name, "_", type, ".pdf"))
  ggplot2::ggsave(filename = plot_path, plot = area_map, width = 8, height = 6)

  message("Map saved to: ", plot_path)
}


