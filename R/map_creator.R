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

# # function to match state code to state name
# get_state_name <- function(state_code) {
#   state_code_to_name <- c(
#     "AB" = "Abia", "AD" = "Adamawa", "AK" = "Akwa Ibom", "AN" = "Anambra", "BA" = "Bauchi", "BY" = "Bayelsa", "BE" = "Benue",
#     "BR" = "Borno", "CR" = "Cross River", "DE" = "Delta", "EB" = "Ebonyi", "ED" = "Edo", "EK" = "Ekiti", "EN" = "Enugu", "GO" = "Gombe",
#     "IM" = "Imo", "JI" = "Jigawa", "KD" = "Kaduna", "KN" = "Kano", "KT" = "Katsina", "KB" = "Kebbi", "KO" = "Kogi", "KW" = "Kwara",
#     "LA" = "Lagos", "NA" = "Nasarawa", "NI" = "Niger", "OG" = "Ogun", "ON" = "Ondo", "OS" = "Osun", "OY" = "Oyo", "PL" = "Plateau",
#     "RI" = "Rivers", "SO" = "Sokoto", "TA" = "Taraba", "YO" = "Yobe", "ZA" = "Zamfara", "FC" = "Federal Capital Territory"
#   )
#   if (state_code %in% names(state_code_to_name)) {
#     return(state_code_to_name[state_code])
#   } else {
#     stop("ERROR: Invalid state code. Please check the input.")
#   }
# }

# ==========================================================================================================================================
## Function to load the shapefile and plot the map for a specified state, ward, or LGA (using name or code)
# ==========================================================================================================================================

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


