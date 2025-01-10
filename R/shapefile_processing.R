# ==========================================================================================================================================
## Script Name: Shapefile Processing
# Author: Grace Legris, Research Data Analyst
# Date: 01/10/25
# To do: Generate a function that reads in any desired shapefile from Nigeria using a user-inputted state code, state name, ward code,
# ward name, or LGA code, and plots the map of the area.
# ==========================================================================================================================================

# directories
Drive <- gsub("OneDrive|Documents", "", Sys.getenv("HOME"))
DriveDir <- file.path(Drive, "Urban Malaria Proj Dropbox", "urban_malaria")
ShpfilesDir <- file.path(DriveDir, "data/nigeria/NMEP_nigeria_shapefiles")
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

# function to load the Nigeria shapefile
load_nigeria_shapefile <- function(shapefile_dir) {
  sf::st_read(file.path(shapefile_dir, "wards", "Nigeria_Wards.shp"))
}

load_nigeria_state_shapefile <- function(shapefile_dir, state_name) {
  # create the path to the shapefile dynamically
  shapefile_path <- file.path(shapefile_dir, "states", state_name, paste0(state_name, "_Wards.shp"))

  # read and return the shapefile
  if (!file.exists(shapefile_path)) {
    stop("ERROR: Shapefile not found. Please check the state name and directory.")
  }

  state_shp <- sf::st_read(shapefile_path)
  return(state_shp)
}

# function to match state code to state name
get_state_name <- function(state_code) {
  state_code_to_name <- c(
    "AB" = "Abia", "AD" = "Adamawa", "AK" = "Akwa Ibom", "AN" = "Anambra", "BA" = "Bauchi", "BY" = "Bayelsa", "BE" = "Benue",
    "BR" = "Borno", "CR" = "Cross River", "DE" = "Delta", "EB" = "Ebonyi", "ED" = "Edo", "EK" = "Ekiti", "EN" = "Enugu", "GO" = "Gombe",
    "IM" = "Imo", "JI" = "Jigawa", "KD" = "Kaduna", "KN" = "Kano", "KT" = "Katsina", "KB" = "Kebbi", "KO" = "Kogi", "KW" = "Kwara",
    "LA" = "Lagos", "NA" = "Nasarawa", "NI" = "Niger", "OG" = "Ogun", "ON" = "Ondo", "OS" = "Osun", "OY" = "Oyo", "PL" = "Plateau",
    "RI" = "Rivers", "SO" = "Sokoto", "TA" = "Taraba", "YO" = "Yobe", "ZA" = "Zamfara", "FC" = "Federal Capital Territory"
  )
  if (state_code %in% names(state_code_to_name)) {
    return(state_code_to_name[state_code])
  } else {
    stop("ERROR: Invalid state code. Please check the input.")
  }
}

# ==========================================================================================================================================
## Function to load the shapefile and plot the map for a specified state, ward, or LGA (using name or code)
# ==========================================================================================================================================

generate_map <- function(shapefile_dir, state_code = NULL, state_name = NULL, ward_code = NULL, ward_name = NULL, lga_code = NULL, output_dir, area_name) {

  ## -----------------------------------------------------------------------------------------------------------------------------------------
  ### Validate inputs
  ## -----------------------------------------------------------------------------------------------------------------------------------------

  # validate input: ensure only one of the area options is provided
  non_null_count <- sum(!sapply(list(state_code, state_name, lga_code, ward_code, ward_name), is.null))
  if (non_null_count > 1) {
    stop("ERROR: Please specify only one of the area options.")
  }

  # validate input - make sure user inputs one of the area options
  if (is.null(state_code) && is.null(state_name) && is.null(lga_code) && is.null(ward_code) && is.null(ward_name)) {
    stop("ERROR: You must specify one of the area options.")
  }

  # create df of valid values from the shapefile
  nigeria_shp <- sf::st_read(file.path(shapefile_dir, "wards", "Nigeria_Wards.shp"))
  valid_values <- nigeria_shp %>%
    dplyr::select(StateCode, WardCode, WardName, LGACode) %>%
    dplyr::distinct()

  # check that input matches a value in the shapefile
  if (!is.null(state_code) && !(state_code %in% valid_values$StateCode)) {
    stop("ERROR: Invalid State Code. Please check the input.")
  }
  if (!is.null(state_name)) {
    valid_state_names <- list(
      "Abia", "Adamawa", "Akwa Ibom", "Anambra", "Bauchi", "Bayelsa", "Benue",
      "Borno", "Cross River", "Delta", "Ebonyi", "Edo", "Ekiti", "Enugu", "Gombe",
      "Imo", "Jigawa", "Kaduna", "Kano", "Katsina", "Kebbi", "Kogi", "Kwara",
      "Lagos", "Nasarawa", "Niger", "Ogun", "Ondo", "Osun", "Oyo", "Plateau",
      "Rivers", "Sokoto", "Taraba", "Yobe", "Zamfara", "Federal Capital Territory"
    )
    if (!(state_name %in% valid_state_names)) {
      stop(paste("ERROR: Invalid State Name. Please check the input. Valid state names are:",
                 paste(valid_state_names, collapse = ", ")))
    }
  }
  if (!is.null(ward_code) && !(ward_code %in% valid_values$WardCode)) {
    stop("ERROR: Invalid Ward Code. Please check the input.")
  }
  if (!is.null(ward_name) && !(ward_name %in% valid_values$WardName)) {
    stop("ERROR: Invalid Ward Name. Please check the input.")
  }
  if (!is.null(lga_code) && !(lga_code %in% valid_values$LGACode)) {
    stop("ERROR: Invalid LGA Code. Please check the input.")
  }

  ## -----------------------------------------------------------------------------------------------------------------------------------------
  ### Load shapefile for user-specified area
  ## -----------------------------------------------------------------------------------------------------------------------------------------

  if (!is.null(state_code)) {
    state_name <- get_state_name(state_code)
    area_shp = load_nigeria_state_shapefile(shapefile_dir = shapefile_dir, state_name = state_name)
    type = "State"
  } else if (!is.null(state_name)) {
    area_shp = load_nigeria_state_shapefile(shapefile_dir = shapefile_dir, state_name = state_name)
    type = "State"
  } else if(!is.null(ward_code)) {
    ward_shp <- sf::st_read(file.path(shapefile_dir, "wards", "Nigeria_Wards.shp"))
    area_shp <- ward_shp %>% dplyr::filter(WardCode == ward_code)
    type = "Ward"
  } else if(!is.null(ward_name)) {
    ward_shp <- sf::st_read(file.path(shapefile_dir, "wards", "Nigeria_Wards.shp"))
    area_shp <- ward_shp %>% dplyr::filter(WardName == ward_name)
    type = "Ward"
  } else if(!is.null(lga_code)) {
    #lga_shp <- sf::st_read(file.path(shapefile_dir, "LGAs", "NGA_LGAs.shp"))
    area_shp = load_nigeria_shapefile(shapefile_dir = shapefile_dir)
    area_shp <- area_shp %>% dplyr::filter(LGACode == lga_code)
    type = "LGA"
  }

  # plot the map of the selected area
  area_map <- ggplot() +
    geom_sf(data = area_shp, aes(geometry = geometry), fill = "#D1E5F4", color = "black", size = 0.2) +
    labs(
      title = paste("Map of", " ", area_name, " ", type, ", Nigeria", sep = ""),
      x = NULL, y = NULL
    ) +
    map_theme()

  # save the plot to the output directory
  plot_path <- file.path(output_dir, paste0(area_name, "_", type, ".pdf"))
  ggplot2::ggsave(filename = plot_path, plot = area_map, width = 8, height = 6)
}
