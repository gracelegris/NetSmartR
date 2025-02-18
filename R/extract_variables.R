#' Extract Raster Data for a Given State
#'
#' This function extracts a variety of environmental and health-related variables from
#' raster datasets for a specified Nigerian state. Variables include EVI, NDVI, rainfall,
#' distance to water bodies, relative humidity, temperature, housing quality, PFPR,
#' night-time lights, flood, NDWI, NDMI, elevation, and surface soil wetness.
#'
#' @param state_name Character. The name of the state for which data is being extracted.
#' @param shapefile_path Character. The file path to the state shapefile containing ward boundaries.
#' @param raster_paths List. A named list of file paths to the raster datasets. Expected names include:
#' \describe{
#'   \item{evi_path}{Path to folder containing EVI GeoTIFF files.}
#'   \item{ndvi_path}{Path to folder containing NDVI GeoTIFF files.}
#'   \item{rainfall_path}{Path to folder containing rainfall GeoTIFF files.}
#'   \item{h2o_distance_path}{File path to the distance-to-water bodies GeoTIFF.}
#'   \item{rh_2023}{File path to the 2023 relative humidity raster data (brick).}
#'   \item{rh_2024}{File path to the 2024 relative humidity raster data (brick).}
#'   \item{temp_2023}{File path to the 2023 temperature raster data (brick).}
#'   \item{temp_2024}{File path to the 2024 temperature raster data (brick).}
#'   \item{housing_quality_path}{File path to the housing quality GeoTIFF.}
#'   \item{pfpr_path}{File path to the PfPR GeoTIFF.}
#'   \item{lights_path}{Path to folder containing night-time light GeoTIFF files.}
#'   \item{flood_path}{Path to folder containing flood GeoTIFF files.}
#'   \item{ndwi_path}{File path to the NDWI GeoTIFF.}
#'   \item{ndmi_path}{File path to the NDMI GeoTIFF.}
#'   \item{elevation_path}{File path to the elevation GeoTIFF.}
#'   \item{surface_soil_wetness_path}{Path to folder containing surface soil wetness GeoTIFF files.}
#'   \item{output_dir}{Directory where the output CSV should be saved.}
#' }
#'
#' @details
#' This function first validates and cleans the provided shapefile and then proceeds to
#' extract values from each provided raster dataset based on the spatial boundaries of wards.
#' Each extraction uses a mean function to compute an average value per ward. The results
#' are then merged with the spatial data and finally exported as a CSV file.
#'
#' @return A data frame containing the extracted variables for each ward.
#'
#' @examples
#' \dontrun{
#' # Define raster paths
#' raster_paths <- list(
#'   evi_path = "path/to/evi/",
#'   ndvi_path = "path/to/ndvi/",
#'   rainfall_path = "path/to/rainfall/",
#'   h2o_distance_path = "path/to/distance_to_water.tif",
#'   rh_2023 = "path/to/rh_2023.tif",
#'   rh_2024 = "path/to/rh_2024.tif",
#'   temp_2023 = "path/to/temp_2023.tif",
#'   temp_2024 = "path/to/temp_2024.tif",
#'   housing_quality_path = "path/to/housing_quality.tif",
#'   pfpr_path = "path/to/pfpr.tif",
#'   lights_path = "path/to/night_lights/",
#'   flood_path = "path/to/flood/",
#'   ndwi_path = "path/to/ndwi.tif",
#'   ndmi_path = "path/to/ndmi.tif",
#'   elevation_path = "path/to/elevation.tif",
#'   surface_soil_wetness_path = "path/to/soil_wetness/",
#'   output_dir = "path/to/output/"
#' )
#'
#' # Run extraction for Kano state
#' output_variables <- extract_raster_data(
#'   state_name = "Kano",
#'   shapefile_path = "path/to/Kano_State.shp",
#'   raster_paths = raster_paths
#' )
#' }
#'
#' @export
extract_raster_data <- function(state_name, shapefile_path, raster_paths) {

  message("Starting extract_variables for ", state_name)

  tryCatch({
    # Load shapefile
    message("Loading shapefile for ", state_name)
    wards <- tryCatch({
      st_make_valid(st_read(shapefile_path))
    }, error = function(e) stop("Failed to load shapefile: ", e$message))

    empty_geo <- st_is_empty(wards)
    wards <- wards[!empty_geo, ]
    wards_sp <- as(wards, "Spatial")

    # check for each path and perform extraction only if the path exists

    # EVI
    if (!is.null(raster_paths$evi_path)) {
      message("Extracting EVI for ", state_name)
      evi_files <- list.files(raster_paths$evi_path, pattern = ".tif", full.names = TRUE)
      if (length(evi_files) == 0) stop("No EVI files found.")
      evi_data <- lapply(evi_files, raster)
      evi_extracted <- purrr::map(evi_data, ~raster::extract(., wards, fun = mean, df = TRUE)) %>%
        purrr::reduce(~merge(.x, .y, by = "ID"))
      evi_extracted$avgEVI <- rowMeans(evi_extracted[, -1], na.rm = TRUE)
      wards$mean_EVI <- evi_extracted$avgEVI
    }

    # NDVI
    if (!is.null(raster_paths$ndvi_path)) {
      message("Extracting NDVI for ", state_name)
      ndvi_files <- list.files(raster_paths$ndvi_path, pattern = ".tif", full.names = TRUE)
      if (length(ndvi_files) == 0) stop("No NDVI files found.")
      ndvi_data <- lapply(ndvi_files, raster)
      ndvi_extracted <- purrr::map(ndvi_data, ~raster::extract(., wards, fun = mean, df = TRUE)) %>%
        purrr::reduce(~merge(.x, .y, by = "ID"))
      ndvi_extracted$avgNDVI <- rowMeans(ndvi_extracted[, -1], na.rm = TRUE)
      wards$mean_NDVI <- ndvi_extracted$avgNDVI
    }

    # RAINFALL
    if (!is.null(raster_paths$rainfall_path)) {
      message("Extracting Rainfall for ", state_name)
      rainfall_files <- list.files(raster_paths$rainfall_path, pattern = ".tif", full.names = TRUE)
      if (length(rainfall_files) == 0) stop("No rainfall files found.")
      rainfall_data <- lapply(rainfall_files, raster)
      rainfall_extracted <- purrr::map(rainfall_data, ~raster::extract(., wards, fun = mean, df = TRUE)) %>%
        purrr::reduce(~merge(.x, .y, by = "ID"))
      rainfall_extracted$avgRainfall <- rowMeans(rainfall_extracted[, -1], na.rm = TRUE)
      wards$mean_rainfall <- rainfall_extracted$avgRainfall
    }

    # DISTANCE TO WATER BODIES
    if (!is.null(raster_paths$h2o_distance_path)) {
      message("Extracting Distance to Water Bodies for ", state_name)
      h2o_distance_raster <- tryCatch({
        raster(raster_paths$h2o_distance_path)
      }, error = function(e) stop("Failed to load water distance raster: ", e$message))
      distance_extracted <- raster::extract(h2o_distance_raster, wards, fun = mean, df = TRUE)
      wards$distance_to_water <- distance_extracted$distance_to_water
    }

    # RELATIVE HUMIDITY
    if(!is.null(raster_paths$rh_2023) && !is.null(raster_paths$rh_2024)) {
      message("Extracting Relative Humidity for ", state_name)
      rh_files <- list(rh_23 <- brick(raster_paths$rh_2023),
                       rh_24 <- brick(raster_paths$rh_2024))
      names(rh_files) <- c("2023", "2024")

      for (i in 1:length(rh_files)) {
        num_layers <- nlayers(rh_files[[i]])
        layer_names <- paste0("RH_", seq_len(num_layers))
        names(rh_files[[i]]) <- layer_names
      }

      ward_list_RH <- list()
      for (i in 1:length(rh_files)) {
        ward_RH_data <- extract(rh_files[[i]], wards_sp, fun = mean, df = TRUE)
        df_RH <- as.data.frame(ward_RH_data)
        df_RH$Year <- names(rh_files)[i]
        ward_list_RH[[i]] <- df_RH
      }

      ward_rh2 <- bind_rows(ward_list_RH)
      rh_ward <- ward_rh2 %>%
        group_by(ID) %>%
        summarize(RH_mean = mean(c_across(starts_with("RH_")), na.rm = TRUE))
      wards$RH_mean <- rh_ward$RH_mean
    }

    # TEMPERATURE
    if(!is.null(raster_paths$temp_2023) && !is.null(raster_paths$temp_2024)) {
      # Extract temperature
      message("Extracting Temperature for ", state_name)
      temp_files <- list(temp_23 <- brick(raster_paths$temp_2023),
                         temp_24 <- brick(raster_paths$temp_2024))
      names(temp_files) <- c("2023", "2024")

      for (i in 1:length(temp_files)) {
        num_layers <- nlayers(temp_files[[i]])
        layer_names <- paste0("temp_", seq_len(num_layers))
        names(temp_files[[i]]) <- layer_names
      }

      ward_list_temp <- list()
      for (i in 1:length(temp_files)) {
        ward_temp_data <- extract(temp_files[[i]], wards_sp, fun = mean, df = TRUE)
        df_temp <- as.data.frame(ward_temp_data)
        df_temp$Year <- names(temp_files)[i]
        ward_list_temp[[i]] <- df_temp
      }

      ward_temp2 <- bind_rows(ward_list_temp)
      temp_ward <- ward_temp2 %>%
        group_by(ID) %>%
        summarize(temp_mean = mean(c_across(starts_with("temp_")), na.rm = TRUE))
      wards$temp_mean <- temp_ward$temp_mean
    }

    # HOUSING QUALITY
    if(!is.null(raster_paths$housing_quality_path)) {
      message("Extracting Housing Quality for ", state_name)
      housing_quality_raster <- tryCatch({
        raster(raster_paths$housing_quality_path)
      }, error = function(e) stop("Failed to load housing quality raster: ", e$message))
      housing_extracted <- raster::extract(housing_quality_raster, wards, fun = mean, df = TRUE)
      wards$housing_quality <- housing_extracted$X2019_Nature_Africa_Housing_2015_NGA
    }

    # PFPR
    if(!is.null(raster_paths$pfpr_path)) {
      message("Extracting PfPR for ", state_name)
      pfpr_raster <- tryCatch({
        raster(raster_paths$pfpr_path)
      }, error = function(e) stop("Failed to load PfPR raster: ", e$message))
      pfpr_extracted <- raster::extract(pfpr_raster, wards, fun = mean, df = TRUE)
      wards$pfpr <- pfpr_extracted$X202406_Global_Pf_Parasite_Rate_NGA_2022_1
    }

    # NIGHT-TIME LIGHT
    if(!is.null(raster_paths$lights_path)) {
      message("Extracting Night-Time Light for ", state_name)
      light_files <- list.files(raster_paths$lights_path, pattern = ".tif", full.names = TRUE)
      if (length(light_files) == 0) stop("No night-time light files found.")
      night_light_data <- lapply(light_files, raster)
      night_light_extracted <- purrr::map(night_light_data, ~raster::extract(., wards, fun = mean, df = TRUE)) %>%
        purrr::reduce(~merge(.x, .y, by = "ID"))
      night_light_extracted$avgRAD <- rowMeans(night_light_extracted[, -1], na.rm = TRUE)
      wards$avgRAD <- night_light_extracted$avgRAD
    }

    # FLOOD
    if(!is.null(raster_paths$flood_path)) {
      message("Extracting Floods for ", state_name)
      flood_2023_files <- list.files(raster_paths$flood_path, pattern = ".tif", full.names = TRUE)
      if (length(flood_2023_files) == 0) stop("No Flood files found.")
      flood_2023_data <- lapply(flood_2023_files, raster)
      #flood_2023_extracted <- purrr::map(flood_2023_data, ~raster::extract(., wards, fun = mean, df = TRUE)) %>%
      #purrr::reduce(~merge(.x, .y, by = "ID"))

      # Combine into a stack if there are multiple rasters
      if (length(flood_2023_data) > 1) {
        flood_stack <- raster::stack(flood_2023_data)
        flood_2023_extracted <- raster::extract(flood_stack, wards, fun = mean, df = TRUE)

        # Calculate mean across layers
        flood_2023_extracted$flood_presence <- rowMeans(flood_2023_extracted[, -1], na.rm = TRUE)
      } else {
        # Single raster case
        flood_2023_extracted <- raster::extract(flood_2023_data[[1]], wards, fun = mean, df = TRUE)
        flood_2023_extracted$flood_presence <- flood_2023_extracted[, 2] # Second column is the extracted value
      }
      wards$flood <- flood_2023_extracted$flood_presence
    }

    # NORMALIZED DIFFERENCE WATER INDEX
    if(!is.null(raster_paths$ndwi_path)) {
      message("Extracting NDWI for ", state_name)
      ndwi_raster <- tryCatch({
        raster(raster_paths$ndwi_path)
      }, error = function(e) stop("Failed to load NDWI raster: ", e$message))
      ndwi_extracted <- raster::extract(ndwi_raster, wards, fun = mean, df = TRUE)
      wards$NDWI <- ndwi_extracted$NDWI
    }

    # NORMALIZED DIFFERENCE MOISTURE INDEX
    if(!is.null(raster_paths$ndmi_path)) {
      message("Extracting NDMI for ", state_name)
      ndmi_raster <- tryCatch({
        raster(raster_paths$ndmi_path)
      }, error = function(e) stop("Failed to load NDMI raster: ", e$message))
      ndmi_extracted <- raster::extract(ndmi_raster, wards, fun = mean, df = TRUE)
      wards$NDMI <- ndmi_extracted$NDMI
    }

    # ELEVATION
    if(!is.null(raster_paths$elevation_path)) {
      message("Extracting Elevation for ", state_name)
      elevation_raster <- tryCatch({
        raster(raster_paths$elevation_path)
      }, error = function(e) stop("Failed to load Elevation raster: ", e$message))
      elevation_extracted <- raster::extract(elevation_raster, wards, fun = mean, df = TRUE)
      wards$elevation <- elevation_extracted$ELE
    }

    # SURFACE SOIL WETNESS
    if(!is.null(raster_paths$surface_soil_wetness_path)) {
      message("Extracting Surface Soil Wetness for ", state_name)
      soil_h2o_files <- list.files(raster_paths$surface_soil_wetness_path, pattern = ".tif", full.names = TRUE)
      if (length(soil_h2o_files) == 0) stop("No Surface Soil Wetness files found.")
      soil_h2o_data <- lapply(soil_h2o_files, raster)
      soil_h2o_extracted <- purrr::map(soil_h2o_data, ~raster::extract(., wards, fun = mean, df = TRUE)) %>%
        purrr::reduce(~merge(.x, .y, by = "ID"))
      soil_h2o_extracted$avg_soilwetness <- rowMeans(soil_h2o_extracted[, -1], na.rm = TRUE)
      wards$mean_soil_wetness <- soil_h2o_extracted$avg_soilwetness
    }

    # Save extracted variables
    message("Saving Extracted Variables for ", state_name)
    output_variables <- st_drop_geometry(wards)
    output_file <- file.path(raster_paths$output_dir, paste0(state_name, "_wards_variables.csv"))
    write.csv(output_variables, output_file, row.names = FALSE)

    message("Extraction completed successfully for ", state_name)
    return(output_variables)

  }, error = function(e) {
    message("An error occurred while processing ", state_name, ": ", e$message)
    NULL
  })
}
