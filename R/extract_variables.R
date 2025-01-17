# ==========================================================================================================================================
## Script Name: Extract Variables
# Author: Grace Legris, Research Data Analyst
# Date: 01/16/25
# Purpose: Extracts EVI, NDVI, rainfall, distance to water bodies, relative humidity, temperature, housing quality,
# PFPR (Plasmodium Falciparum Parasite Rate), night time light, flood, NDWI (Normalized Difference Water Index),
# NDMI (Normalized Difference Moisture Index), elevation, surface soil wetness from any given shapefile and saves as .csv.
# ==========================================================================================================================================

# function to extract raster data for a given shapefile
extract_raster_data <- function(state_name, shapefile_path, raster_paths, output_csv) {
  tic(paste("Starting data extraction for", state_name))

  tryCatch({
    # load shapefile
    message("Loading shapefile for ", state_name)
    wards <- st_read(shapefile_path) %>%
      st_make_valid() %>%
      filter(!st_is_empty(.))

    wards_sp <- as(wards, "Spatial") # convert to spatial object for raster extraction

    results <- list() # to store extraction results

    # loop through raster paths for extraction
    for (raster_name in names(raster_paths)) {
      raster_folder <- raster_paths[[raster_name]]
      message("Processing ", raster_name, "...")

      # list all .tif files in the folder
      tif_files <- list.files(raster_folder, pattern = "\\.tif$", full.names = TRUE)
      if (length(tif_files) == 0) {
        warning(raster_name, " folder contains no .tif files. Skipping.")
        next
      }

      # load and extract data for all .tif files
      raster_layers <- lapply(tif_files, raster)
      extracted_values <- purrr::map(raster_layers, ~raster::extract(., wards_sp, fun = mean, df = TRUE))

      # merge extracted values from multiple .tif files
      merged_values <- purrr::reduce(extracted_values, ~merge(.x, .y, by = "ID"))
      merged_values[[raster_name]] <- rowMeans(merged_values[, -1], na.rm = TRUE)

      # store results
      results[[raster_name]] <- merged_values[, c("ID", raster_name)]
    }

    message("Combining extracted data into one dataframe...")
    # combine all extracted data into one dataframe
    extracted_df <- Reduce(function(x, y) merge(x, y, by = "ID"), results)
    message("Returning extracted_df for inspection...")
    # Return the extracted dataframe for debugging
    return(extracted_df)

    message("Merging extracted data with shapefile...")
    # merge extracted values back to shapefile
    final_data <- left_join(st_as_sf(wards), extracted_df, by = c("ID" = "ID"))

    # save to CSV
    message("Saving extracted data to CSV...")
    write.csv(st_drop_geometry(final_data), output_csv, row.names = FALSE)

    toc()
    message("Extraction completed successfully for ", state_name)
  }, error = function(e) {
    toc()
    stop("Error during extraction for ", state_name, ": ", e$message)
  })
}
