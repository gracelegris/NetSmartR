
# Function to extract raster data for a given shapefile
extract_raster_data <- function(state_name, shapefile_path, raster_paths, output_csv) {
  tic(paste("Starting data extraction for", state_name))

  tryCatch({
    # Load shapefile
    message("Loading shapefile for ", state_name)
    wards <- st_read(shapefile_path) %>%
      st_make_valid() %>%
      filter(!st_is_empty(.))

    # Ensure the key columns exist in the shapefile
    if (!"WardCode" %in% colnames(wards)) {
      stop("The shapefile does not contain the 'WardCode' column.")
    }

    wards_sp <- as(wards, "Spatial") # Convert to spatial object for raster extraction

    results <- list() # To store extraction results

    # Loop through raster paths for extraction
    for (raster_name in names(raster_paths)) {
      raster_folder <- raster_paths[[raster_name]]
      message("Processing ", raster_name, "...")

      # List all .tif files in the folder
      tif_files <- list.files(raster_folder, pattern = "\\.tif$", full.names = TRUE)
      if (length(tif_files) == 0) {
        warning(raster_name, " folder contains no .tif files. Skipping.")
        next
      }

      # Load and extract data for all .tif files
      raster_layers <- lapply(tif_files, raster)
      extracted_values <- purrr::map(raster_layers, ~raster::extract(., wards_sp, fun = mean, df = TRUE))

      # Merge extracted values from multiple .tif files
      merged_values <- purrr::reduce(extracted_values, ~merge(.x, .y, by = "ID"))
      merged_values[[raster_name]] <- rowMeans(merged_values[, -1], na.rm = TRUE)

      # Rename the ID column to match the key column in shapefile
      colnames(merged_values)[1] <- "WardCode"

      # Convert WardCode to character to ensure consistent type
      merged_values$WardCode <- as.character(merged_values$WardCode)

      # Store results
      results[[raster_name]] <- merged_values[, c("WardCode", raster_name)]
    }

    message("Combining extracted data into one dataframe...")
    # Combine all extracted data into one dataframe
    extracted_df <- Reduce(function(x, y) merge(x, y, by = "WardCode"), results)

    # Convert WardCode in shapefile to character for consistency
    wards$WardCode <- as.character(wards$WardCode)

    message("Merging extracted data with shapefile...")
    # Merge extracted values back to shapefile
    final_data <- left_join(st_as_sf(wards), extracted_df, by = "WardCode")

    # Save to CSV
    message("Saving extracted data to CSV...")
    write.csv(st_drop_geometry(final_data), output_csv, row.names = FALSE)

    toc()
    message("Extraction completed successfully for ", state_name)
  }, error = function(e) {
    toc()
    stop("Error during extraction for ", state_name, ": ", e$message)
  })
}
