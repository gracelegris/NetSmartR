# ==========================================================================================================================================
## Script Name: Map Creator
# Author: Grace Legris, Research Data Analyst
# Date: 01/16/25
# Purpose: Transforms any given shapefile to EPSG:4326 and generates the Well-Known Text (WKT) format of the coordinates
# WKT output can be used to retrieve building footprints in that area from Google Open Buildings. Provide the link to their Colab site.
# ==========================================================================================================================================

# function to transform a shapefile to EPSG:4326 and generate WKT
shapefile_to_wkt <- function(shapefile_path) {
  # read the shapefile
  message("Reading in shapefile...")
  shapefile <- st_read(shapefile_path, quiet = TRUE)

  # check if the shapefile is valid
  if (is.null(shapefile) || nrow(shapefile) == 0) {
    stop("Error: The shapefile is empty or invalid.")
  }

  # transform the shapefile to EPSG:4326
  shapefile_transformed <- st_transform(shapefile, crs = 4326)

  # combine geometries into a single feature (if multiple exist)
  combined_geometry <- st_union(shapefile_transformed$geometry)

  # Convert combined geometry to WKT format
  wkt_output <- st_as_text(combined_geometry)

  # Return the WKT string
  return(wkt_output)

  message("Note for users: The WKT output can be used to retrieve building footprints from Google Open Buildings. For more information, visit their Colab site: https://sites.research.google/open-buildings/")
}

# Example usage:
# Replace 'your_shapefile_path.shp' with path to shapefile
# wkt_result <- transform_shapefile_to_wkt("path/to/your/shapefile.shp")
# print(wkt_result)
