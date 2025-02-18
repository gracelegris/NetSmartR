# ==========================================================================================================================================
## Script Name: Map Creator
# Author: Grace Legris, Research Data Analyst
# Date: 01/16/25
# Purpose: Transforms any given shapefile to EPSG:4326 and generates the Well-Known Text (WKT) format of the coordinates
# WKT output can be used to retrieve building footprints in that area from Google Open Buildings. Provide the link to their Colab site.
# ==========================================================================================================================================

#' Transform Shapefile to WKT Format
#'
#' This function reads a shapefile, transforms its coordinate reference system to EPSG:4326,
#' and converts the combined geometries into a Well-Known Text (WKT) string. The resulting WKT
#' can be used to retrieve building footprints from sources such as Google Open Buildings.
#'
#' @param shapefile_path A character string specifying the path to the shapefile.
#'
#' @return A character string containing the WKT representation of the combined geometry from the shapefile.
#'
#' @details The function performs the following steps:
#' \enumerate{
#'   \item Reads the input shapefile using the \code{sf} package.
#'   \item Checks if the shapefile is valid (i.e., not empty).
#'   \item Transforms the shapefile's coordinate reference system to EPSG:4326.
#'   \item Combines multiple geometries (if present) into a single feature.
#'   \item Converts the combined geometry to a WKT string.
#' }
#'
#' \strong{Note:} The WKT output can be used to retrieve building footprints from Google Open Buildings.
#' For more information, please visit their Colab site:
#' \url{https://sites.research.google/open-buildings/}.
#'
#' @examples
#' \dontrun{
#' # Replace 'path/to/your/shapefile.shp' with the actual path to your shapefile
#' wkt_result <- shapefile_to_wkt("path/to/your/shapefile.shp")
#' print(wkt_result)
#' }
#'
#' @import sf
#' @export
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
