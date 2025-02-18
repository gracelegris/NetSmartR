# ==========================================================================================================================================
## Script Name: Get Urban Percentage Data
# Author: Grace Legris, Research Data Analyst
# Date: 02/11/2025
# Purpose: Checks if the package user correctly specified the path to the urban percentage data downloaded after running the Google Earth
# Engine script. If not, prompts the user to run the script and download the data.
# ==========================================================================================================================================

#' Get Urban Percentage Data
#'
#' This function checks whether the specified urban percentage data file exists. If the file does not exist,
#' it prompts the user with detailed instructions on how to run a Google Earth Engine (GEE) script to generate the data.
#'
#' @param urban_data_path A character string specifying the file path to the urban percentage data (CSV) downloaded
#' after running the Google Earth Engine script.
#'
#' @return If the file exists, a data frame containing the urban percentage data is returned. Otherwise, the function
#' stops execution after prompting the user to generate and download the necessary data.
#'
#' @details The function performs the following steps:
#' \enumerate{
#'   \item Checks if the file specified by \code{urban_data_path} exists.
#'   \item If the file does not exist, it prints messages guiding the user through the steps needed to generate and download the data using GEE.
#'   \item If the file exists, the function reads the CSV file into a data frame and returns it.
#' }
#'
#' @examples
#' \dontrun{
#' # Specify the path to your urban percentage data CSV file
#' urban_file_path <- "path/to/Urban_Percentage.csv"
#'
#' # Attempt to load the urban percentage data
#' urban_data <- get_urban_percentage(urban_file_path)
#'
#' # If the file is found, you can proceed with your analysis using urban_data
#' head(urban_data)
#' }
#'
#' @export
get_urban_percentage <- function(urban_data_path) {
  if (!file.exists(urban_data_path)) {
    message("\n⚠️  Urban percentage data is missing! ⚠️")
    message("To proceed, you need to calculate urban percentage for each ward using Google Earth Engine.")
    message("Follow these steps:")
    message("1️⃣  Open this Google Earth Engine script:https://code.earthengine.google.com/65880dede3b29e14f98ff1a037995a6bK")
    message("2️⃣  Run the script to calculate urban percentage for each ward.")
    message("3️⃣  Download the resulting CSV file and save it here: ", urban_data_path)
    message("4️⃣  Rerun this function after saving the file.")

    stop("Urban percentage data is required. Please follow the instructions above and try again.")
  } else {
    message("Urban percentage data found. Proceeding with analysis...")
    urban_data <- read.csv(urban_data_path)
    return(urban_data)
  }
}
