# ==========================================================================================================================================
## Script Name: Get Urban Percentage Data
# Author: Grace Legris, Research Data Analyst
# Date: 02/11/2025
# Purpose: Checks if the package user correctly specified the path to the urban percentage data downloaded after running the Google Earth
# Engine script. If not, prompts the user to run the script and download the data.
# ==========================================================================================================================================

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
