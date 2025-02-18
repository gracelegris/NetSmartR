# NetSmartR
Grace's R package compiling analyses for NMEP request.
# NetSmartR

NetSmartR: Insecticide-Treated Net Reprioritization in Nigeria

Overview

NetSmartR is an R package designed to facilitate the reprioritization of wards in Nigerian states for malaria intervention strategies. This package integrates multiple data sources, including user-specified environmental variables, malaria test positivity rate (TPR) data, and urban percentage estimates obtained from Google Earth Engine. The package provides a structured workflow for calculating composite malaria risk scores and generating reprioritization maps.

Installation

To install and use NetSmartR, you can install it from GitHub using:

install.packages("devtools")
devtools::install_github("yourusername/NetSmartR")

Data Requirements

Before running NetSmartR, you need to gather the necessary datasets:
	1.	Shapefile of the State: A shapefile containing ward boundaries for the selected Nigerian state.
	2.	Malaria TPR Data: A CSV file containing test positivity rates for each ward.
	3.	ITN Distribution Data: A CSV file containing information on insecticide-treated net (ITN) distributions.
	4.	Raster Datasets: GeoTIFF files for the environmental covariates you would like to use in calculating the composite malaria risk score for each ward (e.g., distance to water, elevation).
	5.	Urban Percentage Data: A GeoJSON file containing the percentage of urban land cover for each ward, obtained using Google Earth Engine (GEE).

Getting Urban Percentage Data from Google Earth Engine

Users need to retrieve urban percentage data using a Google Earth Engine script. Follow these steps:
	1.	Open the GEE Script
	•	Visit Google Earth Engine and sign in with your Google account.
	•	Copy and paste the provided GEE script (GEE_script.txt) into the code editor.
	2.	Modify the Asset Path
	•	Update any asset paths in the script to reflect your own Google Earth Engine account.
	•	Example modification:

var wards = ee.FeatureCollection("users/yourusername/Wards");


	3.	Run the Script and Export Data
	•	Run the script to compute urban percentages for each ward.
	•	Export the resulting data as a GeoJSON file to your Google Drive.
	•	Example export command in GEE:

Export.table.toDrive({
  collection: wards_with_urban_percentage,
  description: "Urban_Percentage",
  fileFormat: "GeoJSON"
});


	4.	Download the GeoJSON File
	•	Navigate to your Google Drive, locate the exported file, and download it to your local machine.
	5.	Read the GeoJSON File into R
	•	Use the following R code to load the urban percentage data:

library(sf)
urban_data <- st_read("path/to/Urban_Percentage.geojson")



Running the Reprioritization Analysis

Once all required datasets are available, run the main function:

library(NetSmartR)

results <- reprioritize(
  state_name = "Kano",
  shapefile_path = "path/to/Kano_State.shp",
  tpr_data_path = "path/to/Kano_TPR.csv",
  output_dir = "path/to/output",
  itn_dir = "path/to/ITN_distribution_Kano.csv",
  raster_paths = list(
    h2o_distance_path = "path/to/distance_to_water.tif",
    elevation_path = "path/to/Elevation.tif"
  ),
  risk_factors = c("distance_to_water", "settlement_type", "urban_percentage", "u5_tpr_rdt”),
  urban_data_path = file.path(”file/path”)
)

Output

The package generates:
	•	A composite malaria risk score dataset for each ward.
	•	Ranked ward prioritization tables for intervention planning.
	•	Malaria risk maps showing the wards ranked by composite malaria risk score.
	•	Reprioritization maps showing spatial distributions of malaria risk.

License

This package is licensed under the MIT License.
