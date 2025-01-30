# ==========================================================================================================================================
## Script Name: Calculate Composite Scores
# Author: Grace Legris, Research Data Analyst
# Date: 01/30/25
# Purpose: Calculates composite risk scores for each ward using extracted variable data.
# ==========================================================================================================================================

# read data
scoring_dataset <- read.csv(file.path("/Users/grace/Urban Malaria Proj Dropbox/urban_malaria/projects/urban_microstratification/Shiny App/Final Extractions/kano.csv"))

# function to normalize variables to a range of 0 to 1
normalize <- function(x) {
  ifelse(is.na(x), NA, (x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE)))
}

# apply normalization to relevant variables
df_normalized <- scoring_dataset %>%
  mutate(
    mean_EVI_normal_score = normalize(mean_EVI),
    mean_rainfall_normal_score = normalize(mean_rainfall),
    avg_tpr_normal_score = normalize(u5_tpr_rdt),
    #Slum_Count_normal_score = normalize(Slum_Count),
    #mean_WF_normal_score = normalize(mean_WF)
  )

# create composite malaria risk scores based on different variable combinations
scoring_dataset2 <- df_normalized %>%
  dplyr::select(WardCode, StateCode,
                mean_EVI_normal_score,
                mean_rainfall_normal_score,
                avg_tpr_normal_score,
                settlement_type,
                housing_quality) %>%
  mutate(
    # model01 = avg_tpr_normal_score + mean_EVI_normal_score + mean_rainfall_normal_score,
    # model02 = avg_tpr_normal_score + mean_EVI_normal_score + mean_rainfall_normal_score,
    # model03 = mean_EVI_normal_score + mean_rainfall_normal_score,
    # model04 = avg_tpr_normal_score + mean_rainfall_normal_score,
    # model05 = avg_tpr_normal_score + mean_EVI_normal_score + mean_rainfall_normal_score,
    # model06 = avg_tpr_normal_score + mean_EVI_normal_score,
    # model07 = mean_EVI_normal_score + settlement_type + mean_rainfall_normal_score,
    # model08 = avg_tpr_normal_score + settlement_type + mean_rainfall_normal_score,
    # model09 = avg_tpr_normal_score + mean_EVI_normal_score + mean_rainfall_normal_score,
    # model10 = avg_tpr_normal_score + mean_EVI_normal_score + settlement_type,
    # model11 = settlement_type + mean_rainfall_normal_score + housing_quality,
    # model12 = mean_EVI_normal_score + mean_rainfall_normal_score + housing_quality,
    # model13 = mean_EVI_normal_score + settlement_type + housing_quality,
    # model14 = avg_tpr_normal_score + mean_rainfall_normal_score + housing_quality,
    # model15 = avg_tpr_normal_score + settlement_type + housing_quality,
    # model16 = avg_tpr_normal_score + mean_EVI_normal_score + housing_quality,
    # model17 = settlement_type + mean_rainfall_normal_score,
    # model18 = mean_EVI_normal_score + mean_rainfall_normal_score,
    # model19 = mean_EVI_normal_score + settlement_type,
    # model20 = avg_tpr_normal_score + mean_rainfall_normal_score,
    # model21 = avg_tpr_normal_score + settlement_type,
    # model22 = avg_tpr_normal_score + mean_EVI_normal_score,
    # model23 = mean_rainfall_normal_score + housing_quality,
    # model24 = settlement_type + housing_quality,
    # model25 = mean_EVI_normal_score + housing_quality,
    # model26 = avg_tpr_normal_score + housing_quality
  )

# save cleaned and processed dataset (if needed)
write.csv(scoring_dataset2, file.path(AbidjanDir, "processed_malaria_risk_scores.csv"), row.names = FALSE)
