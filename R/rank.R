# ==========================================================================================================================================
## Script Name: Calculate Rankings from Composite Scores
# Author: Grace Legris, Research Data Analyst
# Date: 02/06/2025
# Purpose: Calculates malaria risk rankings for each ward based on the composite risk scores from each model (different combinations of variables)
# ==========================================================================================================================================

rank <- function(composite_score_data) {
  # extract the urban classification for each ward
  urban_data <- composite_score_data %>% dplyr::select(WardName, Urban, WardCode)

  # reshape the data to long format, keeping only model score columns and ward names
  melted_data <- composite_score_data %>%
    dplyr::select(WardName, WardCode, starts_with("model_")) %>%  # select model columns and ward name
    reshape2::melt(id.vars = c("WardName", "WardCode"), variable.name = "variable", value.name = "value")

  # compute an overall composite score for each ward (e.g., mean of all model scores)
  ward_scores <- melted_data %>%
    group_by(WardName, WardCode) %>%
    summarise(composite_score = mean(value, na.rm = TRUE), .groups = "drop")

  # normalize the composite score between 0 and 1
  ward_scores <- ward_scores %>%
    mutate(
      new_value = (composite_score - min(composite_score)) / (max(composite_score) - min(composite_score)),
      class = cut(new_value, seq(0, 1, 0.2), include.lowest = TRUE)
    ) %>%

    # rank wards based on the composite score
    arrange(composite_score) %>%
    mutate(
      rank = row_number(),
      wardname_rank = paste(WardName, "(", rank, ")")
    )

  # merge back urban classification
  plottingdata <- ward_scores %>%
    left_join(urban_data, by = c("WardName", "WardCode")) %>%
    mutate(flag_not_ideal = ifelse(Urban == "No" & rank <= 5, TRUE, FALSE))

  # print a summary of the processed dataset
  # print("Plotting data summary:")
  # print(summary(plottingdata))

  # return the processed dataset
  return(plottingdata)
}
