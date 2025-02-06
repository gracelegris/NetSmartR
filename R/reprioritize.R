# ==========================================================================================================================================
## Script Name: Create Reprioritization Maps
# Author: Grace Legris, Research Data Analyst
# Date: 01/30/25
# Purpose: Creates and saves final reprioritization maps using composite scores.
# ==========================================================================================================================================

create_reprioritization_map <- function(state_name, shp_dir, output_dir, itn_dir, prioritized_wards, composite_score_data) {

  # load shapefile
  state_shp <- st_read(shp_dir)

  prioritized_wards <- read.csv(prior_dir)

  # load and clean variables - NEED URBAN PERCENTAGE CALCULATION
  # set two urban/rural classification scenarios based on urban percentages
  state_variables <- prioritized_wards %>%
    distinct(WardCode, .keep_all = TRUE) %>%
    dplyr::select(WardCode, WardName, urbanPercentage) %>%
    mutate(
      classification_20 = ifelse(urbanPercentage > 20, "Urban", "Rural"),
      classification_30 = ifelse(urbanPercentage > 30, "Urban", "Rural")
    )

  # read and clean ITN data
  state_itn_data <- read.csv(itn_dir)

  state_itn_data$Ward = state_itn_data$AdminLevel3

  state_itn_data <- state_itn_data %>%
    dplyr::select(N_FamilyMembers, AdminLevel3) %>%
    group_by(AdminLevel3) %>%
    summarise(Population = sum(N_FamilyMembers, na.rm = TRUE))

  # merge composite score data
  malaria_risk_scores <- malaria_risk_scores %>%
    dplyr::select(WardName, model_1)  # Choose the desired model (or modify for multiple models) - ADD AS ARGUMENT????

  combined_wards <- left_join(state_variables, malaria_risk_scores, by = "WardName")
  combined_wards2 <- left_join(combined_wards, state_itn_data, by = c("WardName" = "Ward"))

  # prioritize wards using composite scores instead of rankings
  prioritized_wards_20 <- prioritize_wards(combined_wards2, "Population", "model_1", "classification_20", "WardName", 30)
  prioritized_wards_30 <- prioritize_wards(combined_wards2, "Population", "model_1", "classification_30", "WardName", 30)

  # create risk map using composite scores
  risk_map <- ggplot() +
    geom_sf(data = state_shp %>% left_join(combined_wards2, by = "WardName"),
            aes(geometry = geometry, fill = as.numeric(model_1))) +
    scale_fill_gradient(na.value = "grey") +
    labs(title = paste("Risk Map in", state_name)) +
    map_theme()

  # create reprioritization map
  reprioritization_map <- ggplot() +
    geom_sf(data = state_shp %>% left_join(prioritized_wards_20, by = c("WardName" = "SelectedWards")),
            aes(geometry = geometry, fill = ifelse(is.na(WardPopulation), "Not Reprioritized", "Reprioritized"))) +
    scale_fill_manual(values = c("Not Reprioritized" = "red", "Reprioritized" = "green"), name = "Status") +
    labs(title = paste("Reprioritization Scenario 1 -", state_name),
         caption = "Wards selected for reprioritization based on composite scores and urban classification.") +
    map_theme()

  return(list(risk_map = risk_map, reprioritization_map = reprioritization_map))
}
