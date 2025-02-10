# ==========================================================================================================================================
## Script Name: Create Reprioritization Maps
# Author: Grace Legris, Research Data Analyst
# Date: 01/30/25
# Purpose: Creates and saves final reprioritization maps using composite scores.
# ==========================================================================================================================================

prioritize_wards <- function(data, population_col, rank_col, class_col, ward_col, target_percentage = 30) {
  total_population <- sum(data[[population_col]], na.rm = TRUE)

  selected_wards <- c()
  cumulative_population <- 0
  ward_populations <- c()
  ward_percentages <- c()
  WardCode_x <- c()

  # filter out rows with missing population or rank values
  data_sorted <- data[!is.na(data[[population_col]]) & !is.na(data[[rank_col]]), ]
  data_sorted <- data_sorted[order(data_sorted[[rank_col]]), ]

  for (i in 1:nrow(data_sorted)) {
    ward <- data_sorted[i, ]

    # skip if classification column is missing or if the ward is classified as "Rural"
    if (is.na(ward[[class_col]]) || ward[[class_col]] == "Rural") {
      next
    }

    selected_wards <- c(selected_wards, ward[[ward_col]])
    ward_population <- ward[[population_col]]
    cumulative_population <- cumulative_population + ward_population
    current_percentage <- (ward_population / total_population) * 100
    WardCode_x <- c(WardCode_x, ward$WardCode.x)

    ward_populations <- c(ward_populations, ward_population)
    ward_percentages <- c(ward_percentages, round(current_percentage, 2))

    # stop when the cumulative population reaches or exceeds the target percentage
    if (!is.na(current_percentage) && (cumulative_population / total_population) * 100 >= target_percentage) {
      break
    }
  }

  # create a result dataframe
  result <- data.frame(
    SelectedWards = selected_wards,
    WardCode = WardCode_x,
    WardPopulation = ward_populations,
    WardPercentage = ward_percentages,
    CumulativePopulation = cumsum(ward_populations),
    CumulativePercentage = round(cumsum(ward_populations) / total_population * 100, 2)
  )

  return(result)
}

create_reprioritization_map <- function(state_name, shp_dir, output_dir, itn_dir, extracted_data_dir, ranked_wards) {

  # load shapefile, extracted covariates data, and ranked wards df
  state_shp <- st_read(shp_dir)
  extracted_data <- read.csv(extracted_data_dir)

  # load and clean variables - NEED URBAN PERCENTAGE CALCULATION
  # set two urban/rural classification scenarios based on urban percentages
  state_variables <- extracted_data %>%
    distinct(WardCode, .keep_all = TRUE) %>%
    dplyr::select(WardCode, WardName, urbanPercentage) %>%
    mutate(
      classification_20 = ifelse(urbanPercentage > 20, "Urban", "Rural"),
      classification_30 = ifelse(urbanPercentage > 30, "Urban", "Rural"),
      classification_50 = ifelse(urbanPercentage > 50, "Urban", "Rural"),
      classification_75 = ifelse(urbanPercentage > 75, "Urban", "Rural")
    )

  # read and clean ITN data
  state_itn_data <- read.csv(itn_dir)

  colnames(state_itn_data)[colnames(state_itn_data) == "AdminLevel3"] <- "Ward"
  colnames(state_itn_data)[colnames(state_itn_data) == "N_FamilyMembers"] <- "Population"

  state_itn_data <- state_itn_data %>%
    dplyr::select(Population, Ward) %>%
    group_by(Ward) %>%
    summarise(Population = sum(Population, na.rm = T))

  # merge data
  combined_wards <- left_join(state_variables, ranked_wards, by = "WardName")
  combined_wards2 <- left_join(combined_wards, state_itn_data, by = c("WardName" = "Ward"))

  # run prioritize wards function
  prioritized_wards_20 <- prioritize_wards(data = combined_wards2,
                                         population_col = "Population",
                                         rank_col = "rank",
                                         class_col = "classification_20",
                                         ward_col = "WardName",
                                         target_percentage = 30)

  prioritized_wards_30 <- prioritize_wards(data = combined_wards2,
                                         population_col = "Population",
                                         rank_col = "rank",
                                         class_col = "classification_30",
                                         ward_col = "WardName",
                                         target_percentage = 30)

  prioritized_wards_50 <- prioritize_wards(data = combined_wards2,
                                           population_col = "Population",
                                           rank_col = "rank",
                                           class_col = "classification_50",
                                           ward_col = "WardName",
                                           target_percentage = 30)

  prioritized_wards_75 <- prioritize_wards(data = combined_wards2,
                                           population_col = "Population",
                                           rank_col = "rank",
                                           class_col = "classification_75",
                                           ward_col = "WardName",
                                           target_percentage = 30)

  # create risk map using composite scores
  risk_map <- ggplot() +
    geom_sf(data = state_shp %>% left_join(combined_wards2, by = "WardName"),
            aes(geometry = geometry, fill = rank)) +
    scale_fill_gradient(na.value = "grey") +
    labs(title = paste("Malaria Risk Map in", state_name, "State")) +
    map_theme()

  # create reprioritization maps
  reprioritization_map_20 <- ggplot() +
    geom_sf(data = state_shp %>% left_join(prioritized_wards_20, by = c("WardName" = "SelectedWards")),
            aes(geometry = geometry, fill = ifelse(is.na(WardPopulation), "Not Reprioritized", "Reprioritized"))) +
    scale_fill_manual(values = c("Not Reprioritized" = "#F1F2F2", "Reprioritized" = "#00AEEF"), name = "Status") +
    labs(title = paste("Scenario 1 (20% Urban)")) +
    map_theme()

  reprioritization_map_30 <- ggplot() +
    geom_sf(data = state_shp %>% left_join(prioritized_wards_30, by = c("WardName" = "SelectedWards")),
            aes(geometry = geometry, fill = ifelse(is.na(WardPopulation), "Not Reprioritized", "Reprioritized"))) +
    scale_fill_manual(values = c("Not Reprioritized" = "#F1F2F2", "Reprioritized" = "#00AEEF"), name = "Status") +
    labs(title = paste("Scenario 2 (30% Urban)")) +
    map_theme()

  reprioritization_map_50 <- ggplot() +
    geom_sf(data = state_shp %>% left_join(prioritized_wards_50, by = c("WardName" = "SelectedWards")),
            aes(geometry = geometry, fill = ifelse(is.na(WardPopulation), "Not Reprioritized", "Reprioritized"))) +
    scale_fill_manual(values = c("Not Reprioritized" = "#F1F2F2", "Reprioritized" = "#00AEEF"), name = "Status") +
    labs(title = paste("Scenario 3 (50% Urban)")) +
    map_theme()

  reprioritization_map_75 <- ggplot() +
    geom_sf(data = state_shp %>% left_join(prioritized_wards_75, by = c("WardName" = "SelectedWards")),
            aes(geometry = geometry, fill = ifelse(is.na(WardPopulation), "Not Reprioritized", "Reprioritized"))) +
    scale_fill_manual(values = c("Not Reprioritized" = "#F1F2F2", "Reprioritized" = "#00AEEF"), name = "Status") +
    labs(title = paste("Scenario 4 (75% Urban)")) +
    map_theme()

  # remove legends
  reprioritization_map_20 <- reprioritization_map_20 + theme(legend.position = "none")
  reprioritization_map_30 <- reprioritization_map_30 + theme(legend.position = "none")
  reprioritization_map_50 <- reprioritization_map_50 + theme(legend.position = "none")
  reprioritization_map_75 <- reprioritization_map_75 + theme(legend.position = "none")

  map_grid <- grid.arrange(reprioritization_map_20, reprioritization_map_30, reprioritization_map_50, reprioritization_map_75, nrow = 2, ncol = 2)

  final_grid <- grid.arrange(
    map_grid,
    top = textGrob(paste("Reprioritization Scenarios in", state_name),
                   gp = gpar(fontsize = 12, fontface = "bold", hjust = 0.5))
  )

  return(list(risk_map = risk_map, reprioritization_map = final_grid))
}
