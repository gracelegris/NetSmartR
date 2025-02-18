# ==========================================================================================================================================
## Script Name: Create Reprioritization Maps
# Author: Grace Legris, Research Data Analyst
# Date: 01/30/25
# Purpose: Creates and saves final reprioritization maps using composite scores.
# ==========================================================================================================================================

#' Prioritize Wards Based on Population and Ranking
#'
#' This function selects wards for reprioritization based on their population and malaria risk ranking.
#' Wards are added sequentially (skipping those classified as "Rural" or with missing classification)
#' until the cumulative population of selected wards reaches or exceeds the specified target percentage.
#'
#' @param data A data frame containing ward-level data. It must include:
#'   \itemize{
#'     \item A population column (specified by \code{population_col}).
#'     \item A ranking column (specified by \code{rank_col}).
#'     \item A classification column (specified by \code{class_col}) indicating urban/rural status.
#'     \item A ward identifier column (specified by \code{ward_col}).
#'   }
#' @param population_col A character string specifying the name of the population column in \code{data}.
#' @param rank_col A character string specifying the name of the ranking column in \code{data}.
#' @param class_col A character string specifying the name of the classification column (e.g., urban/rural) in \code{data}.
#' @param ward_col A character string specifying the name of the ward identifier column in \code{data}.
#' @param target_percentage A numeric value (default is 30) representing the target percentage of the total population
#'   that the selected wards should cumulatively cover.
#'
#' @return A data frame containing:
#'   \itemize{
#'     \item \code{SelectedWards}: The names/identifiers of the selected wards.
#'     \item \code{WardCode}: The ward codes (if available).
#'     \item \code{WardPopulation}: The population for each selected ward.
#'     \item \code{WardPercentage}: The percentage of the total population represented by each ward.
#'     \item \code{CumulativePopulation}: The cumulative population sum of the selected wards.
#'     \item \code{CumulativePercentage}: The cumulative population percentage relative to the total population.
#'   }
#'
#' @details The function first filters out rows with missing population or ranking values and sorts the data
#' based on the ranking. It then iteratively adds wards (skipping those with missing or "Rural" classification)
#' until the cumulative population of the selected wards meets or exceeds the \code{target_percentage}.
#'
#' @examples
#' \dontrun{
#' # Assuming combined_data is a data frame with the required columns:
#' prioritized <- prioritize_wards(
#'   data = combined_data,
#'   population_col = "Population",
#'   rank_col = "rank",
#'   class_col = "classification_30",
#'   ward_col = "WardName",
#'   target_percentage = 30
#' )
#' print(prioritized)
#' }
#'
#' @export
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


#' Merge Test Positivity Rate Data with Extracted Data
#'
#' This function reads a CSV file containing malaria test positivity rate (TPR) data and merges it with the
#' extracted covariate data based on the ward name.
#'
#' @param tpr_data_path A character string specifying the file path to the TPR data CSV.
#' @param extracted_data A data frame containing extracted covariate data. This data frame must include a column
#'   named \code{WardName} that will be used to merge with the TPR data.
#'
#' @return A data frame resulting from a left join of \code{extracted_data} with the selected TPR column
#'   (\code{u5_tpr_rdt}) from the TPR data.
#'
#' @details The function selects only the \code{WardName} and \code{u5_tpr_rdt} columns from the TPR data
#' and performs a left join with \code{extracted_data} based on the \code{WardName} column.
#'
#' @examples
#' \dontrun{
#' merged_data <- merge("path/to/tpr_data.csv", extracted_data)
#' head(merged_data)
#' }
#'
#' @import dplyr
#' @export
merge <- function(tpr_data_path, extracted_data) {
  tpr_data <- read.csv(tpr_data_path)
  extracted_data_plus <- extracted_data %>%
    left_join(tpr_data %>% dplyr::select(WardName, u5_tpr_rdt), by = "WardName")
}


#' Create and Save Reprioritization Maps Using Composite Scores
#'
#' This function creates final reprioritization maps for a given state by combining spatial data,
#' extracted covariate data, ITN distribution data, and ranked ward information. It generates a risk map
#' based on composite scores as well as reprioritization maps under multiple urban classification scenarios.
#'
#' @param state_name A character string representing the name of the state.
#' @param shp_dir A character string specifying the file path to the state's shapefile.
#' @param output_dir A character string specifying the directory where the output maps will be saved.
#' @param itn_dir A character string specifying the file path to the ITN distribution data CSV.
#' @param extracted_data_dir A character string specifying the file path to the CSV containing extracted covariate data.
#' @param ranked_wards A data frame containing ward ranking information. This should include at least the ward name and ranking.
#'
#' @return A list containing:
#'   \itemize{
#'     \item \code{risk_map}: A ggplot object representing the malaria risk map.
#'     \item \code{reprioritization_map}: A ggplot grob (grid object) combining multiple reprioritization maps.
#'   }
#'
#' @details The function performs the following steps:
#' \enumerate{
#'   \item Loads the state's shapefile and extracted covariate data.
#'   \item Creates urban/rural classification scenarios based on varying urban percentage thresholds (20%, 30%, 50%, and 75%).
#'   \item Reads and processes ITN distribution data to obtain ward populations.
#'   \item Merges the covariate, ranking, and ITN data to form a comprehensive dataset.
#'   \item Runs the \code{prioritize_wards} function for each urban classification scenario to determine which wards
#'         are reprioritized.
#'   \item Generates a risk map (using composite scores) and four reprioritization maps (one for each urban scenario)
#'         using \code{ggplot2} and a custom map theme (\code{map_theme}).
#'   \item Arranges the reprioritization maps into a grid and adds a title.
#' }
#'
#' @note This function relies on external data files and an active internet connection to download spatial data
#'   (if needed). It also assumes that the custom function \code{map_theme()} is defined in the package.
#'
#' @examples
#' \dontrun{
#' result_maps <- create_reprioritization_map(
#'   state_name = "Kano",
#'   shp_dir = "path/to/state_shapefile.geojson",
#'   output_dir = "path/to/output_directory",
#'   itn_dir = "path/to/itn_data.csv",
#'   extracted_data_dir = "path/to/extracted_data.csv",
#'   ranked_wards = ranked_data
#' )
#'
#' # To view the risk map:
#' print(result_maps$risk_map)
#'
#' # To view the reprioritization map grid:
#' grid::grid.draw(result_maps$reprioritization_map)
#' }
#'
#' @import dplyr
#' @import ggplot2
#' @import gridExtra
#' @import grid
#' @importFrom sf st_read
#' @export
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
    scale_fill_gradient(name = "Rank", na.value = "grey") +
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
