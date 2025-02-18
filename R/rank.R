# ==========================================================================================================================================
## Script Name: Calculate Rankings from Composite Scores
# Author: Grace Legris, Research Data Analyst
# Date: 02/06/2025
# Purpose: Calculates malaria risk rankings for each ward based on the composite risk scores from each model (different combinations of variables)
# ==========================================================================================================================================

#' Calculate Malaria Risk Rankings from Composite Scores
#'
#' This function calculates malaria risk rankings for each ward based on composite risk scores
#' derived from different models (combinations of risk factor variables). It computes an overall
#' composite score for each ward, normalizes the score, categorizes it, and assigns a rank.
#'
#' @param composite_score_data A data frame containing composite risk scores for each ward. This data frame
#'   should include:
#'   \itemize{
#'     \item \code{WardName}: The name of the ward.
#'     \item \code{WardCode}: The code identifying the ward.
#'     \item One or more columns with names starting with \code{"model_"} that represent composite risk scores
#'           from different models.
#'     \item \code{Urban} (optional): The urban classification for each ward.
#'   }
#'
#' @return A data frame containing the following columns:
#'   \itemize{
#'     \item \code{WardName} and \code{WardCode}: Identifiers for each ward.
#'     \item \code{composite_score}: The overall composite score calculated as the mean of the model scores.
#'     \item \code{new_value}: The normalized composite score scaled between 0 and 1.
#'     \item \code{class}: A categorical variable that divides the normalized score into intervals (bins).
#'     \item \code{rank}: The ranking of wards based on their composite scores.
#'     \item \code{wardname_rank}: A combined string of the ward name and its rank.
#'     \item \code{Urban}: The urban classification of the ward (if available).
#'     \item \code{flag_not_ideal}: A logical flag indicating whether a ward is non-urban (\code{Urban == "No"})
#'           and ranks within the top 5, suggesting it may be less ideal for certain interventions.
#'   }
#'
#' @details The function executes the following steps:
#' \enumerate{
#'   \item \strong{Extract Urban Classification:} It extracts urban classification information (if available)
#'         from the input data.
#'   \item \strong{Reshape Data:} The function reshapes the data from wide to long format to consolidate all
#'         model score columns.
#'   \item \strong{Composite Score Calculation:} It computes an overall composite score for each ward by taking
#'         the mean of all model scores.
#'   \item \strong{Normalization and Categorization:} The composite scores are normalized to a 0-1 scale and
#'         then binned into categories using a 0.2 interval.
#'   \item \strong{Ranking:} Wards are ranked based on their composite scores.
#'   \item \strong{Merging Data:} The urban classification is merged back with the computed rankings.
#'   \item \strong{Flagging:} Wards that are non-urban and have a top-5 rank are flagged as not ideal.
#' }
#'
#' @examples
#' \dontrun{
#' # Example usage:
#' # Assume composite_score_data is a data frame with columns "WardName", "WardCode",
#' # one or more "model_*" columns, and optionally an "Urban" column.
#'
#' ranked_data <- rank(composite_score_data)
#' head(ranked_data)
#' }
#'
#' @import dplyr
#' @import reshape2
#' @export
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
