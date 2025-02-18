# ==========================================================================================================================================
## Script Name: Calculate Composite Scores
# Author: Grace Legris, Research Data Analyst
# Date: 01/30/25
# Purpose: Calculates composite risk scores for each ward using extracted variable data.
# ==========================================================================================================================================

#' Normalize a Numeric Vector
#'
#' This helper function normalizes a numeric vector to a range between 0 and 1.
#'
#' @param x A numeric vector.
#'
#' @return A numeric vector with values scaled to the interval [0, 1]. Any \code{NA} values in the input remain \code{NA}.
#'
#' @details The normalization is performed by subtracting the minimum value and dividing by the range of the data (i.e., maximum minus minimum). If the vector contains only \code{NA}s, the result will also be \code{NA}.
#'
#' @examples
#' normalize(c(1, 3, 5, NA))
#'
#' @export
normalize <- function(x) {
  ifelse(is.na(x), NA, (x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE)))
}

#' Calculate Composite Malaria Risk Scores
#'
#' This function calculates composite malaria risk scores for each ward using extracted variable data.
#'
#' @param extracted_data A data frame containing extracted variables for each ward. The data frame should have numeric columns corresponding to various risk factors.
#' @param covariates A character vector of covariate names to be used in the composite score calculation. Only the covariates that exist in \code{extracted_data} will be used.
#'
#' @return A data frame identical to \code{extracted_data} with additional columns representing the composite risk scores. Each new column is named \code{model_X} (where \code{X} is a unique model identifier) corresponding to the average of the normalized values of a unique combination of two or more covariates.
#'
#' @details The function operates as follows:
#'
#' \enumerate{
#'   \item It verifies that at least two valid covariates are available from \code{extracted_data}.
#'   \item It normalizes the selected covariate columns to a 0 to 1 range using the \code{normalize} function.
#'   \item It generates all possible combinations of two or more normalized covariates.
#'   \item For each combination, it computes a composite score by taking the average of the selected normalized covariates.
#' }
#'
#' This composite scoring approach allows users to explore different combinations of risk factors.
#'
#' @examples
#' \dontrun{
#' # Assume you have a CSV file with extracted ward variables
#' extracted_data <- read.csv("path/to/wards_variables.csv")
#'
#' # Define the covariates to use in the composite score calculation
#' covariates <- c("mean_EVI", "mean_NDVI", "mean_rainfall")
#'
#' # Calculate composite malaria risk scores
#' composite_scores <- calculate_malaria_risk_scores(extracted_data, covariates)
#'
#' # View the first few rows of the resulting data frame
#' head(composite_scores)
#' }
#'
#' @import dplyr
#' @importFrom rlang sym
#' @export
calculate_malaria_risk_scores <- function(extracted_data, covariates) {

  # ensure covariates exist in data
  covariates <- covariates[covariates %in% names(extracted_data)]

  if (length(covariates) < 2) {
    stop("At least two valid covariates are required for composite score calculation.")
  }
  message("covariate check passed")

  # normalize selected covariates
  data_normalized <- extracted_data %>%
    mutate(across(all_of(covariates), normalize, .names = "norm_{.col}"))
  message("data normalized.")

  # get normalized column names
  norm_cols <- paste0("norm_", covariates)

  # generate all variable combinations for composite scores
  model_combinations <- list()
  for (i in 2:length(norm_cols)) {
    model_combinations <- c(model_combinations, combn(norm_cols, i, simplify = FALSE))
  }
  message("model combinations done.")

  # compute composite scores: the average of the selected normalized variables
  for (i in seq_along(model_combinations)) {
    model_name <- paste0("model_", i)
    vars <- model_combinations[[i]]

    print(paste("Processing", model_name, "with", length(vars), "variables..."))

    data_normalized <- data_normalized %>%
      mutate(!!sym(model_name) := rowSums(dplyr::select(., all_of(vars))) / length(vars))

    print(paste(model_name, "completed"))
  }
  message("composite scores computed.")

  return(data_normalized)
}
