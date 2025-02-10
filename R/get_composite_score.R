# ==========================================================================================================================================
## Script Name: Calculate Composite Scores
# Author: Grace Legris, Research Data Analyst
# Date: 01/30/25
# Purpose: Calculates composite risk scores for each ward using extracted variable data.
# ==========================================================================================================================================

# function to normalize variables to a range of 0 to 1
normalize <- function(x) {
  ifelse(is.na(x), NA, (x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE)))
}

# function to calculate composite malaria risk scores
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
