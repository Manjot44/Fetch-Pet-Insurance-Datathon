# full model training

View(final_pred)


# full data for training:

# Frequency:

full_data <- rbind(train_data, test_data)

full_data_cat1 <- rbind(train_data_cat1, test_data_cat1)

full_data_cat2 <- rbind(train_data_cat2, test_data_cat2)

full_data_cat3 <- rbind(train_data_cat3, test_data_cat3)

full_data_sev_total <- rbind(full_data_cat1, full_data_cat2, full_data_cat3)

#----Frequency Models----

# Load required libraries
library(caret)
library(reticulate)

# Import the Python interpret library
interpret <- import("interpret")
interpret_glassbox <- interpret$glassbox

# Function to adjust test_data_prem columns, build EBM, and predict
evaluate_and_predict_ebm <- function(train_data, test_data_prem, target_var, vars_to_remove) {
  # Print the test_data_prem dataframe to see its structure and values before any adjustments
  print("The final_pred (test_data_prem) dataframe before adjustments:")
  print(head(test_data_prem))  # Prints the first few rows
  print(str(test_data_prem))   # Prints the structure of the dataframe
  
  # Remove specified variables from training set
  train_data <- train_data[, !names(train_data) %in% vars_to_remove]
  
  # Remove rows with NAs in training data
  train_data <- na.omit(train_data)
  
  # Check if training data is empty after filtering
  if (nrow(train_data) == 0) {
    stop("Training data is empty after removing NAs or specified variables.")
  }
  
  # Prepare predictor variables and target variable for training
  X_train <- train_data[, !names(train_data) %in% target_var, drop = FALSE]
  y_train <- train_data[[target_var]]
  
  # Check for extra columns in test_data_prem that are not in train_data
  extra_columns <- setdiff(colnames(test_data_prem), colnames(train_data))
  
  # Remove extra columns from test_data_prem
  test_data_prem_adjusted <- test_data_prem[, !names(test_data_prem) %in% extra_columns, drop = FALSE]
  
  # Check for missing columns in test_data_prem that are present in train_data
  missing_columns <- setdiff(colnames(train_data), colnames(test_data_prem_adjusted))
  
  # Add missing columns to test_data_prem_adjusted, filling with NA
  for (col in missing_columns) {
    test_data_prem_adjusted[[col]] <- NA
  }
  
  # Handle missing values in test_data_prem_adjusted by filling with 0 (or use other strategies)
  test_data_prem_adjusted[is.na(test_data_prem_adjusted)] <- 0
  
  # Prepare predictor variables for test_data_prem
  X_test_prem <- test_data_prem_adjusted[, !names(test_data_prem_adjusted) %in% target_var, drop = FALSE]
  
  # Convert data frames to matrices for Python
  X_train_matrix <- as.matrix(X_train)
  X_test_matrix <- as.matrix(X_test_prem)
  
  # Check the dimensions of the matrices
  print(dim(X_train_matrix))
  print(dim(X_test_matrix))
  
  # Create and train the EBM model
  ebm <- interpret_glassbox$ExplainableBoostingRegressor()
  ebm$fit(X_train_matrix, y_train)
  
  # Make predictions using the adjusted test data prem
  predictions_prem <- ebm$predict(X_test_matrix)
  
  # Check if predictions are generated
  if (length(predictions_prem) == 0) {
    stop("No predictions were generated.")
  }
  
  # Ensure predictions are not negative
  if (any(predictions_prem < 0)) {
    smallest_positive_value <- min(predictions_prem[predictions_prem > 0], na.rm = TRUE)
    predictions_prem[predictions_prem < 0] <- smallest_positive_value
  }
  
  # Return the predictions
  return(predictions_prem)
}

# Define the list of variables to remove
vars_to_remove <- c("claim_count", "earned_units", "cat1", "cat2", "cat3", 
                    "claim_rate_cat2", "claim_rate_cat3", "Drooling.Potential",
                    "nb_contribution", "Energy.Level", "policy_age_group", 
                    "is_multi_pet_plan", "pet_de_sexed", "UW_season")

# Call the function to predict for test_data_prem using the EBM model for Cat1
predicted_frequencies_prem_cat1 <- evaluate_and_predict_ebm(
  train_data = full_data, 
  test_data_prem = final_pred, 
  target_var = "claim_rate_cat1", 
  vars_to_remove = vars_to_remove
)

# Print the predictions
nrow(predicted_frequencies_prem_cat1)


# Load the interpret library from Python
interpret <- import("interpret")
interpret_glassbox <- interpret$glassbox

# Function to adjust test_data_prem columns, build EBM, and predict for Cat2
evaluate_and_predict_ebm_cat2 <- function(train_data, test_data_prem, target_var, vars_to_remove) {
  # Print the test_data_prem dataframe to see its structure and values before any adjustments
  print("The final_pred (test_data_prem) dataframe before adjustments for Cat2:")
  print(head(test_data_prem))  # Prints the first few rows
  print(str(test_data_prem))   # Prints the structure of the dataframe
  
  # Remove specified variables from training set
  train_data <- train_data[, !names(train_data) %in% vars_to_remove]
  
  # Remove rows with NAs in training data
  train_data <- na.omit(train_data)
  
  # Check if training data is empty after filtering
  if (nrow(train_data) == 0) {
    stop("Training data is empty after removing NAs or specified variables.")
  }
  
  # Prepare predictor variables and target variable for training
  X_train <- train_data[, !names(train_data) %in% target_var, drop = FALSE]
  y_train <- train_data[[target_var]]
  
  # Check for extra columns in test_data_prem that are not in train_data
  extra_columns <- setdiff(colnames(test_data_prem), colnames(train_data))
  
  # Remove extra columns from test_data_prem
  test_data_prem_adjusted <- test_data_prem[, !names(test_data_prem) %in% extra_columns, drop = FALSE]
  
  # Check for missing columns in test_data_prem that are present in train_data
  missing_columns <- setdiff(colnames(train_data), colnames(test_data_prem_adjusted))
  
  # Add missing columns to test_data_prem_adjusted, filling with NA
  for (col in missing_columns) {
    test_data_prem_adjusted[[col]] <- NA
  }
  
  # Handle missing values in test_data_prem_adjusted by filling with 0 (or use other strategies)
  test_data_prem_adjusted[is.na(test_data_prem_adjusted)] <- 0
  
  # Prepare predictor variables for test_data_prem
  X_test_prem <- test_data_prem_adjusted[, !names(test_data_prem_adjusted) %in% target_var, drop = FALSE]
  
  # Convert data frames to matrices for Python
  X_train_matrix <- as.matrix(X_train)
  X_test_matrix <- as.matrix(X_test_prem)
  
  # Check the dimensions of the matrices
  print(dim(X_train_matrix))
  print(dim(X_test_matrix))
  
  # Create and train the EBM model
  ebm <- interpret_glassbox$ExplainableBoostingRegressor()
  ebm$fit(X_train_matrix, y_train)
  
  # Make predictions using the adjusted test data prem
  predictions_prem <- ebm$predict(X_test_matrix)
  
  # Check if predictions are generated
  if (length(predictions_prem) == 0) {
    stop("No predictions were generated.")
  }
  
  # Ensure predictions are not negative
  if (any(predictions_prem < 0)) {
    smallest_positive_value <- min(predictions_prem[predictions_prem > 0], na.rm = TRUE)
    predictions_prem[predictions_prem < 0] <- smallest_positive_value
  }
  
  # Return the predictions
  return(predictions_prem)
}


# Define the list of variables to remove for Cat2 model
vars_to_remove_cat2 <- c("claim_count", "earned_units", "cat1", "cat2", "cat3", 
                         "claim_rate_cat1", "claim_rate_cat3", "is_multi_pet_plan", 
                         "pet_de_sexed", "UW_season", "Energy.Level", "policy_age_group")

# Call the function to predict for test_data_prem using the EBM model for Cat2
predicted_frequencies_prem_cat2 <- evaluate_and_predict_ebm_cat2(train_data = full_data, 
                                                                 test_data_prem = final_pred, 
                                                                 target_var = "claim_rate_cat2", 
                                                                 vars_to_remove = vars_to_remove_cat2)


# Load the interpret library from Python
interpret <- import("interpret")
interpret_glassbox <- interpret$glassbox

# Function to adjust test_data_prem columns, build EBM, and predict for Cat3
evaluate_and_predict_ebm_cat3 <- function(train_data, test_data_prem, target_var, vars_to_remove) {
  # Print the test_data_prem dataframe to see its structure and values before any adjustments
  print("The final_pred (test_data_prem) dataframe before adjustments for Cat3:")
  print(head(test_data_prem))  # Prints the first few rows
  print(str(test_data_prem))   # Prints the structure of the dataframe
  
  # Remove specified variables from training set
  train_data <- train_data[, !names(train_data) %in% vars_to_remove]
  
  # Remove rows with NAs in training data
  train_data <- na.omit(train_data)
  
  # Check if training data is empty after filtering
  if (nrow(train_data) == 0) {
    stop("Training data is empty after removing NAs or specified variables.")
  }
  
  # Prepare predictor variables and target variable for training
  X_train <- train_data[, !names(train_data) %in% target_var, drop = FALSE]
  y_train <- train_data[[target_var]]
  
  # Check for extra columns in test_data_prem that are not in train_data
  extra_columns <- setdiff(colnames(test_data_prem), colnames(train_data))
  
  # Remove extra columns from test_data_prem
  test_data_prem_adjusted <- test_data_prem[, !names(test_data_prem) %in% extra_columns, drop = FALSE]
  
  # Check for missing columns in test_data_prem that are present in train_data
  missing_columns <- setdiff(colnames(train_data), colnames(test_data_prem_adjusted))
  
  # Add missing columns to test_data_prem_adjusted, filling with NA
  for (col in missing_columns) {
    test_data_prem_adjusted[[col]] <- NA
  }
  
  # Handle missing values in test_data_prem_adjusted by filling with 0 (or use other strategies)
  test_data_prem_adjusted[is.na(test_data_prem_adjusted)] <- 0
  
  # Prepare predictor variables for test_data_prem
  X_test_prem <- test_data_prem_adjusted[, !names(test_data_prem_adjusted) %in% target_var, drop = FALSE]
  
  # Convert data frames to matrices for Python
  X_train_matrix <- as.matrix(X_train)
  X_test_matrix <- as.matrix(X_test_prem)
  
  # Check the dimensions of the matrices
  print(dim(X_train_matrix))
  print(dim(X_test_matrix))
  
  # Create and train the EBM model
  ebm <- interpret_glassbox$ExplainableBoostingRegressor()
  ebm$fit(X_train_matrix, y_train)
  
  # Make predictions using the adjusted test data prem
  predictions_prem <- ebm$predict(X_test_matrix)
  
  # Check if predictions are generated
  if (length(predictions_prem) == 0) {
    stop("No predictions were generated.")
  }
  
  # Ensure predictions are not negative
  if (any(predictions_prem < 0)) {
    smallest_positive_value <- min(predictions_prem[predictions_prem > 0], na.rm = TRUE)
    predictions_prem[predictions_prem < 0] <- smallest_positive_value
  }
  
  # Return the predictions
  return(predictions_prem)
}

# Define the list of variables to remove for Cat3 model
vars_to_remove_cat3 <- c("cat1", "cat2", "cat3", "earned_units", "claim_count", "claim_rate_cat2", 
                         "claim_rate_cat1", "UW_season", "is_multi_pet_plan", "policy_age_group", 
                         "Energy.Level", "Intelligence", "Kid.Friendly", "pet_gender", "Sensitivity.Level")

# Call the function to predict for test_data_prem using the EBM model for Cat3
predicted_frequencies_prem_cat3 <- evaluate_and_predict_ebm_cat3(train_data = full_data, 
                                                                 test_data_prem = final_pred, 
                                                                 target_var = "claim_rate_cat3", 
                                                                 vars_to_remove = vars_to_remove_cat3)
# potentially could fit glm for model 1 frequency

#----Severity models----

# Load necessary libraries
library(reticulate)

# Load the interpret library from Python
interpret <- import("interpret")
interpret_glassbox <- interpret$glassbox

# Variables to remove from the model
vars_to_remove <- c("Sensitivity.Level", "Kid.Friendly", "Drooling.Potential", "Potential.For.Mouthiness", "Avg..Life.Span..years", "Friendly.Toward.Strangers",
                    "Energy.Level", "Easy.To.Groom", "Dog.Friendly", "Intelligence", "Prey.Drive", "pet_gender", "Median_employee_income_SA3", "Intensity", "Tolerates.Hot.Weather",
                    "nb_address_type_adj", "inception_month", "owner_age_years", "Total_number_bus", "policy_age_group", "UW_season")

# Function to build and fit the EBM model without specific variables
train_ebm_model <- function(train_data, vars_to_remove) {
  # Remove the specified variables from the training dataset
  train_data <- train_data[, !names(train_data) %in% vars_to_remove]
  
  # Prepare predictor variables and target variable for training
  X_train <- train_data[, !names(train_data) %in% c("claim_paid", "merged_cluster"), drop = FALSE]  # Predictor variables for training
  y_train <- train_data$claim_paid  # Target variable for training
  
  # Create and train the EBM model
  ebm <- interpret_glassbox$ExplainableBoostingRegressor()  # For regression
  ebm$fit(X_train, y_train)
  
  return(ebm)
}

# Train the EBM model without the specified variables
ebm_model_cat1 <- train_ebm_model(full_data_cat1, vars_to_remove)

# Function to ensure correct columns and make predictions
predict_claim_paid <- function(ebm_model, test_data_prem, train_data_cat1, vars_to_remove) {
  # Remove unnecessary variables from both the training and test datasets
  test_data_prem <- test_data_prem[, !names(test_data_prem) %in% vars_to_remove]
  train_data_cat1 <- train_data_cat1[, !names(train_data_cat1) %in% vars_to_remove]
  
  # Identify the predictor columns used in training (excluding "claim_paid")
  matching_columns <- setdiff(colnames(train_data_cat1), "claim_paid")
  
  # Check for missing columns in test_data_prem
  missing_columns <- setdiff(matching_columns, colnames(test_data_prem))
  if (length(missing_columns) > 0) {
    stop(paste("The following columns are missing from test_data_prem:", paste(missing_columns, collapse = ", ")))
  }
  
  # Check for extra columns in test_data_prem that were not in train_data_cat1
  extra_columns <- setdiff(colnames(test_data_prem), matching_columns)
  if (length(extra_columns) > 0) {
    # Remove the extra columns from test_data_prem
    test_data_prem <- test_data_prem[, !names(test_data_prem) %in% extra_columns]
  }
  
  # Ensure that test_data_prem contains the same predictors as train_data_cat1
  test_data_prem_adjusted <- test_data_prem[, matching_columns, drop = FALSE]
  
  # Make predictions using the adjusted test data
  predicted_claim_paid <- ebm_model$predict(test_data_prem_adjusted)
  
  return(predicted_claim_paid)
}

# Make predictions using the adjusted test_data_prem
predicted_claim_paid_prem1 <- predict_claim_paid(ebm_model = ebm_model_cat1, 
                                                 test_data_prem = final_pred, 
                                                 train_data_cat1 = full_data_cat1, 
                                                 vars_to_remove = vars_to_remove)



# Variables to remove from the model
vars_to_remove <- c("UW_season", "Intelligence", "Kid.Friendly", "Average_household_size", "nb_address_type_adj", "pet_de_sexed",
                    "Sensitivity.Level", "Friendly.Toward.Strangers", "Tendency.To.Bark.Or.Howl", "Energy.Level", "Prey.Drive", "Easy.To.Groom",
                    "Potential.For.Weight.Gain", "Tolerates.Cold.Weather", "Total_number_bus", "policy_age_group")

# Function to build and fit the EBM model without specific variables
train_ebm_model <- function(train_data, vars_to_remove) {
  # Remove the specified variables from the training dataset
  train_data <- train_data[, !names(train_data) %in% vars_to_remove]
  
  # Prepare predictor variables and target variable for training
  X_train <- train_data[, !names(train_data) %in% c("claim_paid", "merged_cluster"), drop = FALSE]  # Predictor variables for training
  y_train <- train_data$claim_paid  # Target variable for training
  
  # Create and train the EBM model
  ebm <- interpret_glassbox$ExplainableBoostingRegressor()  # For regression
  ebm$fit(X_train, y_train)
  
  return(ebm)
}

# Train the EBM model without the specified variables
ebm_model_cat2 <- train_ebm_model(full_data_cat2, vars_to_remove)

# Function to ensure correct columns and make predictions
predict_claim_paid <- function(ebm_model, test_data_prem, train_data_cat2, vars_to_remove) {
  # Remove unnecessary variables from both the training and test datasets
  test_data_prem <- test_data_prem[, !names(test_data_prem) %in% vars_to_remove]
  train_data_cat1 <- train_data_cat1[, !names(train_data_cat1) %in% vars_to_remove]
  
  # Identify the predictor columns used in training (excluding "claim_paid")
  matching_columns <- setdiff(colnames(train_data_cat1), "claim_paid")
  
  # Check for missing columns in test_data_prem
  missing_columns <- setdiff(matching_columns, colnames(test_data_prem))
  if (length(missing_columns) > 0) {
    stop(paste("The following columns are missing from test_data_prem:", paste(missing_columns, collapse = ", ")))
  }
  
  # Check for extra columns in test_data_prem that were not in train_data_cat1
  extra_columns <- setdiff(colnames(test_data_prem), matching_columns)
  if (length(extra_columns) > 0) {
    # Remove the extra columns from test_data_prem
    test_data_prem <- test_data_prem[, !names(test_data_prem) %in% extra_columns]
  }
  
  # Ensure that test_data_prem contains the same predictors as train_data_cat1
  test_data_prem_adjusted <- test_data_prem[, matching_columns, drop = FALSE]
  
  # Make predictions using the adjusted test data
  predicted_claim_paid <- ebm_model$predict(test_data_prem_adjusted)
  
  return(predicted_claim_paid)
}

# Make predictions using the adjusted test_data_prem
predicted_claim_paid_prem2 <- predict_claim_paid(ebm_model = ebm_model_cat2, 
                                                 test_data_prem = final_pred, 
                                                 train_data_cat2 = full_data_cat2, 
                                                 vars_to_remove = vars_to_remove)



# Variables to remove from the model
vars_to_remove <- c("Potential.For.Weight.Gain", "Drooling.Potential", "Kid.Friendly", "owner_age_years", "Intelligence", "pet_gender",
                    "nb_address_type_adj", "UW_season", "Sensitivity.Level", "policy_age_group")

# Function to build and fit the EBM model without specific variables
train_ebm_model <- function(train_data, vars_to_remove) {
  # Remove the specified variables from the training dataset
  train_data <- train_data[, !names(train_data) %in% vars_to_remove]
  
  # Prepare predictor variables and target variable for training
  X_train <- train_data[, !names(train_data) %in% c("claim_paid", "merged_cluster"), drop = FALSE]  # Predictor variables for training
  y_train <- train_data$claim_paid  # Target variable for training
  
  # Create and train the EBM model
  ebm <- interpret_glassbox$ExplainableBoostingRegressor()  # For regression
  ebm$fit(X_train, y_train)
  
  return(ebm)
}

# Train the EBM model without the specified variables
ebm_model_cat3 <- train_ebm_model(train_data_cat3, vars_to_remove)


# Function to ensure correct columns and make predictions
predict_claim_paid <- function(ebm_model, test_data_prem, train_data_cat3, vars_to_remove) {
  # Remove unnecessary variables from both the training and test datasets
  test_data_prem <- test_data_prem[, !names(test_data_prem) %in% vars_to_remove]
  train_data_cat1 <- train_data_cat1[, !names(train_data_cat1) %in% vars_to_remove]
  
  # Identify the predictor columns used in training (excluding "claim_paid")
  matching_columns <- setdiff(colnames(train_data_cat1), "claim_paid")
  
  # Check for missing columns in test_data_prem
  missing_columns <- setdiff(matching_columns, colnames(test_data_prem))
  if (length(missing_columns) > 0) {
    stop(paste("The following columns are missing from test_data_prem:", paste(missing_columns, collapse = ", ")))
  }
  
  # Check for extra columns in test_data_prem that were not in train_data_cat1
  extra_columns <- setdiff(colnames(test_data_prem), matching_columns)
  if (length(extra_columns) > 0) {
    # Remove the extra columns from test_data_prem
    test_data_prem <- test_data_prem[, !names(test_data_prem) %in% extra_columns]
  }
  
  # Ensure that test_data_prem contains the same predictors as train_data_cat1
  test_data_prem_adjusted <- test_data_prem[, matching_columns, drop = FALSE]
  
  # Make predictions using the adjusted test data
  predicted_claim_paid <- ebm_model$predict(test_data_prem_adjusted)
  
  return(predicted_claim_paid)
}

# Make predictions using the adjusted test_data_prem
predicted_claim_paid_prem3 <- predict_claim_paid(ebm_model = ebm_model_cat3, 
                                                 test_data_prem = final_pred, 
                                                 train_data_cat3 = full_data_cat3, 
                                                 vars_to_remove = vars_to_remove)

#----Total severity model----
# Variables to remove from the model
vars_to_remove <- c("UW_season", "policy_age_group")

# Function to build and fit the EBM model without specific variables
train_ebm_model <- function(train_data, vars_to_remove) {
  # Remove the specified variables from the training dataset
  train_data <- train_data[, !names(train_data) %in% vars_to_remove]
  
  # Prepare predictor variables and target variable for training
  X_train <- train_data[, !names(train_data) %in% c("claim_paid", "merged_cluster"), drop = FALSE]  # Predictor variables for training
  y_train <- train_data$claim_paid  # Target variable for training
  
  # Create and train the EBM model
  ebm <- interpret_glassbox$ExplainableBoostingRegressor()  # For regression
  ebm$fit(X_train, y_train)
  
  return(ebm)
}

# Train the EBM model without the specified variables
ebm_model_total <- train_ebm_model(full_data_sev_total, vars_to_remove)

### Predict on test_data_prem

# Function to ensure correct columns and make predictions
predict_claim_paid <- function(ebm_model, test_data_prem, train_data_cat2, vars_to_remove) {
  # Remove unnecessary variables from both the training and test datasets
  test_data_prem <- test_data_prem[, !names(test_data_prem) %in% vars_to_remove]
  train_data_cat1 <- train_data_cat1[, !names(train_data_cat1) %in% vars_to_remove]
  
  # Identify the predictor columns used in training (excluding "claim_paid")
  matching_columns <- setdiff(colnames(train_data_cat1), "claim_paid")
  
  # Check for missing columns in test_data_prem
  missing_columns <- setdiff(matching_columns, colnames(test_data_prem))
  if (length(missing_columns) > 0) {
    stop(paste("The following columns are missing from test_data_prem:", paste(missing_columns, collapse = ", ")))
  }
  
  # Check for extra columns in test_data_prem that were not in train_data_cat1
  extra_columns <- setdiff(colnames(test_data_prem), matching_columns)
  if (length(extra_columns) > 0) {
    # Remove the extra columns from test_data_prem
    test_data_prem <- test_data_prem[, !names(test_data_prem) %in% extra_columns]
  }
  
  # Ensure that test_data_prem contains the same predictors as train_data_cat1
  test_data_prem_adjusted <- test_data_prem[, matching_columns, drop = FALSE]
  
  # Make predictions using the adjusted test data
  predicted_claim_paid <- ebm_model$predict(test_data_prem_adjusted)
  
  return(predicted_claim_paid)
}

# Make predictions using the adjusted test_data_prem
predicted_claim_paid_total <- predict_claim_paid(ebm_model = ebm_model_total, 
                                                 test_data_prem = final_pred, 
                                                 train_data_cat2 = full_data_sev_total, 
                                                 vars_to_remove = vars_to_remove)

#----Credibility Adjustment----

## Bulmann-Straub Approach
# Function to calculate scaled credibility factors based on the number of rows in the training data
calculate_scaled_credibility <- function(train_data, overall_data, response_variable, total_rows) {
  # Number of observations in the category
  n <- nrow(train_data)
  
  # Mean of the response variable for individual model
  mean_individual <- mean(train_data[[response_variable]], na.rm = TRUE)
  
  # Overall mean of the response variable
  mean_overall <- mean(overall_data[[response_variable]], na.rm = TRUE)
  
  # Within-group variance (individual risk category)
  var_within <- var(train_data[[response_variable]], na.rm = TRUE)
  
  # Between-group variance (variance between individual mean and overall mean)
  var_between <- mean((mean_individual - mean_overall)^2)
  
  # Unscaled credibility factor Z (traditional formula)
  Z <- n / (n + (var_within / var_between))
  
  # Scale the credibility factor by the number of rows relative to the total
  Z_scaled <- Z
  
  return(list(credibility_factor = Z_scaled, mean_individual = mean_individual, mean_overall = mean_overall))
}

# Total number of rows in the overall data
total_rows <- nrow(train_data_total)

# Example calculation for train_data_cat1, train_data_cat2, train_data_cat3
credibility_cat1 <- calculate_scaled_credibility(train_data_cat1, train_data_total, "claim_paid", total_rows)
credibility_cat2 <- calculate_scaled_credibility(train_data_cat2, train_data_total, "claim_paid", total_rows)
credibility_cat3 <- calculate_scaled_credibility(train_data_cat3, train_data_total, "claim_paid", total_rows)

# Scaled Credibility factors for each category
Z_cat1 <- credibility_cat1$credibility_factor
Z_cat2 <- credibility_cat2$credibility_factor
Z_cat3 <- credibility_cat3$credibility_factor

# Now let's calculate the adjusted severity for each category
# Assuming you have predicted severities from the individual models and the overall model

# Adjusted severity calculation using the scaled credibility factors
adjusted_severity_cat1 <- Z_cat1 * predicted_claim_paid_prem1 + (1 - Z_cat1) * predicted_claim_paid_total
adjusted_severity_cat2 <- Z_cat2 * predicted_claim_paid_prem2 + (1 - Z_cat2) * predicted_claim_paid_total
adjusted_severity_cat3 <- Z_cat3 * predicted_claim_paid_prem3 + (1 - Z_cat3) * predicted_claim_paid_total


predicted_frequencies_prem_cat1
predicted_frequencies_prem_cat2
predicted_frequencies_prem_cat3

adjusted_severity_cat1
adjusted_severity_cat2
adjusted_severity_cat3

# Multiply corresponding entries
result_cat1 <- predicted_frequencies_prem_cat1 * adjusted_severity_cat1
result_cat2 <- predicted_frequencies_prem_cat2 * adjusted_severity_cat2
result_cat3 <- predicted_frequencies_prem_cat3 * adjusted_severity_cat3

# Perform the element-wise sum
total_result <- result_cat1 + result_cat2 + result_cat3

Full_month_premium <- total_result

# Assuming total_result and final_pred are both vectors/dataframes of the same length
final_pred_new$Full_month_premium <- Full_month_premium

# Export the updated dataframe as a CSV file
write.csv(final_pred_new, "final_pred_with_premiums.csv", row.names = FALSE)

# This will create a file named 'final_pred_with_premiums.csv' in your working directory


colnames(final_pred_new)

set.seed(123)
# Create a single train-test split for the frequency data (80% train, 20% test)
train_index <- sample(seq_len(nrow(frequency_data)), size = 0.8 * nrow(frequency_data))
train_data <- frequency_data[train_index, ]
test_data <- frequency_data[-train_index, ]

test_data_prem <- frequency_data2[-train_index, ]

# Step 1: Summarize claim_paid in the claims dataset
claims_summary <- claims %>%
  group_by(exposure_id) %>%
  summarize(total_claim_paid = sum(claim_paid, na.rm = TRUE), .groups = 'drop')

# Step 2: Join the summary back to test_data_prem
test_data_prem <- test_data_prem %>%
  left_join(claims_summary, by = "exposure_id")

# Replace NA values in total_claim_paid with 0
test_data_prem$total_claim_paid[is.na(test_data_prem$total_claim_paid)] <- 0

# Optionally, check the changes
print(head(test_data_prem$total_claim_paid))  # Display the first few entries

# Assuming previous code remains the same

# After calculating total_result and ensuring test_data_prem is correctly set up
test_data_prem$total_claim_paid[is.na(test_data_prem$total_claim_paid)] <- 0

# Ensure that both vectors are the same length
if(length(test_data_prem$total_claim_paid) == length(total_result)) {
  # Calculate the pairwise squared differences
  squared_differences <- (test_data_prem$total_claim_paid - total_result) ^ 2
  
  # Sum all squared differences
  total_squared_difference <- sum(squared_differences)
  
  # Print the total squared difference
  print(paste("Total Pairwise Squared Difference:", total_squared_difference))
  
  # Calculate RMSE
  rmse <- sqrt(total_squared_difference / length(test_data_prem$total_claim_paid))
  print(paste("RMSE:", rmse))
} else {
  print("Error: Lengths of total_claim_paid and total_result do not match.")
}

