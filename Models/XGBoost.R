# Load necessary libraries
library(dplyr)
library(xgboost)  # For XGBoost

# Set a seed for reproducibility
set.seed(123)

# Step 1: Create a single train-test split for the frequency data (80% train, 20% test)
train_index <- sample(seq_len(nrow(frequency_data)), size = 0.8 * nrow(frequency_data))
train_data <- frequency_data[train_index, ]
test_data <- frequency_data[-train_index, ]

# Prepare test_data_prem from frequency_data2 by splitting in the same way
test_data_prem <- frequency_data2[-train_index, ]

# Step 2: Define the list of variables to remove
vars_to_remove <- c("claim_count", "cat2", "cat3", "cat1",
                    "claim_rate_cat2", "claim_rate_cat3", "Drooling.Potential", 
                    "nb_contribution", "Energy.Level", "policy_age_group", 
                    "is_multi_pet_plan", "pet_de_sexed", "earned_units")

# Prepare the data by removing the specified variables
train_data_cleaned <- train_data[, !names(train_data) %in% vars_to_remove]
test_data_prem_cleaned <- test_data_prem[, !names(test_data_prem) %in% vars_to_remove]

# Step 3: Remove rows with missing values (NA)
train_data_cleaned <- na.omit(train_data_cleaned)
test_data_prem_cleaned <- na.omit(test_data_prem_cleaned)

# Step 4: Convert character columns to factors and then to numeric
train_data_cleaned <- train_data_cleaned %>% 
  mutate_if(is.character, as.factor) %>%
  mutate_if(is.factor, as.numeric)

test_data_prem_cleaned <- test_data_prem_cleaned %>%
  mutate_if(is.character, as.factor) %>%
  mutate_if(is.factor, as.numeric)

# Step 5: Align test data columns with train data columns
test_data_prem_cleaned <- test_data_prem_cleaned[, names(train_data_cleaned)]

# Step 6: Prepare the data for XGBoost
# Separate features and response variable for training data
train_matrix <- xgb.DMatrix(
  data = as.matrix(train_data_cleaned[ , -which(names(train_data_cleaned) == "claim_rate_cat1")]), 
  label = train_data_cleaned$claim_rate_cat1
)
test_matrix <- xgb.DMatrix(
  data = as.matrix(test_data_prem_cleaned[ , -which(names(test_data_prem_cleaned) == "claim_rate_cat1")]), 
  label = test_data_prem_cleaned$claim_rate_cat1
)

# Step 7: Define XGBoost parameters
params <- list(
  objective = "reg:squarederror",  # For regression
  eta = 0.001,                     # Learning rate
  max_depth = 2,                   # Tree depth
  min_child_weight = 20,           # Minimum sum of instance weight needed in a child
  eval_metric = "rmse"             # Root Mean Square Error for evaluation
)

# Step 8: Cross-validation to determine the best number of rounds
xgb_cv <- xgb.cv(
  params = params,
  data = train_matrix,
  nrounds = 2000,                 # Number of boosting iterations
  nfold = 5,                       # Cross-validation folds
  early_stopping_rounds = 10,      # Early stopping
  verbose = 0
)
best_trees <- xgb_cv$best_iteration

# Step 9: Train the final model using the optimal number of trees
xgb_model <- xgboost(
  params = params,
  data = train_matrix,
  nrounds = best_trees,
  verbose = 0
)

# Step 10: Make predictions on the cleaned test data (test_data_prem)
predicted_frequencies_prem_cat1 <- predict(xgb_model, newdata = test_matrix)

# Step 11: Print the first few predictions for claim_rate_cat1
print(head(predicted_frequencies_prem_cat1))

# Optional: View variable importance
importance_matrix <- xgb.importance(model = xgb_model)
print(importance_matrix)
xgb.plot.importance(importance_matrix)


# Load necessary libraries
library(dplyr)
library(xgboost)

# Set a seed for reproducibility
set.seed(123)

# Step 1: Create a single train-test split for the frequency data (80% train, 20% test)
train_index <- sample(seq_len(nrow(frequency_data)), size = 0.8 * nrow(frequency_data))
train_data <- frequency_data[train_index, ]
test_data <- frequency_data[-train_index, ]

# Prepare test_data_prem from frequency_data2 by splitting in the same way
test_data_prem <- frequency_data2[-train_index, ]

# Define common XGBoost parameters
params <- list(
  objective = "reg:squarederror",  # For regression
  eta = 0.001,                     # Learning rate
  max_depth = 2,                   # Tree depth
  min_child_weight = 20,           # Minimum sum of instance weight needed in a child
  eval_metric = "rmse"             # Root Mean Square Error for evaluation
)

### Model for Cat2 ###

# Step 2: Define the list of variables to remove for Cat2 model
vars_to_remove_cat2 <- c("claim_count", "earned_units", "cat1", "cat2", "cat3", 
                         "claim_rate_cat1", "claim_rate_cat3", "is_multi_pet_plan", 
                         "pet_de_sexed", "UW_season", "Energy.Level", "policy_age_group")

# Prepare the data by removing the specified variables
train_data_cleaned_cat2 <- train_data[, !names(train_data) %in% vars_to_remove_cat2]
test_data_prem_cleaned_cat2 <- test_data_prem[, !names(test_data_prem) %in% vars_to_remove_cat2]

# Step 3: Remove rows with missing values (NA)
train_data_cleaned_cat2 <- na.omit(train_data_cleaned_cat2)
test_data_prem_cleaned_cat2 <- na.omit(test_data_prem_cleaned_cat2)

# Step 4: Convert character columns to factors and then to numeric
train_data_cleaned_cat2 <- train_data_cleaned_cat2 %>% 
  mutate_if(is.character, as.factor) %>%
  mutate_if(is.factor, as.numeric)

test_data_prem_cleaned_cat2 <- test_data_prem_cleaned_cat2 %>%
  mutate_if(is.character, as.factor) %>%
  mutate_if(is.factor, as.numeric)

# Step 5: Align test data columns with train data columns for Cat2
test_data_prem_cleaned_cat2 <- test_data_prem_cleaned_cat2[, names(train_data_cleaned_cat2)]

# Step 6: Prepare the data for XGBoost
train_matrix_cat2 <- xgb.DMatrix(
  data = as.matrix(train_data_cleaned_cat2[ , -which(names(train_data_cleaned_cat2) == "claim_rate_cat2")]), 
  label = train_data_cleaned_cat2$claim_rate_cat2
)
test_matrix_cat2 <- xgb.DMatrix(
  data = as.matrix(test_data_prem_cleaned_cat2[ , -which(names(test_data_prem_cleaned_cat2) == "claim_rate_cat2")]), 
  label = test_data_prem_cleaned_cat2$claim_rate_cat2
)

# Step 7: Cross-validation for Cat2
xgb_cv_cat2 <- xgb.cv(
  params = params,
  data = train_matrix_cat2,
  nrounds = 2000,
  nfold = 5,
  early_stopping_rounds = 10,
  verbose = 0
)
best_trees_cat2 <- xgb_cv_cat2$best_iteration

# Step 8: Train final model for Cat2
xgb_model_cat2 <- xgboost(
  params = params,
  data = train_matrix_cat2,
  nrounds = best_trees_cat2,
  verbose = 0
)

# Step 9: Make predictions for Cat2
predicted_frequencies_prem_cat2 <- predict(xgb_model_cat2, newdata = test_matrix_cat2)
print(head(predicted_frequencies_prem_cat2))

# Optional: View variable importance for Cat2
importance_matrix_cat2 <- xgb.importance(model = xgb_model_cat2)
print(importance_matrix_cat2)
xgb.plot.importance(importance_matrix_cat2)

### Model for Cat3 ###

# Step 2: Define the list of variables to remove for Cat3 model
vars_to_remove_cat3 <- c("Potential.For.Weight.Gain", "Drooling.Potential", "Kid.Friendly", 
                         "owner_age_years", "Intelligence", "pet_gender", 
                         "nb_address_type_adj", "UW_season", "Sensitivity.Level", 
                         "cat1", "cat2", "claim_rate_cat1", "claim_rate_cat2", 
                         "cat3", "claim_count", "earned_units")

# Prepare the training and test data by removing the specified variables
train_data_cleaned_cat3 <- train_data[, !names(train_data) %in% vars_to_remove_cat3]
test_data_prem_cleaned_cat3 <- test_data_prem[, !names(test_data_prem) %in% vars_to_remove_cat3]

# Step 3: Remove rows with missing values (NA)
train_data_cleaned_cat3 <- na.omit(train_data_cleaned_cat3)
test_data_prem_cleaned_cat3 <- na.omit(test_data_prem_cleaned_cat3)

# Step 4: Convert character columns to factors and then to numeric
train_data_cleaned_cat3 <- train_data_cleaned_cat3 %>% 
  mutate_if(is.character, as.factor) %>%
  mutate_if(is.factor, as.numeric)

test_data_prem_cleaned_cat3 <- test_data_prem_cleaned_cat3 %>%
  mutate_if(is.character, as.factor) %>%
  mutate_if(is.factor, as.numeric)

# Step 5: Align test data columns with train data columns for Cat3
test_data_prem_cleaned_cat3 <- test_data_prem_cleaned_cat3[, names(train_data_cleaned_cat3)]

# Step 6: Prepare the data for XGBoost
train_matrix_cat3 <- xgb.DMatrix(
  data = as.matrix(train_data_cleaned_cat3[ , -which(names(train_data_cleaned_cat3) == "claim_rate_cat3")]), 
  label = train_data_cleaned_cat3$claim_rate_cat3
)
test_matrix_cat3 <- xgb.DMatrix(
  data = as.matrix(test_data_prem_cleaned_cat3[ , -which(names(test_data_prem_cleaned_cat3) == "claim_rate_cat3")]), 
  label = test_data_prem_cleaned_cat3$claim_rate_cat3
)

# Step 7: Cross-validation for Cat3
xgb_cv_cat3 <- xgb.cv(
  params = params,
  data = train_matrix_cat3,
  nrounds = 2000,
  nfold = 5,
  early_stopping_rounds = 10,
  verbose = 0
)
best_trees_cat3 <- xgb_cv_cat3$best_iteration

# Step 8: Train final model for Cat3
xgb_model_cat3 <- xgboost(
  params = params,
  data = train_matrix_cat3,
  nrounds = best_trees_cat3,
  verbose = 0
)

# Step 9: Make predictions for Cat3
predicted_frequencies_prem_cat3 <- predict(xgb_model_cat3, newdata = test_matrix_cat3)
print(head(predicted_frequencies_prem_cat3))

# Optional: View variable importance for Cat3
importance_matrix_cat3 <- xgb.importance(model = xgb_model_cat3)
print(importance_matrix_cat3)
xgb.plot.importance(importance_matrix_cat3)





# Load necessary libraries
library(dplyr)
library(xgboost)

# Set a seed for reproducibility
set.seed(123)

# Define common XGBoost parameters
params <- list(
  objective = "reg:squarederror",  # For regression
  eta = 0.001,                     # Learning rate
  max_depth = 2,                   # Tree depth
  min_child_weight = 20,           # Minimum sum of instance weight needed in a child
  eval_metric = "rmse"             # Root Mean Square Error for evaluation
)

### Model for Cat1 ###

# Load necessary libraries
library(dplyr)
library(xgboost)

# Set a seed for reproducibility
set.seed(123)

# Define common XGBoost parameters
params <- list(
  objective = "reg:squarederror",  # For regression
  eta = 0.001,                     # Learning rate
  max_depth = 2,                   # Tree depth
  min_child_weight = 20,           # Minimum sum of instance weight needed in a child
  eval_metric = "rmse"             # Root Mean Square Error for evaluation
)

### Model for Cat1 ###

# Load necessary libraries
library(dplyr)
library(xgboost)  # For XGBoost

# Set a seed for reproducibility
set.seed(123)

# Define the list of variables to remove for the `cat1` model
vars_to_remove_cat1 <- c("Sensitivity.Level", "Kid.Friendly", "Drooling.Potential", 
                         "Potential.For.Mouthiness", "Avg..Life.Span..years", 
                         "Friendly.Toward.Strangers", "Energy.Level", "Easy.To.Groom", 
                         "Dog.Friendly", "Intelligence", "Prey.Drive", "pet_gender", 
                         "Median_employee_income_SA3", "Intensity", "Tolerates.Hot.Weather", 
                         "nb_address_type_adj", "inception_month", "owner_age_years", 
                         "Total_number_bus", "policy_age_group")

# Step 1: Prepare the training and test data by removing the specified variables for `cat1`
train_data_cleaned_cat1 <- train_data_cat1[, !names(train_data_cat1) %in% vars_to_remove_cat1]
test_data_prem_cleaned_cat1 <- test_data_prem[, !names(test_data_prem) %in% vars_to_remove_cat1]

# Step 2: Remove rows with missing values (NA)
train_data_cleaned_cat1 <- na.omit(train_data_cleaned_cat1)
test_data_prem_cleaned_cat1 <- na.omit(test_data_prem_cleaned_cat1)

# Step 3: Ensure consistent one-hot encoding for both train and test sets

# One-hot encode the training and test sets
train_matrix <- model.matrix(~ . - 1, data = train_data_cleaned_cat1)  # `claim_paid` should be excluded manually here if necessary
train_label <- train_data_cleaned_cat1$claim_paid

test_matrix <- model.matrix(~ . - 1, data = test_data_prem_cleaned_cat1)

# Ensure columns in train and test match
common_cols <- intersect(colnames(train_matrix), colnames(test_matrix))
train_matrix <- train_matrix[, common_cols]
test_matrix <- test_matrix[, common_cols]

# Step 4: Set parameters for xgboost and perform cross-validation
params <- list(
  objective = "reg:squarederror",  # Equivalent to Gaussian for regression
  max_depth = 2,
  eta = 0.001,                     # Equivalent to shrinkage/learning rate
  min_child_weight = 20,           # Equivalent to n.minobsinnode
  subsample = 1,
  colsample_bytree = 1
)

# Using cross-validation to determine the optimal number of rounds (trees)
xgb_cv <- xgb.cv(
  params = params,
  data = train_matrix,
  label = train_label,
  nrounds = 2000,
  nfold = 5,
  verbose = 0,
  early_stopping_rounds = 10
)

# Optimal number of rounds based on cross-validation
best_trees_cat1 <- xgb_cv$best_iteration

# Step 5: Train final model with optimal number of trees
xgb_model_cat1 <- xgboost(
  params = params,
  data = train_matrix,
  label = train_label,
  nrounds = best_trees_cat1,
  verbose = 0
)

# Step 6: Make predictions on the cleaned test data for `cat1`
predicted_claim_paid_prem1 <- predict(xgb_model_cat1, newdata = test_matrix)

# Step 7: Print the first few predictions
print(head(predicted_claim_paid_prem1))

# Optional: View feature importance for `cat1`
importance_matrix <- xgb.importance(feature_names = colnames(train_matrix), model = xgb_model_cat1)
xgb.plot.importance(importance_matrix)




# Load necessary libraries
library(dplyr)
library(xgboost)  # For XGBoost

# Set a seed for reproducibility
set.seed(123)

# Define the list of variables to remove for the `cat2` model
vars_to_remove_cat2 <- c("UW_season", "Intelligence", "Kid.Friendly", "Average_household_size", 
                         "nb_address_type_adj", "pet_de_sexed", "Sensitivity.Level", 
                         "Friendly.Toward.Strangers", "Tendency.To.Bark.Or.Howl", "Energy.Level", 
                         "Prey.Drive", "Easy.To.Groom", "Potential.For.Weight.Gain", 
                         "Tolerates.Cold.Weather", "Total_number_bus")

# Step 1: Prepare the training and test data by removing the specified variables for `cat2`
train_data_cleaned_cat2 <- train_data_cat2[, !names(train_data_cat2) %in% vars_to_remove_cat2]
test_data_prem_cleaned_cat2 <- test_data_prem[, !names(test_data_prem) %in% vars_to_remove_cat2]

# Step 2: Remove rows with missing values (NA)
train_data_cleaned_cat2 <- na.omit(train_data_cleaned_cat2)
test_data_prem_cleaned_cat2 <- na.omit(test_data_prem_cleaned_cat2)

# Step 3: Ensure consistent one-hot encoding for both train and test sets

# One-hot encode the training and test sets
train_matrix <- model.matrix(~ . - 1, data = train_data_cleaned_cat2)  # `claim_paid` should be excluded manually here if necessary
train_label <- train_data_cleaned_cat2$claim_paid

test_matrix <- model.matrix(~ . - 1, data = test_data_prem_cleaned_cat2)

# Ensure columns in train and test match
common_cols <- intersect(colnames(train_matrix), colnames(test_matrix))
train_matrix <- train_matrix[, common_cols]
test_matrix <- test_matrix[, common_cols]

# Step 4: Set parameters for xgboost and perform cross-validation
params <- list(
  objective = "reg:squarederror",  # Equivalent to Gaussian for regression
  max_depth = 2,
  eta = 0.001,                     # Equivalent to shrinkage/learning rate
  min_child_weight = 20,           # Equivalent to n.minobsinnode
  subsample = 1,
  colsample_bytree = 1
)

# Using cross-validation to determine the optimal number of rounds (trees)
xgb_cv <- xgb.cv(
  params = params,
  data = train_matrix,
  label = train_label,
  nrounds = 2000,
  nfold = 5,
  verbose = 0,
  early_stopping_rounds = 10
)

# Optimal number of rounds based on cross-validation
best_trees_cat2 <- xgb_cv$best_iteration

# Step 5: Train final model with optimal number of trees
xgb_model_cat2 <- xgboost(
  params = params,
  data = train_matrix,
  label = train_label,
  nrounds = best_trees_cat2,
  verbose = 0
)

# Step 6: Make predictions on the cleaned test data for `cat2`
predicted_claim_paid_prem2 <- predict(xgb_model_cat2, newdata = test_matrix)

# Step 7: Print the first few predictions for `claim_paid` for `cat2`
print(head(predicted_claim_paid_prem2))

# Optional: View feature importance for `cat2`
importance_matrix <- xgb.importance(feature_names = colnames(train_matrix), model = xgb_model_cat2)
xgb.plot.importance(importance_matrix)


# Load necessary libraries
library(dplyr)
library(xgboost)  # For XGBoost

# Set a seed for reproducibility
set.seed(123)

# Define the list of variables to remove for the `cat3` model
vars_to_remove_cat3 <- c("Potential.For.Weight.Gain", "Drooling.Potential", "Kid.Friendly", 
                         "owner_age_years", "Intelligence", "pet_gender", 
                         "nb_address_type_adj", "UW_season", "Sensitivity.Level")

# Step 1: Prepare the training and test data by removing the specified variables for `cat3`
train_data_cleaned_cat3 <- train_data_cat3[, !names(train_data_cat3) %in% vars_to_remove_cat3]
test_data_prem_cleaned_cat3 <- test_data_prem[, !names(test_data_prem) %in% vars_to_remove_cat3]

# Step 2: Remove rows with missing values (NA)
train_data_cleaned_cat3 <- na.omit(train_data_cleaned_cat3)
test_data_prem_cleaned_cat3 <- na.omit(test_data_prem_cleaned_cat3)

# Step 3: Ensure consistent one-hot encoding for both train and test sets

# One-hot encode the training and test sets
train_matrix <- model.matrix(~ . - 1, data = train_data_cleaned_cat3)  # `claim_paid` should be excluded manually here if necessary
train_label <- train_data_cleaned_cat3$claim_paid

test_matrix <- model.matrix(~ . - 1, data = test_data_prem_cleaned_cat3)

# Ensure columns in train and test match
common_cols <- intersect(colnames(train_matrix), colnames(test_matrix))
train_matrix <- train_matrix[, common_cols]
test_matrix <- test_matrix[, common_cols]

# Step 4: Set parameters for xgboost and perform cross-validation
params <- list(
  objective = "reg:squarederror",  # Equivalent to Gaussian for regression
  max_depth = 2,
  eta = 0.001,                     # Equivalent to shrinkage/learning rate
  min_child_weight = 20,           # Equivalent to n.minobsinnode
  subsample = 1,
  colsample_bytree = 1
)

# Using cross-validation to determine the optimal number of rounds (trees)
xgb_cv <- xgb.cv(
  params = params,
  data = train_matrix,
  label = train_label,
  nrounds = 2000,
  nfold = 5,
  verbose = 0,
  early_stopping_rounds = 10
)

# Optimal number of rounds based on cross-validation
best_trees_cat3 <- xgb_cv$best_iteration

# Step 5: Train final model with optimal number of trees
xgb_model_cat3 <- xgboost(
  params = params,
  data = train_matrix,
  label = train_label,
  nrounds = best_trees_cat3,
  verbose = 0
)

# Step 6: Make predictions on the cleaned test data for `cat3`
predicted_claim_paid_prem3 <- predict(xgb_model_cat3, newdata = test_matrix)

# Step 7: Print the first few predictions for `claim_paid` for `cat3`
print(head(predicted_claim_paid_prem3))

# Optional: View feature importance for `cat3`
importance_matrix <- xgb.importance(feature_names = colnames(train_matrix), model = xgb_model_cat3)
xgb.plot.importance(importance_matrix)




predicted_frequencies_prem_cat1
predicted_frequencies_prem_cat2
predicted_frequencies_prem_cat3

predicted_claim_paid_prem1
predicted_claim_paid_prem2
predicted_claim_paid_prem3

# Multiply corresponding entries
result_cat1 <- predicted_frequencies_prem_cat1 * predicted_claim_paid_prem1
result_cat2 <- predicted_frequencies_prem_cat2 * predicted_claim_paid_prem2
result_cat3 <- predicted_frequencies_prem_cat3 * predicted_claim_paid_prem3

# Perform the element-wise sum
total_result <- result_cat1 + result_cat2 + result_cat3

hist(total_result)

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


library(dplyr)

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

# Gini index function (robust approach)
gini_index <- function(actual, predicted) {
  # Combine actual and predicted into a data frame
  data <- data.frame(actual = actual, predicted = predicted)
  
  # Sort by predicted values in descending order
  data <- data[order(data$predicted, decreasing = TRUE), ]
  
  # Calculate cumulative values for Gini calculation
  total_losses <- sum(data$actual)
  cumulative_actual <- cumsum(data$actual) / total_losses
  cumulative_baseline <- (1:nrow(data)) / nrow(data)
  
  # Calculate Gini index as the area between cumulative actuals and the baseline
  gini_sum <- sum(cumulative_actual - cumulative_baseline) / nrow(data)
  
  # Convert Gini sum to positive if needed
  return(abs(gini_sum))
}

# Calculate Gini for the model and actual values
gini_model <- gini_index(test_data_prem$total_claim_paid, total_result)
gini_actual <- gini_index(test_data_prem$total_claim_paid, test_data_prem$total_claim_paid)

# Calculate normalized Gini index
normalized_gini <- gini_model / gini_actual

# Print the Gini values and the normalized Gini index
print(paste("Gini (Model):", gini_model))
print(paste("Gini (Actual):", gini_actual))
print(paste("Normalized Gini Index:", normalized_gini))



