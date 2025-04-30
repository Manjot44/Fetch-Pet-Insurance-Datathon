# Load necessary libraries
library(dplyr)
library(rpart)  # For regression tree

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

# Step 4: Fit the regression tree model on the cleaned training data
# Here, 'cat1' is assumed to be the response variable
tree_formula <- as.formula("claim_rate_cat1 ~ .")
regression_tree <- rpart(tree_formula, data = train_data_cleaned, method = "anova")

# Step 5: Make predictions on the cleaned test data (test_data_prem)
predicted_frequencies_prem_cat1 <- predict(regression_tree, newdata = test_data_prem_cleaned)

# Step 6: Summary of the regression tree model
print(summary(regression_tree))

# Optional: Plot the regression tree
plot(regression_tree)
text(regression_tree, pretty = 1)


# Load necessary libraries
library(dplyr)
library(rpart)  # For regression tree

# Set a seed for reproducibility
set.seed(123)

# Step 1: Create a single train-test split for the frequency data (80% train, 20% test)
train_index <- sample(seq_len(nrow(frequency_data)), size = 0.8 * nrow(frequency_data))
train_data <- frequency_data[train_index, ]
test_data <- frequency_data[-train_index, ]

# Prepare test_data_prem from frequency_data2 by splitting in the same way
test_data_prem <- frequency_data2[-train_index, ]

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

# Step 4: Fit the regression tree model on the cleaned training data
# Here, 'cat2' is assumed to be the response variable
tree_formula_cat2 <- as.formula("claim_rate_cat2 ~ .")
regression_tree_cat2 <- rpart(tree_formula_cat2, data = train_data_cleaned_cat2, method = "anova")

# Step 5: Make predictions on the cleaned test data (test_data_prem)
predicted_frequencies_prem_cat2 <- predict(regression_tree_cat2, newdata = test_data_prem_cleaned_cat2)

# Step 6: Print the first few predictions for Cat2
print(head(predicted_frequencies_prem_cat2))



# Load necessary libraries
library(dplyr)
library(rpart)  # For regression tree

# Set a seed for reproducibility
set.seed(123)

# Step 1: Create a single train-test split for the frequency data (80% train, 20% test)
train_index <- sample(seq_len(nrow(frequency_data)), size = 0.8 * nrow(frequency_data))
train_data <- frequency_data[train_index, ]
test_data <- frequency_data[-train_index, ]

# Prepare test_data_prem from frequency_data2 by splitting in the same way
test_data_prem <- frequency_data2[-train_index, ]

# Step 2: Define the list of variables to remove for Cat3 model
vars_to_remove <- c("Potential.For.Weight.Gain", "Drooling.Potential", "Kid.Friendly", 
                    "owner_age_years", "Intelligence", "pet_gender", 
                    "nb_address_type_adj", "UW_season", "Sensitivity.Level", 
                    "cat1", "cat2", "claim_rate_cat1", "claim_rate_cat2", 
                    "cat3", "claim_count", "earned_units")

# Prepare the training and test data by removing the specified variables
train_data_cleaned_cat3 <- train_data[, !names(train_data) %in% vars_to_remove]
test_data_prem_cleaned_cat3 <- test_data_prem[, !names(test_data_prem) %in% vars_to_remove]

# Step 3: Remove rows with missing values (NA)
train_data_cleaned_cat3 <- na.omit(train_data_cleaned_cat3)
test_data_prem_cleaned_cat3 <- na.omit(test_data_prem_cleaned_cat3)

# Step 4: Fit the regression tree model on the cleaned training data
# Here, 'cat3' is assumed to be the response variable
tree_formula_cat3 <- as.formula("claim_rate_cat3 ~ .")
regression_tree_cat3 <- rpart(tree_formula_cat3, data = train_data_cleaned_cat3, method = "anova")

# Step 5: Make predictions on the cleaned test data (test_data_prem)
predicted_frequencies_prem_cat3 <- predict(regression_tree_cat3, newdata = test_data_prem_cleaned_cat3)

# Step 6: Print the first few predictions for Cat3
print(head(predicted_frequencies_prem_cat3))



# Load necessary libraries
library(dplyr)
library(rpart)  # For regression tree

# Define the list of variables to remove from the model
vars_to_remove <- c("Sensitivity.Level", "Kid.Friendly", "Drooling.Potential", 
                    "Potential.For.Mouthiness", "Avg..Life.Span..years", 
                    "Friendly.Toward.Strangers", "Energy.Level", "Easy.To.Groom", 
                    "Dog.Friendly", "Intelligence", "Prey.Drive", "pet_gender", 
                    "Median_employee_income_SA3", "Intensity", "Tolerates.Hot.Weather", 
                    "nb_address_type_adj", "inception_month", "owner_age_years", 
                    "Total_number_bus", "policy_age_group")

# Step 1: Prepare the training data by removing the specified variables
train_data_cleaned <- train_data_cat1[, !names(train_data_cat1) %in% vars_to_remove]

# Step 2: Prepare the test data by removing the specified variables
test_data_prem_cleaned <- test_data_prem[, !names(test_data_prem) %in% vars_to_remove]

# Step 3: Remove rows with missing values (NA) from the training and test datasets
train_data_cleaned <- na.omit(train_data_cleaned)
test_data_prem_cleaned <- na.omit(test_data_prem_cleaned)

# Step 4: Fit the regression tree model for predicting 'claim_paid'
# Here, 'claim_paid' is the response variable
tree_formula <- as.formula("claim_paid ~ .")
regression_tree_model <- rpart(tree_formula, data = train_data_cleaned, method = "anova")

# Step 5: Make predictions on the cleaned test data (test_data_prem)
predicted_claim_paid_prem1 <- predict(regression_tree_model, newdata = test_data_prem_cleaned)

# Step 6: Print the first few predictions for claim_paid
print(head(predicted_claim_paid_prem1))

# Optional: Summary of the regression tree model
print(summary(regression_tree_model))



# Load necessary libraries
library(dplyr)
library(rpart)  # For regression tree

# Define the list of variables to remove for train_data_cat2 model
vars_to_remove_cat2 <- c("UW_season", "Intelligence", "Kid.Friendly", "Average_household_size", 
                         "nb_address_type_adj", "pet_de_sexed", "Sensitivity.Level", 
                         "Friendly.Toward.Strangers", "Tendency.To.Bark.Or.Howl", "Energy.Level", 
                         "Prey.Drive", "Easy.To.Groom", "Potential.For.Weight.Gain", 
                         "Tolerates.Cold.Weather", "Total_number_bus")

# Step 1: Prepare the training data by removing the specified variables for cat2
train_data_cleaned_cat2 <- train_data_cat2[, !names(train_data_cat2) %in% vars_to_remove_cat2]

# Step 2: Prepare the test data by removing the specified variables for cat2
test_data_prem_cleaned_cat2 <- test_data_prem[, !names(test_data_prem) %in% vars_to_remove_cat2]

# Step 3: Remove rows with missing values (NA) from the training and test datasets for cat2
train_data_cleaned_cat2 <- na.omit(train_data_cleaned_cat2)
test_data_prem_cleaned_cat2 <- na.omit(test_data_prem_cleaned_cat2)

# Step 4: Fit the regression tree model for predicting 'claim_paid' for cat2
tree_formula_cat2 <- as.formula("claim_paid ~ .")
regression_tree_cat2 <- rpart(tree_formula_cat2, data = train_data_cleaned_cat2, method = "anova")

# Step 5: Make predictions on the cleaned test data for cat2
predicted_claim_paid_prem2 <- predict(regression_tree_cat2, newdata = test_data_prem_cleaned_cat2)

# Step 6: Print the first few predictions for claim_paid for cat2
print(head(predicted_claim_paid_prem2))

# Optional: Summary of the regression tree model for cat2
print(summary(regression_tree_cat2))

# Optional: Plot the regression tree for cat2
plot(regression_tree_cat2)
text(regression_tree_cat2, pretty = 1)


# Load necessary libraries
library(dplyr)

# Define the list of variables to remove for train_data_cat3 model
vars_to_remove_cat3 <- c("Potential.For.Weight.Gain", "Drooling.Potential", "Kid.Friendly", 
                         "owner_age_years", "Intelligence", "pet_gender", 
                         "nb_address_type_adj", "UW_season", "Sensitivity.Level")

# Step 1: Prepare the training data by removing the specified variables for cat3
train_data_cleaned_cat3 <- train_data_cat3[, !names(train_data_cat3) %in% vars_to_remove_cat3]

# Step 2: Prepare the test data by removing the specified variables for cat3
test_data_prem_cleaned_cat3 <- test_data_prem[, !names(test_data_prem) %in% vars_to_remove_cat3]

# Step 3: Remove rows with missing values (NA) from the training and test datasets for cat3
train_data_cleaned_cat3 <- na.omit(train_data_cleaned_cat3)
test_data_prem_cleaned_cat3 <- na.omit(test_data_prem_cleaned_cat3)

# Step 4: Fit the regression tree model for predicting 'claim_paid' for cat3
tree_formula_cat3 <- as.formula("claim_paid ~ .")
regression_tree_cat3 <- rpart(tree_formula_cat3, data = train_data_cleaned_cat3, method = "anova")

# Step 5: Make predictions on the cleaned test data for cat3
predicted_claim_paid_prem3 <- predict(regression_tree_cat3, newdata = test_data_prem_cleaned_cat3)

# Step 6: Print the first few predictions for claim_paid for cat3
print(head(predicted_claim_paid_prem3))

# Optional: Summary of the regression tree model for cat3
print(summary(regression_tree_cat3))

# Optional: Plot the regression tree for cat3
plot(regression_tree_cat3)
text(regression_tree_cat3, pretty = 1)


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


