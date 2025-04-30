# Load the nnet package
library(nnet)

# Set a seed for reproducibility
set.seed(123)

# Step 1: Standardize the data
train_y <- scale(train_data_cleaned$claim_rate_cat1)
test_y <- scale(test_data_prem_cleaned$claim_rate_cat1)
train_x <- as.matrix(scale(train_data_cleaned[, -which(names(train_data_cleaned) == "claim_rate_cat1")]))
test_x <- as.matrix(scale(test_data_prem_cleaned[, -which(names(test_data_prem_cleaned) == "claim_rate_cat1")]))

# Step 2: Tune hyperparameters
# Use larger hidden layers and higher max iterations
# Here, we set the hidden layer to 20 units and use more iterations for training
# We’ll use decay for regularization and aim to prevent overfitting
nnet_model <- nnet(
  x = train_x, 
  y = train_y, 
  size = 20,               # Increased number of units in the hidden layer
  linout = TRUE,           # Linear output for regression
  maxit = 1000,             # Increase max iterations
  decay = 0.001            # Regularization to prevent overfitting
)

# Step 3: Make predictions on the test data
predicted_frequencies_prem_cat1 <- predict(nnet_model, test_x)

# Step 4: Rescale predictions back (if needed)
predicted_frequencies_prem_cat1 <- predicted_frequencies_prem_cat1 * attr(train_y, "scaled:scale") + attr(train_y, "scaled:center")

# Step 5: Evaluate the model
# Print the first few predictions
print(head(predicted_frequencies_prem_cat1))

# Load the nnet package
library(nnet)

# Set a seed for reproducibility
set.seed(123)

# Step 1: Create a single train-test split for the frequency data (80% train, 20% test)
train_index <- sample(seq_len(nrow(frequency_data)), size = 0.8 * nrow(frequency_data))
train_data <- frequency_data[train_index, ]
test_data <- frequency_data[-train_index, ]

# Prepare test_data_prem from frequency_data2 by splitting in the same way
test_data_prem <- frequency_data2[-train_index, ]

# Common preprocessing: scaling and data cleaning
preprocess_data <- function(train_data, test_data, vars_to_remove, target_var) {
  # Remove specified variables
  train_data_cleaned <- train_data[, !names(train_data) %in% vars_to_remove]
  test_data_cleaned <- test_data[, !names(test_data) %in% vars_to_remove]
  
  # Remove rows with missing values
  train_data_cleaned <- na.omit(train_data_cleaned)
  test_data_cleaned <- na.omit(test_data_cleaned)
  
  # Convert character columns to factors, then numeric
  train_data_cleaned <- train_data_cleaned %>%
    mutate_if(is.character, as.factor) %>%
    mutate_if(is.factor, as.numeric)
  
  test_data_cleaned <- test_data_cleaned %>%
    mutate_if(is.character, as.factor) %>%
    mutate_if(is.factor, as.numeric)
  
  # Align columns between train and test data
  test_data_cleaned <- test_data_cleaned[, names(train_data_cleaned)]
  
  # Scale features and target
  train_x <- as.matrix(scale(train_data_cleaned[, -which(names(train_data_cleaned) == target_var)]))
  train_y <- scale(train_data_cleaned[[target_var]])
  test_x <- as.matrix(scale(test_data_cleaned[, -which(names(test_data_cleaned) == target_var)]))
  test_y <- scale(test_data_cleaned[[target_var]])
  
  return(list(train_x = train_x, train_y = train_y, test_x = test_x, test_y = test_y))
}

### Model for Cat2 ###

# Define variables to remove for Cat2 and target variable
vars_to_remove_cat2 <- c("claim_count", "earned_units", "cat1", "cat2", "cat3", 
                         "claim_rate_cat1", "claim_rate_cat3", "is_multi_pet_plan", 
                         "pet_de_sexed", "UW_season", "Energy.Level", "policy_age_group")
target_var_cat2 <- "claim_rate_cat2"

# Preprocess data for Cat2
data_cat2 <- preprocess_data(train_data, test_data_prem, vars_to_remove_cat2, target_var_cat2)
train_x_cat2 <- data_cat2$train_x
train_y_cat2 <- data_cat2$train_y
test_x_cat2 <- data_cat2$test_x
test_y_cat2 <- data_cat2$test_y

# Define and train the neural network model for Cat2
nnet_model_cat2 <- nnet(
  x = train_x_cat2, 
  y = train_y_cat2, 
  size = 15,               # Adjusted hidden layer size
  linout = TRUE,           # Linear output for regression
  maxit = 500,             # Increase max iterations
  decay = 0.001            # Regularization
)

# Make predictions for Cat2
predicted_frequencies_prem_cat2 <- predict(nnet_model_cat2, test_x_cat2)
predicted_frequencies_prem_cat2 <- predicted_frequencies_prem_cat2 * attr(train_y_cat2, "scaled:scale") + attr(train_y_cat2, "scaled:center")

# Print the first few predictions for Cat2
print(head(predicted_frequencies_prem_cat2))

### Model for Cat3 ###

# Define variables to remove for Cat3 and target variable
vars_to_remove_cat3 <- c("Potential.For.Weight.Gain", "Drooling.Potential", "Kid.Friendly", 
                         "owner_age_years", "Intelligence", "pet_gender", 
                         "nb_address_type_adj", "UW_season", "Sensitivity.Level", 
                         "cat1", "cat2", "claim_rate_cat1", "claim_rate_cat2", 
                         "cat3", "claim_count", "earned_units")
target_var_cat3 <- "claim_rate_cat3"

# Preprocess data for Cat3
data_cat3 <- preprocess_data(train_data, test_data_prem, vars_to_remove_cat3, target_var_cat3)
train_x_cat3 <- data_cat3$train_x
train_y_cat3 <- data_cat3$train_y
test_x_cat3 <- data_cat3$test_x
test_y_cat3 <- data_cat3$test_y

# Define and train the neural network model for Cat3
nnet_model_cat3 <- nnet(
  x = train_x_cat3, 
  y = train_y_cat3, 
  size = 15,               # Adjusted hidden layer size
  linout = TRUE,           # Linear output for regression
  maxit = 500,             # Increase max iterations
  decay = 0.001            # Regularization
)

# Make predictions for Cat3
predicted_frequencies_prem_cat3 <- predict(nnet_model_cat3, test_x_cat3)
predicted_frequencies_prem_cat3 <- predicted_frequencies_prem_cat3 * attr(train_y_cat3, "scaled:scale") + attr(train_y_cat3, "scaled:center")

# Print the first few predictions for Cat3
print(head(predicted_frequencies_prem_cat3))







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

