# Load necessary libraries
library(splines)  # For generating spline terms
library(glmnet)   # For Lasso regression

# Step 1: Create a single train-test split for the frequency data (80% train, 20% test)
set.seed(123)
train_index <- sample(seq_len(nrow(frequency_data)), size = 0.8 * nrow(frequency_data))
train_data <- frequency_data[train_index, ]
test_data <- frequency_data[-train_index, ]

# Prepare test_data_prem from frequency_data2 by splitting in the same way
test_data_prem <- frequency_data2[-train_index, ]

# Step 2: Define and remove variables that are unnecessary for modeling
vars_to_remove <- c("claim_count", "claim_rate_cat1", "cat2", "cat3", 
                    "claim_rate_cat2", "claim_rate_cat3", "Drooling.Potential",
                    "nb_contribution", "Energy.Level", "policy_age_group", 
                    "is_multi_pet_plan", "pet_de_sexed", "Avg..Life.Span..years", "Intensity")

train_data_filtered <- train_data[, !names(train_data) %in% vars_to_remove]
train_data_filtered <- na.omit(train_data_filtered)

# Step 3: Generate spline-transformed predictors for selected variables
spline_terms <- model.matrix(~ ns(inception_month, df = 6) + ns(pet_age_months, df = 2) + 
                               ns(nb_contribution_excess, df = 5) + ns(nb_average_breed_size, df = 6) + 
                               ns(Total_number_bus, df = 4) + ns(Participation_rate, df = 4) + 
                               Median_employee_income_SA3 * Potential.For.Weight.Gain + 
                               pet_age_months * Agriculture_forestry_fishing_per + 
                               nb_excess * nb_average_breed_size + 
                               nb_excess * Dog.Friendly, data = train_data_filtered)[, -1]  # Remove intercept column

# Step 4: Prepare the response variable and offset for Lasso
y_train <- train_data_filtered$cat1
offset_train <- log(train_data_filtered$earned_units)

# Step 5: Fit the Lasso model using glmnet on the transformed spline predictors
cv_lasso <- cv.glmnet(spline_terms, y_train, family = "poisson", offset = offset_train, alpha = 1)

# Extract the best lambda value from cross-validation
best_lambda <- cv_lasso$lambda.min
cat("Best lambda value:", best_lambda, "\n")

# Fit the final Lasso model with the best lambda
lasso_gam_model <- glmnet(spline_terms, y_train, family = "poisson", offset = offset_train, alpha = 1, lambda = best_lambda)

# Step 6: Generate spline-transformed predictors for the test data
spline_terms_test <- model.matrix(~ ns(inception_month, df = 6) + ns(pet_age_months, df = 2) + 
                                    ns(nb_contribution_excess, df = 5) + ns(nb_average_breed_size, df = 6) + 
                                    ns(Total_number_bus, df = 4) + ns(Participation_rate, df = 4) + 
                                    Median_employee_income_SA3 * Potential.For.Weight.Gain + 
                                    pet_age_months * Agriculture_forestry_fishing_per + 
                                    nb_excess * nb_average_breed_size + 
                                    nb_excess * Dog.Friendly, data = test_data_prem)[, -1]

# Define the offset for the test data
offset_test <- log(test_data_prem$earned_units)

# Step 7: Make predictions on the test data using the Lasso-GAM model
predicted_frequencies_prem_cat1 <- predict(lasso_gam_model, newx = spline_terms_test, s = best_lambda, type = "response", newoffset = offset_test)

# Print the first few predictions
print(head(predicted_frequencies_prem_cat1))



# Load necessary libraries
library(dplyr)
library(glmnet)
library(splines)

# Step 1: Create a single train-test split for the frequency data (80% train, 20% test)
set.seed(123)  # Set seed for reproducibility
train_index <- sample(seq_len(nrow(frequency_data)), size = 0.8 * nrow(frequency_data))
train_data <- frequency_data[train_index, ]
test_data <- frequency_data[-train_index, ]

# Prepare test_data_prem from frequency_data2 by splitting in the same way
test_data_prem <- frequency_data2[-train_index, ]

# Step 2: Define the list of variables to remove for Cat2 model
vars_to_remove2 <- c("claim_count", "cat1", "claim_rate_cat2", "cat3", 
                     "claim_rate_cat1", "claim_rate_cat3", "is_multi_pet_plan", 
                     "pet_de_sexed", "UW_season", "Energy.Level", "policy_age_group")

# Remove the specified variables from train_data and test_data_prem
train_data_filtered <- train_data[, !names(train_data) %in% vars_to_remove2]
test_data_prem_filtered <- test_data_prem[, !names(test_data_prem) %in% vars_to_remove2]

# Step 3: Remove rows with missing values (NA)
train_data_filtered <- na.omit(train_data_filtered)
test_data_prem_filtered <- na.omit(test_data_prem_filtered)

# Step 4: Create spline terms for selected predictors
spline_terms_train <- model.matrix(~ ns(pet_age_months, df = 2) + ns(nb_average_breed_size, df = 5) +
                                     ns(Participation_rate, df = 4) + ns(Median_employee_income_SA3, df = 7), 
                                   data = train_data_filtered)[, -1]  # Remove intercept

# Step 5: Define the response variable and offset for Lasso
response_cat2 <- train_data_filtered$cat2
offset_train <- log(train_data_filtered$earned_units)

# Step 6: Fit the Lasso model using glmnet on the transformed spline predictors
cv_lasso_gam <- cv.glmnet(spline_terms_train, response_cat2, family = "poisson", offset = offset_train, alpha = 1)

# Extract the best lambda value from cross-validation
best_lambda <- cv_lasso_gam$lambda.min
cat("Best lambda value:", best_lambda, "\n")

# Fit the final Lasso-GAM model with the best lambda
lasso_gam_model <- glmnet(spline_terms_train, response_cat2, family = "poisson", offset = offset_train, alpha = 1, lambda = best_lambda)

# Step 7: Create spline terms for test data
spline_terms_test <- model.matrix(~ ns(pet_age_months, df = 2) + ns(nb_average_breed_size, df = 5) +
                                    ns(Participation_rate, df = 4) + ns(Median_employee_income_SA3, df = 7), 
                                  data = test_data_prem_filtered)[, -1]

# Define the offset for the test data
offset_test <- log(test_data_prem_filtered$earned_units)

# Step 8: Make predictions on the test data using the Lasso-GAM model
predicted_frequencies_prem_cat2 <- predict(lasso_gam_model, newx = spline_terms_test, s = best_lambda, type = "response", newoffset = offset_test)

# Print the first few predictions for Cat2
print(head(predicted_frequencies_prem_cat2))


# Load necessary libraries
library(dplyr)
library(splines)
library(glmnet)

# Set a seed for reproducibility
set.seed(123)

# Step 1: Create a single train-test split for the frequency data (80% train, 20% test)
train_index <- sample(seq_len(nrow(frequency_data)), size = 0.8 * nrow(frequency_data))
train_data <- frequency_data[train_index, ]
test_data <- frequency_data[-train_index, ]

# Prepare test_data_prem from frequency_data2 by splitting in the same way
test_data_prem <- frequency_data2[-train_index, ]

# Step 2: Define the list of variables to remove for Cat3 model
vars_to_remove3 <- c("cat1", "cat2", "claim_rate_cat3", "claim_count", "claim_rate_cat2", "claim_rate_cat1", 
                     "UW_season", "is_multi_pet_plan", "policy_age_group", "Energy.Level", 
                     "Intelligence", "Kid.Friendly", "pet_gender", "Sensitivity.Level")

# Remove the specified variables from train_data and test_data_prem
train_data_filtered <- train_data[, !names(train_data) %in% vars_to_remove3]
test_data_prem_filtered <- test_data_prem[, !names(test_data_prem) %in% vars_to_remove3]

# Step 3: Remove rows with missing values (NA)
train_data_filtered <- na.omit(train_data_filtered)
test_data_prem_filtered <- na.omit(test_data_prem_filtered)

# Step 4: Generate spline terms for specified numeric variables
numeric_vars <- c("inception_month", "nb_contribution_excess", "nb_contribution", 
                  "nb_excess", "pet_age_months", "nb_average_breed_size", 
                  "owner_age_years", "Total_number_bus")

# Create spline-transformed matrix for the numeric variables
spline_terms_train <- model.matrix(~ ns(inception_month, df = 4) + ns(nb_contribution_excess, df = 4) + 
                                     ns(nb_contribution, df = 4) + ns(nb_excess, df = 4) + 
                                     ns(pet_age_months, df = 4) + ns(nb_average_breed_size, df = 4) + 
                                     ns(owner_age_years, df = 4) + ns(Total_number_bus, df = 4), 
                                   data = train_data_filtered)[, -1]  # Remove intercept

# Step 5: Define response variable and offset for Lasso model
response_cat3 <- train_data_filtered$cat3
offset_train <- log(train_data_filtered$earned_units)

# Step 6: Fit the Lasso model using glmnet on the spline-transformed predictors
cv_lasso_gam <- cv.glmnet(spline_terms_train, response_cat3, family = "poisson", offset = offset_train, alpha = 1)

# Extract the best lambda value from cross-validation
best_lambda <- cv_lasso_gam$lambda.min
cat("Best lambda value:", best_lambda, "\n")

# Fit the final Lasso-GAM model with the best lambda
lasso_gam_model <- glmnet(spline_terms_train, response_cat3, family = "poisson", offset = offset_train, alpha = 1, lambda = best_lambda)

# Step 7: Create spline terms for test data
spline_terms_test <- model.matrix(~ ns(inception_month, df = 4) + ns(nb_contribution_excess, df = 4) + 
                                    ns(nb_contribution, df = 4) + ns(nb_excess, df = 4) + 
                                    ns(pet_age_months, df = 4) + ns(nb_average_breed_size, df = 4) + 
                                    ns(owner_age_years, df = 4) + ns(Total_number_bus, df = 4), 
                                  data = test_data_prem_filtered)[, -1]

# Define the offset for the test data
offset_test <- log(test_data_prem_filtered$earned_units)

# Step 8: Make predictions on the test data using the Lasso-GAM model
predicted_frequencies_prem_cat3 <- predict(lasso_gam_model, newx = spline_terms_test, s = best_lambda, type = "response", newoffset = offset_test)

# Print the first few predictions for Cat3
print(head(predicted_frequencies_prem_cat3))


#---- Severity models ----

# Load necessary libraries
library(dplyr)

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

# Step 4: Fit the Gamma GLM model with a log link function
glm_formula <- as.formula("claim_paid ~ .")  # Formula for Gamma GLM
gamma_glm_model <- glm(glm_formula, data = train_data_cleaned, family = Gamma(link = "log"))

# Step 5: Make predictions on the cleaned test data (test_data_prem)
predicted_claim_paid_prem1 <- predict(gamma_glm_model, newdata = test_data_prem_cleaned, type = "response")

# Step 6: Print the first few predictions for claim_paid
head(predicted_claim_paid_prem1)

# Step 7: Summary of the GLM model
summary(gamma_glm_model)


# Load necessary libraries
library(dplyr)

# Define the list of variables to remove for train_data_cat2 model
vars_to_remove <- c("UW_season", "Intelligence", "Kid.Friendly", "Average_household_size", 
                    "nb_address_type_adj", "pet_de_sexed", "Sensitivity.Level", 
                    "Friendly.Toward.Strangers", "Tendency.To.Bark.Or.Howl", "Energy.Level", 
                    "Prey.Drive", "Easy.To.Groom", "Potential.For.Weight.Gain", 
                    "Tolerates.Cold.Weather", "Total_number_bus")

# Step 1: Prepare the training data by removing the specified variables
train_data_cleaned_cat2 <- train_data_cat2[, !names(train_data_cat2) %in% vars_to_remove]

# Step 2: Prepare the test data by removing the specified variables
test_data_prem_cleaned_cat2 <- test_data_prem[, !names(test_data_prem) %in% vars_to_remove]

# Step 3: Remove rows with missing values (NA) from the training and test datasets
train_data_cleaned_cat2 <- na.omit(train_data_cleaned_cat2)
test_data_prem_cleaned_cat2 <- na.omit(test_data_prem_cleaned_cat2)

# Step 4: Fit the Gamma GLM model with a log link function
glm_formula_cat2 <- as.formula("claim_paid ~ .")  # Formula for Gamma GLM
gamma_glm_model_cat2 <- glm(glm_formula_cat2, data = train_data_cleaned_cat2, family = Gamma(link = "log"))

# Step 5: Make predictions on the cleaned test data (test_data_prem) and store in predicted_claim_paid_prem2
predicted_claim_paid_prem2 <- predict(gamma_glm_model_cat2, newdata = test_data_prem_cleaned_cat2, type = "response")

# Step 6: Print the first few predictions for claim_paid
head(predicted_claim_paid_prem2)

# Step 7: Summary of the GLM model for train_data_cat2
summary(gamma_glm_model_cat2)


# Load necessary libraries
library(dplyr)

# Define the list of variables to remove for train_data_cat3 model
vars_to_remove <- c("Potential.For.Weight.Gain", "Drooling.Potential", "Kid.Friendly", 
                    "owner_age_years", "Intelligence", "pet_gender", 
                    "nb_address_type_adj", "UW_season", "Sensitivity.Level")

# Step 1: Prepare the training data by removing the specified variables
train_data_cleaned_cat3 <- train_data_cat3[, !names(train_data_cat3) %in% vars_to_remove]

# Step 2: Prepare the test data by removing the specified variables
test_data_prem_cleaned_cat3 <- test_data_prem[, !names(test_data_prem) %in% vars_to_remove]

# Step 3: Remove rows with missing values (NA) from the training and test datasets
train_data_cleaned_cat3 <- na.omit(train_data_cleaned_cat3)
test_data_prem_cleaned_cat3 <- na.omit(test_data_prem_cleaned_cat3)

# Step 4: Fit the Gamma GLM model with a log link function
glm_formula_cat3 <- as.formula("claim_paid ~ .")  # Formula for Gamma GLM
gamma_glm_model_cat3 <- glm(glm_formula_cat3, data = train_data_cleaned_cat3, family = Gamma(link = "log"))

# Step 5: Make predictions on the cleaned test data (test_data_prem) and store in predicted_claim_paid_prem3
predicted_claim_paid_prem3 <- predict(gamma_glm_model_cat3, newdata = test_data_prem_cleaned_cat3, type = "response")

# Step 6: Print the first few predictions for claim_paid
head(predicted_claim_paid_prem3)

# Step 7: Summary of the GLM model for train_data_cat3
summary(gamma_glm_model_cat3)

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

# Ensure `total_result` is a vector
total_result <- as.vector(total_result)

# Re-run the gini model
gini_model <- gini_index(test_data_prem$total_claim_paid, total_result)


# Calculate Gini for the model and actual values
gini_actual <- gini_index(test_data_prem$total_claim_paid, test_data_prem$total_claim_paid)

# Calculate normalized Gini index
normalized_gini <- gini_model / gini_actual

# Print the Gini values and the normalized Gini index
print(paste("Gini (Model):", gini_model))
print(paste("Gini (Actual):", gini_actual))
print(paste("Normalized Gini Index:", normalized_gini))




