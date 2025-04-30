
### Load packages
library(ggplot2)
library(dplyr)
library(glmnet)
library(randomForest)
library(plotly)
library(caret)
library(tidyr)
library(stringdist)
library(cluster)
library(factoextra)


### Set Plot Themes
theme_set(theme_bw())
theme_update(plot.title = element_text(hjust = 0.5, size = 10, face = 'bold'))
theme_update(text = element_text(size = 10))

#---- Data Preparation----

earned <- read.csv('UNSW_earned_data_adjusted_Sep27.csv')

# Remove time part from UW_Date and nb_policy_first_inception_date
earned$UW_Date <- gsub(" .*", "", earned$UW_Date)
earned$nb_policy_first_inception_date <- gsub(" .*", "", earned$nb_policy_first_inception_date)

# Convert to Date class assuming the format is day/month/year (DD/MM/YYYY)
earned$UW_Date <- as.Date(earned$UW_Date, format = "%d/%m/%Y")
earned$nb_policy_first_inception_date <- as.Date(earned$nb_policy_first_inception_date, format = "%d/%m/%Y")
earned$person_dob <- as.Date(earned$person_dob, format = "%d/%m/%Y")

# Use gsub to remove everything in parentheses, including the parentheses themselves
earned$quote_time_group <- gsub("\\s*\\(.*?\\)", "", earned$quote_time_group)

# Remove the exposure_id_1 column using the subset function
earned <- subset(earned, select = -exposure_id_1)

# Replace age ranges with categories
earned$pet_de_sexed_age <- gsub("0-3 (mo|months)", "de_sex_0-3mo", earned$pet_de_sexed_age)
earned$pet_de_sexed_age <- gsub("4-6 mo", "de_sex_4-6mo", earned$pet_de_sexed_age)   # 4-6 months -> Young
earned$pet_de_sexed_age <- gsub("7-12 (mo|months)", "de_sex_7-12mo", earned$pet_de_sexed_age) # 7-12 months -> Juvenile
earned$pet_de_sexed_age <- gsub("1-2 yr", "de_sex_1-2yr", earned$pet_de_sexed_age)    # 1-2 years -> Adult
# Replace "2+ yr" with "Mature", escaping the "+" character
earned$pet_de_sexed_age <- gsub("2\\+ yr", "de_sex_2+yr", earned$pet_de_sexed_age)    # 2+ years -> Mature
earned$pet_de_sexed_age <- gsub("2\\+ years", "de_sex_2+yr", earned$pet_de_sexed_age) # In case of "2+ years" as well

# Replace blank entries with "Never de-sexed" and keep "Not Sure"
earned$pet_de_sexed_age <- ifelse(earned$pet_de_sexed_age == "", "Never de-sexed", earned$pet_de_sexed_age)
earned$pet_de_sexed_age <- ifelse(earned$pet_de_sexed_age == "Not Sure", "Not Sure", earned$pet_de_sexed_age)

# Convert the column to a factor (categorical variable)
earned$pet_de_sexed_age <- factor(earned$pet_de_sexed_age, 
                                  levels = c("de_sex_0-3mo", "de_sex_4-6mo", "de_sex_7-12mo", "de_sex_1-2yr", "de_sex_2+yr", "Not Sure", "Never de-sexed"))

# Encoding pet_gender, pet_de_sexed, nb_address_type_adj, is_multi_pet_plan, quote_time_group
earned$pet_gender <- factor(earned$pet_gender, levels = c("male", "female"))
earned$pet_de_sexed <- factor(earned$pet_de_sexed, levels = c("TRUE", "FALSE"))
earned$nb_address_type_adj <- factor(earned$nb_address_type_adj)
earned$is_multi_pet_plan <- factor(earned$is_multi_pet_plan, levels = c("TRUE", "FALSE"))
earned$quote_time_group <- factor(earned$quote_time_group)

# Removing the 'X' column
earned <- earned[ , !(names(earned) %in% c("X"))]
earned <- earned[ , !(names(earned) %in% c("row_num"))]

# Summarise the data, accounting for all columns
earned <- earned %>%
  group_by(exposure_id) %>%
  summarise(
    # Date Columns
    UW_Date = max(UW_Date),  # Most recent underwriting date
    nb_policy_first_inception_date = min(nb_policy_first_inception_date, na.rm = TRUE),  # Earliest inception date
    
    # Numeric Columns
    tenure = max(tenure, na.rm = TRUE),  # Maximum tenure
    pet_age_months = mean(pet_age_months, na.rm = TRUE),  # Average pet age in months
    pet_age_years = mean(pet_age_years, na.rm = TRUE),  # Average pet age in years
    nb_contribution = mean(nb_contribution, na.rm = TRUE), # Average contribution
    nb_contribution_excess = mean(nb_contribution_excess, na.rm = TRUE),  # Average contribution excess
    nb_excess = mean(nb_excess, na.rm = TRUE),  # Average excess
    nb_number_of_breeds = max(nb_number_of_breeds, na.rm = TRUE),  # Maximum number of breeds
    nb_average_breed_size = mean(nb_average_breed_size, na.rm = TRUE),  # Average breed size
    earned_units = sum(earned_units, na.rm = TRUE),  # Sum of earned units
    
    # Boolean Columns
    is_multi_pet_plan = any(is_multi_pet_plan == "TRUE"),  # TRUE if any multi-pet plan exists
    pet_de_sexed = first(pet_de_sexed),  # First occurrence of pet de-sexed
    
    # Categorical Columns
    pet_gender = first(pet_gender),  # First occurrence of pet gender
    pet_de_sexed_age = first(pet_de_sexed_age),  # First occurrence of pet de-sexed age
    nb_address_type_adj = first(nb_address_type_adj),  # First occurrence of address type
    nb_postcode = first(nb_postcode),  # First postcode
    nb_state = first(nb_state),  # First state
    nb_suburb = first(nb_suburb),  # First suburb
    nb_breed_type = first(nb_breed_type),  # First breed type
    nb_breed_trait = first(nb_breed_trait),  # First breed trait
    nb_breed_name_unique = first(nb_breed_name_unique),  # First breed name
    nb_breed_name_unique_concat = first(nb_breed_name_unique_concat),  # First concatenated breed name
    quote_time_group = first(quote_time_group),  # First quote time group
    
    # Person-related columns (if applicable)
    person_dob = first(person_dob),  # First person date of birth
    owner_age_years = max(owner_age_years, na.rm = TRUE),  # Average owner age
    
    # Additional Columns
    pet_is_switcher = first(pet_is_switcher),  # First occurrence of switcher
    lead_date_day = first(lead_date_day),  # First occurrence of lead date day
    quote_date = first(quote_date),  # First quote date
  )

# Identify columns with NAs and count the number of NAs in each
na_counts <- sapply(earned, function(x) sum(is.na(x)))
na_counts <- na_counts[na_counts > 0]  # Filter to show only columns with NAs

earned$person_dob

# Print the results
print(na_counts)


earned <- earned[earned$tenure >= 0, ]

# Remove rows with NA in person_dob
earned <- earned[!is.na(earned$person_dob), ]

claims <- read.csv('UNSW_claims_data.csv')

# Convert 'claim_start_date' to Date type
claims$claim_start_date <- as.Date(claims$claim_start_date, format="%d/%m/%Y")

# Ensure 'claim_paid' and 'total_claim_amount' are numeric
claims$claim_paid <- as.numeric(claims$claim_paid)
claims$total_claim_amount <- as.numeric(claims$total_claim_amount)

# Remove the 'tenure' column from the 'claims' dataset
claims <- claims[, !names(claims) %in% 'tenure']

#----Creating severity dataset----

# Remove rows where claim_paid == 0
claims <- claims %>%
  filter(claim_paid != 0)

#----Calculating Frequency of claims per exposure----

# Count the number of claims for each exposure_id and store it in 'claim_counts'
claim_counts <- claims %>%
  group_by(exposure_id) %>%
  summarise(claim_count = n()) %>%
  ungroup()

# Step 2: Merge the claim frequencies into the 'earned' dataset without modifying 'claims'
frequency_data <- earned %>%
  left_join(claim_counts, by = "exposure_id") %>%
  mutate(claim_count = ifelse(is.na(claim_count), 0, claim_count))


### Creating the test dataset for the overall RMSE calculation (test_data_prem)
# Step 1: Expand the 'claims' dataset so each row corresponds to a separate claim, with claim_count = 1
# For each exposure_id, create a row for each individual claim
expanded_claims <- claims %>%
  mutate(claim_count = 1, 
         claim_paid = ifelse(is.na(claim_paid), 0, claim_paid))  # Set claim_paid to 0 if it is NA

frequency_data2 <- earned %>%
  left_join(expanded_claims, by = "exposure_id") %>%
  mutate(claim_count = ifelse(is.na(claim_count), 0, claim_count),
         claim_paid = ifelse(is.na(claim_paid), 0, claim_paid))  # Set claim_paid to 0 for non-claim rows

#----Adding in sets of potential predictors

### Adding in SA2, SA3 and SA4 location

SA3 <- read.csv('POSTCODE_SA3_MAPPING.csv')

# Ensure unique mapping for each postcode in SA3, picking the first match
SA3_unique <- SA3 %>%
  group_by(POSTCODE) %>%
  summarise(SA3_CODE_2011 = first(SA3_CODE_2011))  # Pick the first match for SA3

# Perform the left join to add SA3_CODE_2011
frequency_data <- frequency_data %>%
  left_join(SA3_unique, by = c("nb_postcode" = "POSTCODE"))

#----Incorporating ABS related data----
income_sa3 <- read.csv('income_sa3.csv')

library(dplyr)
frequency_data <- frequency_data %>%
  left_join(income_sa3 %>% select(Code, Median_employee_income_SA3, Mean_employee_income_SA3), 
            by = c("SA3_CODE_2011" = "Code"))

added_var_abs <- read.csv('add_abs_variables.csv')
colnames(added_var_abs)

# Left join frequency_data with added_var_abs using SA3_CODE_2011 and selecting specific columns from added_var_abs
frequency_data <- frequency_data %>%
  left_join(
    added_var_abs %>% 
      select(Code, 
             Total_number_bus, 
             Number_non.employing_bus,  # Updated to the correct column name
             Total_employed_aged_15_and_over, 
             Median_price_established_house_transfers, 
             Value_private_sector_houses, 
             Agriculture_forestry_fishing_per, 
             Health_care_social_assistance_per, 
             Professional_scientific_technical_services_per, 
             Number_employee_jobs_agriculture_forestry_fishing, 
             Number_employee._jobs_health_care,  # Updated to the correct column name
             Total_persons_aged_15_years_over, 
             Persons_who_made_HELP_repayment, 
             Number_employee_jobs_professional_scientific_technical, 
             Graduate_diploma_perc, 
             Bachelor_degree_perc, 
             Completed_year_12_perc, 
             Worked_from_home, 
             Average_household_size, 
             Lone_person_households, 
             Median_weekly_household_rent, 
             Persons_who_made_gift_or_donation, 
             Total_protected_land_area, 
             National_parks, 
             Nature_reserves, 
             X_1.499_per_week, 
             Participation_rate, 
             Professionals_perc, 
             Bachelor_degree_perc.1, 
             X_3000_or_more_per_week, 
             Managers, 
             Unemployment_rate, 
             X_2000_2999_per_week, 
             Not_in_labour_force), 
    by = c("SA3_CODE_2011" = "Code")
  )

# Convert appropriate columns to factor or numeric
frequency_data <- frequency_data %>%
  mutate(
    # Convert logical columns to factor
    is_multi_pet_plan = as.factor(is_multi_pet_plan),
    pet_de_sexed = as.factor(pet_de_sexed),
    pet_gender = as.factor(pet_gender),
    pet_de_sexed_age = as.factor(pet_de_sexed_age),
    nb_address_type_adj = as.factor(nb_address_type_adj),
    nb_breed_type = as.factor(nb_breed_type),
    
    # Convert character columns to factor if needed
    nb_state = as.factor(nb_state),
    nb_suburb = as.factor(nb_suburb),
    
    # Ensure numeric variables are correctly encoded
    Median_employee_income_SA3 = as.numeric(as.character(Median_employee_income_SA3)),
    Mean_employee_income_SA3 = as.numeric(as.character(Mean_employee_income_SA3)),
    Total_number_bus = as.numeric(as.character(Total_number_bus)),
    Number_non.employing_bus = as.numeric(as.character(Number_non.employing_bus)),
    Total_employed_aged_15_and_over = as.numeric(as.character(Total_employed_aged_15_and_over)),
    Agriculture_forestry_fishing_per = as.numeric(as.character(Agriculture_forestry_fishing_per)),
    Health_care_social_assistance_per = as.numeric(as.character(Health_care_social_assistance_per)),
    Professional_scientific_technical_services_per = as.numeric(as.character(Professional_scientific_technical_services_per)),
    Number_employee_jobs_agriculture_forestry_fishing = as.numeric(as.character(Number_employee_jobs_agriculture_forestry_fishing)),
    Number_employee._jobs_health_care = as.numeric(as.character(Number_employee._jobs_health_care)),
    Worked_from_home = as.numeric(as.character(Worked_from_home)),
    Lone_person_households = as.numeric(as.character(Lone_person_households)),
    X_1.499_per_week = as.numeric(as.character(X_1.499_per_week)),
    Participation_rate = as.numeric(as.character(Participation_rate)),
    Professionals_perc = as.numeric(as.character(Professionals_perc)),
    Bachelor_degree_perc.1 = as.numeric(as.character(Bachelor_degree_perc.1)),
    X_3000_or_more_per_week = as.numeric(as.character(X_3000_or_more_per_week)),
    Managers = as.numeric(as.character(Managers)),
    Unemployment_rate = as.numeric(as.character(Unemployment_rate)),
    X_2000_2999_per_week = as.numeric(as.character(X_2000_2999_per_week)),
    Not_in_labour_force = as.numeric(as.character(Not_in_labour_force)),
    Completed_year_12_perc = as.numeric(as.character(Completed_year_12_perc)),
    Bachelor_degree_perc = as.numeric(as.character(Bachelor_degree_perc)),
    Graduate_diploma_perc = as.numeric(as.character(Graduate_diploma_perc)),
    Number_employee_jobs_professional_scientific_technical = as.numeric(as.character(Number_employee_jobs_professional_scientific_technical)),
    Persons_who_made_HELP_repayment = as.numeric(as.character(Persons_who_made_HELP_repayment)),
    National_parks = as.numeric(as.character(National_parks)),
    Nature_reserves = as.numeric(as.character(Nature_reserves)),
    Total_protected_land_area = as.numeric(as.character(Total_protected_land_area)),
    Persons_who_made_gift_or_donation = as.numeric(as.character(Persons_who_made_gift_or_donation)),
  )

#---- incorporating breed behavioral characteristic related data----

new_dog_data <- read.csv('dogs_cleaned.csv')

# Ensure the format of new_dog_data$Breed.Name matches frequency_data$nb_breed_name_unique
new_dog_data$Breed.Name <- tolower(new_dog_data$Breed.Name)  # Convert to lowercase
new_dog_data$Breed.Name <- trimws(new_dog_data$Breed.Name)   # Remove leading/trailing whitespace

frequency_data$nb_breed_name_unique <- tolower(frequency_data$nb_breed_name_unique)  # Convert to lowercase
frequency_data$nb_breed_name_unique <- trimws(frequency_data$nb_breed_name_unique)   # Remove leading/trailing whitespace

# Get unique breed names from both datasets
unique_frequency_breeds <- unique(frequency_data$nb_breed_name_unique)
unique_new_dog_breeds <- unique(new_dog_data$Breed.Name)

# Find breeds in frequency_data that are not in new_dog_data
missing_breeds <- setdiff(unique_frequency_breeds, unique_new_dog_breeds)

# Calculate the string distance matrix between missing breeds and new dog breeds
distance_matrix <- stringdistmatrix(missing_breeds, unique_new_dog_breeds, method = "jw")  # Jaro-Winkler method

# Find the index of the closest match for each missing breed
closest_match_indices <- apply(distance_matrix, 1, which.min)

# Get the closest matches from new_dog_data
closest_matches <- unique_new_dog_breeds[closest_match_indices]

# Create a data frame with missing breeds and their closest match
matching_df <- data.frame(
  Missing_Breeds = missing_breeds,
  Suggested_Matches = closest_matches,
  stringsAsFactors = FALSE
)

# Create a named vector for easier replacement
replacement_vector <- setNames(matching_df$Suggested_Matches, matching_df$Missing_Breeds)

# Replace values in 'nb_breed_name_unique' with the corresponding 'Suggested_Matches' values
frequency_data$nb_breed_name_unique <- ifelse(
  frequency_data$nb_breed_name_unique %in% matching_df$Missing_Breeds, 
  replacement_vector[frequency_data$nb_breed_name_unique], 
  frequency_data$nb_breed_name_unique
)

# Merge frequency_data with new_dog_data on breed names
frequency_data <- merge(frequency_data, new_dog_data, by.x = "nb_breed_name_unique", by.y = "Breed.Name", all.x = TRUE)

# Convert specified columns to factors
frequency_data$Dog.Breed.Group <- as.factor(frequency_data$Dog.Breed.Group)
frequency_data$Dog.Size <- as.factor(frequency_data$Dog.Size)
frequency_data$Height <- as.factor(frequency_data$Height)
frequency_data$Weight <- as.factor(frequency_data$Weight)
frequency_data$Life.Span <- as.factor(frequency_data$Life.Span)

# Assuming both 'claims' and 'frequency_data' datasets have a common 'exposure_id' column
severity_data <- claims %>%
  left_join(frequency_data, by = "exposure_id")

# Removing 'National_parks' and 'Nature_reserves' from frequency_data and severity_data
frequency_data <- frequency_data %>%
  select(-National_parks, -Nature_reserves)

severity_data <- severity_data %>%
  select(-National_parks, -Nature_reserves)

# Remove the specified columns from frequency_data and severity_data
columns_to_remove <- c('Total_protected_land_area', 'Persons_who_made_gift_or_donation', 
                       'Median_price_established_house_transfers', 'Value_private_sector_houses')

# Removing from frequency_data
frequency_data <- frequency_data %>%
  select(-all_of(columns_to_remove))

# Removing from severity_data
severity_data <- severity_data %>%
  select(-all_of(columns_to_remove))

# Remove rows where earned_units is 0 in frequency_data
frequency_data <- frequency_data[frequency_data$earned_units > 0, ]



# Define a function to add the exp_future_life column to a dataset
add_exp_future_life <- function(data) {
  if("Avg..Life.Span..years" %in% colnames(data) & "pet_age_months" %in% colnames(data)) {
    data$exp_future_life <- data$Avg..Life.Span..years - (data$pet_age_months / 12)
  } else {
    print("One or both columns 'Avg..Life.Span..years' and 'pet_age_months' are missing in the dataset.")
  }
  return(data)
}

# Apply the function to each dataset
frequency_data <- add_exp_future_life(frequency_data)
severity_data <- add_exp_future_life(severity_data)

# Define a function to add the new feature based on nb_address_type_adj
add_apartment_feature <- function(data) {
  data$happy_in_apartment <- ifelse(data$nb_address_type_adj == "Apartment", 
                                    data$Adapts.Well.To.Apartment.Living, 
                                    0)
  return(data)
}

# Apply the function to both frequency_data and frequency_data2
frequency_data <- add_apartment_feature(frequency_data)
severity_data <- add_apartment_feature(severity_data)

# Define a function to create the dog_safety interaction feature
add_dog_safety_feature <- function(data) {
  data$dog_safety <- ifelse(data$is_multi_pet_plan == TRUE, data$Dog.Friendly, 0)
  return(data)
}

# Apply the function to both frequency_data and severity_data
frequency_data <- add_dog_safety_feature(frequency_data)
severity_data <- add_dog_safety_feature(severity_data)

# Apply the function to both frequency_data and severity_data
frequency_data <- add_dog_safety_feature(frequency_data)
severity_data <- add_dog_safety_feature(severity_data)

# Define a function to add the expected_aggression feature
add_expected_aggression <- function(data) {
  data$expected_aggression <- ifelse(data$pet_de_sexed == TRUE, 
                                     0, 
                                     5 - data$All.Around.Friendliness)
  return(data)
}

# Apply the function to both frequency_data and severity_data
frequency_data <- add_expected_aggression(frequency_data)
severity_data <- add_expected_aggression(severity_data)


# Run the following after running 'Adding_data_new2' up to line 420
#---- Clustering by Breed Trait

# Step 1: Count the number of claims for each exposure_id and nb_breed_trait level
claim_counts2 <- severity_data %>%
  filter(!is.na(nb_breed_trait)) %>%  # Remove rows where nb_breed_trait is NA
  group_by(exposure_id, nb_breed_trait) %>%
  summarise(claim_count = n()) %>%
  ungroup()

# Merge with earned data to get earned_units and calculate frequency
frequency_data_clus <- claim_counts2 %>%
  left_join(earned %>% select(-nb_breed_trait), by = "exposure_id") %>%
  mutate(frequency = claim_count / earned_units) %>%
  select(exposure_id, nb_breed_trait, claim_count, earned_units, frequency)

# Calculate mean and standard deviation of frequency for each nb_breed_trait level
frequency_summary <- frequency_data_clus %>%
  group_by(nb_breed_trait) %>%
  summarise(
    mean_frequency = mean(frequency, na.rm = TRUE),
    sd_frequency = sd(frequency, na.rm = TRUE)
  )

# Calculate mean and standard deviation of claim_paid for severity data
severity_summary <- severity_data %>%
  filter(!is.na(nb_breed_trait)) %>%
  group_by(nb_breed_trait) %>%
  summarise(
    mean_severity = mean(claim_paid, na.rm = TRUE),
    sd_severity = sd(claim_paid, na.rm = TRUE)
  )

# Merge both summaries into a single data frame
health_summary <- merge(frequency_summary, severity_summary, by = "nb_breed_trait")

# Add count of observations to health_summary
health_summary <- health_summary %>%
  left_join(
    severity_data %>%
      filter(!is.na(nb_breed_trait)) %>%
      group_by(nb_breed_trait) %>%
      summarise(n_observations = n()),
    by = "nb_breed_trait"
  )

# Clustering based on mean frequency and mean severity
health_summary_scaled <- scale(health_summary[, c("mean_frequency", "mean_severity")])
k_clusters <- kmeans(health_summary_scaled, centers = 3)  # Adjust centers as needed
health_summary$cluster <- as.factor(k_clusters$cluster)

# Plotting the scatterplot with clusters
scatter <- ggplot(health_summary, aes(x = mean_frequency, y = mean_severity, size = n_observations, color = cluster)) +
  geom_point(alpha = 0.7) +  # Points with transparency
  geom_text_repel(aes(label = nb_breed_trait), size = 4, color = "black") +  # Labels with text repel
  scale_color_manual(values = c("blue", "#08306B", "#9ECAE1")) +  # Different colors for clusters
  scale_size_continuous(range = c(3, 10), guide = "none") +  # Adjust size range and remove size legend
  labs(
    x = "Average Frequency",
    y = "Average Severity",
    title = "Clustering by Breed Trait",
    color = "Cluster"
  ) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 11, face = 'bold'),
    text = element_text(size = 11),
    legend.position = c(0.88, 0.8),  # Top right corner
    legend.background = element_rect(color = "black", fill = "white", size = 0.5),
    legend.title = element_text(size = 9),
    legend.text = element_text(size = 8),
    legend.key.size = unit(0.5, "lines")
  ) +
  coord_cartesian(
    xlim = c(min(health_summary$mean_frequency) - 0.05, max(health_summary$mean_frequency) + 0.05),
    ylim = c(min(health_summary$mean_severity) - 50, max(health_summary$mean_severity) + 50)
  )

# Save the plot
ggsave(filename = "scatter_clusters.png", height = 8, width = 13, units = 'cm', dpi = 1080, plot = scatter)


# Run the following after running 'Adding_data_new2' up to line 420
#---- Clustering by General.Health score

# Step 1: Count the number of claims for each exposure_id and General.Health level
claim_counts2 <- severity_data %>%
  filter(!is.na(General.Health)) %>%  # Remove rows where General.Health is NA
  group_by(exposure_id, General.Health) %>%
  summarise(claim_count = n()) %>%
  ungroup()

# Merge with earned data to get earned_units
frequency_data_clus <- claim_counts2 %>%
  left_join(earned, by = "exposure_id") %>%
  select(exposure_id, General.Health, claim_count, earned_units)

# Calculate claims per earned_units for frequency
frequency_data_clus <- frequency_data_clus %>%
  mutate(frequency = claim_count / earned_units)

# Calculate mean and standard deviation of frequency for each General.Health level
frequency_summary <- frequency_data_clus %>%
  group_by(General.Health) %>%
  summarise(
    mean_frequency = mean(frequency, na.rm = TRUE),
    sd_frequency = sd(frequency, na.rm = TRUE)
  )

# Calculate mean and standard deviation of claim_paid for severity data, excluding NA values in General.Health
severity_summary <- severity_data %>%
  filter(!is.na(General.Health)) %>%
  group_by(General.Health) %>%
  summarise(
    mean_severity = mean(claim_paid, na.rm = TRUE),
    sd_severity = sd(claim_paid, na.rm = TRUE)
  )

# Merging both summaries into a single data frame
health_summary <- merge(frequency_summary, severity_summary, by = "General.Health")

# Adding count of observations to `health_summary`
health_summary <- health_summary %>%
  left_join(
    severity_data %>%
      filter(!is.na(General.Health)) %>%
      group_by(General.Health) %>%
      summarise(n_observations = n()),
    by = "General.Health"
  )

# Ensure General.Health is a factor for discrete coloring
health_summary$General.Health <- as.factor(health_summary$General.Health)

# Load necessary libraries
library(ggplot2)
library(ggrepel)

# Plotting the scatterplot with the updated data
scatter <- ggplot(health_summary, aes(x = mean_frequency, y = mean_severity, size = n_observations, color = General.Health)) +
  geom_point(alpha = 0.7) +  # Points with transparency
  geom_text_repel(aes(label = General.Health), size = 4, color = "black") +  # Labels with text repel
  scale_color_manual(values = c("#08306B", "#9ECAE1", "deepskyblue", "blue", "navyblue")) +  # Different shades of blue
  scale_size_continuous(range = c(3, 10), guide = "none") +  # Adjust size range and remove size legend
  labs(
    x = "Average Frequency",
    y = "Average Severity",
    title = "Clustering by General Health Rating",
    color = "General Health"
  ) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 11, face = 'bold'),
    text = element_text(size = 11),
    legend.position = c(0.88, 0.8),  # Top right corner
    legend.background = element_rect(color = "black", fill = "white", size = 0.5),  # Border for the legend
    legend.title = element_text(size = 9),
    legend.text = element_text(size = 8),
    legend.key.size = unit(0.5, "lines")
  ) +
  coord_cartesian(xlim = c(min(health_summary$mean_frequency) - 0.05, max(health_summary$mean_frequency) + 0.05),
                  ylim = c(min(health_summary$mean_severity) - 50, max(health_summary$mean_severity) + 50))

# Save the plot
ggsave(filename = "scatter.png", height = 8, width = 13, units = 'cm', dpi = 1080, plot = scatter)



