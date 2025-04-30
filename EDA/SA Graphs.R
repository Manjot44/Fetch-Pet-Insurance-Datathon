
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
earned$pet_de_sexed_age <- gsub("0-3 mo", "de_sex_0-3mo", earned$pet_de_sexed_age)  # 0-3 months -> Infant
earned$pet_de_sexed_age <- gsub("4-6 mo", "de_sex_4-6mo", earned$pet_de_sexed_age)   # 4-6 months -> Young
earned$pet_de_sexed_age <- gsub("7-12 mo", "de_sex_7-12mo", earned$pet_de_sexed_age) # 7-12 months -> Juvenile
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

head(severity_data)



## replace postcode with LGA to incorporate accurate economic data
LGA_to_postcode <- read.csv('LGA_to_postcode2.csv')

sort(unique(LGA_to_postcode$LGA.region))

severity_data$nb_postcode <- as.character(severity_data$nb_postcode)
LGA_to_postcode$Postcode <- as.character(LGA_to_postcode$Postcode)

# Remove duplicates in LGA_to_postcode, keeping only the first occurrence of each postcode
LGA_to_postcode_unique <- LGA_to_postcode[!duplicated(LGA_to_postcode$Postcode), ]

unique(LGA_to_postcode_unique$LGA.region)

# Perform the merge again with the unique postcodes
severity_data <- merge(severity_data, LGA_to_postcode_unique, 
              by.x = "nb_postcode", 
              by.y = "Postcode", 
              all.x = TRUE)

# Step 1: Calculate the median claim_paid grouped by LGA.region
median_claim_by_lga <- severity_data %>%
  group_by(LGA.region) %>%
  summarise(
    median_claim_paid = median(claim_paid, na.rm = TRUE),  # Median claim paid
    .groups = 'drop'  # Drop grouping after summarising
  ) %>%
  ungroup()  # Ungroup after summarizing

# Step 2: Load the LGA boundaries from ozmaps
lga_boundaries <- ozmaps::ozmap("abs_lga")

# Function to clean LGA names by removing suffixes and extra spaces
clean_lga_names <- function(name) {
  name %>% 
    tolower() %>%  # Convert to lowercase
    trimws() %>%   # Trim white space
    gsub("\\s*\\(.*\\)", "", .)  # Remove everything in parentheses, e.g., (C), (S), etc.
}

# Step 3: Clean the 'NAME' column in lga_boundaries
lga_boundaries <- lga_boundaries %>%
  mutate(NAME_clean = clean_lga_names(NAME))

# Step 4: Clean the LGA.region column in median_claim_by_lga
median_claim_by_lga <- median_claim_by_lga %>%
  mutate(LGA_clean = clean_lga_names(LGA.region))

# Step 5: Merge the LGA boundaries with the median claim data
lga_boundaries_merged <- lga_boundaries %>%
  left_join(median_claim_by_lga, by = c("NAME_clean" = "LGA_clean"))

mean(median_claim_by_lga$median_claim_paid)

geo_heatmap <- ggplot(data = lga_boundaries_merged) +
  geom_sf(aes(fill = median_claim_paid, geometry = geometry), color = "black", size = 0.1) +
  scale_fill_gradientn(
    colors = c("#E3F2FD", "#9ECAE1", "#08519C", "#08306B"),  # Updated 4 colors
    values = scales::rescale(c(10.55, 100, 208.52, 500, 2469.47)),  # Full scale range
    na.value = "#E3F2FD",  # Color for missing data
    breaks = c(400, 900, 1400),  # Display up to 1500 on the legend
    labels = c("500", "1000", "1500+"),  # Custom labels for the legend
    limits = c(0, 1500),  # Limit the displayed range on the legend
    name = "Claim"
  ) +
  labs(title = NULL) +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    legend.position = c(0.90, 0.75),
    legend.background = element_rect(color = "black", fill = "white", size = 0.5),
    legend.title = element_text(size = 8),
    legend.text = element_text(size = 7),
    legend.key.size = unit(0.5, "lines")
  )

# Save the plot
ggsave(filename = "geo_heatmap.png", height = 10, width = 10, units = 'cm', dpi = 1080, plot = geo_heatmap)






# Step 1: Calculate the median claim_paid grouped by nb_postcode
median_claim_by_postcode <- severity_data %>%
  group_by(nb_postcode) %>%
  summarise(
    median_claim_paid = median(claim_paid, na.rm = TRUE),  # Median claim paid
    .groups = 'drop'
  ) %>%
  ungroup()

# Step 2: Load or use a postcode boundary dataset (replace `postcode_boundaries` with actual data)
postcode_boundaries <- postcode_boundaries %>%
  mutate(NAME_clean = clean_lga_names(NAME))  # Clean names for consistency

# Step 3: Merge the postcode boundaries with the median claim data
postcode_boundaries_merged <- postcode_boundaries %>%
  left_join(median_claim_by_postcode, by = c("NAME_clean" = "nb_postcode"))

# Step 4: Create the heatmap
geo_heatmap <- ggplot(data = postcode_boundaries_merged) +
  geom_sf(aes(fill = median_claim_paid, geometry = geometry), color = "black", size = 0.1) +
  scale_fill_gradientn(
    colors = c("#E3F2FD", "#9ECAE1", "#08519C", "#08306B"),
    values = scales::rescale(c(10.55, 100, 208.52, 500, 2469.47)),
    na.value = "#E3F2FD",
    breaks = c(400, 900, 1400),
    labels = c("500", "1000", "1500+"),
    limits = c(0, 1500),
    name = "Claim"
  ) +
  labs(title = NULL) +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    legend.position = c(0.90, 0.75),
    legend.background = element_rect(color = "black", fill = "white", size = 0.5),
    legend.title = element_text(size = 8),
    legend.text = element_text(size = 7),
    legend.key.size = unit(0.5, "lines")
  )

# Save the plot
ggsave(filename = "geo_heatmap.png", height = 10, width = 10, units = 'cm', dpi = 1080, plot = geo_heatmap)



# Install viridis if not already installed
# install.packages("viridis")

library(ozmaps)
library(viridis)

# Load the CED boundaries from ozmaps
ced_boundaries <- ozmaps::ozmap("abs_ced")

# Plot with a color scale
plot(ced_boundaries, col = viridis::viridis(nrow(ced_boundaries)))





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
earned$pet_de_sexed_age <- gsub("0-3 mo", "de_sex_0-3mo", earned$pet_de_sexed_age)  # 0-3 months -> Infant
earned$pet_de_sexed_age <- gsub("4-6 mo", "de_sex_4-6mo", earned$pet_de_sexed_age)   # 4-6 months -> Young
earned$pet_de_sexed_age <- gsub("7-12 mo", "de_sex_7-12mo", earned$pet_de_sexed_age) # 7-12 months -> Juvenile
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

SA2 <- read.csv('SA2_MAPPING_2011.csv')

SA3 <- read.csv('POSTCODE_SA3_MAPPING.csv')

SA4 <- read.csv('POSTCODE_SA4_MAPPING.csv')

# Ensure unique mapping for each postcode in SA3, picking the first match
SA2_unique <- SA2 %>%
  group_by(POSTCODE, SA2_NAME_2011) %>%
  summarise(SA2_CODE_2011 = first(SA2_CODE_2011))  # Pick the first match for SA3

# Ensure unique mapping for each postcode in SA3, picking the first match
SA3_unique <- SA3 %>%
  group_by(POSTCODE, SA3_NAME_2011) %>%
  summarise(SA3_CODE_2011 = first(SA3_CODE_2011))  # Pick the first match for SA3

# Ensure unique mapping for each postcode in SA3, picking the first match
SA4_unique <- SA4 %>%
  group_by(POSTCODE, SA4_NAME_2011) %>%
  summarise(SA4_CODE_2011 = first(SA4_CODE_2011))  # Pick the first match for SA3

# Perform the left join to add SA3_CODE_2011
frequency_data <- frequency_data %>%
  left_join(SA3_unique, by = c("nb_postcode" = "POSTCODE"))

# Perform the left join to add SA3_CODE_2011
frequency_data <- frequency_data %>%
  left_join(SA4_unique, by = c("nb_postcode" = "POSTCODE"))

# Perform the left join to add SA3_CODE_2011
frequency_data <- frequency_data %>%
  left_join(SA2_unique, by = c("nb_postcode" = "POSTCODE"))

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


library(ozmaps)
library(dplyr)
library(ggplot2)
library(viridis)

# Assuming severity_data contains 'SA4_CODE_2011' and 'claim_paid'
# Calculate the median claim_paid grouped by SA4_CODE_2011
median_claim_by_sa4 <- severity_data %>%
  group_by(SA4_CODE_2011) %>%
  summarise(
    median_claim_paid = median(claim_paid, na.rm = TRUE),  # Median claim paid
    .groups = 'drop'  # Drop grouping after summarising
  ) %>%
  ungroup()  # Ungroup after summarizing

# Load the SA4 boundaries from ozmaps
sa4_boundaries <- ozmaps::ozmap("abs_sa4")

# Merge the SA4 boundaries with the median claim data
sa4_boundaries_merged <- sa4_boundaries %>%
  left_join(median_claim_by_sa4, by = c("SA4_CODE_2011" = "SA4_CODE_2011"))

# Plot the geographic heatmap for median claim paid
geo_heatmap <- ggplot(data = sa4_boundaries_merged) +
  geom_sf(aes(fill = median_claim_paid, geometry = geometry), color = "black", size = 0.1) +
  scale_fill_viridis(option = "C", direction = -1, name = "Median Claim Paid") +  # Adjust color scale as needed
  labs(title = "Geographic Heatmap of Median Claim Paid by SA4") +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    legend.position = c(0.90, 0.75),
    legend.background = element_rect(color = "black", fill = "white", size = 0.5),
    legend.title = element_text(size = 8),
    legend.text = element_text(size = 7),
    legend.key.size = unit(0.5, "lines")
  )

# Save the plot
ggsave(filename = "geo_heatmap_sa4.png", height = 10, width = 10, units = 'cm', dpi = 1080, plot = geo_heatmap)



# Load necessary packages
library(ozmaps)
library(sf)
library(ggplot2)
library(dplyr)
library(viridis)

library(ozmaps)
library(sf)
library(ggplot2)


devtools::install_github("mdsumner/ozmaps.data")


LCC <- "+proj=lcc +lon_0=130 +lat_0=-20 +lat_1=-43 +lat_2=-10 +datum=WGS84"

library(ozmaps.data)
library(sf)
#> Linking to GEOS 3.7.0, GDAL 2.3.2, PROJ 5.2.0
library(ggplot2)

plot(abs_ced, main = "Commonwealth Electoral Divisions")

plot(st_transform(abs_gccsa, LCC), main = "Greater Capital City Statistical Areas", graticule = TRUE)

ggplot(abs_ireg, aes(fill = NAME)) + geom_sf() + guides(fill = FALSE) + coord_sf(crs = 3577)  + ggtitle("Indigenous Regions (EPSG:3577)")



plot(abs_ra, main = "Regional Areas")



plot(st_transform(abs_sa2, 3112), main = 'abs_sa2', graticule = TRUE)

plot(st_transform(abs_sa3, 28354), main = 'abs_sa3', graticule = TRUE)

# Plot the SA4 map flat, without a 3D perspective
plot(
  st_transform(abs_sa4, 4326),
  main = 'SA4 Regions',
  axes = TRUE  # Optionally include axes if needed
)

colnames(severity_data)


library(sf)
library(ggplot2)
library(dplyr)

# Transform to WGS 84 for a flat map
abs_sa4_flat <- st_transform(abs_sa4, crs = 4326)

# Calculate median claim_paid grouped by SA4 region
median_claim_by_sa4 <- severity_data %>%
  group_by(SA4_NAME_2011) %>%
  summarise(
    median_claim_paid = median(claim_paid, na.rm = TRUE),
    .groups = 'drop'
  )

# Merge the SA4 boundaries with the median claim data
sa4_boundaries_merged <- abs_sa4_flat %>%
  left_join(median_claim_by_sa4, by = c("NAME" = "SA4_NAME_2011"))

# Plot the flat SA4 map with custom color for zero values and NA values
geo_heatmap <- ggplot(data = sa4_boundaries_merged) +
  geom_sf(aes(fill = median_claim_paid), color = "black", size = 0.1) +
  scale_fill_gradientn(
    colors = c("#E3F2FD", "#9ECAE1", "#08519C", "#08306B"),
    values = scales::rescale(c(0, 10, 100, 500, max(median_claim_by_sa4$median_claim_paid, na.rm = TRUE))),
    na.value = "#E3F2FD",
    breaks = c(80, 180, 280, 380),
    labels = c("100", "200", "300", "400"),
    name = "Median Claims"
  ) +
  labs(title = NULL) +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    legend.position = c(0.88, 0.78),
    legend.background = element_rect(color = "black", fill = "white", size = 0.5),
    legend.title = element_text(size = 7),
    legend.text = element_text(size = 6),
    legend.key.size = unit(0.5, "lines")
  )

# Save the plot
ggsave(filename = "sa4_geo_heatmap.png", height = 10, width = 10, units = 'cm', dpi = 1080, plot = geo_heatmap)




# Transform to WGS 84 for a flat map
abs_sa3_flat <- st_transform(abs_sa3, crs = 4326)

# Calculate median claim_paid grouped by SA4 region
median_claim_by_sa3 <- severity_data %>%
  group_by(SA3_NAME_2011) %>%
  summarise(
    median_claim_paid = median(claim_paid, na.rm = TRUE),
    .groups = 'drop'
  )

# Merge the SA4 boundaries with the median claim data
sa3_boundaries_merged <- abs_sa3_flat %>%
  left_join(median_claim_by_sa3, by = c("NAME" = "SA3_NAME_2011"))

# Plot the flat SA4 map with custom color for zero values and NA values
geo_heatmap2 <- ggplot(data = sa3_boundaries_merged) +
  geom_sf(aes(fill = median_claim_paid), color = "black", size = 0.1) +
  scale_fill_gradientn(
    colors = c("#E3F2FD", "#9ECAE1", "#08519C", "#08306B"),
    values = scales::rescale(c(0, 100, 250, 500, max(median_claim_by_sa3$median_claim_paid, na.rm = TRUE))),
    na.value = "#E3F2FD",
    breaks = c(200, 450, 700, 950),
    labels = c("250", "500", "750", "1000+"),
    limits = c(0, 1000),
    name = "Median Claims"
  ) +
  labs(title = "Claim Severity by SA3 Region") +
  theme(plot.title = element_text(hjust = 0.5, size = 11, face = 'bold'),
        text = element_text(size = 11),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    legend.position = c(0.88, 0.78),
    legend.background = element_rect(color = "black", fill = "white", size = 0.5),
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 9),
    legend.key.size = unit(0.5, "lines")
  )

# Save the plot
ggsave(filename = "sa3_geo_heatmap.png", height = 8, width = 13, units = 'cm', dpi = 1080, plot = geo_heatmap2)




# Step 4: Create the heatmap
geo_heatmap <- ggplot(data = postcode_boundaries_merged) +
  geom_sf(aes(fill = median_claim_paid, geometry = geometry), color = "black", size = 0.1) +
  scale_fill_gradientn(
    colors = c("#E3F2FD", "#9ECAE1", "#08519C", "#08306B"),
    values = scales::rescale(c(10.55, 100, 208.52, 500, 2469.47)),
    na.value = "#E3F2FD",
    breaks = c(400, 900, 1400),
    labels = c("500", "1000", "1500+"),
    limits = c(0, 1500),
    name = "Claim"
  ) +
  labs(title = NULL) +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    legend.position = c(0.90, 0.75),
    legend.background = element_rect(color = "black", fill = "white", size = 0.5),
    legend.title = element_text(size = 8),
    legend.text = element_text(size = 7),
    legend.key.size = unit(0.5, "lines")
  )





library(ggplot2)
library(sf)

# Ensure flat projection
abs_ra_transformed <- st_transform(abs_ra, crs = 4326)

blue_palette <- c("#08306B", "#08519C", "#6A9BCB", "#9ECAE1", "#E3F2FD")

# Define a named vector for legend labels
region_labels <- c(
  "Inner Regional Australia" = "Inner Rural",
  "Major Cities of Australia" = "Major City",
  "Outer Regional Australia" = "Outer Rural",
  "Remote Australia" = "Remote",
  "Very Remote Australia" = "Very Remote"
)

# Plot with custom blue colors and formatted legend
ra_map <- ggplot(data = abs_ra_transformed) +
  geom_sf(aes(fill = factor(NAME)), color = "black", size = 0.2) + 
  scale_fill_manual(values = blue_palette, name = "Regions", labels = region_labels) +
  labs(title = "Claim Severity by Regional Area") +
  theme(plot.title = element_text(hjust = 0.5, size = 11, face = 'bold'),
        text = element_text(size = 11),
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    legend.position = c(0.88, 0.77),  # Top-right corner
    legend.background = element_rect(color = "black", fill = "white", size = 0.5),
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 9),
    legend.key.size = unit(0.5, "lines")
  )

# Save the plot
ggsave("ra_map.png", plot = ra_map, height = 8, width = 13, units = "cm", dpi = 1080)




# Transform to WGS 84 for a flat map
abs_sa2_flat <- st_transform(abs_sa2, crs = 4326)

colnames(severity_data)

# Calculate median claim_paid grouped by SA4 region
median_claim_by_sa2 <- severity_data %>%
  group_by(SA2_NAME_2011) %>%
  summarise(
    median_claim_paid = median(claim_paid, na.rm = TRUE),
    .groups = 'drop'
  )

# Merge the SA4 boundaries with the median claim data
sa2_boundaries_merged <- abs_sa2_flat %>%
  left_join(median_claim_by_sa2, by = c("NAME" = "SA2_NAME_2011"))

# Plot the flat SA4 map with custom color for zero values and NA values
geo_heatmap3 <- ggplot(data = sa2_boundaries_merged) +
  geom_sf(aes(fill = median_claim_paid), color = "black", size = 0.1) +
  scale_fill_gradientn(
    colors = c("#E3F2FD", "#9ECAE1", "#08519C", "#08306B"),
    values = scales::rescale(c(0, 250, 800, 1500, max(median_claim_by_sa2$median_claim_paid, na.rm = TRUE))),
    na.value = "#E3F2FD",
    breaks = c(200, 450, 700, 950),
    labels = c("250", "500", "750", "1000+"),
    limits = c(0, 1000),
    name = "Median Claims"
  ) +
  labs(title = "Claim Severity by SA3") +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    legend.position = c(0.88, 0.78),
    legend.background = element_rect(color = "black", fill = "white", size = 0.5),
    legend.title = element_text(size = 7),
    legend.text = element_text(size = 6),
    legend.key.size = unit(0.5, "lines")
  )

# Save the plot
ggsave(filename = "sa3_geo_heatmap.png", height = 10, width = 10, units = 'cm', dpi = 1080, plot = geo_heatmap3)


mean(sa2_boundaries_merged$median_claim_paid, na.rm = TRUE)

max(sa2_boundaries_merged$median_claim_paid, na.rm = TRUE)

