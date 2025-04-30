### Final_pred dataset set up

### Run 'Adding_data2 (new)' sheet first

final_pred_new <- read.csv('New_Customers_Pricing_Output_File.csv')

final_pred <- read.csv('New_Customers_Pricing_Output_File.csv')

final_pred <- final_pred %>%
  filter(!nb_breed_trait %in% c('siamese', 'forest cat', 'spotted cats', 'persian', 'burmese'))

final_pred <- final_pred[ , !names(final_pred) %in% "Full_month_premium"]


final_pred_new <- final_pred_new %>%
  filter(!nb_breed_trait %in% c('siamese', 'forest cat', 'spotted cats', 'persian', 'burmese'))

final_pred_new <- final_pred_new[ , !names(final_pred_new) %in% "Full_month_premium"]


final_pred$person_dob <- as.Date(final_pred$person_dob, format = "%d/%m/%Y")

final_pred$quote_time_group <- as.Date(final_pred$quote_time_group, format = "%d/%m/%Y")

final_pred$pet_gender <- factor(final_pred$pet_gender, levels = c("male", "female"))
final_pred$pet_de_sexed <- factor(final_pred$pet_de_sexed, levels = c("TRUE", "FALSE"))
final_pred$nb_address_type_adj <- factor(final_pred$nb_address_type_adj)
final_pred$is_multi_pet_plan <- factor(final_pred$is_multi_pet_plan, levels = c("TRUE", "FALSE"))

# Summarise the data, accounting for all columns
final_pred <- final_pred %>%
  group_by(exposure_id) %>%
  summarise(
    # Numeric Columns
    pet_age_months = mean(pet_age_months, na.rm = TRUE),  # Average pet age in months
    pet_age_years = mean(pet_age_years, na.rm = TRUE),  # Average pet age in years
    nb_contribution = mean(nb_contribution, na.rm = TRUE), # Average contribution
    nb_contribution_excess = mean(nb_contribution_excess, na.rm = TRUE),  # Average contribution excess
    nb_excess = mean(nb_excess, na.rm = TRUE),  # Average excess
    nb_number_of_breeds = max(nb_number_of_breeds, na.rm = TRUE),  # Maximum number of breeds
    nb_average_breed_size = mean(nb_average_breed_size, na.rm = TRUE),  # Average breed size
    
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

# Perform the left join to add SA3_CODE_2011
final_pred <- final_pred %>%
  left_join(SA3_unique, by = c("nb_postcode" = "POSTCODE"))

final_pred <- final_pred %>%
  left_join(income_sa3 %>% select(Code, Median_employee_income_SA3, Mean_employee_income_SA3), 
            by = c("SA3_CODE_2011" = "Code"))

final_pred <- final_pred %>%
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
final_pred <- final_pred %>%
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

final_pred$nb_breed_name_unique <- tolower(final_pred$nb_breed_name_unique)  # Convert to lowercase
final_pred$nb_breed_name_unique <- trimws(final_pred$nb_breed_name_unique)   # Remove leading/trailing whitespace


unique_new_dog_breeds2 <- unique(new_dog_data$Breed.Name)

unique_frequency_breeds2 <- unique(final_pred$nb_breed_name_unique)

missing_breeds2 <- setdiff(unique_frequency_breeds2, unique_new_dog_breeds2)

distance_matrix2 <- stringdistmatrix(missing_breeds2, unique_new_dog_breeds2, method = "jw")  # Jaro-Winkler method

closest_match_indices2 <- apply(distance_matrix2, 1, which.min)

# Get the closest matches from new_dog_data
closest_matches2 <- unique_new_dog_breeds[closest_match_indices2]

# Create a data frame with missing breeds and their closest match
matching_df2 <- data.frame(
  Missing_Breeds = missing_breeds2,
  Suggested_Matches = closest_matches2,
  stringsAsFactors = FALSE
)

replacement_vector2 <- setNames(matching_df2$Suggested_Matches, matching_df2$Missing_Breeds)

# Replace values in 'nb_breed_name_unique' with the corresponding 'Suggested_Matches' values
final_pred$nb_breed_name_unique <- ifelse(
  final_pred$nb_breed_name_unique %in% matching_df2$Missing_Breeds, 
  replacement_vector2[final_pred$nb_breed_name_unique], 
  final_pred$nb_breed_name_unique
)

# Merge frequency_data with new_dog_data on breed names
final_pred <- merge(final_pred, new_dog_data, by.x = "nb_breed_name_unique", by.y = "Breed.Name", all.x = TRUE)

# Convert specified columns to factors
final_pred$Dog.Breed.Group <- as.factor(final_pred$Dog.Breed.Group)
final_pred$Dog.Size <- as.factor(final_pred$Dog.Size)
final_pred$Height <- as.factor(final_pred$Height)
final_pred$Weight <- as.factor(final_pred$Weight)
final_pred$Life.Span <- as.factor(final_pred$Life.Span)

final_pred <- add_exp_future_life(final_pred)

final_pred <- add_apartment_feature(final_pred)

final_pred <- add_dog_safety_feature(final_pred)

final_pred <- add_expected_aggression(final_pred)

# Assuming frequency_data is your original data frame
final_pred <- final_pred %>%
  select(exp_future_life,
         nb_contribution_excess, nb_contribution, nb_excess,
         pet_age_months, nb_average_breed_size,
         is_multi_pet_plan, pet_de_sexed, pet_gender,
         nb_address_type_adj, nb_breed_type, owner_age_years,
         Total_number_bus, Agriculture_forestry_fishing_per,
         Health_care_social_assistance_per, Completed_year_12_perc,
         Average_household_size, Participation_rate,
         Median_employee_income_SA3, Dog.Breed.Group,
         Sensitivity.Level,dog_safety, happy_in_apartment,
         Tolerates.Cold.Weather, Tolerates.Hot.Weather,
         Kid.Friendly, Friendly.Toward.Strangers,
         Drooling.Potential, Easy.To.Groom, General.Health,
         Potential.For.Weight.Gain, Intelligence,
         Potential.For.Mouthiness, Prey.Drive, expected_aggression,
         Tendency.To.Bark.Or.Howl, Wanderlust.Potential,
         Energy.Level, Intensity)

final_pred <- final_pred[, !(names(final_pred) %in% c("Dog.Breed.Group"))]

# Convert these columns to ordinal variables with 5 levels
final_pred <- final_pred %>%
  mutate(across(all_of(cols_to_change), ~ cut(., breaks = 5, labels = 1:5, ordered_result = TRUE)))

library(dplyr)

# Fill NA values with the mean for numeric columns and the most frequent value for ordinal factors
final_pred <- final_pred %>%
  mutate(across(everything(), ~ if (is.numeric(.)) {
    # Replace NA values with the mean for numeric columns
    replace(., is.na(.), mean(., na.rm = TRUE))
  } else if (is.factor(.) || is.ordered(.)) {
    # Replace NA values with the most frequent value for factors/ordinals
    most_frequent_value <- names(sort(table(.), decreasing = TRUE))[1]
    replace(., is.na(.), most_frequent_value)
  } else {
    .
  }))


# Check for missing values and count them for each column
missing_values_count <- sapply(final_pred, function(x) sum(is.na(x)))

# Display the result
missing_values_count

str(final_pred)

str(train_data)



