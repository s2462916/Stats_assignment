# Load the readxl package for reading Excel files
library(readxl)

# Read the Excel files into R data frames
biomarker_data <- read_excel("biomarkers.xlsx")
demographic_data <- read_excel("covariates.xlsx")

# Extract patient ID from biomarker data
biomarker_data$PatientID <- sub("-.*", "", biomarker_data$`Biomarker`)

# Merge the two data frames based on the Patient ID column
combined_data <- merge(biomarker_data, demographic_data, by = "PatientID")

# View the merged data to ensure it looks correct
head(combined_data)

# Extract week information from the "Biomarker" column
combined_data$Week <- sub(".*-", "", combined_data$Biomarker)

# Subset data to include only measurements at inclusion (0 weeks)
inclusion_data <- combined_data[combined_data$Week == "0weeks", ]

# Separate data for males and females
male_data <- inclusion_data[inclusion_data$Sex == 1, ]
female_data <- inclusion_data[inclusion_data$Sex == 2, ]

# List of biomarkers
biomarkers <- c("IL-8", "VEGF-A", "OPG", "TGF-beta-1", "IL-6", "CXCL9", "CXCL1", "IL-18", "CSF-1")

# Create an empty data frame to store the results
results <- data.frame(Biomarker = character(),
                      t_value = numeric(),
                      df = numeric(),
                      p_value = numeric(),
                      confidence_interval_lower = numeric(),
                      confidence_interval_upper = numeric(),
                      mean_of_x = numeric(),
                      mean_of_y = numeric(),
                      stringsAsFactors = FALSE)

# Perform t-test for each biomarker
for (biomarker in biomarkers) {
  cat("Biomarker:", biomarker, "\n")
  
  # Subset data for the biomarker
  male_values <- male_data[[biomarker]]
  female_values <- female_data[[biomarker]]
  
  # Perform t-test
  t_test_result <- t.test(male_values, female_values)
  
  # Store results in the data frame
  results[nrow(results) + 1, ] <- list(
    Biomarker = biomarker,
    t_value = t_test_result$statistic,
    df = t_test_result$parameter,
    p_value = t_test_result$p.value,
    confidence_interval_lower = t_test_result$conf.int[1],
    confidence_interval_upper = t_test_result$conf.int[2],
    mean_of_x = mean(male_values),
    mean_of_y = mean(female_values)
  )
}
print(results)


# Create an empty data frame to store the results
results_adj <- data.frame(Biomarker = character(),
                          t_value = numeric(),
                          df = numeric(),
                          p_value = numeric(),
                          confidence_interval_lower = numeric(),
                          confidence_interval_upper = numeric(),
                          mean_of_x = numeric(),
                          mean_of_y = numeric(),
                          stringsAsFactors = FALSE)

# Perform t-test for each biomarker
for (biomarker in biomarkers) {
  cat("Biomarker:", biomarker, "\n")
  
  # Subset data for the biomarker
  male_values <- male_data[[biomarker]]
  female_values <- female_data[[biomarker]]
  
  # Perform t-test
  t_test_result_adj <- t.test(male_values, female_values, conf.level = 0.9944)
  
  # Store results in the data frame
  results_adj[nrow(results_adj) + 1, ] <- list(
    Biomarker = biomarker,
    t_value = t_test_result_adj$statistic,
    df = t_test_result_adj$parameter,
    p_value = t_test_result_adj$p.value,
    confidence_interval_lower = t_test_result_adj$conf.int[1],
    confidence_interval_upper = t_test_result_adj$conf.int[2],
    mean_of_x = mean(male_values),
    mean_of_y = mean(female_values)
  )
}
print(results_adj)


# Define new column names
new_colnames <- c( "PatientID", "Biomarker", "IL_8" , "VEGF_A" , "OPG" , "TGF_beta_1" , "IL_6" , "CXCL9" , "CXCL1" , "IL_18" , "CSF_1" , "Age" , "Sex" , "Smoker" , "VAS_at_inclusion", "Vas_12_months", "Week")

# Assign new column names to the data frame
colnames(inclusion_data) <- new_colnames


# Split the inclusion_data into training and testing sets
set.seed(123) # for reproducibility
train_indices <- sample(1:nrow(inclusion_data), 0.8 * nrow(inclusion_data))
train_data <- inclusion_data[train_indices, ]
test_data <- inclusion_data[-train_indices, ]

# Fit the regression model
model <- lm(Vas_12_months ~ IL_8 + VEGF_A + OPG + TGF_beta_1 + IL_6 + CXCL9 + CXCL1 + IL_18 + CSF_1 + Age + Sex + Smoker + VAS_at_inclusion, data = train_data)

# Extract the parameter estimates
parameter_values <- summary(model)$coefficients

# Print the parameter values table
print(parameter_values)

# Get the actual values from your dataset
actual_values <- train_data$Vas_12_months  # Replace 'dataset' with the name of your dataset and 'VAS_at_12months' with the actual column name

# Use the predict() function to obtain the predicted values
explanatory_vars <- c("IL_8", "VEGF_A", "OPG", "TGF_beta_1", "IL_6", "CXCL9", "CXCL1", "IL_18", "CSF_1", "Age", "Sex", "Smoker", "VAS_at_inclusion")
subset_data_for_predict <- train_data[, explanatory_vars]
predicted_values <- predict(model, newdata = subset_data_for_predict)  # Replace 'model' with the name of your regression model and 'dataset' with the name of your dataset

# Predict 12-month VAS for the remaining 20% of patients
predicted_values <- predict(model, newdata = test_data)

# Compare predicted values to actual values
comparison <- data.frame(Actual = test_data$Vas_12_months, Predicted = predicted_values)
comparison

# Install and load required packages
install.packages("ggplot2")
library(ggplot2)

# Assuming 'test_data' is your dataframe and 'predicted_values' is your list of predicted values
# Combine actual and predicted values into a dataframe
comparison_dataframe <- data.frame(PatientID = test_data$PatientID,
                                   Actual = test_data$Vas_12_month,
                                   Predicted = predicted_values)

# Create a scatter plot of actual vs. predicted values
ggplot(comparison_dataframe, aes(x = PatientID)) +
  geom_point(aes(y = Actual), color = "blue", size = 3) +  # Plot actual values in blue
  geom_point(aes(y = Predicted), color = "red", size = 3) +  # Plot predicted values in red
  labs(title = "Actual vs. Predicted 12-month VAS Ratings",
       x = "Patient ID",
       y = "12-month VAS Rating") +
  theme_minimal() +
  scale_color_manual(name = "Data", values = c("Actual" = "blue", "Predicted" = "red")) +  # Set colors and legend labels
  guides(color = guide_legend(override.aes = list(size = 3))) +  # Adjust legend size
  theme(legend.position = "top-right")  # Move legend to the top

# Fit a quadratic regression model with polynomial terms for each predictor
quadratic_model <- lm(Vas_12_months ~ I(IL_8^2) + I(VEGF_A^2) + I(OPG^2) + I(TGF_beta_1^2) + I(IL_6^2) + I(CXCL9^2) + I(CXCL1^2) + I(IL_18^2) + I(CSF_1^2) + I(Age^2) + I(Sex^2) + I(Smoker^2) + I(VAS_at_inclusion^2), data = train_data)

# Summary of the model
summary(quadratic_model)

# Fit a cubic regression model with polynomial terms for each predictor
cubic_model <- lm(Vas_12_months ~ I(IL_8^2) + I(VEGF_A^2) + I(OPG^2) + I(TGF_beta_1^2) + I(IL_6^2) + I(CXCL9^2) + I(CXCL1^2) + I(IL_18^2) + I(CSF_1^2) + I(Age^2) + I(Sex^2) + I(Smoker^2) + I(VAS_at_inclusion^2) +
                    I(IL_8^3) + I(VEGF_A^3) + I(OPG^3) + I(TGF_beta_1^3) + I(IL_6^3) + I(CXCL9^3) + I(CXCL1^3) + I(IL_18^3) + I(CSF_1^3) + I(Age^3) + I(Sex^3) + I(Smoker^3) + I(VAS_at_inclusion^3), data = train_data)

# Summary of the model
summary(cubic_model)


# List of variable names to transform to log values
predictor_vars <- c("IL_8", "VEGF_A", "OPG", "TGF_beta_1", "IL_6", "CXCL9", "CXCL1", "IL_18", "CSF_1", "Age", "Sex", "Smoker", "VAS_at_inclusion")

# Loop through each predictor variable
for (var in predictor_vars) {
  # Log transformation
  # Add a small positive value to the variable before taking the logarithm to deal with empty cells in the datast
  
  epsilon <- 1e-6  # small positive value
  train_data[[paste0(var, "_log")]] <- log(train_data[[var]] + epsilon)
}


# List of variable names to transform to logarithm values
vars_log <- c("IL_8_log", "VEGF_A_log", "OPG_log", "TGF_beta_1_log", "IL_6_log", "CXCL9_log", "CXCL1_log", "IL_18_log", "CSF_1_log", "Age_log", "Sex_log", "Smoker_log", "VAS_at_inclusion_log", "Vas_12_months")
train_data_log <- train_data[, vars_log]

# Fit the regression model using transformed predictor variables
log_model <- lm(Vas_12_months ~ IL_8_log + VEGF_A_log + OPG_log + TGF_beta_1_log + IL_6_log + CXCL9_log + CXCL1_log + IL_18_log + CSF_1_log + Age_log + Sex_log + Smoker_log + VAS_at_inclusion_log, data = train_data_log)

# Print the summary of the model
summary(log_model)
