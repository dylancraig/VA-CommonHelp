# Author: Dylan Craig ---------------------------------------------------------
# Date Last Updated: 12/28/2024 ---------------------------------------------
# Purpose: Process and aggregate ACS data from multiple sources into a unified dataset ---
# File Name: VDSS_ZCTA_ACS_Characteristics.R

# Load Necessary Libraries ---------------------------------------------------
library(dplyr)
library(readr)
library(stringr)
library(writexl)
library(tidyverse)

# Function to Read, Combine, and Rename Columns with Prefix ------------------
read_combine_and_rename <- function(file_path, prefix) {
  files <- list.files(file_path, full.names = TRUE, pattern = "\\.csv$")
  
  # Read and process each file
  data_list <- lapply(files, function(file) {
    data <- read_csv(file)
    data <- mutate_all(data, as.character)  # Ensure all columns are initially character
    return(data)
  })
  
  # Combine all datasets into one
  combined_data <- reduce(data_list, full_join)
  
  # Identify numeric columns and convert them
  numeric_columns <- combined_data %>%
    select(-c(YEAR, ZCTA)) %>%
    select(where(~ all(!is.na(as.numeric(.)), na.rm = TRUE))) %>%
    names()
  
  combined_data <- combined_data %>%
    mutate(across(all_of(numeric_columns), as.numeric))
  
  # Rename columns with the specified prefix
  new_colnames <- colnames(combined_data)
  new_colnames <- ifelse(new_colnames %in% c("YEAR", "ZCTA"), 
                         new_colnames, 
                         paste0(prefix, "_", new_colnames))
  colnames(combined_data) <- new_colnames
  
  return(combined_data)
}

# Define File Paths for Each Dataset -----------------------------------------
file_path_population <- "Raw Data/VDSS_ZCTA_ACS_Characteristics/B01003_Total_Population"
file_path_sex_by_age <- "Raw Data/VDSS_ZCTA_ACS_Characteristics/B01001_Sex_By_Age"
file_path_race_by_sex <- "Raw Data/VDSS_ZCTA_ACS_Characteristics/B02001_Race_By_Sex"
file_path_hispanic_latino <- "Raw Data/VDSS_ZCTA_ACS_Characteristics/B03002_Hispanic_Latino_By_Race"
file_path_poverty <- "Raw Data/VDSS_ZCTA_ACS_Characteristics/B17001_Poverty_Last_12_Months_By_Sex_By_Age"
file_path_income <- "Raw Data/VDSS_ZCTA_ACS_Characteristics/B19013_Median_Income"
file_path_education <- "Raw Data/VDSS_ZCTA_ACS_Characteristics/B15001_Sex_By_Age_By_Educational_Attainment_Over_18"
file_path_marital_status <- "Raw Data/VDSS_ZCTA_ACS_Characteristics/B12002_Marital_Status_By_Age_Over_15"
file_path_employment <- "Raw Data/VDSS_ZCTA_ACS_Characteristics/Selected_Economic_Characteristics"

# Define Output Folder -------------------------------------------------------
output_folder <- "Data Outputs/VDSS_ZCTA_ACS_Characteristics/"

# Function to Save Datasets as CSV and Excel ---------------------------------
save_data <- function(data, prefix) {
  csv_path <- paste0(output_folder, prefix, ".csv")
  excel_path <- paste0(output_folder, prefix, ".xlsx")
  write_csv(data, csv_path)
  write_xlsx(data, excel_path)
}

# Process and Save Individual Datasets ---------------------------------------
combined_population_data <- read_combine_and_rename(file_path_population, "POPULATION")
save_data(combined_population_data, "Population_Data")

combined_sex_by_age_data <- read_combine_and_rename(file_path_sex_by_age, "SEX")
save_data(combined_sex_by_age_data, "Sex_By_Age_Data")

combined_race_by_sex_data <- read_combine_and_rename(file_path_race_by_sex, "RACE")
save_data(combined_race_by_sex_data, "Race_By_Sex_Data")

combined_hispanic_latino_data <- read_combine_and_rename(file_path_hispanic_latino, "ETHNICITY")
save_data(combined_hispanic_latino_data, "Hispanic_Latino_By_Race_Data")

combined_poverty_data <- read_combine_and_rename(file_path_poverty, "POVERTY")
save_data(combined_poverty_data, "Poverty_Status_Data")

combined_income_data <- read_combine_and_rename(file_path_income, "INCOME")
save_data(combined_income_data, "Median_Income_Data")

combined_education_data <- read_combine_and_rename(file_path_education, "EDUCATION")
save_data(combined_education_data, "Education_Attainment_Data")

combined_marital_status_data <- read_combine_and_rename(file_path_marital_status, "MARITAL_STATUS")
save_data(combined_marital_status_data, "Marital_Status_Data")

combined_employment_data <- read_combine_and_rename(file_path_employment, "EMPLOYMENT")
save_data(combined_employment_data, "Employment_Status_Data")

# Merge All Combined Dataframes by ZCTA and YEAR -----------------------------
final_combined_data <- reduce(list(combined_population_data, combined_sex_by_age_data,
                                   combined_race_by_sex_data, combined_hispanic_latino_data,
                                   combined_poverty_data, combined_income_data,
                                   combined_education_data, combined_marital_status_data,
                                   combined_employment_data),
                              full_join, by = c("ZCTA", "YEAR"))

# Save the Final Combined Dataset --------------------------------------------
save_data(final_combined_data, "VDSS_ZCTA_ACS_Characteristics_Aggregated")
