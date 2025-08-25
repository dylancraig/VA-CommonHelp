# Author: Dylan Craig ---------------------------------------------------------

# Date Last Updated: 1/17/2024 ----------------------------------------------

# File Name: USPS_Unique_Zip_Counties

# Purpose: Create a total dataset of all unique ZIP-county combinations and save
# the final output to a specified location.

# --- Load Necessary Libraries ---
library(readxl)      # For reading Excel files
library(dplyr)       # For data manipulation
library(stringr)     # For string operations
library(writexl)     # For saving Excel files

# --- Step 1: Processing ZIP-County Data ---

# Set the folder path and list all relevant files
folder_path <- here::here("Raw Data/VDSS_Zip_Office_Distances/ZIP_COUNTY")
file_list <- list.files(path = folder_path, pattern = "ZIP_COUNTY_\\d{6}\\.xlsx", full.names = TRUE)

# Function to read and process each file
process_file <- function(file_path) {
  # Extract the date from the file name
  file_name <- basename(file_path)
  date_str <- str_extract(file_name, "\\d{6}")
  
  # Convert to date format MM/01/YYYY
  month_year <- paste0(substr(date_str, 1, 2), "/01/20", substr(date_str, 5, 6))
  
  # Read the file and convert column names to uppercase
  data <- read_excel(file_path)
  colnames(data) <- toupper(colnames(data))
  
  # Add the date column and filter for Virginia counties
  data <- data %>%
    mutate(MONTH_YEAR = as.Date(month_year, format = "%m/%d/%Y")) %>%
    filter(str_starts(COUNTY, "51"))  # Virginia counties have FIPS starting with '51'
  
  return(data)
}

# Apply the function to all files and combine them
data <- purrr::map_dfr(file_list, process_file)

# Remove unnecessary columns
if (any(grepl("\\.\\.\\.", colnames(data)))) {
  data <- data %>% select(-starts_with("..."))
}

# Generate unique ZIP-COUNTY combinations
unique_zip_county <- data %>%
  select(ZIP, COUNTY) %>%
  distinct()

# --- Step 2: Save Final Output ---

# Specify output file path
output_path <- here::here("Data Outputs/VDSS_Zip_Office_Distances/USPS_Unique_Zip_Counties.xlsx")

# Save the unique ZIP-COUNTY combinations to an Excel file
write_xlsx(unique_zip_county, output_path)

# Confirmation message
cat("Unique ZIP-COUNTY combinations saved to:", output_path)
