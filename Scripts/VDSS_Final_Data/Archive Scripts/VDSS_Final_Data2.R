# Author: Dylan Craig ---------------------------------------------------------
# Date Last Updated: 12/29/2024 ----------------------------------------------
# File Name: VDSS_Final_Data.R

# Purpose: Process and integrate multiple datasets to create a final dataset 
# with ZIP classification, ZCTA crosswalk, treatment, and quality data. 

# --- Load Necessary Libraries ---
library(readxl)      # For reading Excel files
library(dplyr)       # For data manipulation
library(haven)       # For reading .dta files
library(writexl)     # For saving Excel files
library(stringr)     # For string operations
library(lubridate)   # For date manipulations
library(tidyverse)   # For data wrangling and analysis

# --- Step 1: Processing ZIP-County Data ---
# Set the folder path and list all relevant files
folder_path <- "Raw Data/VDSS_Final_Data/ZIP_COUNTY"
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
data <- map_dfr(file_list, process_file)

# Remove any unnecessary Boolean columns (e.g., ...7)
data <- data %>% select(-starts_with("...7"))

# Generate unique ZIP-COUNTY combinations for each MONTH_YEAR
all_combinations <- data %>%
  select(ZIP, COUNTY) %>%
  distinct() %>%
  crossing(MONTH_YEAR = unique(data$MONTH_YEAR))

# Perform a left join to retain all valid combinations
data <- left_join(all_combinations, data, by = c("ZIP", "COUNTY", "MONTH_YEAR"))

# Sort by MONTH_YEAR for chronological clarity
data <- data %>%
  arrange(MONTH_YEAR)

# Unique combinations of ZIP and COUNTY
unique_zip_county <- data %>%
  select(ZIP, COUNTY) %>%
  distinct()

# --- Step 2: Removing Deleted ZIPs ---
# Load Bailey Helmuth ZIP changes data
helmuth_data <- read_excel("Raw Data/VDSS_Final_Data/Bailey_Helmuth_Zip_Changes.xlsx")
colnames(helmuth_data) <- toupper(colnames(helmuth_data))

# Ensure ZIP_DELETE is character and convert effective dates to Date type
helmuth_data <- helmuth_data %>%
  mutate(ZIP_DELETE = as.character(ZIP_DELETE),
         EFFECTIVE_DATE = as.Date(EFFECTIVE_DATE, format = "%m/%d/%Y"))

# Exclude ZIPs with effective deletion dates from the dataset
data <- data %>%
  left_join(helmuth_data, by = c("ZIP" = "ZIP_DELETE")) %>%
  filter(is.na(EFFECTIVE_DATE) | MONTH_YEAR < EFFECTIVE_DATE) %>%
  select(-EFFECTIVE_DATE)

# --- Step 3: Classifying ZIPs as Residential or Non-Residential ---
data <- data %>%
  group_by(ZIP) %>%
  mutate(RESIDENTIAL_FLAG = max(RES_RATIO > 0, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(RESIDENTIAL = ifelse(RESIDENTIAL_FLAG == 1, "RESIDENTIAL", "NON_RESIDENTIAL")) %>%
  select(-RESIDENTIAL_FLAG)

# --- Step 4: Classifying ZIPs as Standard, Unique, or PO Box ---
# Load ZIP type data and exclude FIPS
zip_type_data <- read_excel("Data Outputs/VDSS_Zip_Type/VDSS_Zip_Type.xlsx")
colnames(zip_type_data) <- toupper(colnames(zip_type_data))
zip_type_data <- zip_type_data %>%
  select(ZIP, TYPE) %>%
  mutate(ZIP = as.character(ZIP))

# Remove duplicate rows and keep only distinct rows
zip_type_data <- zip_type_data %>%
  distinct()

# Merge ZIP type information with the main dataset
data <- data %>%
  left_join(zip_type_data, by = "ZIP")

# --- Step 5: Adding ZCTA Crosswalk Data ---
zcta_data <- read_excel("Data Outputs/VDSS_Zip_ZCTA_Crosswalk/HRSA_ZCTA.xlsx")
colnames(zcta_data) <- toupper(colnames(zcta_data))
zcta_data <- zcta_data %>%
  select(ZIP, ZCTA)

# Merge ZCTA crosswalk data with the main dataset
data <- data %>%
  left_join(zcta_data, by = "ZIP")

# --- Step 6: Integrating Treatment and Quality Data ---
# Load bad_zipcounty_rate data
bad_zipcounty_rate <- read_dta("Raw Data/VDSS_Final_Data/bad_zipcounty_rate.dta") %>%
  rename(ZIP = zip) %>%
  mutate(ZIP = as.character(ZIP))

# Merge with the main dataset
data <- data %>%
  left_join(bad_zipcounty_rate, by = "ZIP")

# Load zip_treat data
zip_treat <- read_dta("Raw Data/VDSS_Final_Data/zip_treat.dta") %>%
  rename(ZIP = zip) %>%
  mutate(ZIP = as.character(ZIP))

# --- Function to Format `ldss_fips` ---
format_ldss_fips <- function(fips_code) {
  fips_code <- as.numeric(fips_code)  # Ensure numeric
  if (fips_code < 1000) {
    # Add "51" prefix and zero-pad to 5 digits
    formatted_fips <- sprintf("51%03d", fips_code)
  } else {
    # If 4 digits, assume "51" should be prepended (e.g., 840 becomes 51840)
    formatted_fips <- paste0("5", fips_code)
  }
  return(formatted_fips)
}

# --- Apply Formatting to `ldss_fips` ---
zip_treat <- zip_treat %>%
  mutate(ldss_fips = sapply(ldss_fips, format_ldss_fips))

# --- Merge with the Main Dataset ---
data <- data %>%
  left_join(zip_treat, by = c("ZIP", "COUNTY" = "ldss_fips"))

# Check for duplicate rows in the "data" dataframe
duplicate_rows <- data[duplicated(data), ]

# --- Step 7: Preparing the Final Dataset ---

# Generate summary statistics split by RESIDENTIAL status
summary_statistics <- data %>%
  group_by(RESIDENTIAL) %>%
  summarise(
    Total_ZIPs = n_distinct(ZIP),
    Total_ZIP_COUNTY_Combos = n_distinct(paste(ZIP, COUNTY, sep = "_"))
  )

# Calculate unique ZIP and ZIP-COUNTY combinations by MONTH_YEAR
unique_zip_counts <- data %>%
  group_by(MONTH_YEAR) %>%
  summarise(Unique_ZIPs = n_distinct(ZIP))

unique_zip_county_counts <- data %>%
  group_by(MONTH_YEAR) %>%
  summarise(Unique_ZIP_COUNTYs = n_distinct(paste(ZIP, COUNTY, sep = "_")))

# --- Save Results ---
output_path <- "Data Outputs/VDSS_Final_Data/Final_Data2.xlsx"
write_xlsx(list(
  "Final_Data_With_ZCTA_and_Treat" = data,
  "Summary_Statistics" = summary_statistics,
  "Unique_ZIP_Counts" = unique_zip_counts,
  "Unique_ZIP_County_Counts" = unique_zip_county_counts
), path = output_path)

# Notify the user
cat("Final dataset and summary statistics saved to:", output_path, "\n")

