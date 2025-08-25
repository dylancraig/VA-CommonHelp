# Author: Dylan Craig ---------------------------------------------------------
# Date Last Updated: 1/19/2025 ----------------------------------------------
# File Name: VDSS_Final_Data.R

# Purpose: Process and integrate multiple datasets to create a final dataset 
# with ZIP classification, ZCTA crosswalk, bad_zipcounty, office locations/distances. 

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

# --- Step 7: Integrating Office Locations and Distances ---

# --- Step 8: Integrating Office Locations and Distances ---
# Import 2012 distances data
data_2012 <- read_excel("Data Outputs/VDSS_Zip_Office_Distances/VDSS_Office_Zip_GeoCode_2012_Distances.xlsx") %>%
  mutate(ZIP_VDSS_clean = str_remove(ZIP_VDSS, "-\\d{4}$"),  # Remove ZIP suffix
         ZIP = as.character(ZIP),  # Convert ZIP to character
         COUNTY = as.character(COUNTY))  # Convert COUNTY to character

# Merge with main dataset and clean
data <- data %>%
  left_join(data_2012, by = c("COUNTY" = "COUNTY", "ZIP" = "ZIP"))  # Merge on COUNTY and ZIP

# Calculate percentage of missing data
missing_percentage <- data %>%
  summarise(across(everything(), ~ mean(is.na(.)) * 100)) %>%
  pivot_longer(cols = everything(), names_to = "variable", values_to = "percent_missing")  # Reshape for readability

# Find rows where "HAVERSINE_DISTANCE_METERS" is NA and unique ZIP counts
missing_haversine <- data %>%
  filter(is.na(HAVERSINE_DISTANCE_METERS))

zip_counts <- missing_haversine %>%
  group_by(ZIP) %>%
  summarise(count = n())

# --- Step 8: Preparing the Final Dataset ---

# --- Select and Reorder Variables ---
data <- data %>%
  select(
    # General information
    ZIP, COUNTY, MONTH_YEAR, RES_RATIO, BUS_RATIO, OTH_RATIO, TOT_RATIO, RESIDENTIAL, TYPE, ZCTA, 
    bad_zipcounty, N, REGIONAL_OFFICE, LOCALITY, DATE_CHANGE, DATE_START, DATE_END, STREET, PO_BOX, 
    CITY_COUNTY_TOWN, STATE, ZIP_VDSS, VDSS_COORDS, ZIP_COORDS, 
    
    # Distance and time variables
    HAVERSINE_DISTANCE_METERS, 
    DRIVING_DISTANCE_MILES, 
    DRIVING_TIME_NONE_MINUTES, 
    DRIVING_TIME_OPTIMISTIC_MINUTES, 
    DRIVING_TIME_PESSIMISTIC_MINUTES, 
    DRIVING_TIME_BEST_GUESS_MINUTES, 
    TRANSIT_DISTANCE_MILES, 
    TRANSIT_TIME_MINUTES
  )


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
output_path <- "Data Outputs/VDSS_Final_Data/Final_Data.xlsx"
write_xlsx(list(
  "Final_Data_With_ZCTA_and_Treat" = data,
  "Summary_Statistics" = summary_statistics,
  "Unique_ZIP_Counts" = unique_zip_counts,
  "Unique_ZIP_County_Counts" = unique_zip_county_counts
), path = output_path)

# Notify the user
cat("Final dataset and summary statistics saved to:", output_path, "\n")
