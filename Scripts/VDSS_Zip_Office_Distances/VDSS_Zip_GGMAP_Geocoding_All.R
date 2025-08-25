# Author: Dylan Craig
# Data Updated: 12/29/2024
# File Name: VDSS_Zip_GGMAP_Geocoding_All.R

# Description: This script geocodes unique ZIP codes from a merged dataset of VDSS offices 
# and USPS ZIP-FIPS mappings. The resulting dataset includes latitude and longitude for each ZIP code 
# and is saved as an Excel file.

# --- Load Required Libraries ------------------------------------------------
library(ggmap)      # For geocoding
library(tidyverse)  # For data manipulation and visualization
library(readxl)     # For reading Excel files
library(writexl)    # For writing Excel files

# --- Register Google API Key ------------------------------------------------
# Ensure the Google Maps API key has permissions for geocoding services
register_google(key = "")

# --- Load and Prepare Data --------------------------------------------------

# Define file paths for input datasets
vdss_path <- "Raw Data/VDSS_Zip_Office_Distances/VDSS_Offices_All.xlsx"
usps_path <- "Raw Data/VDSS_Zip_Office_Distances/USPS_Zip_County.xlsx"

# Load VDSS offices (all years) and USPS ZIP-FIPS mapping
VDSS_All <- read_excel(vdss_path)
USPS_ZIP_FIPS <- read_excel(usps_path)

# Perform a left join using the FIPS column to merge VDSS and USPS data
# Assumes 'FIPS' exists in both datasets
MERGED_DATA <- left_join(VDSS_All, USPS_ZIP_FIPS, by = "FIPS")

# --- Geocode Unique ZIP Codes -----------------------------------------------

# Extract unique ZIP codes and remove NA values
ZIP_CODES <- MERGED_DATA$ZIP %>%
  unique() %>%
  na.omit() %>%
  as.character()

# Perform geocoding on the unique ZIP codes
GEOCODE_RESULTS <- geocode(ZIP_CODES)

# Combine ZIP codes with their corresponding coordinates
GEOCODE_RESULTS <- cbind(ZIP = ZIP_CODES, GEOCODE_RESULTS)

# Rename the latitude and longitude columns for clarity
GEOCODE_RESULTS <- GEOCODE_RESULTS %>%
  rename(ZIP_MID_LAT = lat, ZIP_MID_LONG = lon)

# --- Merge Geocoding Results ------------------------------------------------

# Ensure ZIP columns in both datasets are of character type
MERGED_DATA <- MERGED_DATA %>%
  mutate(ZIP = as.character(ZIP))

GEOCODE_RESULTS <- GEOCODE_RESULTS %>%
  mutate(ZIP = as.character(ZIP))

# Merge geocoding results with the original merged data
MERGED_DATA <- left_join(MERGED_DATA, GEOCODE_RESULTS, by = "ZIP")

# --- Save the Final Dataset -------------------------------------------------

# Define the output file path
output_path <- "Data Outputs/VDSS_Zip_Office_Distances/VDSS_Office_Zip_GeoCode_All.xlsx"

# Save the updated dataset as an Excel file
write_xlsx(MERGED_DATA, output_path)

# Notify the user of successful file saving
cat("Geocoded dataset saved to:", output_path, "\n")