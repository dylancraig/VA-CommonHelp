# Author: Dylan Craig
# Data Updated: 12/29/2024
# File Name: VDSS_Zip_Residential_Status.R

# Description: This script merges USPS ZIP-County data with IRS ZIP code data and determines 
# the residential status of ZIP codes based on population values. The output is saved as an Excel file.

# --- Load Required Libraries ------------------------------------------------
library(dplyr)     # For data manipulation
library(readxl)    # For reading Excel files
library(writexl)   # For writing Excel files

# --- Load Data --------------------------------------------------------------

# Define file paths for input datasets
usps_path <- "Raw Data/VDSS_Zip_Residential_Status/USPS_Zip_County.xlsx"
irs_path <- "Raw Data/VDSS_Zip_Residential_Status/zip_code_IRS_2020.xls"

# Load the USPS ZIP-County Excel file
usps_zip_county <- read_excel(usps_path)

# Load the IRS ZIP code data
zip_code_irs <- read_excel(irs_path)

# --- Prepare Data -----------------------------------------------------------

# Drop the FIPS column from USPS data and retain unique ZIP codes
usps_zip_county <- usps_zip_county %>%
  select(-FIPS) %>%
  distinct(ZIP)

# Merge the two datasets on ZIP code
merged_data <- left_join(usps_zip_county, zip_code_irs, by = "ZIP")

# --- Assign Residential Status ----------------------------------------------

# Determine residential status based on the POPULATION column
merged_data <- merged_data %>%
  mutate(RESIDENTIAL_STATUS = case_when(
    POPULATION > 0 ~ "RESIDENTIAL",         # Residential if population > 0
    POPULATION == 0 ~ "NON_RESIDENTIAL",    # Non-residential if population is 0
    is.na(POPULATION) ~ "UNKNOWN"           # Unknown if population is missing
  ))

# --- Save Output ------------------------------------------------------------

# Define the output file path
output_path <- "Data Outputs/VDSS_Zip_Residential_Status/Zip_Residential_Status.xlsx"

# Save the final dataset as an Excel file
write_xlsx(merged_data, output_path)

# Notify the user of successful file saving
cat("Final dataset with residential status saved to:", output_path, "\n")
