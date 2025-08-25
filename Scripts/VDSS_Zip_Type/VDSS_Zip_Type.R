# Author: Dylan Craig
# Data Updated: 12/29/2024
# File Name: VDSS_Zip_Type.R

# Description: This script merges USPS ZIP-County data with VDSS ZIP data types. 
# It identifies ZIP codes that failed to merge and exports both the merged dataset and the failed merge data.

# --- Load Required Libraries ------------------------------------------------
library(readxl)    # For reading Excel files
library(dplyr)     # For data manipulation
library(openxlsx)  # For writing Excel files

# --- Load Data --------------------------------------------------------------

# Define file paths for input datasets
usps_path <- "Raw Data/VDSS_Zip_Type/USPS_Zip_County.xlsx"
vdss_path <- "Raw Data/VDSS_Zip_Type/VDSS_ZIPDATAMAPS_TYPE.xlsx"

# Read in the datasets
usps_zip_county <- read_excel(usps_path)
vdss_zipdata_type <- read_excel(vdss_path)

# --- Merge Data -------------------------------------------------------------

# Perform a full join on the ZIP column to merge the datasets
merged_data <- full_join(usps_zip_county, vdss_zipdata_type, by = "ZIP")

# Identify ZIP codes that failed to merge (have missing values in key columns)
failed_merge <- merged_data %>%
  filter(is.na(ZIP) | is.na(TYPE))  # Assuming 'TYPE' is a column in vdss_zipdata_type

# --- Save Outputs -----------------------------------------------------------

# Define output paths
merged_output_path <- "Data Outputs/VDSS_Zip_Type/VDSS_Zip_Type.xlsx"
failed_merge_output_path <- "Data Outputs/VDSS_Zip_Type/VDSS_Zip_Type_No_Type.xlsx"

# Save the merged dataset
write.xlsx(merged_data, merged_output_path, row.names = FALSE)
cat("Merged data saved to:", merged_output_path, "\n")

# Save the failed merge dataset
write.xlsx(failed_merge, failed_merge_output_path, row.names = FALSE)
cat("Failed merge data saved to:", failed_merge_output_path, "\n")
