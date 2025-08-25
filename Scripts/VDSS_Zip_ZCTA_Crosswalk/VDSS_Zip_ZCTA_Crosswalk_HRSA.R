# Author: Dylan Craig
# Data Updated: 12/29/2024
# File Name: VDSS_Zip_ZCTA_Crosswalk_HRSA.R

# Description: This script processes HRSA ZCTA data, filtering for Virginia ZIP codes,
# selecting necessary columns, and renaming them for clarity. The cleaned data is saved to an output file.

# --- Load Required Libraries ---
library(readxl)    # For reading Excel files
library(dplyr)     # For data manipulation
library(openxlsx)  # For saving Excel files

# --- Define File Paths ---
# Input file containing HRSA ZCTA data
input_file <- "Raw Data/VDSS_Zip_ZCTA_Crosswalk/HRSA_ZCTA.xlsx"

# Output file for saving cleaned data
output_file <- "Data Outputs/VDSS_Zip_ZCTA_Crosswalk/HRSA_ZCTA.xlsx"

# --- Load the Data ---
# Read the raw HRSA ZCTA data from the input file
zcta_data <- read_excel(input_file)

# --- Clean the Data ---
# Filter for Virginia (STATE == "VA"), select only relevant columns, and rename them
cleaned_data <- zcta_data %>%
  filter(STATE == "VA") %>%   # Keep only records where the state is Virginia
  select(ZIP_CODE, zcta) %>%  # Select columns for ZIP codes and ZCTAs
  rename(ZIP = ZIP_CODE,      # Rename ZIP_CODE to ZIP for consistency
         ZCTA = zcta)         # Rename zcta to ZCTA for clarity

# --- Save the Cleaned Data ---
# Save the cleaned dataset to the specified output file
write.xlsx(cleaned_data, output_file)

# Notify the user
cat("Cleaned ZCTA data saved to:", output_file, "\n")
