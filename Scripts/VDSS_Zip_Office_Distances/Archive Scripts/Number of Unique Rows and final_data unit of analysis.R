# Load required libraries
library(tidyverse)

# -------- Part 1: VDSS Final Data -------- #

# Set relative file path for final data
file_path_final <- "Data Outputs/VDSS_Final_Data/Final_Data.xlsx"

# Import final data
data_final <- read_excel(file_path_final)

# Identify duplicate rows by ZIP, COUNTY, MONTH_YEAR, zip_ldss, ldss_fips
duplicates_final <- data_final %>%
  group_by(ZIP, COUNTY, MONTH_YEAR, zip_ldss, ldss_fips) %>%  # Group by key variables
  filter(n() > 1) %>%  # Keep only rows that appear more than once within each group
  ungroup()

# Print summary of duplicate rows
cat("Number of duplicate rows in Final Data:", nrow(duplicates_final), "\n")

# Detailed explanation of units of observation:
# - Unit of observation: ZIP code, county FIPS code associated with that ZIP, month/year,
#   LDSS office ZIP, and LDSS FIPS code.
# - Example: ZIP 12345 is associated with Lynchburg City in March 2015 and is linked to 
#   the Lynchburg City LDSS office that serves Lynchburg City.

# -------- Part 2: VDSS Office Zip Distances -------- #

# Count unique ZIP-COUNTY combinations
unique_zip_county <- data_final %>%
  distinct(ZIP, COUNTY) %>%  # Get unique pairs of ZIP and COUNTY
  count()  # Count the number of unique pairs

# Print the result
cat("Number of unique ZIP-COUNTY combinations in Final Data:", unique_zip_county$n, "\n")

## Number of unique ZIP-COUNTY combinations in Final Data: 1715 

# Set relative file path for distances data
file_path_distances <- "Data Outputs/VDSS_Zip_Office_Distances/VDSS_Office_Zip_GeoCode_All_Distances.xlsx"

# Import distances data
data_distances <- read_excel(file_path_distances)

# Count unique FIPS-ZIP_MATCH combinations
unique_fips_zip_match <- data_distances %>%
  distinct(FIPS, ZIP_MATCH) %>%  # Get unique pairs of FIPS and ZIP_MATCH
  count()  # Count the number of unique pairs

# Print the result with context
cat("Number of unique FIPS-ZIP_MATCH combinations:", unique_fips_zip_match$n, "\n")

## Number of unique FIPS-ZIP_MATCH combinations: 1646

