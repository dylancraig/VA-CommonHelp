# Script Name: VDSS_Zip_Border_Population_Filtered.R -------------------------
# Author: Dylan Craig -------------------------------------------------------
# Date Last Updated: 12/29/2024 ---------------------------------------------
# Purpose: Process ZIP code, FIPS, and bordering county data for Virginia, 
# filtering and transforming the data into a structured format. Outputs a 
# filtered dataset with relevant FIPS and bordering FIPS codes to Excel.

# Load Necessary Libraries ---------------------------------------------------
library(readxl)       # For reading Excel files
library(dplyr)        # For data manipulation
library(tidyr)        # For data reshaping
library(openxlsx)     # For saving data to Excel files
library(purrr)        # For functional programming

# 1. Load Input Data ---------------------------------------------------------
# Read in the USPS ZIP-to-county data
zip_code_list <- read_excel("Raw Data/VDSS_Zip_Border_Status/usps_zip_county.xlsx")

# Read in the Virginia bordering counties data with FIPS codes
virginia_bordering_counties <- read_excel("Data Outputs/VDSS_Zip_Border_Status/virginia_bordering_counties_fips.xlsx")

# Read in the ZIP-to-FIPS ratios data
zip_fips_ratios <- read_excel("Raw Data/VDSS_Zip_Border_Status/ZIP_COUNTY_122012.xlsx")

# 2. Filter Input Data -------------------------------------------------------
# Filter the ZIP code list to only include Virginia FIPS codes (prefix 51)
zip_code_list <- zip_code_list %>%
  filter(grepl("^51", FIPS))

# Filter ZIP-to-FIPS ratios to include only pairs with TOT_RATIO >= 0.01
zip_fips_ratios <- zip_fips_ratios %>%
  filter(TOT_RATIO >= 0.01)

# Convert Virginia bordering counties data to long format
virginia_bordering_counties_long <- virginia_bordering_counties %>%
  pivot_longer(cols = starts_with("Border_"), 
               names_to = "Border", 
               values_to = "BORDER_FIPS") %>%
  filter(!is.na(BORDER_FIPS)) %>%
  select(County, BORDER_FIPS)

# 3. Process Each ZIP Code ---------------------------------------------------
# Create a data frame to store the final results
final_df <- data.frame()

# Loop through each unique ZIP code in the filtered ZIP code list
for (zip in unique(zip_code_list$ZIP)) {
  # Get associated FIPS codes for the ZIP code from the filtered ratios
  associated_fips <- zip_fips_ratios %>%
    filter(ZIP == zip) %>%
    select(FIPS) %>%
    unlist()
  
  # Initialize lists to store FIPS and bordering FIPS codes
  fips_columns <- list()
  border_fips_columns <- list()
  
  # Loop through each associated FIPS code
  for (fips in associated_fips) {
    # Add the current FIPS code to the list
    fips_columns <- append(fips_columns, fips)
    
    # Get bordering FIPS codes, excluding the current FIPS code
    border_fips <- virginia_bordering_counties_long %>%
      filter(County == fips) %>%
      select(BORDER_FIPS) %>%
      unlist()
    border_fips <- setdiff(border_fips, associated_fips)
    
    # Add the bordering FIPS codes to the list
    border_fips_columns <- append(border_fips_columns, border_fips)
  }
  
  # Remove duplicates from the bordering FIPS codes list
  border_fips_columns <- unique(unlist(border_fips_columns))
  
  # Create a data frame for the current ZIP code
  zip_df <- data.frame(
    ZIP = zip,
    FIPS = I(list(fips_columns)),
    BORDER_FIPS = I(list(border_fips_columns))
  )
  
  # Append the data frame to the final results
  final_df <- bind_rows(final_df, zip_df)
}

# 4. Transform and Expand Columns --------------------------------------------
# Function to safely set names on a list element with a prefix
safe_set_names <- function(x, prefix) {
  if (length(x) > 0) {
    setNames(as.list(x), paste0(prefix, seq_along(x)))
  } else {
    setNames(list(NA), paste0(prefix, "_1"))
  }
}

# Split the FIPS lists into separate columns
final_df <- final_df %>%
  mutate(FIPS = purrr::map(FIPS, ~safe_set_names(.x, "FIPS"))) %>%
  unnest_wider(FIPS)

# Split the BORDER_FIPS lists into separate columns, handling empty lists
final_df <- final_df %>%
  mutate(BORDER_FIPS = purrr::map(BORDER_FIPS, ~safe_set_names(.x, "BORDER_FIPS"))) %>%
  unnest_wider(BORDER_FIPS)

# Remove FIPS_1 and BORDER_FIPS_1 columns for cleanup
final_df <- final_df %>%
  select(-FIPS_1, -BORDER_FIPS_1)

# 5. Save the Final Data Frame -----------------------------------------------
# Save the processed data to an Excel file
write.xlsx(final_df, 
           "Data Outputs/VDSS_Zip_Border_Status/VDSS_Zip_FIPS_Border_Population_Filtered.xlsx", 
           rowNames = FALSE)

# End of Script --------------------------------------------------------------
