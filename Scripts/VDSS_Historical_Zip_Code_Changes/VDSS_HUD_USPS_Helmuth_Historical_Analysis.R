# Author: Dylan Craig ---------------------------------------------------------
# Date Last Updated: 12/28/2024 ---------------------------------------------
# Purpose: Analyzing Historical ZIP Type Changes for Bailey Helmuth Dataset ---

# --- Load Necessary Libraries ---
library(readxl)    # For reading Excel files
library(dplyr)     # For data manipulation
library(writexl)   # For writing Excel files

# --- Load the Dataset ---
# Read the dataset containing ZIP type and historical changes
zip_data <- read_excel("Data Outputs/VDSS_Historical_Zip_Type_Changes/VA_HUD_USPS_Zip_Data_Collapsed.xlsx")

# --- Identify All Unique YEAR_QUARTER Values ---
# Extract all unique year-quarter combinations from the dataset
all_year_quarters <- unique(zip_data$YEAR_QUARTER)

# --- Create a DataFrame for Bailey Helmuth ZIPs ---
# Define the OLD and NEW ZIP codes for analysis
Bailey_Helmuth_Zips <- data.frame(
  OLD_ZIP_CODE = c("23459", "23521", "23521", "22721", "23101"),
  NEW_ZIP_CODE = c("23451", "23439", "23455", "22727", "23139"),
  stringsAsFactors = FALSE
)

# --- Function to Analyze Each ZIP Code ---
# This function summarizes ZIP code characteristics:
# - NUMBER_TYPES: Whether the ZIP had multiple or single types.
# - TYPE: List of types associated with the ZIP code.
# - MISSING: Whether all year-quarters are present in the dataset.
# - POST_2010: Whether any data exists after Q1 2010.
analyze_zip <- function(zip_code, zip_data, all_year_quarters) {
  zip_info <- zip_data %>%
    filter(ZIP == zip_code) %>%
    summarise(
      NUMBER_TYPES = if_else(n_distinct(TYPE) > 1, "MULTIPLE", "SINGLE"),
      TYPE = paste(unique(TYPE), collapse = ", "),
      MISSING = if_else(all(all_year_quarters %in% YEAR_QUARTER), "PRESENT", "MISSING"),
      POST_2010 = if_else(any(YEAR_QUARTER > "Q1 2010"), "yes", "no")
    )
  
  # Handle cases where no data exists for the ZIP code
  if (nrow(zip_info) == 0) {
    zip_info <- data.frame(NUMBER_TYPES = NA, TYPE = NA, MISSING = NA, POST_2010 = NA)
  }
  
  return(zip_info)
}

# --- Apply the Analysis to OLD_ZIP_CODE and NEW_ZIP_CODE ---
# Analyze OLD ZIP codes
old_zip_info <- do.call(rbind, lapply(Bailey_Helmuth_Zips$OLD_ZIP_CODE, analyze_zip, zip_data = zip_data, all_year_quarters = all_year_quarters))

# Analyze NEW ZIP codes
new_zip_info <- do.call(rbind, lapply(Bailey_Helmuth_Zips$NEW_ZIP_CODE, analyze_zip, zip_data = zip_data, all_year_quarters = all_year_quarters))

# --- Combine the Analysis with the Original DataFrame ---
# Add OLD ZIP analysis results
Bailey_Helmuth_Zips <- Bailey_Helmuth_Zips %>%
  bind_cols(old_zip_info %>% rename(OLD_NUMBER_TYPES = NUMBER_TYPES, OLD_TYPE = TYPE, OLD_STATUS = MISSING, OLD_POST_2010 = POST_2010)) %>%
  # Add NEW ZIP analysis results
  bind_cols(new_zip_info %>% rename(NEW_NUMBER_TYPES = NUMBER_TYPES, NEW_TYPE = TYPE, NEW_STATUS = MISSING, NEW_POST_2010 = POST_2010))

# --- Save the Bailey_Helmuth_Zips Analysis ---
# Save the analysis results to an Excel file
output_path <- "Data Outputs/VDSS_Historical_Zip_Code_Changes/Bailey_Helmuth_Zips_Analysis.xlsx"
write_xlsx(Bailey_Helmuth_Zips, output_path)

# Notify the user
cat("Bailey Helmuth ZIPs analysis saved to:", output_path, "\n")
