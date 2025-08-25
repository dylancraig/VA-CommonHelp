# Author: Dylan Craig ---------------------------------------------------------
# Date Last Updated: 12/28/2024 ---------------------------------------------
# File Name: VDSS_HUD_USPS_2011_2012_Type_Comparison.R

# Purpose: Analyze ZIP data for Q4 2011 and Q1 2012, focusing on TYPE and NUMBER_TYPES distributions.

# --- Load Required Libraries ---
library(readxl)      # For reading Excel files
library(dplyr)       # For data manipulation
library(writexl)     # For exporting data to Excel

# --- Load Collapsed ZIP Data ---
# Read the dataset containing historical ZIP data
collapsed_data <- read_excel("Data Outputs/VDSS_Historical_Zip_Type_Changes/VA_HUD_USPS_Zip_Data_Collapsed.xlsx")

# --- Filter the Data for Specific YEAR_QUARTER Values ---
# Keep only the rows for Q4 2011 and Q1 2012
filtered_data <- collapsed_data %>%
  filter(YEAR_QUARTER %in% c("Q4 2011", "Q1 2012"))

# --- Check TYPE and NUMBER_TYPES Distributions ---
# Summarize the count of ZIP codes by YEAR_QUARTER and TYPE
type_distribution <- filtered_data %>%
  group_by(YEAR_QUARTER, TYPE) %>%
  summarize(Count = n(), .groups = 'drop')

# Summarize the count of ZIP codes by YEAR_QUARTER and NUMBER_TYPES
number_types_distribution <- filtered_data %>%
  group_by(YEAR_QUARTER, NUMBER_TYPES) %>%
  summarize(Count = n(), .groups = 'drop')

# --- Create a List of Data Frames for Export ---
# Combine TYPE and NUMBER_TYPES distributions into a list for export
export_list <- list(
  "TYPE Distribution" = type_distribution,
  "NUMBER_TYPES Distribution" = number_types_distribution
)

# --- Define File Path for the Output Excel File ---
output_file_path <- "Data Outputs/VDSS_Historical_Zip_Type_Changes/VA_HUD_USPS_2011_2012_TYPE_COMPARISON.xlsx"

# --- Export the Data Frames to Excel ---
write_xlsx(export_list, output_file_path)

# --- Print Confirmation Message ---
print(paste("File saved as:", output_file_path))
