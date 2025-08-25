# Author: Dylan Craig ---------------------------------------------------------
# Date Last Updated: 12/28/2024 ---------------------------------------------
# File Name: VDSS_HUD_USPS_Historical_Zip_Type_Missing_Analysis.R

# Purpose: Analyze ZIP presence across year-quarters and evaluate missing statuses,
# including breakdowns by NUMBER_TYPES and TYPE.

# --- Load Necessary Libraries ---
library(readxl)      # For reading Excel files
library(dplyr)       # For data manipulation
library(writexl)     # For saving data to Excel

# --- Load the Dataset ---
# Read the historical ZIP data containing year-quarter information
zip_data <- read_excel("Data Outputs/VDSS_Historical_Zip_Type_Changes/VA_HUD_USPS_Zip_Data_Collapsed.xlsx")

# --- Identify All Unique YEAR_QUARTER Values ---
# Extract all unique year-quarter combinations for analysis
all_year_quarters <- unique(zip_data$YEAR_QUARTER)

# --- Check for ZIP Codes Missing in Any YEAR_QUARTER ---
# Determine whether each ZIP is PRESENT or MISSING across all year-quarters
zip_status <- zip_data %>%
  group_by(ZIP) %>%
  summarise(
    MISSING = if_else(all(all_year_quarters %in% YEAR_QUARTER), "PRESENT", "MISSING"),
    TYPE = first(TYPE),  # Take the first available TYPE value
    NUMBER_TYPES = first(NUMBER_TYPES)  # Take the first available NUMBER_TYPES value
  )

# --- Analysis 1: Percentages of ZIP Codes That Are MISSING or PRESENT ---
# Calculate the percentage of ZIP codes that are PRESENT or MISSING
percent_missing_present <- zip_status %>%
  group_by(MISSING) %>%
  summarise(Count = n()) %>%
  mutate(Percent = (Count / sum(Count)) * 100)

# --- Analysis 2: MULTIPLE vs SINGLE Types for MISSING or PRESENT ---
# Analyze NUMBER_TYPES (SINGLE vs MULTIPLE) by MISSING or PRESENT status
missing_analysis_by_number_types <- zip_status %>%
  group_by(NUMBER_TYPES, MISSING) %>%
  summarise(Count = n()) %>%
  mutate(Percent = (Count / sum(Count)) * 100)

# --- Analysis 3: SINGLE Type ZIP Codes by TYPE and MISSING Status ---
# Focus on SINGLE type ZIP codes and analyze by TYPE and MISSING status
missing_type_analysis_single <- zip_status %>%
  filter(NUMBER_TYPES == "SINGLE") %>%
  group_by(TYPE, MISSING) %>%
  summarise(Count = n()) %>%
  mutate(Percent = (Count / sum(Count)) * 100)

# --- Save All Results ---
# Save the ZIP status and analysis results to Excel files
write_xlsx(zip_status, "Data Outputs/VDSS_Historical_Zip_Type_Changes/VA_HUD_USPS_Zip_Status.xlsx")
write_xlsx(percent_missing_present, "Data Outputs/VDSS_Historical_Zip_Type_Changes/VA_HUD_USPS_Zip_Percent_Missing_Present.xlsx")
write_xlsx(missing_analysis_by_number_types, "Data Outputs/VDSS_Historical_Zip_Type_Changes/VA_HUD_USPS_Missing_Analysis_By_Number_Types.xlsx")
write_xlsx(missing_type_analysis_single, "Data Outputs/VDSS_Historical_Zip_Type_Changes/VA_HUD_USPS_Missing_Type_Analysis_Single.xlsx")

# --- Print Confirmation Messages ---
print("ZIP status data saved to: VA_HUD_USPS_Zip_Status.xlsx")
print("Percentage analysis saved to: VA_HUD_USPS_Zip_Percent_Missing_Present.xlsx")
print("Missing analysis by NUMBER_TYPES saved to: VA_HUD_USPS_Missing_Analysis_By_Number_Types.xlsx")
print("Type analysis for SINGLE ZIPs saved to: VA_HUD_USPS_Missing_Type_Analysis_Single.xlsx")
