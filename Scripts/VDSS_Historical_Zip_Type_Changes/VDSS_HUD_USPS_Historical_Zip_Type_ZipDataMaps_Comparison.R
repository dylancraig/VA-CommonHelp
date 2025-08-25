# Author: Dylan Craig ---------------------------------------------------------
# Date Last Updated: 12/28/2024 ---------------------------------------------
# File Name: VDSS_HUD_USPS_Historical_Zip_Type_ZipDataMaps_Comparison.R

# Purpose: Collapse VA HUD USPS ZIP data by ZIP and YEAR_QUARTER, analyze type classifications, 
# and compare with ZIP data from VDSS_Zip_Type.xlsx.

# --- Load Required Libraries ---
library(tidyverse)   # For data manipulation and analysis
library(pbapply)     # For applying functions with progress bars
library(writexl)     # For exporting Excel files

# --- Load the Combined VA HUD USPS ZIP Data ---
va_zip_data <- read_excel("Data Outputs/VDSS_Historical_Zip_Type_Changes/VA_HUD_USPS_Zip_Data_All_Years.xlsx")

# --- Collapse by ZIP and YEAR_QUARTER with Progress Bar ---
collapsed_zip_data <- pbapply::pblapply(split(va_zip_data, list(va_zip_data$ZIP, va_zip_data$YEAR_QUARTER)), function(group) {
  if (nrow(group) > 0) {  # Check if the group is not empty
    summarised_group <- group %>%
      summarise(across(c(RES_RATIO, BUS_RATIO, OTH_RATIO, TOT_RATIO), sum, na.rm = TRUE))
    
    summarised_group$ZIP <- unique(group$ZIP)
    summarised_group$YEAR_QUARTER <- unique(group$YEAR_QUARTER)
    return(summarised_group)
  }
  return(NULL)  # Return NULL if the group is empty
}) %>% bind_rows()

# --- Determine ZIP Code Type Based on Given Criteria ---
collapsed_zip_data <- collapsed_zip_data %>%
  mutate(TYPE = case_when(
    RES_RATIO > 0 ~ "Standard",
    RES_RATIO == 0 & OTH_RATIO == 0 & BUS_RATIO > 0 ~ "Unique",
    RES_RATIO == 0 & OTH_RATIO > 0 & BUS_RATIO == 0 ~ "PO Box",
    RES_RATIO == 0 & OTH_RATIO > 0 & BUS_RATIO > 0 ~ "Unique PO Box",
    TRUE ~ "UNKNOWN"
  ))

# --- Identify if a ZIP Code Has Multiple Types or a Single Type ---
collapsed_zip_data <- collapsed_zip_data %>%
  group_by(ZIP) %>%
  mutate(NUMBER_TYPES = if_else(n_distinct(TYPE) > 1, "MULTIPLE", "SINGLE")) %>%
  ungroup()

# --- Save the Collapsed ZIP Data ---
write_xlsx(collapsed_zip_data, "Data Outputs/VDSS_Historical_Zip_Type_Changes/VA_HUD_USPS_Zip_Data_Collapsed.xlsx")

# --- Create Separate DataFrames for SINGLE and MULTIPLE Types ---
collapsed_zip_data_single <- collapsed_zip_data %>%
  filter(NUMBER_TYPES == "SINGLE") %>%
  select(ZIP, TYPE, NUMBER_TYPES) %>%
  distinct()

collapsed_zip_data_multiple <- collapsed_zip_data %>%
  filter(NUMBER_TYPES == "MULTIPLE") %>%
  select(ZIP, TYPE, NUMBER_TYPES) %>%
  distinct()

# --- Adjust ZIP Type for UNIQUE PO BOX ---
collapsed_zip_data_single_unique <- collapsed_zip_data_single %>%
  mutate(TYPE = if_else(TYPE == "Unique PO Box", "Unique", TYPE))

collapsed_zip_data_single_pobox <- collapsed_zip_data_single %>%
  mutate(TYPE = if_else(TYPE == "Unique PO Box", "PO Box", TYPE))

# --- Load Additional ZIP Data Source (zipdata.com Equivalent) ---
zipdata_source <- read_excel("Data Outputs/VDSS_Zip_Type/VDSS_Zip_Type.xlsx")

# --- Convert ZIP Columns to Character for Join Compatibility ---
collapsed_zip_data_single_pobox$ZIP <- as.character(collapsed_zip_data_single_pobox$ZIP)
collapsed_zip_data_single_unique$ZIP <- as.character(collapsed_zip_data_single_unique$ZIP)
zipdata_source$ZIP <- as.character(zipdata_source$ZIP)

# --- Merge HUD USPS Data with ZIP Source for PO Box Conversion ---
comparison_data_pobox <- full_join(collapsed_zip_data_single_pobox, zipdata_source, by = "ZIP", suffix = c("_HUD", "_ZIPDATA"))

comparison_data_pobox <- comparison_data_pobox %>%
  mutate(COMPARISON = case_when(
    !is.na(TYPE_HUD) & TYPE_HUD == TYPE_ZIPDATA ~ "x",  # Correctly classified and exist in both
    !is.na(TYPE_HUD) & !is.na(TYPE_ZIPDATA) & TYPE_HUD != TYPE_ZIPDATA ~ "y",  # Misclassified but exist in both
    !is.na(TYPE_HUD) & is.na(TYPE_ZIPDATA) ~ "a",  # Exists in HUD USPS, missing in website
    is.na(TYPE_HUD) & !is.na(TYPE_ZIPDATA) ~ "b",  # Exists in website, missing in HUD USPS
    TRUE ~ "0"  # Catch-all for unexpected cases
  ))

# --- Summarize into a Table for PO Box Conversion ---
table_pobox <- comparison_data_pobox %>%
  group_by(TYPE_ZIPDATA, TYPE_HUD) %>%
  summarise(Count = n(), .groups = 'drop') %>%
  pivot_wider(names_from = TYPE_HUD, values_from = Count, values_fill = 0) %>%
  rename(`ZIPDATA.COM` = TYPE_ZIPDATA, 
         `Standard (HUD USPS)` = `Standard`, 
         `Unique (HUD USPS)` = `Unique`, 
         `PO Box (HUD USPS)` = `PO Box`)

# --- Save the Table for PO Box Conversion ---
write_xlsx(table_pobox, "Data Outputs/VDSS_Historical_Zip_Type_Changes/Comparison_Table_PO_Box.xlsx")

# --- Merge HUD USPS Data with ZIP Source for Unique Conversion ---
comparison_data_unique <- full_join(collapsed_zip_data_single_unique, zipdata_source, by = "ZIP", suffix = c("_HUD", "_ZIPDATA"))

comparison_data_unique <- comparison_data_unique %>%
  mutate(COMPARISON = case_when(
    !is.na(TYPE_HUD) & TYPE_HUD == TYPE_ZIPDATA ~ "x",  # Correctly classified and exist in both
    !is.na(TYPE_HUD) & !is.na(TYPE_ZIPDATA) & TYPE_HUD != TYPE_ZIPDATA ~ "y",  # Misclassified but exist in both
    !is.na(TYPE_HUD) & is.na(TYPE_ZIPDATA) ~ "a",  # Exists in HUD USPS, missing in website
    is.na(TYPE_HUD) & !is.na(TYPE_ZIPDATA) ~ "b",  # Exists in website, missing in HUD USPS
    TRUE ~ "0"  # Catch-all for unexpected cases
  ))

# --- Summarize into a Table for Unique Conversion ---
table_unique <- comparison_data_unique %>%
  group_by(TYPE_ZIPDATA, TYPE_HUD) %>%
  summarise(Count = n(), .groups = 'drop') %>%
  pivot_wider(names_from = TYPE_HUD, values_from = Count, values_fill = 0) %>%
  rename(`ZIPDATA.COM` = TYPE_ZIPDATA, 
         `Standard (HUD USPS)` = `Standard`, 
         `Unique (HUD USPS)` = `Unique`, 
         `PO Box (HUD USPS)` = `PO Box`)

# --- Save the Table for Unique Conversion ---
write_xlsx(table_unique, "Data Outputs/VDSS_Historical_Zip_Type_Changes/VA_HUD_USPS_ZipDataMaps_Comparison.xlsx")
