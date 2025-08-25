# Author: Dylan Craig
# Data Updated: 12/29/2024
# File Name: VDSS_Zip_Border_Status_Type_Bad_Zipcounty_Rate.R

# Description: This script merges datasets related to ZIP types, border statuses, and bad ZIP-County rates.
# It calculates summary statistics and performs t-tests by BORDER_STATUS and TYPE, saving the results to an Excel file.

# --- Load Necessary Libraries ---
library(dplyr)      # For data manipulation
library(readxl)     # For reading Excel files
library(writexl)    # For saving Excel files
library(haven)      # For reading .dta files

# --- Load the Data ---
# Read the VDSS ZIP Type data
vdss_zip_type <- read_excel("Data Outputs/VDSS_Zip_Type/VDSS_Zip_Type.xlsx")

# Read the VDSS ZIP Border Status data
vdss_zip_border_status <- read_excel("Data Outputs/VDSS_Zip_Border_Status/VDSS_Zip_FIPS_Border_Status_Final_Designation.xlsx")

# --- Prepare ZIP Type Data ---
# Extract unique ZIP and TYPE values while removing FIPS and other duplicates
vdss_zip_type <- vdss_zip_type %>%
  select(ZIP, TYPE) %>%
  distinct()

# Convert ZIP columns to character format to ensure consistency across datasets
vdss_zip_type <- vdss_zip_type %>%
  mutate(ZIP = as.character(ZIP))

vdss_zip_border_status <- vdss_zip_border_status %>%
  mutate(ZIP = as.character(ZIP))

# Merge ZIP Type and Border Status data
merged_data <- left_join(vdss_zip_type, vdss_zip_border_status, by = "ZIP")

# --- Load Bad ZIP-County Rate Data ---
bad_zipcounty_rate <- read_dta("Raw Data/VDSS_Zip_Border_Status_Type_Bad_ZipCounty_Rate/bad_zipcounty_rate.dta")

# Convert ZIP column to character format and rename for consistency
bad_zipcounty_rate <- bad_zipcounty_rate %>%
  mutate(zip = as.character(zip)) %>%
  rename(ZIP = zip)

# Merge with existing merged data
merged_data <- left_join(merged_data, bad_zipcounty_rate, by = "ZIP")

# --- Add Weighted Bad ZIP-County Rate ---
# Calculate weighted bad ZIP-County rate using bad_zipcounty and N
merged_data <- merged_data %>%
  mutate(weighted_bad_zipcounty = bad_zipcounty * N)

# --- Functions for T-Tests ---
# Perform pairwise t-tests for BORDER_STATUS groups
perform_t_test <- function(df, var, group1, group2) {
  data1 <- df %>% filter(BORDER_STATUS == group1) %>% pull(var)
  data2 <- df %>% filter(BORDER_STATUS == group2) %>% pull(var)
  if (length(data1) > 1 & length(data2) > 1) {
    t.test(data1, data2)
  } else {
    return(NULL)
  }
}

# Perform pairwise t-tests for TYPE groups
perform_t_test_status <- function(df, var, status1, status2) {
  data1 <- df %>% filter(TYPE == status1) %>% pull(var)
  data2 <- df %>% filter(TYPE == status2) %>% pull(var)
  if (length(data1) > 1 & length(data2) > 1) {
    t.test(data1, data2)
  } else {
    return(NULL)
  }
}

# --- Summary Statistics and T-Tests by BORDER_STATUS ---
# Calculate summary statistics grouped by BORDER_STATUS
border_status_summary_stats <- merged_data %>%
  group_by(BORDER_STATUS) %>%
  summarise(
    avg_N = mean(N, na.rm = TRUE),
    avg_bad_zipcounty = mean(bad_zipcounty, na.rm = TRUE),
    weighted_bad_zipcounty = sum(weighted_bad_zipcounty, na.rm = TRUE) / sum(N, na.rm = TRUE),
    count_N = sum(!is.na(N)),
    count_bad_zipcounty = sum(!is.na(bad_zipcounty))
  )

# Pairwise t-tests for BORDER_STATUS groups
border_status_groups <- unique(merged_data$BORDER_STATUS)
t_test_results_border_status_N <- list()
t_test_results_border_status_bad_zipcounty <- list()
t_test_results_border_status_weighted_bad_zipcounty <- list()

for (i in 1:(length(border_status_groups) - 1)) {
  for (j in (i + 1):length(border_status_groups)) {
    group1 <- border_status_groups[i]
    group2 <- border_status_groups[j]
    t_test_results_border_status_N[[paste(group1, group2, sep = " vs ")]] <- perform_t_test(merged_data, "N", group1, group2)
    t_test_results_border_status_bad_zipcounty[[paste(group1, group2, sep = " vs ")]] <- perform_t_test(merged_data, "bad_zipcounty", group1, group2)
    t_test_results_border_status_weighted_bad_zipcounty[[paste(group1, group2, sep = " vs ")]] <- perform_t_test(merged_data, "weighted_bad_zipcounty", group1, group2)
  }
}

# --- Summary Statistics and T-Tests by TYPE ---
# Calculate summary statistics grouped by TYPE
type_summary_stats <- merged_data %>%
  group_by(TYPE) %>%
  summarise(
    avg_N = mean(N, na.rm = TRUE),
    avg_bad_zipcounty = mean(bad_zipcounty, na.rm = TRUE),
    weighted_bad_zipcounty = sum(weighted_bad_zipcounty, na.rm = TRUE) / sum(N, na.rm = TRUE),
    count_N = sum(!is.na(N)),
    count_bad_zipcounty = sum(!is.na(bad_zipcounty))
  )

# Perform pairwise t-tests for TYPE (only for PO Box, Standard, and NA)
valid_type_groups <- c("PO Box", "Standard", NA)
t_test_results_type_N <- list()
t_test_results_type_bad_zipcounty <- list()
t_test_results_type_weighted_bad_zipcounty <- list()

for (i in 1:(length(valid_type_groups) - 1)) {
  for (j in (i + 1):length(valid_type_groups)) {
    group1 <- valid_type_groups[i]
    group2 <- valid_type_groups[j]
    t_test_results_type_N[[paste(group1, group2, sep = " vs ")]] <- perform_t_test_status(merged_data, "N", group1, group2)
    t_test_results_type_bad_zipcounty[[paste(group1, group2, sep = " vs ")]] <- perform_t_test_status(merged_data, "bad_zipcounty", group1, group2)
    t_test_results_type_weighted_bad_zipcounty[[paste(group1, group2, sep = " vs ")]] <- perform_t_test_status(merged_data, "weighted_bad_zipcounty", group1, group2)
  }
}

# --- Print and Save Results ---
# Print summary statistics for BORDER_STATUS
print("Summary Statistics by BORDER_STATUS:")
print(border_status_summary_stats)

# Print t-test results for BORDER_STATUS
print("Pairwise t-test results by BORDER_STATUS for N:")
print(t_test_results_border_status_N)
print("Pairwise t-test results by BORDER_STATUS for bad_zipcounty:")
print(t_test_results_border_status_bad_zipcounty)
print("Pairwise t-test results by BORDER_STATUS for weighted_bad_zipcounty:")
print(t_test_results_border_status_weighted_bad_zipcounty)

# Print summary statistics for TYPE
print("Summary Statistics by TYPE:")
print(type_summary_stats)

# Print t-test results for TYPE
print("Pairwise t-test results by TYPE for N:")
print(t_test_results_type_N)
print("Pairwise t-test results by TYPE for bad_zipcounty:")
print(t_test_results_type_bad_zipcounty)
print("Pairwise t-test results by TYPE for weighted_bad_zipcounty:")
print(t_test_results_type_weighted_bad_zipcounty)

# Save the summary statistics to an Excel file
write_xlsx(list(
  BORDER_STATUS_Summary = border_status_summary_stats,
  TYPE_Summary = type_summary_stats
), "Data Outputs/VDSS_Zip_Border_Status_Type_Bad_ZipCounty_Rate/VDSS_Summary_Statistics_and_T_Tests.xlsx")
