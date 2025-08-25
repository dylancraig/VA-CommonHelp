# Load necessary packages
library(readxl)
library(dplyr)
library(writexl)  # For saving Excel files

# Define file paths
file1 <- "Data Outputs/VDSS_Final_Data/Final_Data.xlsx"
file2 <- "Data Outputs/VDSS_Final_Data/Final_Data2.xlsx"
output_path <- "C:/Users/dscra/OneDrive - University of Virginia/VA_CommonHelp/Data Outputs/VDSS_Final_Data"

# Read the datasets
final_data1 <- read_excel(file1)
final_data2 <- read_excel(file2)

# Add a new column LDSS_FIPS by removing "510" or "51" prefix from COUNTY in final_data2
final_data2 <- final_data2 %>%
  mutate(ldss_fips = case_when(
    substr(COUNTY, 1, 3) == "510" ~ as.numeric(substr(COUNTY, 4, 6)),  # Remove "510"
    substr(COUNTY, 1, 2) == "51" ~ as.numeric(substr(COUNTY, 3, 5)),   # Remove "51"
    TRUE ~ as.numeric(COUNTY)  # Leave unchanged if it doesn't match
  ))

# Full join to include all rows and add indicator columns to see where each row comes from
merged_data <- full_join(final_data1 %>% mutate(Source = "Final_Data1"),
                         final_data2 %>% mutate(Source = "Final_Data2"),
                         by = colnames(final_data1))

# Save merged data
write_xlsx(merged_data, file.path(output_path, "Merged_Data.xlsx"))

# Find rows exclusive to Final_Data1
only_in_final_data1 <- anti_join(final_data1, final_data2, by = colnames(final_data1))
write_xlsx(only_in_final_data1, file.path(output_path, "Only_In_Final_Data1.xlsx"))

# Find rows exclusive to Final_Data2
only_in_final_data2 <- anti_join(final_data2, final_data1, by = colnames(final_data1))
write_xlsx(only_in_final_data2, file.path(output_path, "Only_In_Final_Data2.xlsx"))

# Convert COUNTY to keep only the last 2 or 3 digits and compare with ldss_fips in merged_data
merged_data <- merged_data %>%
  mutate(
    COUNTY_CONVERTED = case_when(
      substr(COUNTY, 1, 3) == "510" ~ as.numeric(substr(COUNTY, 4, 5)),  # Keep last 2 digits for "510" prefix
      substr(COUNTY, 1, 2) == "51" ~ as.numeric(substr(COUNTY, 3, 5)),   # Keep last 3 digits for "51" prefix
      TRUE ~ as.numeric(COUNTY)  # Default case if no prefix
    ),
    MATCH = (COUNTY_CONVERTED == ldss_fips)  # Create a MATCH column
  )

# Identify mismatched rows in merged_data
mismatched_rows <- merged_data %>%
  filter(!MATCH)
write_xlsx(mismatched_rows, file.path(output_path, "Mismatched_Rows.xlsx"))

# Convert COUNTY in final_data1 and final_data2 and compare
final_data1 <- final_data1 %>%
  mutate(
    COUNTY_CONVERTED = case_when(
      substr(COUNTY, 1, 3) == "510" ~ as.numeric(substr(COUNTY, 4, 5)),  # Keep last 2 digits for "510" prefix
      substr(COUNTY, 1, 2) == "51" ~ as.numeric(substr(COUNTY, 3, 5)),   # Keep last 3 digits for "51" prefix
      TRUE ~ as.numeric(COUNTY)  # Default case if no prefix
    ),
    MATCH = (COUNTY_CONVERTED == ldss_fips)  # Create a MATCH column
  )

final_data2 <- final_data2 %>%
  mutate(
    COUNTY_CONVERTED = case_when(
      substr(COUNTY, 1, 3) == "510" ~ as.numeric(substr(COUNTY, 4, 5)),  # Keep last 2 digits for "510" prefix
      substr(COUNTY, 1, 2) == "51" ~ as.numeric(substr(COUNTY, 3, 5)),   # Keep last 3 digits for "51" prefix
      TRUE ~ as.numeric(COUNTY)  # Default case if no prefix
    ),
    MATCH = (COUNTY_CONVERTED == ldss_fips)  # Create a MATCH column
  )

# Identify mismatched rows in final_data1 and final_data2
mismatched_rows_final_data1 <- final_data1 %>%
  filter(!MATCH)
write_xlsx(mismatched_rows_final_data1, file.path(output_path, "Mismatched_Rows_Final_Data1.xlsx"))

mismatched_rows_final_data2 <- final_data2 %>%
  filter(!MATCH)
write_xlsx(mismatched_rows_final_data2, file.path(output_path, "Mismatched_Rows_Final_Data2.xlsx"))

# Print summary to console
cat("Number of rows in Final_Data1:", nrow(final_data1), "\n")
cat("Number of rows in Final_Data2:", nrow(final_data2), "\n")
cat("Number of rows only in Final_Data1:", nrow(only_in_final_data1), "\n")
cat("Number of rows only in Final_Data2:", nrow(only_in_final_data2), "\n")
cat("Number of mismatched rows in Merged_Data:", nrow(mismatched_rows), "\n")
cat("Number of mismatched rows in Final_Data1:", nrow(mismatched_rows_final_data1), "\n")
cat("Number of mismatched rows in Final_Data2:", nrow(mismatched_rows_final_data2), "\n")


