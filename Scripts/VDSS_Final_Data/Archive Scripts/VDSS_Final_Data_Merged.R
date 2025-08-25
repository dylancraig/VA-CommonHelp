# Load required libraries
library(tidyverse)
library(writexl)  # For writing to Excel
library(readxl)

# -------- File Paths -------- #
# Set relative file paths for datasets
path_2012 <- "Data Outputs/VDSS_Zip_Office_Distances/VDSS_Office_Zip_GeoCode_2012_Distances.xlsx"
path_final <- "Data Outputs/VDSS_Final_Data/Final_Data2.xlsx"

# -------- Data Import -------- #
# Import 2012 distances data
data_2012 <- read_excel(path_2012)

# Import final data
final_data <- read_excel(path_final)

# -------- Data Cleaning -------- #
# Remove ZIP suffix (e.g., "-1234") from ZIP_VDSS in 2012 dataset
data_2012 <- data_2012 %>%
  mutate(ZIP_VDSS_clean = str_remove(ZIP_VDSS, "-\\d{4}$"))  # Removes 4-digit suffix after hyphen

# Convert ZIP columns to character for consistent merging
final_data <- final_data %>%
  mutate(zip_ldss = as.character(zip_ldss),  # LDSS office ZIP code as character
         ZIP = as.character(ZIP))  # Recipient ZIP code as character

data_2012 <- data_2012 %>%
  mutate(ZIP = as.character(ZIP))  # ZIP code in 2012 data as character

data_2012 <- data_2012 %>%
  mutate(COUNTY = as.character(COUNTY))  # ZIP code in 2012 data as character

# -------- Merge Data -------- #
# Perform a left join to merge final data and 2012 distances data
merged_data <- final_data %>%
  left_join(data_2012, by = c("COUNTY" = "COUNTY",  # Merge by COUNTY (FIPS code),
                              "ZIP" = "ZIP"))  # Merge by recipient ZIP

# Drop specified columns from merged_data
merged_data <- merged_data %>%
  select(-dist, -treat_zip, -treat_g50, -treat_g75, -treat_l50, -treat_l75)

# -------- Calculate Missing Data Percentage -------- #
# Calculate percentage of missing values for each variable
missing_percentage <- merged_data %>%
  summarise(across(everything(), ~ mean(is.na(.)) * 100)) %>%  # Calculate % of missing values
  pivot_longer(cols = everything(), names_to = "variable", values_to = "percent_missing")  # Reshape for readability

# Find rows where "HAVERSINE_DISTANCE_METERS" is NA
missing_haversine <- merged_data %>%
  filter(is.na(HAVERSINE_DISTANCE_METERS))

# Find unique ZIP values and their counts
zip_counts <- missing_haversine %>%
  group_by(ZIP) %>%
  summarise(count = n())

# -------- Save to Excel -------- #
# Save merged data and missing percentages to the specified path
output_path <- "Data Outputs/VDSS_Final_Data/final_data_merged2.xlsx"

write_xlsx(
  list("Merged Data" = merged_data, "Missing Percentages" = missing_percentage),
  path = output_path
)

cat("Data saved to:", output_path, "\n")