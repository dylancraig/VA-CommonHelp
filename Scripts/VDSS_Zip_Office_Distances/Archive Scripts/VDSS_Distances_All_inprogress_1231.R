library(readxl) # Load the readxl package to read Excel files
library(tidyverse) # Load the tidyverse package for data manipulation

# Define the base directory relative to the R project
base_dir <- "Data Outputs"

# Construct file paths relative to the base directory
file_vdss_zip_all <- file.path(base_dir, "VDSS_Zip_Office_Distances", "VDSS_Office_Zip_GeoCode_All_Distances.xlsx")
file_vdss_final <- file.path(base_dir, "VDSS_Final_Data", "Final_Data.xlsx")

# Read the Excel files into R as data frames
data_vdss_zip_all <- read_excel(file_vdss_zip_all)
data_vdss_final <- read_excel(file_vdss_final)

# Select only the specified columns
filtered_data <- data_vdss_zip_all %>%
  dplyr::select(zip_ldss = ZIP_LDSS, ZIP = ZIP_MATCH, LDSS_COORDS, ZIP_COORDS, 
                HAVERSINE_DISTANCE_MILES, DRIVING_DISTANCE_MILES, 
                DRIVING_TIME_MINUTES, TRANSIT_DISTANCE_MILES, TRANSIT_TIME_MINUTES)

# Report duplicate rows
duplicate_rows <- filtered_data[duplicated(filtered_data), ]
cat("Number of duplicate rows:", nrow(duplicate_rows), "\n")

# Filter to unique rows
unique_data <- filtered_data %>% distinct()

# Ensure zip_ldss and ZIP in unique_data and data_vdss_final have the same type
unique_data <- unique_data %>% mutate(zip_ldss = as.character(zip_ldss), ZIP = as.character(ZIP))
data_vdss_final <- data_vdss_final %>% mutate(zip_ldss = as.character(zip_ldss), ZIP = as.character(ZIP))

# Merge unique_data with the final data based on shared zip_ldss and ZIP, allowing for many-to-many relationship
merged_data <- unique_data %>%
  dplyr::inner_join(data_vdss_final, by = c("zip_ldss" = "zip_ldss", "ZIP" = "ZIP"), relationship = "many-to-many")

# Print a summary of the merged data frame
summary(merged_data)

