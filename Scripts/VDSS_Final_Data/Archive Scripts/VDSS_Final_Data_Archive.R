# Load necessary libraries
library(readxl)
library(dplyr)
library(haven)
library(writexl)

# Load the VDSS Zip Border Status dataset
vdss_zip_border_status <- read_excel("Data Outputs/VDSS_Zip_Border_Status/VDSS_Zip_FIPS_Border_Status_Final_Designation.xlsx")

# Load the VDSS Zip Residential Status dataset
zip_residential_status <- read_excel("Data Outputs/VDSS_Zip_Residential_Status/Zip_Residential_Status.xlsx")

# Load the VDSS Zip Type dataset
vdss_zip_type <- read_excel("Data Outputs/VDSS_Zip_Type/VDSS_Zip_Type.xlsx")

# Load the Zip to ZCTA Crosswalk dataset
zip_zcta_crosswalk <- read_excel("Data Outputs/VDSS_Zip_ZCTA_Crosswalk/Zip_ZCTA_Crosswalk.xlsx")

# Load the Zip Treat dataset
zip_treat <- read_dta("Raw Data/Zip_Treat_Map/zip_treat.dta")

# Load the Bad Zip County Rate dataset
bad_zipcounty_rate <- read_dta("Raw Data/Bad_ZipCounty_Rate_Heat_Map/bad_zipcounty_rate.dta")

# Convert 'zip' to 'ZIP' in the bad_zipcounty_rate and zip_treat datasets to ensure consistency
bad_zipcounty_rate <- bad_zipcounty_rate %>%
  rename(ZIP = zip)

zip_treat <- zip_treat %>%
  rename(ZIP = zip)

# Ensure all ZIP columns are of character type
vdss_zip_border_status <- vdss_zip_border_status %>%
  mutate(ZIP = as.character(ZIP))

zip_residential_status <- zip_residential_status %>%
  mutate(ZIP = as.character(ZIP))

vdss_zip_type <- vdss_zip_type %>%
  mutate(ZIP = as.character(ZIP))

zip_zcta_crosswalk <- zip_zcta_crosswalk %>%
  mutate(ZIP = as.character(ZIP))

zip_treat <- zip_treat %>%
  mutate(ZIP = as.character(ZIP))

bad_zipcounty_rate <- bad_zipcounty_rate %>%
  mutate(ZIP = as.character(ZIP))

# Merge the datasets by ZIP
merged_data <- vdss_zip_border_status %>%
  full_join(zip_residential_status, by = "ZIP") %>%
  full_join(vdss_zip_type, by = "ZIP") %>%
  full_join(zip_zcta_crosswalk, by = "ZIP") %>%
  full_join(zip_treat, by = "ZIP") %>%
  full_join(bad_zipcounty_rate, by = "ZIP")

# Deduplicate by ZIP and FIPS
merged_data_dedup <- merged_data %>%
  distinct(ZIP, FIPS, .keep_all = TRUE)

# Missing data from vdss_zip_border_status
missing_vdss_zip_border_status <- anti_join(vdss_zip_border_status, merged_data_dedup, by = "ZIP")

# Missing data from zip_residential_status
missing_zip_residential_status <- anti_join(zip_residential_status, merged_data_dedup, by = "ZIP")

# Missing data from vdss_zip_type
missing_vdss_zip_type <- anti_join(vdss_zip_type, merged_data_dedup, by = "ZIP")

# Missing data from zip_zcta_crosswalk
missing_zip_zcta_crosswalk <- anti_join(zip_zcta_crosswalk, merged_data_dedup, by = "ZIP")

# Missing data from zip_treat
missing_zip_treat <- anti_join(zip_treat, merged_data_dedup, by = "ZIP")

# Missing data from bad_zipcounty_rate
missing_bad_zipcounty_rate <- anti_join(bad_zipcounty_rate, merged_data_dedup, by = "ZIP")

# Print the number of missing rows for each dataset
cat("Missing from vdss_zip_border_status:", nrow(missing_vdss_zip_border_status), "\n")
cat("Missing from zip_residential_status:", nrow(missing_zip_residential_status), "\n")
cat("Missing from vdss_zip_type:", nrow(missing_vdss_zip_type), "\n")
cat("Missing from zip_zcta_crosswalk:", nrow(missing_zip_zcta_crosswalk), "\n")
cat("Missing from zip_treat:", nrow(missing_zip_treat), "\n")
cat("Missing from bad_zipcounty_rate:", nrow(missing_bad_zipcounty_rate), "\n")

# Save the deduplicated merged data to an Excel file
write_xlsx(merged_data_dedup, "Data Outputs/VDSS_Final_Data/VDSS_Final_Data.xlsx")
