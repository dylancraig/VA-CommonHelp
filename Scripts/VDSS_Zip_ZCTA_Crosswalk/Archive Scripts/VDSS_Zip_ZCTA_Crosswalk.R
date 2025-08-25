# Load necessary libraries
library(dplyr)
library(readr)
library(readxl)
library(writexl)

# Load the germuska ZIP to ZCTA crosswalk CSV file
zip_zcta_crosswalk <- read_csv("Raw Data/VDSS_Zip_ZCTA_Crosswalk/germuska_zip_zcta_crosswalk.csv")

# Load the USPS ZIP-County Excel file
usps_zip_county <- read_excel("Raw Data/VDSS_Zip_ZCTA_Crosswalk/USPS_Zip_County.xlsx")

# Drop the FIPS column from the USPS data and get unique ZIP codes
usps_zip_county <- usps_zip_county %>%
  select(-FIPS) %>%
  distinct(ZIP)

# Join the datasets by ZIP
zip_zcta_crosswalk_joined <- left_join(usps_zip_county, zip_zcta_crosswalk, by = "ZIP")

# Save the result to an Excel file
write_xlsx(zip_zcta_crosswalk_joined, "Data Outputs/VDSS_Zip_ZCTA_Crosswalk/Zip_ZCTA_Crosswalk.xlsx")