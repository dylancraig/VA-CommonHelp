# Script Name: VDSS_Zip_FIPS_Border_Status_Final_Designation.R --------------
# Author: Dylan Craig -------------------------------------------------------
# Date Last Updated: 12/29/2024 ---------------------------------------------
# Purpose: Classify ZIP codes in Virginia into categories (Interior, Bordering, 
# Overlapping, or No Shapefile) based on their intersection with county boundaries.
# Outputs the final classification to an Excel file.

# Load Necessary Libraries ---------------------------------------------------
library(dplyr)
library(readxl)
library(tigris)
library(sf)
library(future.apply)
library(writexl)
library(progressr)

# 1. Load Data ---------------------------------------------------------------
# Load ZIP to FIPS mapping data from the preprocessed dataset
zip_fips_border_fips <- read_excel("Data Outputs/VDSS_Zip_Border_Status/VDSS_Zip_FIPS_Border_Population_Filtered.xlsx") %>%
  mutate(ZIP = as.character(ZIP))  # Ensure ZIP codes are treated as character strings

# Define the state of interest
state <- "VA"

# 2. Load Shapefiles ---------------------------------------------------------
# Load ZIP code shapefile for Virginia (2010 ZCTAs) using tigris
zip_codes <- zctas(state = state, class = "sf", year = 2010)

# Load county boundaries for Virginia and transform to CRS 4326 (WGS84)
counties <- counties(state = state, cb = TRUE) %>%
  st_transform(4326)

# Ensure the ZIP codes shapefile is in the same CRS as counties
zip_codes <- st_transform(zip_codes, 4326)

# 3. Setup Progress Monitoring -----------------------------------------------
# Set up progress handlers for monitoring long-running operations
handlers("progress")

# Define a function to intersect a single ZIP code with counties
intersect_zips <- function(zip, counties) {
  st_intersection(zip, counties)
}

# Use future.apply and progressr to process intersections in parallel
with_progress({
  p <- progressor(along = seq_len(nrow(zip_codes)))  # Initialize progress tracker
  zip_county_intersection <- future_lapply(seq_len(nrow(zip_codes)), function(i) {
    p()  # Update progress
    intersect_zips(zip_codes[i, ], counties)
  }, future.seed = TRUE) %>%
    bind_rows()  # Combine results into a single dataframe
})

# 4. Determine County Intersections ------------------------------------------
# Calculate the number of unique counties each ZIP code intersects with
zip_county_counts <- zip_county_intersection %>%
  st_drop_geometry() %>%  # Drop spatial geometries for processing
  group_by(ZCTA5CE10) %>%  # Group by ZIP code
  summarize(COUNT = n_distinct(NAME))  # Count unique counties for each ZIP

# 5. Classify ZIP Codes ------------------------------------------------------
# Perform a full join with the ZIP-to-FIPS data to ensure all ZIP codes are included
zip_classification <- full_join(zip_fips_border_fips, zip_county_counts, by = c("ZIP" = "ZCTA5CE10"))

# Classify ZIP codes into categories based on their intersections and FIPS information
zip_classification <- zip_classification %>%
  mutate(TYPE = case_when(
    COUNT == 1 & is.na(FIPS2) ~ "Interior",     # Only intersects one county and has no secondary FIPS
    COUNT > 1 & is.na(FIPS2) ~ "Bordering",     # Intersects multiple counties and has no secondary FIPS
    !is.na(FIPS2) ~ "Overlapping",             # Has a secondary FIPS indicating overlap
    TRUE ~ "No Shapefile"                      # Assign "No Shapefile" to missing types
  ))

# Rename the TYPE column to BORDER_STATUS for clarity
zip_classification <- zip_classification %>%
  rename(BORDER_STATUS = TYPE)

# 6. Save Results -----------------------------------------------------------
# Save the classified data to an Excel file for further use
write_xlsx(zip_classification, "Data Outputs/VDSS_Zip_Border_Status/VDSS_Zip_FIPS_Border_Status_Final_Designation.xlsx")

# End of Script -------------------------------------------------------------
