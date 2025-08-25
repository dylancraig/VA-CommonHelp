# Script Name: VDSS_Zip_Border_Status.R -------------------------------------
# Author: Dylan Craig -------------------------------------------------------
# Date Last Updated: 12/29/2024 ---------------------------------------------
# Purpose: Identify neighboring counties for Virginia based on geographic 
# boundaries. Outputs neighboring counties and their FIPS codes to Excel.

# Load Necessary Libraries ---------------------------------------------------
library(sf)           # For handling spatial data
library(dplyr)        # For data manipulation
library(tigris)       # For loading geographic data
library(pbapply)      # For progress bar during list operations
library(openxlsx)     # For saving data to Excel files

# 1. Load Virginia County Boundaries -----------------------------------------
# Get Virginia county boundaries for 2022 using tigris
va_counties <- counties(state = "VA", year = 2022, class = "sf")

# Transform the shapefile to CRS 4326 (WGS84) for consistency
va_counties <- st_transform(va_counties, 4326)

# 2. Create a Function to Find Neighboring Counties --------------------------
# Define a function to identify neighboring counties for a given county
find_neighbors <- function(i, va_counties) {
  # Get the current county
  current_county <- va_counties[i, ]
  
  # Find counties that touch the current county
  neighbors <- st_touches(current_county, va_counties, sparse = FALSE)
  
  # Extract the names of neighboring counties using NAMELSAD
  neighboring_counties <- va_counties[neighbors, ]$NAMELSAD
  
  # Return the current county name and its neighbors
  c(current_county$NAMELSAD, neighboring_counties)
}

# 3. Identify Neighbors for All Counties -------------------------------------
# Use pblapply to process each county with a progress bar
neighbors_list <- pblapply(1:nrow(va_counties), function(i) find_neighbors(i, va_counties))

# Convert the list of neighbors to a dataframe
neighbors_df <- do.call(rbind, lapply(neighbors_list, function(x) {
  c(x, rep(NA, max(lengths(neighbors_list)) - length(x)))  # Pad rows with NA
}))
neighbors_df <- as.data.frame(neighbors_df)

# Rename the columns for clarity
colnames(neighbors_df) <- c("County", paste0("Border_", 1:(ncol(neighbors_df) - 1)))

# 4. Create a Dataframe with FIPS Codes --------------------------------------
# Extract FIPS codes and county names from the va_counties dataframe
fips_codes <- va_counties %>% 
  st_drop_geometry() %>%  # Drop spatial geometry for non-spatial processing
  select(NAMELSAD, GEOID) # Select columns for names and FIPS codes

# Define a function to replace county names with FIPS codes
replace_with_fips <- function(name, fips_codes) {
  fips <- fips_codes %>% filter(NAMELSAD == name) %>% pull(GEOID)
  if (length(fips) == 0) return(NA) else return(fips)
}

# Replace county names with FIPS codes in the neighbors dataframe
neighbors_fips_df <- neighbors_df %>%
  mutate(across(starts_with("Border_"), ~sapply(., replace_with_fips, fips_codes = fips_codes)),
         County = sapply(County, replace_with_fips, fips_codes = fips_codes))

# 5. Save Results to Excel ---------------------------------------------------
# Save the neighbors dataframe with names to an Excel file
write.xlsx(neighbors_df, "Data Outputs/VDSS_Zip_Border_Status/virginia_bordering_counties.xlsx")

# Save the neighbors dataframe with FIPS codes to a separate Excel file
write.xlsx(neighbors_fips_df, "Data Outputs/VDSS_Zip_Border_Status/virginia_bordering_counties_fips.xlsx")

# End of Script --------------------------------------------------------------
