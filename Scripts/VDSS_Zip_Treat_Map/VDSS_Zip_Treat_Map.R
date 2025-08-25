# Author: Dylan Craig
# Data Updated: 12/29/2024
# File Name: VDSS_Zip_Treat_Map.R

# Description: This script creates a thematic map displaying ZIP codes in Virginia categorized 
# by treatment status based on the `treat_g50` variable. The map is exported in PNG, PDF, and HTML formats.

# --- Load Required Libraries ------------------------------------------------
library(haven)    # For reading .dta files
library(sf)       # For handling shapefiles
library(tmap)     # For creating thematic maps
library(dplyr)    # For data manipulation
library(tigris)   # For accessing TIGER/Line shapefiles
library(readxl)   # For reading Excel files

# --- Load and Prepare Data --------------------------------------------------

# Read the ZIP treatment data from the .dta file
zip_treat <- read_dta("Raw Data/VDSS_Zip_Treat_Map/zip_treat.dta")

# Convert the ZIP column to character format
zip_treat <- zip_treat %>%
  mutate(zip = as.character(zip))

# Calculate the average `treat_g50` value for each ZIP code
zip_treat_avg <- zip_treat %>%
  group_by(zip) %>%
  summarise(avg_treat_g50 = mean(treat_g50))

# Categorize the average `treat_g50` values into treatment status categories
zip_treat_avg <- zip_treat_avg %>%
  mutate(Treatment_Status = case_when(
    avg_treat_g50 == 0 ~ "Non-Treated",
    avg_treat_g50 == 1 ~ "Treated",
    avg_treat_g50 > 0 & avg_treat_g50 < 1 ~ "Partially Treated",
    TRUE ~ "Unknown"
  ))

# --- Load Shapefiles --------------------------------------------------------

# Define the state of interest
state <- "VA"

# Load the ZIP codes shapefile for Virginia using tigris
va_zips <- zctas(state = state, class = "sf", year = 2010)

# Merge the averaged ZIP treatment data with the Virginia ZIP codes shapefile
merged_data <- va_zips %>%
  left_join(zip_treat_avg, by = c("ZCTA5CE10" = "zip"))

# Calculate summary statistics for treatment status
summary_stats_avg_merged <- merged_data %>%
  group_by(Treatment_Status) %>%
  summarise(count = n()) %>%
  mutate(percent = count / sum(count) * 100)

# Print summary statistics
print(summary_stats_avg_merged)

# --- Prepare County Shapefile ------------------------------------------------

# Load the county shapefile for Virginia using tigris
va_counties <- counties(state = state, class = "sf")

# Ensure the county shapefile CRS matches the ZIP codes shapefile CRS
va_counties <- st_transform(va_counties, st_crs(va_zips))

# --- Create and Customize the Map -------------------------------------------

# Switch to plot mode for tmap
tmap_mode("plot")

# Create the map with categorical styling for treatment status
tm <- tm_shape(merged_data) +
  tm_polygons(col = "Treatment_Status", 
              title = "Treated Zip Codes", 
              palette = c("Treated" = "#ADD8E6", 
                          "Non-Treated" = "#FFE4B5", 
                          "Partially Treated" = "#90EE90"),  # Specific colors
              style = "cat",  # Categorical styling
              labels = c("Non-Treated", "Partially Treated", "Treated"),
              border.col = "#BDBDBD",  # Light gray borders for ZIP codes
              border.lwd = 0.5, 
              showNA = FALSE) +  # Exclude NA values
  tm_shape(va_counties) +
  tm_borders(lwd = 2, col = "#636363") +  # Thicker dark gray borders for counties
  tm_layout(title = "Treated Zip Codes", 
            legend.outside = TRUE)

# Render the non-interactive map
tm

# --- Save the Map in Multiple Formats ----------------------------------------

# Save the map as a PNG file
png_path <- "Plots/VDSS_Zip_Treat_Map/Zip_Treat_Map.png"
tmap_save(tm, png_path, width = 1920, height = 1080, dpi = 100)
cat("Map saved as PNG to:", png_path, "\n")

# Save the map as a PDF file
pdf_path <- "Plots/VDSS_Zip_Treat_Map/Zip_Treat_Map.pdf"
tmap_save(tm, pdf_path)
cat("Map saved as PDF to:", pdf_path, "\n")

# Save the map as an HTML file
html_path <- "Plots/VDSS_Zip_Treat_Map/Zip_Treat_Map.html"
tmap_save(tm, html_path)
cat("Map saved as HTML to:", html_path, "\n")