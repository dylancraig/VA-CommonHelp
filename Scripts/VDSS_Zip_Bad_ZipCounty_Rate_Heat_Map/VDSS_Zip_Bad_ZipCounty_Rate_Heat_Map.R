# Author: Dylan Craig
# Data Updated: 12/29/2024
# File Name: VDSS_Zip_Bad_ZipCounty_Rate_Heat_Map.R

# Description: This script creates an interactive heat map showing the "Bad ZipCounty Rate" for Zip Codes in Virginia.
# The map overlays county boundaries and is exported as HTML, PDF, and PNG files.

# --- Load Required Libraries ------------------------------------------------
library(haven)    # For reading .dta files
library(sf)       # For handling shapefiles
library(tmap)     # For creating thematic maps
library(dplyr)    # For data manipulation
library(tigris)   # For accessing TIGER/Line shapefiles

# --- Load and Prepare Data --------------------------------------------------

# Load the dataset from a .dta file
data <- read_dta("Raw Data/VDSS_Bad_ZipCounty_Rate_Heat_Map/bad_zipcounty_rate.dta")

# Convert the ZIP column to character format
data <- data %>%
  mutate(zip = as.character(zip))

# Retrieve the ZCTA shapefile for Virginia
va_zips <- zctas(state = "VA", year = 2010, cb = FALSE, class = "sf")

# Merge the ZCTA shapefile with the dataset on the ZIP column
merged_data <- va_zips %>%
  left_join(data, by = c("ZCTA5CE10" = "zip"))

# Identify and log missing ZIP codes from the dataset
missing_zips <- data %>%
  filter(!zip %in% merged_data$ZCTA5CE10)

cat("Missing ZIP codes:", paste(missing_zips$zip, collapse = ", "), "\n")

# --- Define Heat Map Breaks -------------------------------------------------

# Extract the `bad_zipcounty` values and remove NA entries
rate_values <- merged_data$bad_zipcounty
rate_values <- rate_values[!is.na(rate_values)]

# Define the lower and upper bounds for the interval of interest
lower_bound <- 0.01
upper_bound <- 0.1

# Filter rate values within the defined bounds
filtered_values <- rate_values[rate_values > lower_bound & rate_values < upper_bound]

# Calculate the median value within the filtered range
mid_value <- quantile(filtered_values, probs = 0.5)

# Define breaks for the heat map
breaks <- c(0, lower_bound, mid_value, upper_bound, 0.4, 1)

# --- Prepare County Shapefile ------------------------------------------------

# Retrieve the county shapefile for Virginia
va_counties <- counties(state = "VA", class = "sf")

# Ensure the county shapefile CRS matches the ZCTA shapefile CRS
va_counties <- st_transform(va_counties, st_crs(va_zips))

# --- Create and Customize the Heat Map --------------------------------------

# Switch to interactive mapping mode
tmap_mode("view")

# Create the interactive heat map
tm <- tm_shape(merged_data) +
  tm_polygons("bad_zipcounty", 
              title = "Incorrect LDSS Rate", 
              palette = "plasma", 
              style = "fixed", 
              breaks = breaks,  # Use the defined breaks
              legend.hist = TRUE,
              id = "ZCTA5CE10") +  # Display ZCTA5CE10 on hover
  tm_shape(va_counties) +
  tm_borders(lwd = 1.5, col = "black") +
  tm_layout(title = "Bad ZipCounty Rate Heat Map",  # Updated title
            legend.outside = TRUE,
            legend.hist.height = 0.3,
            legend.hist.width = 0.6)

# Render the interactive map
tm

# --- Save the Heat Map ------------------------------------------------------

# Define output paths
html_output <- "Plots/VDSS_Bad_ZipCounty_Rate_Heat_Map/VDSS_Bad_ZipCounty_Rate_Heat_Map.html"
pdf_output <- "Plots/VDSS_Bad_ZipCounty_Rate_Heat_Map/VDSS_Bad_ZipCounty_Rate_Heat_Map.pdf"
png_output <- "Plots/VDSS_Bad_ZipCounty_Rate_Heat_Map/VDSS_Bad_ZipCounty_Rate_Heat_Map.png"

# Save the map in multiple formats
tmap_save(tm, html_output)
cat("Interactive map saved as HTML to:", html_output, "\n")

tmap_save(tm, pdf_output)
cat("Map saved as PDF to:", pdf_output, "\n")

tmap_save(tm, png_output)
cat("Map saved as PNG to:", png_output, "\n")
