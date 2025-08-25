# Author: Dylan Craig
# Data Updated: 1/7/2025
# File Name: VDSS_Distances_All.R

# Description: This script calculates haversine, driving, and transit distances and times between LDSS offices and ZIP codes 
# using geospatial data, converts them to miles and minutes, and outputs an updated Excel file.

# Load required libraries
library(readxl)       # For reading Excel files
library(writexl)      # For writing Excel files
library(geosphere)    # For geospatial calculations like haversine distance
library(gmapsdistance) # For interfacing with Google Maps API
library(dplyr)        # For data transformation
library(progress)     # For creating progress bars

# --- Load and Prepare Data --------------------------------------------------
# Define the file path to the input dataset
data <- read_excel("Data Outputs/VDSS_Zip_Office_Distances/VDSS_Office_Zip_GeoCode_All_Corrected.xlsx")

# --- Set Google API Key -----------------------------------------------------
google_api_key <- ""

# --- Calculate Haversine Distance -------------------------------------------
# Calculate the straight-line distance (haversine) between LDSS office and ZIP code coordinates
data$HAVERSINE_DISTANCE_METERS <- distHaversine(
  cbind(data$LDSS_LONGITUDE, data$LDSS_LATITUDE), # LDSS office coordinates
  cbind(data$ZIP_LONGITUDE, data$ZIP_LATITUDE)   # ZIP code coordinates
)

# --- Prepare Data for Google Maps API ---------------------------------------
# Combine latitude and longitude into a single string for use in Google Maps API
data$LDSS_COORDS <- paste(data$LDSS_LATITUDE, data$LDSS_LONGITUDE, sep = "+")
data$ZIP_COORDS <- paste(data$ZIP_LATITUDE, data$ZIP_LONGITUDE, sep = "+")

# --- Driving Distance and Time Calculation -----------------------------------
pb_driving <- progress_bar$new(
  format = "Driving [:bar] :current/:total (:percent) - ETA: :eta",
  total = nrow(data), clear = FALSE, width = 60
)

gmaps_results_driving <- vector("list", nrow(data))
for (i in 1:nrow(data)) {
  pb_driving$tick()
  gmaps_results_driving[[i]] <- gmapsdistance(
    origin = data$LDSS_COORDS[i],
    destination = data$ZIP_COORDS[i],
    mode = "driving",
    key = google_api_key
  )
}

data$DRIVING_DISTANCE_METERS <- NA
data$DRIVING_TIME_SECONDS <- NA

for (i in seq_along(gmaps_results_driving)) {
  result <- gmaps_results_driving[[i]]
  if (!is.null(result) && result$Status == "OK") {
    data$DRIVING_DISTANCE_METERS[i] <- as.numeric(result$Distance)
    data$DRIVING_TIME_SECONDS[i] <- as.numeric(result$Time)
  }
}

# --- Transit Distance and Time Calculation -----------------------------------
# Set departure time for 1/15/2025 at 8:00 AM EST
departure_time_est <- as.integer(as.POSIXct("2025-01-15 08:00:00", tz = "America/New_York"))

pb_transit <- progress_bar$new(
  format = "Transit [:bar] :current/:total (:percent) - ETA: :eta",
  total = nrow(data), clear = FALSE, width = 60
)

gmaps_results_transit <- vector("list", nrow(data))
for (i in 1:nrow(data)) {
  pb_transit$tick()
  gmaps_results_transit[[i]] <- gmapsdistance(
    origin = data$LDSS_COORDS[i],
    destination = data$ZIP_COORDS[i],
    mode = "transit",
    departure = departure_time_est,
    key = google_api_key
  )
}

data$TRANSIT_DISTANCE_METERS <- NA
data$TRANSIT_TIME_SECONDS <- NA

for (i in seq_along(gmaps_results_transit)) {
  result <- gmaps_results_transit[[i]]
  if (!is.null(result) && result$Status == "OK") {
    data$TRANSIT_DISTANCE_METERS[i] <- as.numeric(result$Distance)
    data$TRANSIT_TIME_SECONDS[i] <- as.numeric(result$Time)
  }
}

# --- Convert Units and Select Relevant Columns ------------------------------
# Conversion factors
meters_to_miles <- 0.000621371  # Meters to miles
seconds_to_minutes <- 1 / 60    # Seconds to minutes

# Transform distances and times to miles and minutes
data <- data %>%
  mutate(
    HAVERSINE_DISTANCE_MILES = HAVERSINE_DISTANCE_METERS * meters_to_miles,
    DRIVING_DISTANCE_MILES = DRIVING_DISTANCE_METERS * meters_to_miles,
    DRIVING_TIME_MINUTES = DRIVING_TIME_SECONDS * seconds_to_minutes,
    TRANSIT_DISTANCE_MILES = TRANSIT_DISTANCE_METERS * meters_to_miles,
    TRANSIT_TIME_MINUTES = TRANSIT_TIME_SECONDS * seconds_to_minutes)

# --- Save Updated Dataset ---------------------------------------------------
write_xlsx(data, "Data Outputs/VDSS_Zip_Office_Distances/VDSS_Office_Zip_GeoCode_All_Distances.xlsx")

cat("Updated dataset with distances in miles and times in minutes saved to:",
    "Data Outputs/VDSS_Zip_Office_Distances/VDSS_Office_Zip_GeoCode_All_Distances.xlsx\n")
