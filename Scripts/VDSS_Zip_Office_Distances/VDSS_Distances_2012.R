# Author: Dylan Craig
# Data Updated: 1/8/2025
# File Name: VDSS_Distances_2012.R

# Description: This script calculates haversine, driving, and transit distances and times 
# between VDSS office midpoints and ZIP code midpoints for the 2012 dataset, converts them to miles and minutes, 
# and saves the updated Excel file.

# --- Load Required Libraries ------------------------------------------------
library(readxl)       # For reading Excel files
library(writexl)      # For writing Excel files
library(geosphere)    # For geospatial calculations like haversine distance
library(gmapsdistance) # For interfacing with Google Maps API
library(dplyr)        # For data transformation
library(progress)     # For creating progress bars

# --- Load Data --------------------------------------------------------------

# Define the file path for the input dataset
input_file_path <- "Data Outputs/VDSS_Zip_Office_Distances/VDSS_Office_Zip_GeoCode_2012_Corrected.xlsx"

# Define the file path for the output dataset
output_file_path <- "Data Outputs/VDSS_Zip_Office_Distances/VDSS_Office_Zip_GeoCode_2012_Distances.xlsx"

# Read the data from the Excel file
data <- read_excel(input_file_path)

# --- Set Google API Key -----------------------------------------------------
google_api_key <- ""

# --- Calculate Haversine Distances ------------------------------------------

# Calculate the haversine distance (in meters) between VDSS office and ZIP code midpoints
data$HAVERSINE_DISTANCE_METERS <- distHaversine(
  cbind(data$VDSS_LONG_MID, data$VDSS_LAT_MID), # VDSS office midpoint coordinates
  cbind(data$ZIP_MID_LONG, data$ZIP_MID_LAT)    # ZIP code midpoint coordinates
)

# --- Driving Distance and Time Calculation with Traffic Models ---
library(progress)

# Prepare coordinates for Google Maps API
data$VDSS_COORDS <- paste(data$VDSS_LAT_MID, data$VDSS_LONG_MID, sep = "+")
data$ZIP_COORDS <- paste(data$ZIP_MID_LAT, data$ZIP_MID_LONG, sep = "+")

# Initialize a progress bar
pb_driving <- progress_bar$new(
  format = "Driving [:bar] :current/:total (:percent) - ETA: :eta",
  total = nrow(data), clear = FALSE, width = 60
)

# Placeholder for driving results
gmaps_results <- vector("list", nrow(data))

# Define the departure date and time
departure_date <- "2025-10-08" # A Wednesday in October 2025
departure_time <- "12:00:00"   # Set to noon

# Add placeholders for traffic models and no traffic
# Driving time is in SECONDS
data$DRIVING_TIME_NONE_SECONDS <- NA
data$DRIVING_TIME_OPTIMISTIC_SECONDS <- NA
data$DRIVING_TIME_PESSIMISTIC_SECONDS <- NA
data$DRIVING_TIME_BEST_GUESS_SECONDS <- NA
data$DRIVING_DISTANCE_METERS <- NA

# Loop for driving distances and times
for (i in 1:nrow(data)) {
  pb_driving$tick()
  
  # No Traffic (Default)
  result_none <- gmapsdistance(
    origin = data$VDSS_COORDS[i],
    destination = data$ZIP_COORDS[i],
    mode = "driving",
    dep_date = departure_date,
    dep_time = departure_time,
    traffic_model = "None",
    key = google_api_key
  )
  
  # Optimistic Traffic
  result_optimistic <- gmapsdistance(
    origin = data$VDSS_COORDS[i],
    destination = data$ZIP_COORDS[i],
    mode = "driving",
    dep_date = departure_date,
    dep_time = departure_time,
    traffic_model = "optimistic",
    key = google_api_key
  )
  
  # Pessimistic Traffic
  result_pessimistic <- gmapsdistance(
    origin = data$VDSS_COORDS[i],
    destination = data$ZIP_COORDS[i],
    mode = "driving",
    dep_date = departure_date,
    dep_time = departure_time,
    traffic_model = "pessimistic",
    key = google_api_key
  )
  
  # Best Guess Traffic
  result_best_guess <- gmapsdistance(
    origin = data$VDSS_COORDS[i],
    destination = data$ZIP_COORDS[i],
    mode = "driving",
    dep_date = departure_date,
    dep_time = departure_time,
    traffic_model = "best_guess",
    key = google_api_key
  )
  
  # Store results
  if (!is.null(result_none) && result_none$Status == "OK") {
    data$DRIVING_DISTANCE_METERS[i] <- as.numeric(result_none$Distance) # In meters
    data$DRIVING_TIME_NONE_SECONDS[i] <- as.numeric(result_none$Time)  # In seconds
  }
  if (!is.null(result_optimistic) && result_optimistic$Status == "OK") {
    data$DRIVING_TIME_OPTIMISTIC_SECONDS[i] <- as.numeric(result_optimistic$Time)  # In seconds
  }
  if (!is.null(result_pessimistic) && result_pessimistic$Status == "OK") {
    data$DRIVING_TIME_PESSIMISTIC_SECONDS[i] <- as.numeric(result_pessimistic$Time)  # In seconds
  }
  if (!is.null(result_best_guess) && result_best_guess$Status == "OK") {
    data$DRIVING_TIME_BEST_GUESS_SECONDS[i] <- as.numeric(result_best_guess$Time)  # In seconds
  }
}


# --- Transit Distance and Time Calculation -----------------------------------

# Initialize a progress bar
pb_transit <- progress_bar$new(
  format = "Transit [:bar] :current/:total (:percent) - ETA: :eta",
  total = nrow(data), clear = FALSE, width = 60
)

# Placeholder for transit results
gmaps_results_transit <- vector("list", nrow(data))

# Loop for transit distances and times
for (i in 1:nrow(data)) {
  pb_transit$tick()
  gmaps_results_transit[[i]] <- gmapsdistance(
    origin = data$VDSS_COORDS[i],
    destination = data$ZIP_COORDS[i],
    mode = "transit",
    key = google_api_key
  )
}

# Extract transit results
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
library(dplyr)

# Conversion factors
meters_to_miles <- 0.000621371  # Meters to miles
seconds_to_minutes <- 1 / 60    # Seconds to minutes

# Transform distances and times to miles and minutes
data <- data %>%
  mutate(
    # Convert Haversine distance
    HAVERSINE_DISTANCE_MILES = HAVERSINE_DISTANCE_METERS * meters_to_miles,
    
    # Convert driving distances and times
    DRIVING_DISTANCE_MILES = DRIVING_DISTANCE_METERS * meters_to_miles,
    DRIVING_TIME_NONE_MINUTES = DRIVING_TIME_NONE_SECONDS * seconds_to_minutes,
    DRIVING_TIME_OPTIMISTIC_MINUTES = DRIVING_TIME_OPTIMISTIC_SECONDS * seconds_to_minutes,
    DRIVING_TIME_PESSIMISTIC_MINUTES = DRIVING_TIME_PESSIMISTIC_SECONDS * seconds_to_minutes,
    DRIVING_TIME_BEST_GUESS_MINUTES = DRIVING_TIME_BEST_GUESS_SECONDS * seconds_to_minutes,
    
    # Convert transit distances and times
    TRANSIT_DISTANCE_MILES = TRANSIT_DISTANCE_METERS * meters_to_miles,
    TRANSIT_TIME_MINUTES = TRANSIT_TIME_SECONDS * seconds_to_minutes
  )


# --- Save Updated Data ------------------------------------------------------

# Save the updated dataset with all distances and times to the new file
write_xlsx(data, output_file_path)

# Notify the user of successful file saving
cat("Updated dataset with distances and times saved to:", output_file_path, "\n")

