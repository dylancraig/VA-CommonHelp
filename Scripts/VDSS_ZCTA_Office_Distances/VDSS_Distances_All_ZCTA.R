# Author: Dylan Craig
# Date Last Updated: 12/29/2024
# File Name: VDSS_Distances_All_ZCTA.R

# Purpose: This script calculates Haversine, driving, and transit distances and times
# between ZCTA midpoints and LDSS midpoints using ZCTA, ZIP-FIPS, and VDSS office datasets for all years.

# --- Load Necessary Libraries ---
library(readxl)      # For reading Excel files
library(dplyr)       # For data manipulation
library(tigris)      # For accessing TIGER/Line shapefiles
library(sf)          # For spatial operations
library(openxlsx)    # For writing Excel files
library(geosphere)   # For calculating Haversine distances
library(gmapsdistance) # For Google Maps API distances
library(progress)    # For progress bars

# --- Set Google API Key ---
google_api_key <- ""

# --- Read Input Data ---
# Load ZCTA crosswalk data
zcta_data <- read_excel("Data Outputs/VDSS_Zip_ZCTA_Crosswalk/HRSA_ZCTA.xlsx")

# Load VDSS office data for all years
vdss_data <- read_excel("Raw Data/VDSS_Zip_Office_Distances/VDSS_Offices_All.xlsx")

# Load USPS ZIP-FIPS mapping data
usps_zip_county <- read_excel("Raw Data/VDSS_Zip_Office_Distances/USPS_Zip_County.xlsx")

# --- Get ZCTA Shape Data for Virginia ---
options(tigris_use_cache = TRUE)
virginia_zctas <- zctas(state = "VA", year = 2010, cb = FALSE)
virginia_zctas_centroids <- st_centroid(virginia_zctas)
zcta_coords <- st_coordinates(virginia_zctas_centroids)

zcta_centroids <- data.frame(
  zcta = virginia_zctas$ZCTA5CE10,
  zcta_lat = zcta_coords[, "Y"],
  zcta_long = zcta_coords[, "X"]
)

# --- Merge Datasets ---
merged_zcta_data <- merge(zcta_data, zcta_centroids, by.x = "ZCTA", by.y = "zcta")
merged_zip_fips <- merge(usps_zip_county, merged_zcta_data, by.x = "ZIP", by.y = "ZIP")
merged_data <- merge(vdss_data, merged_zip_fips, by = "FIPS")

# --- Calculate Haversine Distances ---
merged_data <- merged_data %>%
  mutate(HAVERSINE_DISTANCE_METERS = distHaversine(
    cbind(zcta_long, zcta_lat), 
    cbind(VDSS_LONG_MID, VDSS_LAT_MID)
  ))

# --- Prepare Coordinates for Google Maps API ---
merged_data <- merged_data %>%
  mutate(
    LDSS_COORDS = paste(VDSS_LAT_MID, VDSS_LONG_MID, sep = "+"),
    ZCTA_COORDS = paste(zcta_lat, zcta_long, sep = "+")
  )

# --- Driving Distances and Times ---
pb_driving <- progress_bar$new(
  format = "Driving [:bar] :current/:total (:percent) - ETA: :eta",
  total = nrow(merged_data), clear = FALSE, width = 60
)

gmaps_results_driving <- vector("list", nrow(merged_data))
for (i in 1:nrow(merged_data)) {
  pb_driving$tick()
  gmaps_results_driving[[i]] <- gmapsdistance(
    origin = merged_data$LDSS_COORDS[i],
    destination = merged_data$ZCTA_COORDS[i],
    mode = "driving",
    key = google_api_key
  )
}

merged_data <- merged_data %>%
  mutate(
    DRIVING_DISTANCE_METERS = sapply(gmaps_results_driving, function(x) if (!is.null(x) && x$Status == "OK") as.numeric(x$Distance) else NA),
    DRIVING_TIME_SECONDS = sapply(gmaps_results_driving, function(x) if (!is.null(x) && x$Status == "OK") as.numeric(x$Time) else NA)
  )

# --- Transit Distances and Times ---
pb_transit <- progress_bar$new(
  format = "Transit [:bar] :current/:total (:percent) - ETA: :eta",
  total = nrow(merged_data), clear = FALSE, width = 60
)

gmaps_results_transit <- vector("list", nrow(merged_data))
for (i in 1:nrow(merged_data)) {
  pb_transit$tick()
  gmaps_results_transit[[i]] <- gmapsdistance(
    origin = merged_data$LDSS_COORDS[i],
    destination = merged_data$ZCTA_COORDS[i],
    mode = "transit",
    key = google_api_key
  )
}

merged_data <- merged_data %>%
  mutate(
    TRANSIT_DISTANCE_METERS = sapply(gmaps_results_transit, function(x) if (!is.null(x) && x$Status == "OK") as.numeric(x$Distance) else NA),
    TRANSIT_TIME_SECONDS = sapply(gmaps_results_transit, function(x) if (!is.null(x) && x$Status == "OK") as.numeric(x$Time) else NA)
  )

# --- Convert Units ---
meters_to_miles <- 0.000621371
seconds_to_minutes <- 1 / 60

merged_data <- merged_data %>%
  mutate(
    HAVERSINE_DISTANCE_MILES = HAVERSINE_DISTANCE_METERS * meters_to_miles,
    DRIVING_DISTANCE_MILES = DRIVING_DISTANCE_METERS * meters_to_miles,
    DRIVING_TIME_MINUTES = DRIVING_TIME_SECONDS * seconds_to_minutes,
    TRANSIT_DISTANCE_MILES = TRANSIT_DISTANCE_METERS * meters_to_miles,
    TRANSIT_TIME_MINUTES = TRANSIT_TIME_SECONDS * seconds_to_minutes
  )

# --- Save Results ---
output_path <- "Data Outputs/VDSS_ZCTA_Office_Distances/VDSS_Office_ZCTA_GeoCode_All_Distances.xlsx"
write.xlsx(merged_data, output_path)

cat("Updated dataset with distances and times saved to:", output_path, "\n")

