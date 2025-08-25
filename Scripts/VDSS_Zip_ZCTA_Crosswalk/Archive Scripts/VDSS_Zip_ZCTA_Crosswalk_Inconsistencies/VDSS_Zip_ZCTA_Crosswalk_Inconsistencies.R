# Load required libraries
library(sf)
library(tigris)
library(readxl)
library(leaflet)
library(dplyr)
library(writexl)
library(htmlwidgets)

# Read the Excel file
zip_data <- read_excel("Raw Data/VDSS_Zip_ZCTA_Crosswalk/VDSS_Zip_ZCTA_Crosswalk_Inconsistencies/VDSS_Zip_ZCTA_Crosswalk_Sample.xlsx")

# Convert to an sf object with NAD83 CRS
zip_data_sf <- st_as_sf(zip_data, coords = c("Zip_Code_Long", "Zip_Code_Lat"), crs = 4269)

# Reproject to WGS84 (EPSG:4326)
zip_data_sf <- st_transform(zip_data_sf, crs = 4326)

# Download the ZCTA shapefile for Virginia (year 2010)
va_zctas <- zctas(state = "VA", year = 2010, cb = FALSE)

# Download the ZCTA shapefile for Washington, D.C. (year 2010)
dc_zctas <- zctas(state = "DC", year = 2010, cb = FALSE)

# Ensure both ZCTA shapefiles are in WGS84 (EPSG:4326)
va_zctas <- st_transform(va_zctas, crs = 4326)
dc_zctas <- st_transform(dc_zctas, crs = 4326)

# Combine Virginia and D.C. ZCTAs into one object
zctas <- rbind(va_zctas, dc_zctas)

# Perform spatial join to associate each ZIP code with the ZCTA it falls within
zip_data_sf <- st_join(zip_data_sf, zctas, join = st_within, left = FALSE)

# Create a new column to indicate whether the ZCTA is correct
zip_data_sf <- zip_data_sf %>%
  mutate(ZCTA_Status = ifelse(ZCTA == ZCTA5CE10, "Correct ZCTA", "Incorrect ZCTA"))

# Save the dataframe with the correct or incorrect designations to a new Excel file
write_xlsx(as.data.frame(zip_data_sf), "Data Outputs/VDSS_Zip_ZCTA_Crosswalk/VDSS_Zip_ZCTA_Crosswalk_Inconsistencies/VDSS_Zip_ZCTA_Crosswalk_Status.xlsx")

# Define a color palette based on ZCTA
pal <- colorFactor(topo.colors(length(unique(zip_data_sf$ZCTA))), zip_data_sf$ZCTA)

# Create an interactive map
map <- leaflet() %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addPolygons(data = zctas, color = "blue", weight = 1, fillOpacity = 0.2, group = "ZCTAs",
              label = ~ZCTA5CE10,  # Add ZCTA label
              popup = ~paste("ZCTA:", ZCTA5CE10)) %>%
  addCircleMarkers(data = zip_data_sf,
                   radius = 4,
                   color = ~pal(ZCTA),
                   label = ~paste("ZIP Code:", Zip_Code),  # Add ZIP Code label
                   popup = ~paste("ZIP Code:", Zip_Code, "<br>ZCTA:", ZCTA, "<br>Status:", ZCTA_Status),
                   group = "Zip Codes") %>%
  addLayersControl(
    overlayGroups = c("ZCTAs", "Zip Codes"),
    options = layersControlOptions(collapsed = FALSE)
  )

# Save the interactive map to an .html file
map_output_path <- "Plots/VDSS_Zip_ZCTA_Crosswalk/VDSS_Zip_ZCTA_Crosswalk_Inconsistencies/VDSS_Zip_ZCTA_Crosswalk_Map.html"
saveWidget(map, map_output_path, selfcontained = TRUE)
