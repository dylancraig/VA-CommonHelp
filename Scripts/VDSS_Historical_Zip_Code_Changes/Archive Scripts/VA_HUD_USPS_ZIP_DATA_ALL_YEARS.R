# Load necessary libraries
library(readxl)
library(dplyr)
library(writexl)
library(pbapply)
library(ggplot2)

# Function to generate year-quarter string
get_year_quarter <- function(year, quarter) {
  paste0("Q", quarter, " ", year)
}

# Initialize an empty list to store data frames
data_list <- list()

# Create a vector of years and quarters to loop over
years <- 2010:2024
quarters <- 1:4
total_iterations <- length(years) * length(quarters)

# Loop through years and quarters with a progress bar
pb <- txtProgressBar(min = 0, max = total_iterations, style = 3)
iteration <- 0

for (year in years) {
  for (quarter in quarters) {
    # Construct file path and year-quarter label
    quarter_string <- sprintf("%02d", quarter * 3)
    file_path <- paste0("Raw Data/VDSS_Historical_Zip_Type_Changes/HUD_USPS_ZIP_CROSSWALK/ZIP_TRACT_", quarter_string, year, ".xlsx")
    
    # Check if the file exists to avoid errors
    if (file.exists(file_path)) {
      year_quarter <- get_year_quarter(year, quarter)
      
      # Read the data and add the Year_Quarter column
      temp_data <- read_excel(file_path, col_types = "text") %>%
        mutate(Year_Quarter = year_quarter)
      
      # Append the data frame to the list
      data_list[[length(data_list) + 1]] <- temp_data
    }
    
    # Update progress bar
    iteration <- iteration + 1
    setTxtProgressBar(pb, iteration)
  }
}

# Close the progress bar
close(pb)

# Convert all column names to uppercase and ensure uniqueness in each dataframe
data_list <- lapply(data_list, function(df) {
  df <- df %>% rename_with(toupper)
  df <- df %>% rename_with(~ make.unique(.))
  return(df)
})

# Combine all dataframes into one
combined_zip_data <- bind_rows(data_list)

# Load the USPS data which is already filtered for VA ZIP codes
usps_data <- read_excel("Raw Data/VDSS_Historical_Zip_Type_Changes/USPS_Zip_County.xlsx", col_types = "text") %>%
  rename_with(toupper)

# Filter the combined data to include only VA ZIP codes
va_zip_data <- combined_zip_data %>% filter(ZIP %in% usps_data$ZIP)

# Convert ratio columns to numeric
va_zip_data <- va_zip_data %>%
  mutate(across(c(RES_RATIO, BUS_RATIO, OTH_RATIO, TOT_RATIO), as.numeric))

# Save the va_zip_data dataframe to an Excel file in the specified folder using R project format
write_xlsx(va_zip_data, "Data Outputs/VDSS_Historical_Zip_Type_Changes/VA_HUD_USPS_ZIP_DATA_ALL_YEARS.xlsx")
write_xlsx(va_zip_data, "Data Outputs/VDSS_Historical_Zip_Code_Changes/VA_HUD_USPS_zIP_DATA_ALL_YEARS.xlsx")