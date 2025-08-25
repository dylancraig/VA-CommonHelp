# Load necessary libraries
library(readxl)
library(dplyr)
library(writexl)

# Load the USPS data which is already filtered for VA ZIP codes
usps_data <- read_excel("Raw Data/VDSS_Historical_Zip_Code_Changes/USPS_Zip_County.xlsx", col_types = "text")

# Load the Q4 2010 and Q4 2016 datasets
data_2010_q4 <- read_excel("Raw Data/VDSS_Historical_Zip_Code_Changes/HUS_USPS_ZIP_CROSSWALK/ZIP_TRACT_122010.xlsx", col_types = "text")
data_2016_q4 <- read_excel("Raw Data/VDSS_Historical_Zip_Code_Changes/HUS_USPS_ZIP_CROSSWALK/ZIP_TRACT_122016.xlsx", col_types = "text")

# Filter for ZIP codes in Virginia using the USPS data
va_zips <- unique(usps_data$ZIP)

zips_2010_q4 <- unique(data_2010_q4$ZIP[data_2010_q4$ZIP %in% va_zips])
zips_2016_q4 <- unique(data_2016_q4$ZIP[data_2016_q4$ZIP %in% va_zips])

# Create the 2x2 table for comparison
in_2010_q4_not_2016_q4 <- length(setdiff(zips_2010_q4, zips_2016_q4))
in_2016_q4_not_2010_q4 <- length(setdiff(zips_2016_q4, zips_2010_q4))
in_both_2010_q4_and_2016_q4 <- length(intersect(zips_2010_q4, zips_2016_q4))

# Create a data frame for the 2x2 table
comparison_table <- data.frame(
  "2010_Q4" = c("Not in 2010_Q4", "In 2010_Q4"),
  "Not in 2016_Q4" = c(0, in_2010_q4_not_2016_q4),
  "In 2016_Q4" = c(in_2016_q4_not_2010_q4, in_both_2010_q4_and_2016_q4)
)

# Save the comparison table to the specified file path in R project format
write_xlsx(comparison_table, "Data Outputs/VDSS_Historical_Zip_Code_Changes/VA_HUD_USPS_Zip_Comparison_Q4_2010_Q4_2016.xlsx")

