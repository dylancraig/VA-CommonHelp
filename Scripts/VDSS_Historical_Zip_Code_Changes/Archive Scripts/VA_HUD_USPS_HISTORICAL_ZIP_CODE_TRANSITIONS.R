# Load necessary libraries
library(readxl)
library(dplyr)
library(tidyr)
library(writexl)

# Load the dataset
zip_data <- read_excel("Data Outputs/VDSS_Historical_Zip_Type_Changes/VA_HUD_USPS_Zip_Data_Collapsed.xlsx")

# Extract year and quarter components from YEAR_QUARTER
zip_data <- zip_data %>%
  mutate(
    YEAR = as.numeric(substr(YEAR_QUARTER, 4, 7)),
    QUARTER = as.numeric(substr(YEAR_QUARTER, 2, 2))
  )

# Sort the dataset by year and quarter
zip_data <- zip_data %>%
  arrange(YEAR, QUARTER)

# Identify all unique ZIP codes and sorted YEAR_QUARTER values
unique_zips <- unique(zip_data$ZIP)
all_year_quarters <- unique(zip_data$YEAR_QUARTER)

# Create a dataframe with every unique ZIP code and a column for each YEAR_QUARTER
zip_presence_df <- data.frame(ZIP = unique_zips)

# For each YEAR_QUARTER, add a column to the dataframe indicating MISSING or PRESENT
for (quarter in all_year_quarters) {
  zip_presence_df[[quarter]] <- ifelse(zip_presence_df$ZIP %in% zip_data$ZIP[zip_data$YEAR_QUARTER == quarter], "PRESENT", "MISSING")
}

# Function to count appearances and disappearances
count_transitions <- function(row) {
  status_changes <- diff(match(row[-1], c("MISSING", "PRESENT")))
  appearances <- sum(status_changes == 1)  # MISSING to PRESENT
  disappearances <- sum(status_changes == -1)  # PRESENT to MISSING
  return(c(appearances, disappearances))
}

# Apply the transition counting function to each ZIP code
transition_counts <- t(apply(zip_presence_df[-1], 1, count_transitions))
colnames(transition_counts) <- c("Appearances", "Disappearances")

# Combine the counts with the ZIP code dataframe
zip_presence_df <- cbind(zip_presence_df, transition_counts)

# Count the number of ZIP codes that appear and disappear 0, 1, 2, 3, 4 times
appearance_count <- as.data.frame(table(zip_presence_df$Appearances))
disappearance_count <- as.data.frame(table(zip_presence_df$Disappearances))

# Combine the counts into a single dataframe
transition_summary <- data.frame(
  Times = 0:4,
  Appearances = appearance_count$Freq[match(0:4, appearance_count$Var1)],
  Disappearances = disappearance_count$Freq[match(0:4, disappearance_count$Var1)]
)

# Replace NA with 0 for cases where there are no ZIP codes with a certain count
transition_summary[is.na(transition_summary)] <- 0

# Save the results
write_xlsx(zip_presence_df, "Data Outputs/VDSS_Historical_Zip_Code_Changes/VA_HUD_USPS_Zip_Appearance_Disappearance_Counts.xlsx")
write_xlsx(transition_summary, "Data Outputs/VDSS_Historical_Zip_Code_Changes/VA_HUD_USPS_Zip_Appearance_Disappearance_Summary.xlsx")

# Print the transition summary
print(transition_summary)
