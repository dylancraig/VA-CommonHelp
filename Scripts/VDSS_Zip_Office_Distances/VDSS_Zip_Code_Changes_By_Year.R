# Author: Dylan Craig
# Data Updated: 12/29/2024
# File Name: VDSS_Zip_Code_Changes_By_Year.R

# Description: This script analyzes and visualizes changes in 5-digit ZIP codes by locality over time. 
# It identifies the first instance of ZIP code changes, aggregates the number of changes by year, 
# and generates a bar plot. The output is saved as a PDF.

# --- Load Required Libraries ------------------------------------------------
library(readxl)    # For reading Excel files
library(dplyr)     # For data manipulation
library(ggplot2)   # For creating plots
library(stringr)   # For string operations
library(lubridate) # For date manipulation
library(tidyr)     # For handling missing data

# --- Load and Prepare Data --------------------------------------------------

# Define the file path for the input dataset
file_path <- "Raw Data/VDSS_Zip_Office_Distances/VDSS_Offices_All.xlsx"

# Load the data from the Excel file
data <- read_excel(file_path)

# Extract the 5-digit ZIP code from the full ZIP code
data <- data %>%
  mutate(ZIP_5_DIGIT = str_sub(ZIP_VDSS, 1, 5))

# Arrange data by locality and the date of change for clarity
data <- data %>%
  arrange(LOCALITY, DATE_CHANGE)

# --- Identify ZIP Code Changes ----------------------------------------------

# Detect the first instance of a ZIP code change within each locality
data <- data %>%
  group_by(LOCALITY) %>%
  mutate(ZIP_CHANGE = ZIP_5_DIGIT != lag(ZIP_5_DIGIT, default = first(ZIP_5_DIGIT))) %>%
  ungroup()

# --- Count ZIP Code Changes by Year -----------------------------------------

# Count the number of localities with ZIP code changes by year
changes_by_year <- data %>%
  filter(ZIP_CHANGE == TRUE) %>%
  group_by(year = year(DATE_CHANGE)) %>%
  summarise(Num_Changes = n()) %>%
  arrange(year)

# Create a complete sequence of years from the minimum to maximum year
all_years <- tibble(year = seq(min(year(data$DATE_CHANGE)), max(year(data$DATE_CHANGE)), by = 1))

# Ensure all years are represented, even those with no ZIP code changes
changes_by_year <- left_join(all_years, changes_by_year, by = "year") %>%
  replace_na(list(Num_Changes = 0))  # Replace NA with 0 for missing years

# --- Generate and Save Bar Plot ---------------------------------------------

# Create a bar plot showing the number of ZIP code changes by year
plot <- ggplot(changes_by_year, aes(x = factor(year), y = Num_Changes)) +
  geom_bar(stat = "identity", fill = "blue") +
  geom_text(aes(label = Num_Changes), vjust = -0.5) +  # Add numbers above bars
  labs(
    title = "Number of 5-Digit ZIP Code Changes by Year",
    x = "Year",
    y = "Number of Localities with ZIP Code Changes"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for readability

# Display the plot
print(plot)

# Save the plot as a PDF in the specified folder
output_path <- "Plots/VDSS_Zip_Office_Distances/VDSS_Zip_Code_Changes_By_Year.pdf"
ggsave(output_path, plot)
cat("Bar plot saved to:", output_path, "\n")

