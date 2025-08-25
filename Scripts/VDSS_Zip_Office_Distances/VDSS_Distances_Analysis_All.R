# Author: Dylan Craig
# Date Updated: 12/30/2024
# File Name: VDSS_Distances_Analysis_All.R

# Description: This script analyzes geospatial distance and time data from LDSS offices to ZIP codes. 
# It includes data transformations, summary statistics, histogram visualizations, missing data analysis, 
# and correlation coefficient calculations. Outputs are saved as Word documents and PDF plots.

# --- Load Required Libraries ------------------------------------------------
library(readxl)    # For reading Excel files
library(ggplot2)   # For creating plots
library(dplyr)     # For data manipulation
library(officer)   # For generating Word documents

# --- Load and Clean Data ----------------------------------------------------

# Define the file path (use forward slashes for cross-platform compatibility)
file_path <- "Data Outputs/VDSS_Zip_Office_Distances/VDSS_Office_Zip_GeoCode_All_Distances.xlsx"

# Load the dataset from Excel
data <- read_excel(file_path)

# Retain only relevant columns and remove duplicates
data <- data %>%
  select(
    HAVERSINE_DISTANCE_MILES,
    LDSS_COORDS,
    ZIP_COORDS,
    TRANSIT_DISTANCE_MILES,
    TRANSIT_TIME_MINUTES,
    DRIVING_DISTANCE_MILES,
    DRIVING_TIME_MINUTES
  ) %>%
  distinct()

# --- Summary Statistics -----------------------------------------------------

# Compute summary statistics for distances and times
summary_stats <- data.frame(
  Percentile = c("1st", "5th", "10th", "25th", "50th (Median)", "75th", "90th", "95th", "99th", "Max"),
  Haversine_Miles = round(quantile(data$HAVERSINE_DISTANCE_MILES, probs = c(0.01, 0.05, 0.10, 0.25, 0.5, 0.75, 0.90, 0.95, 0.99, 1), na.rm = TRUE), 2),
  Driving_Miles = round(quantile(data$DRIVING_DISTANCE_MILES, probs = c(0.01, 0.05, 0.10, 0.25, 0.5, 0.75, 0.90, 0.95, 0.99, 1), na.rm = TRUE), 2),
  Transit_Miles = round(quantile(data$TRANSIT_DISTANCE_MILES, probs = c(0.01, 0.05, 0.10, 0.25, 0.5, 0.75, 0.90, 0.95, 0.99, 1), na.rm = TRUE), 2),
  Driving_Minutes = round(quantile(data$DRIVING_TIME_MINUTES, probs = c(0.01, 0.05, 0.10, 0.25, 0.5, 0.75, 0.90, 0.95, 0.99, 1), na.rm = TRUE), 2),
  Transit_Minutes = round(quantile(data$TRANSIT_TIME_MINUTES, probs = c(0.01, 0.05, 0.10, 0.25, 0.5, 0.75, 0.90, 0.95, 0.99, 1), na.rm = TRUE), 2)
)

# Add observation count as a row to the summary statistics
n_obs <- data.frame(
  Percentile = "Number of Observations",
  Haversine_Miles = sum(!is.na(data$HAVERSINE_DISTANCE_MILES)),
  Driving_Miles = sum(!is.na(data$DRIVING_DISTANCE_MILES)),
  Transit_Miles = sum(!is.na(data$TRANSIT_DISTANCE_MILES)),
  Driving_Minutes = sum(!is.na(data$DRIVING_TIME_MINUTES)),
  Transit_Minutes = sum(!is.na(data$TRANSIT_TIME_MINUTES))
)
summary_stats <- rbind(summary_stats, n_obs)

# --- Save Summary Statistics to Word Document -------------------------------

output_path <- "Plots/VDSS_Zip_Office_Distances/Distance_Summary_Statistics_Miles.docx"

doc <- read_docx() %>%
  body_add_par("Summary Statistics for Distances and Times (in Miles and Minutes)", style = "heading 1") %>%
  body_add_table(value = summary_stats, style = "table_template") %>%
  body_add_par("", style = "Normal") # Spacer

print(doc, target = output_path)
cat("Word document generated and saved at:", output_path, "\n")

# --- Plot Distribution Histograms -------------------------------------------

# Define folder path for saving plots
output_folder <- "Plots/VDSS_Zip_Office_Distances"

# Define function for histogram plots
add_frequencies <- function(plot_data, x_var, binwidth) {
  ggplot(plot_data, aes(x = !!sym(x_var))) +
    geom_histogram(binwidth = binwidth, fill = "blue", alpha = 0.5, color = "black", size = 1, boundary = 0) +
    labs(x = "Distance (Miles)", y = "Frequency") +
    theme_minimal()
}

# Save histograms for each metric as PDF
metrics <- c("HAVERSINE_DISTANCE_MILES", "DRIVING_DISTANCE_MILES", "TRANSIT_DISTANCE_MILES",
             "DRIVING_TIME_MINUTES", "TRANSIT_TIME_MINUTES")
titles <- c("Haversine Distance Distribution", "Driving Distance Distribution", 
            "Transit Distance Distribution", "Driving Time Distribution", "Transit Time Distribution")
x_labels <- c("Distance (Miles)", "Distance (Miles)", "Distance (Miles)", "Time (Minutes)", "Time (Minutes)")

for (i in seq_along(metrics)) {
  pdf(file = file.path(output_folder, paste0(titles[i], ".pdf")), width = 8, height = 6)
  add_frequencies(data, metrics[i], 5) +
    labs(title = titles[i], x = x_labels[i])
  dev.off()
}

# --- Missing Data Analysis --------------------------------------------------

# Analyze missing transit distances
missing_transit <- mean(is.na(data$TRANSIT_DISTANCE_MILES)) * 100
cat("Proportion of missing transit distances:", missing_transit, "%\n")

# Compare rows with and without missing transit distances
comparison <- data %>%
  mutate(Missing_Transit = ifelse(is.na(TRANSIT_DISTANCE_MILES), "Missing", "Not Missing")) %>%
  group_by(Missing_Transit) %>%
  summarise(
    Avg_Haversine = mean(HAVERSINE_DISTANCE_MILES, na.rm = TRUE),
    Avg_Driving = mean(DRIVING_DISTANCE_MILES, na.rm = TRUE),
    Count = n()
  )

# Save missing data analysis to Word
missing_data_doc_path <- "Plots/VDSS_Zip_Office_Distances/Missing_Data_Analysis.docx"
missing_data_doc <- read_docx() %>%
  body_add_par("Missing Data Analysis", style = "heading 1") %>%
  body_add_par(paste("Proportion of missing transit distances:", round(missing_transit, 2), "%"), style = "Normal") %>%
  body_add_par("Comparison of Rows with and without Missing Transit Distances", style = "heading 2") %>%
  body_add_table(value = comparison, style = "table_template")

print(missing_data_doc, target = missing_data_doc_path)
cat("Word document for missing data analysis saved at:", missing_data_doc_path, "\n")

# --- Correlation Matrix -----------------------------------------------------

# Compute correlation coefficients between distances and times
correlation_data <- data %>%
  select(
    HAVERSINE_DISTANCE_MILES,
    DRIVING_DISTANCE_MILES,
    TRANSIT_DISTANCE_MILES,
    DRIVING_TIME_MINUTES,
    TRANSIT_TIME_MINUTES
  )

correlation_matrix <- round(cor(correlation_data, use = "pairwise.complete.obs"), 2)

# Rename variables for clarity
short_names <- c(
  HAVERSINE_DISTANCE_MILES = "Haversine (mi)",
  DRIVING_DISTANCE_MILES = "Driving (mi)",
  TRANSIT_DISTANCE_MILES = "Transit (mi)",
  DRIVING_TIME_MINUTES = "Driving (min)",
  TRANSIT_TIME_MINUTES = "Transit (min)"
)
colnames(correlation_matrix) <- short_names
rownames(correlation_matrix) <- short_names

# Save correlation matrix to Word
correlation_doc_path <- "Plots/VDSS_Zip_Office_Distances/Correlation_Matrix.docx"
correlation_doc <- read_docx() %>%
  body_add_par("Correlation Coefficients", style = "heading 1") %>%
  body_add_table(value = as.data.frame(correlation_matrix)) %>%
  body_add_par("", style = "Normal")

print(correlation_doc, target = correlation_doc_path)
cat("Word document with correlation matrix saved at:", correlation_doc_path, "\n")

# --- Compute Averages and Missing Shares by Haversine Ventiles ----------------

# Create a column for Haversine ventiles (1-20)
data <- data %>%
  mutate(
    HAVERSINE_VENTILE = ntile(HAVERSINE_DISTANCE_MILES, 20)
  )

# Compute averages and missing shares conditional on Haversine ventiles
ventile_analysis <- data %>%
  group_by(HAVERSINE_VENTILE) %>%
  summarise(
    `Avg Haversine Miles` = round(mean(HAVERSINE_DISTANCE_MILES, na.rm = TRUE), 2),
    `Avg Driving Miles` = round(mean(DRIVING_DISTANCE_MILES, na.rm = TRUE), 2),
    `Avg Transit Miles` = round(mean(TRANSIT_DISTANCE_MILES, na.rm = TRUE), 2),
    `Avg Driving Minutes` = round(mean(DRIVING_TIME_MINUTES, na.rm = TRUE), 2),
    `Avg Transit Minutes` = round(mean(TRANSIT_TIME_MINUTES, na.rm = TRUE), 2),
    `Share Missing Driving Miles (%)` = round(mean(is.na(DRIVING_DISTANCE_MILES)) * 100, 2),
    `Share Missing Transit Miles (%)` = round(mean(is.na(TRANSIT_DISTANCE_MILES)) * 100, 2),
    `Share Missing Driving Minutes (%)` = round(mean(is.na(DRIVING_TIME_MINUTES)) * 100, 2),
    `Share Missing Transit Minutes (%)` = round(mean(is.na(TRANSIT_TIME_MINUTES)) * 100, 2)
  )

# Save ventile analysis results to Word document
ventile_analysis_doc_path <- "Plots/VDSS_Zip_Office_Distances/Haversine_Ventile_Analysis.docx"
ventile_analysis_doc <- read_docx() %>%
  body_add_par("Haversine Ventile Analysis", style = "heading 1") %>%
  body_add_table(value = ventile_analysis, style = "table_template") %>%
  body_add_par("", style = "Normal")

print(ventile_analysis_doc, target = ventile_analysis_doc_path)
cat("Word document for Haversine ventile analysis saved at:", ventile_analysis_doc_path, "\n")