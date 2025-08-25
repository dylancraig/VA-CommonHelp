# Load necessary libraries
library(readxl)
library(dplyr)
library(writexl)
library(ggplot2)
library(zoo)

# Load the all-years dataset
combined_zip_data <- read_excel("Data Outputs/VDSS_Historical_Zip_Type_Changes/VA_HUD_USPS_zIP_DATA_ALL_YEARS.xlsx")

# Filter for unique instances of ZIP and YEAR_QUARTER
unique_va_zip_data <- combined_zip_data %>%
  select(ZIP, YEAR_QUARTER) %>%
  distinct()

# Count the number of unique ZIP codes per YEAR_QUARTER
zip_count_per_quarter <- unique_va_zip_data %>%
  group_by(YEAR_QUARTER) %>%
  summarise(NUMBER_OF_ZIPS = n())

# Separate YEAR_QUARTER into Year and Quarter for ordering
zip_count_per_quarter <- zip_count_per_quarter %>%
  separate(YEAR_QUARTER, into = c("QUARTER", "YEAR"), sep = " ") %>%
  mutate(YEAR = as.integer(YEAR),
         QUARTER = factor(QUARTER, levels = c("Q1", "Q2", "Q3", "Q4"))) %>%
  arrange(YEAR, QUARTER) %>%
  mutate(YEAR_QUARTER = paste(QUARTER, YEAR)) %>%
  select(YEAR_QUARTER, NUMBER_OF_ZIPS)

# Ensure YEAR_QUARTER is ordered correctly from first to last
zip_count_per_quarter$YEAR_QUARTER <- factor(zip_count_per_quarter$YEAR_QUARTER, levels = unique(zip_count_per_quarter$YEAR_QUARTER))

# Create the plot
zip_plot <- ggplot(zip_count_per_quarter, aes(x = YEAR_QUARTER, y = NUMBER_OF_ZIPS, group = 1)) +
  geom_line(color = "blue", size = 1) +
  geom_point(color = "red", size = 2) +
  theme_minimal() +
  labs(title = "Unique Zip Codes per Year Quarter",
       x = "Year Quarter",
       y = "Number of ZIP Codes") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

# Save the plot as a PNG file within the R project
ggsave("Plots/VDSS_Historical_Zip_Code_Changes/VA_HUD_USPS_ZIP_COUNT_YEAR_QUARTER.png", plot = zip_plot, width = 10, height = 6)

# Save the plot as a PDF file within the R project
ggsave("Plots/VDSS_Historical_Zip_Code_Changes/VA_HUD_USPS_ZIP_COUNT_YEAR_QUARTER.pdf", plot = zip_plot, width = 10, height = 6)
