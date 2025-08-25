# Author: Dylan Craig ---------------------------------------------------------
# Date Last Updated: 12/28/2024 -----------------------------------------------
# Purpose: Process and aggregate ACS data from multiple sources into a unified dataset ---
# File Name: VDSS_ZCTA_ACS_Characteristics.R ----------------------------------

# Load Necessary Libraries ---------------------------------------------------
library(dplyr)        # Data manipulation
library(readr)        # Reading CSV files
library(stringr)      # String manipulation
library(writexl)      # Writing to Excel
library(tidyverse)    # Comprehensive data manipulation and visualization

# Set options to avoid scientific notation
options(scipen = 999)

# Load the dataset using R project-relative path
file_path <- "Data Outputs/VDSS_ZCTA_ACS_Characteristics/VDSS_ZCTA_ACS_Characteristics_Aggregated.csv"
data <- read_csv(file_path)

# 1. Population --------------------------------------------------------
population_data <- data %>%
  select(YEAR, ZCTA, `POPULATION_Estimate Total`) %>%
  rename(Total_Population = `POPULATION_Estimate Total`)


# 2. Employment ------------------------------------------------
# Filter 2010-2012 data
employment_industry_2010_2012 <- data %>%
  filter(YEAR %in% c(2010, 2011, 2012)) %>%
  select(
    YEAR, ZCTA,
    
    # Employment-related variables (2010-2012)
    `EMPLOYMENT_Estimate EMPLOYMENT STATUS Population 16 years and over`,
    `EMPLOYMENT_Estimate EMPLOYMENT STATUS In labor force Civilian labor force Employed...907`,
    `EMPLOYMENT_Estimate EMPLOYMENT STATUS In labor force Civilian labor force Unemployed`,
    `EMPLOYMENT_Estimate EMPLOYMENT STATUS In labor force Armed Forces`,
    `EMPLOYMENT_Estimate EMPLOYMENT STATUS Not in labor force`,
    
    # Industry-related variables (2010-2012)
    `EMPLOYMENT_Estimate INDUSTRY Civilian employed population 16 years and over`,
    `EMPLOYMENT_Estimate INDUSTRY Agriculture, forestry, fishing and hunting, and mining`,
    `EMPLOYMENT_Estimate INDUSTRY Construction`,
    `EMPLOYMENT_Estimate INDUSTRY Manufacturing`,
    `EMPLOYMENT_Estimate INDUSTRY Wholesale trade`,
    `EMPLOYMENT_Estimate INDUSTRY Retail trade`,
    `EMPLOYMENT_Estimate INDUSTRY Transportation and warehousing, and utilities`,
    `EMPLOYMENT_Estimate INDUSTRY Information`,
    `EMPLOYMENT_Estimate INDUSTRY Finance and insurance, and real estate and rental and leasing`,
    `EMPLOYMENT_Estimate INDUSTRY Professional, scientific, and management, and administrative and waste management services`,
    `EMPLOYMENT_Estimate INDUSTRY Educational services, and health care and social assistance`,
    `EMPLOYMENT_Estimate INDUSTRY Arts, entertainment, and recreation, and accommodation and food services`,
    `EMPLOYMENT_Estimate INDUSTRY Other services, except public administration`,
    `EMPLOYMENT_Estimate INDUSTRY Public administration`
  )

# Filter 2013-2022 data and include the two missing variables
employment_industry_2013_2022 <- data %>%
  filter(YEAR >= 2013 & YEAR <= 2022) %>%
  select(
    YEAR, ZCTA,
    
    # Employment-related variables (2013-2022)
    `EMPLOYMENT_Estimate EMPLOYMENT STATUS Population 16 years and over`,  # Add this variable
    `EMPLOYMENT_Estimate EMPLOYMENT STATUS Population 16 years and over In labor force Civilian labor force Employed`,
    `EMPLOYMENT_Estimate EMPLOYMENT STATUS Population 16 years and over In labor force Civilian labor force Unemployed`,
    `EMPLOYMENT_Estimate EMPLOYMENT STATUS Population 16 years and over In labor force Armed Forces`,
    `EMPLOYMENT_Estimate EMPLOYMENT STATUS Population 16 years and over Not in labor force`,
    
    # Industry-related variables (2013-2022)
    `EMPLOYMENT_Estimate INDUSTRY Civilian employed population 16 years and over`,  # Add this variable
    `EMPLOYMENT_Estimate INDUSTRY Civilian employed population 16 years and over Agriculture, forestry, fishing and hunting, and mining`,
    `EMPLOYMENT_Estimate INDUSTRY Civilian employed population 16 years and over Construction`,
    `EMPLOYMENT_Estimate INDUSTRY Civilian employed population 16 years and over Manufacturing`,
    `EMPLOYMENT_Estimate INDUSTRY Civilian employed population 16 years and over Wholesale trade`,
    `EMPLOYMENT_Estimate INDUSTRY Civilian employed population 16 years and over Retail trade`,
    `EMPLOYMENT_Estimate INDUSTRY Civilian employed population 16 years and over Transportation and warehousing, and utilities`,
    `EMPLOYMENT_Estimate INDUSTRY Civilian employed population 16 years and over Information`,
    `EMPLOYMENT_Estimate INDUSTRY Civilian employed population 16 years and over Finance and insurance, and real estate and rental and leasing`,
    `EMPLOYMENT_Estimate INDUSTRY Civilian employed population 16 years and over Professional, scientific, and management, and administrative and waste management services`,
    `EMPLOYMENT_Estimate INDUSTRY Civilian employed population 16 years and over Educational services, and health care and social assistance`,
    `EMPLOYMENT_Estimate INDUSTRY Civilian employed population 16 years and over Arts, entertainment, and recreation, and accommodation and food services`,
    `EMPLOYMENT_Estimate INDUSTRY Civilian employed population 16 years and over Other services, except public administration`,
    `EMPLOYMENT_Estimate INDUSTRY Civilian employed population 16 years and over Public administration`
  )

# Now, rename and append as before
employment_industry_2013_2022_renamed <- employment_industry_2013_2022 %>%
  rename(
    `EMPLOYMENT_Estimate EMPLOYMENT STATUS In labor force Civilian labor force Employed...907` = `EMPLOYMENT_Estimate EMPLOYMENT STATUS Population 16 years and over In labor force Civilian labor force Employed`,
    `EMPLOYMENT_Estimate EMPLOYMENT STATUS In labor force Civilian labor force Unemployed` = `EMPLOYMENT_Estimate EMPLOYMENT STATUS Population 16 years and over In labor force Civilian labor force Unemployed`,
    `EMPLOYMENT_Estimate EMPLOYMENT STATUS In labor force Armed Forces` = `EMPLOYMENT_Estimate EMPLOYMENT STATUS Population 16 years and over In labor force Armed Forces`,
    `EMPLOYMENT_Estimate EMPLOYMENT STATUS Not in labor force` = `EMPLOYMENT_Estimate EMPLOYMENT STATUS Population 16 years and over Not in labor force`,
    `EMPLOYMENT_Estimate INDUSTRY Agriculture, forestry, fishing and hunting, and mining` = `EMPLOYMENT_Estimate INDUSTRY Civilian employed population 16 years and over Agriculture, forestry, fishing and hunting, and mining`,
    `EMPLOYMENT_Estimate INDUSTRY Construction` = `EMPLOYMENT_Estimate INDUSTRY Civilian employed population 16 years and over Construction`,
    `EMPLOYMENT_Estimate INDUSTRY Manufacturing` = `EMPLOYMENT_Estimate INDUSTRY Civilian employed population 16 years and over Manufacturing`,
    `EMPLOYMENT_Estimate INDUSTRY Wholesale trade` = `EMPLOYMENT_Estimate INDUSTRY Civilian employed population 16 years and over Wholesale trade`,
    `EMPLOYMENT_Estimate INDUSTRY Retail trade` = `EMPLOYMENT_Estimate INDUSTRY Civilian employed population 16 years and over Retail trade`,
    `EMPLOYMENT_Estimate INDUSTRY Transportation and warehousing, and utilities` = `EMPLOYMENT_Estimate INDUSTRY Civilian employed population 16 years and over Transportation and warehousing, and utilities`,
    `EMPLOYMENT_Estimate INDUSTRY Information` = `EMPLOYMENT_Estimate INDUSTRY Civilian employed population 16 years and over Information`,
    `EMPLOYMENT_Estimate INDUSTRY Finance and insurance, and real estate and rental and leasing` = `EMPLOYMENT_Estimate INDUSTRY Civilian employed population 16 years and over Finance and insurance, and real estate and rental and leasing`,
    `EMPLOYMENT_Estimate INDUSTRY Professional, scientific, and management, and administrative and waste management services` = `EMPLOYMENT_Estimate INDUSTRY Civilian employed population 16 years and over Professional, scientific, and management, and administrative and waste management services`,
    `EMPLOYMENT_Estimate INDUSTRY Educational services, and health care and social assistance` = `EMPLOYMENT_Estimate INDUSTRY Civilian employed population 16 years and over Educational services, and health care and social assistance`,
    `EMPLOYMENT_Estimate INDUSTRY Arts, entertainment, and recreation, and accommodation and food services` = `EMPLOYMENT_Estimate INDUSTRY Civilian employed population 16 years and over Arts, entertainment, and recreation, and accommodation and food services`,
    `EMPLOYMENT_Estimate INDUSTRY Other services, except public administration` = `EMPLOYMENT_Estimate INDUSTRY Civilian employed population 16 years and over Other services, except public administration`,
    `EMPLOYMENT_Estimate INDUSTRY Public administration` = `EMPLOYMENT_Estimate INDUSTRY Civilian employed population 16 years and over Public administration`
  )

# Append the two datasets vertically (stack them on top of each other)
employment_industry_data <- bind_rows(employment_industry_2010_2012, employment_industry_2013_2022_renamed)

# Now, calculate the labor force at the very end
employment_industry_data <- employment_industry_data %>%
  mutate(`EMPLOYMENT_Estimate EMPLOYMENT STATUS In labor force` = `EMPLOYMENT_Estimate EMPLOYMENT STATUS In labor force Civilian labor force Employed` +
      `EMPLOYMENT_Estimate EMPLOYMENT STATUS In labor force Armed Forces` +
      `EMPLOYMENT_Estimate EMPLOYMENT STATUS In labor force Civilian labor force Unemployed`
  )

# 3. Age Groups -----------------------------------------------------------
age_data <- data %>%
  mutate(
    Age_0_4 = rowSums(select(., 
                             `SEX_Estimate Total Male Under 5 years`, 
                             `SEX_Estimate Total Female Under 5 years`), na.rm = TRUE),
    
    Age_5_9 = rowSums(select(., 
                             `SEX_Estimate Total Male 5 to 9 years`, 
                             `SEX_Estimate Total Female 5 to 9 years`), na.rm = TRUE),
    
    Age_10_17 = rowSums(select(., 
                               `SEX_Estimate Total Male 10 to 14 years`, 
                               `SEX_Estimate Total Male 15 to 17 years`,
                               `SEX_Estimate Total Female 10 to 14 years`, 
                               `SEX_Estimate Total Female 15 to 17 years`), na.rm = TRUE),
    
    Age_18_24 = rowSums(select(., 
                               `SEX_Estimate Total Male 18 and 19 years`, 
                               `SEX_Estimate Total Male 20 years`, 
                               `SEX_Estimate Total Male 21 years`, 
                               `SEX_Estimate Total Male 22 to 24 years`,
                               `SEX_Estimate Total Female 18 and 19 years`, 
                               `SEX_Estimate Total Female 20 years`, 
                               `SEX_Estimate Total Female 21 years`, 
                               `SEX_Estimate Total Female 22 to 24 years`), na.rm = TRUE),
    
    Age_25_34 = rowSums(select(., 
                               `SEX_Estimate Total Male 25 to 29 years`, 
                               `SEX_Estimate Total Male 30 to 34 years`,
                               `SEX_Estimate Total Female 25 to 29 years`, 
                               `SEX_Estimate Total Female 30 to 34 years`), na.rm = TRUE),
    
    Age_35_44 = rowSums(select(., 
                               `SEX_Estimate Total Male 35 to 39 years`, 
                               `SEX_Estimate Total Male 40 to 44 years`,
                               `SEX_Estimate Total Female 35 to 39 years`, 
                               `SEX_Estimate Total Female 40 to 44 years`), na.rm = TRUE),
    
    Age_45_54 = rowSums(select(., 
                               `SEX_Estimate Total Male 45 to 49 years`, 
                               `SEX_Estimate Total Male 50 to 54 years`,
                               `SEX_Estimate Total Female 45 to 49 years`, 
                               `SEX_Estimate Total Female 50 to 54 years`), na.rm = TRUE),
    
    Age_55_64 = rowSums(select(., 
                               `SEX_Estimate Total Male 55 to 59 years`, 
                               `SEX_Estimate Total Male 60 and 61 years`, 
                               `SEX_Estimate Total Male 62 to 64 years`,
                               `SEX_Estimate Total Female 55 to 59 years`, 
                               `SEX_Estimate Total Female 60 and 61 years`, 
                               `SEX_Estimate Total Female 62 to 64 years`), na.rm = TRUE),
    
    Age_65_Plus = rowSums(select(., 
                                 `SEX_Estimate Total Male 65 and 66 years`, 
                                 `SEX_Estimate Total Male 67 to 69 years`, 
                                 `SEX_Estimate Total Male 70 to 74 years`, 
                                 `SEX_Estimate Total Male 75 to 79 years`, 
                                 `SEX_Estimate Total Male 80 to 84 years`, 
                                 `SEX_Estimate Total Male 85 years and over`,
                                 `SEX_Estimate Total Female 65 and 66 years`, 
                                 `SEX_Estimate Total Female 67 to 69 years`, 
                                 `SEX_Estimate Total Female 70 to 74 years`, 
                                 `SEX_Estimate Total Female 75 to 79 years`, 
                                 `SEX_Estimate Total Female 80 to 84 years`, 
                                 `SEX_Estimate Total Female 85 years and over`), na.rm = TRUE),
    
    # New variable for child population (ages 0 to 17)
   Age_0_17 = Age_0_4 + Age_5_9 + Age_10_17
  ) %>%
  # Select relevant columns including total population for age
  select(YEAR, ZCTA, Age_Population = `SEX_Estimate Total`, Age_0_4, Age_5_9, Age_10_17, Age_0_17, Age_18_24, Age_25_34, Age_35_44, Age_45_54, Age_55_64, Age_65_Plus)

# 4. Median Income -----------------------------------------------------
median_income_data <- data %>%
  select(YEAR, ZCTA, `INCOME_Estimate Median household income in the past 12 months (in 2011 inflation-adjusted dollars)`) %>%
  rename(Median_Income = `INCOME_Estimate Median household income in the past 12 months (in 2011 inflation-adjusted dollars)`)

# 5. Race and Ethnicity ------------------------------------------------
race_ethnicity_data <- data %>%
  select(YEAR, ZCTA, 
         `ETHNICITY_Estimate Total Hispanic or Latino`, 
         `ETHNICITY_Estimate Total Not Hispanic or Latino White alone`,
         `ETHNICITY_Estimate Total Not Hispanic or Latino Black or African American alone`,
         `ETHNICITY_Estimate Total Not Hispanic or Latino Asian alone`,
         `ETHNICITY_Estimate Total Not Hispanic or Latino American Indian and Alaska Native alone`,
         `ETHNICITY_Estimate Total Not Hispanic or Latino Native Hawaiian and Other Pacific Islander alone`,
         `ETHNICITY_Estimate Total Not Hispanic or Latino Some other race alone`,
         `ETHNICITY_Estimate Total Not Hispanic or Latino Two or more races`,
         `ETHNICITY_Estimate Total`) %>%
  
  # Convert selected columns to numeric to avoid errors with rowSums
  mutate(across(c(`ETHNICITY_Estimate Total Not Hispanic or Latino American Indian and Alaska Native alone`,
                  `ETHNICITY_Estimate Total Not Hispanic or Latino Native Hawaiian and Other Pacific Islander alone`,
                  `ETHNICITY_Estimate Total Not Hispanic or Latino Some other race alone`,
                  `ETHNICITY_Estimate Total Not Hispanic or Latino Two or more races`), as.numeric)) %>%
  
  # Create a new column for "Other non-Hispanic" by summing less common race categories
  mutate(Other_non_Hispanic = rowSums(select(., 
                                             `ETHNICITY_Estimate Total Not Hispanic or Latino American Indian and Alaska Native alone`,
                                             `ETHNICITY_Estimate Total Not Hispanic or Latino Native Hawaiian and Other Pacific Islander alone`,
                                             `ETHNICITY_Estimate Total Not Hispanic or Latino Some other race alone`,
                                             `ETHNICITY_Estimate Total Not Hispanic or Latino Two or more races`), na.rm = TRUE)) %>%
  
  # Rename the columns to create individual columns for each group
  rename(Hispanic = `ETHNICITY_Estimate Total Hispanic or Latino`,
         White_non_Hispanic = `ETHNICITY_Estimate Total Not Hispanic or Latino White alone`,
         Black_non_Hispanic = `ETHNICITY_Estimate Total Not Hispanic or Latino Black or African American alone`,
         Asian_non_Hispanic = `ETHNICITY_Estimate Total Not Hispanic or Latino Asian alone`,
         Race_Population = `ETHNICITY_Estimate Total`) %>%
  
  # Select only the relevant columns
  select(YEAR, ZCTA, Hispanic, White_non_Hispanic, Black_non_Hispanic, Asian_non_Hispanic, Other_non_Hispanic, Race_Population)

# 6. Sex ---------------------------------------------------------------
sex_data <- data %>%
  select(YEAR, ZCTA, `SEX_Estimate Total Male`, `SEX_Estimate Total Female`, `SEX_Estimate Total`) %>%
  rename(Sex_Population = `SEX_Estimate Total`,
         Male = `SEX_Estimate Total Male`,
         Female = `SEX_Estimate Total Female`)

# 7. Poverty --------------------------------------------------------
poverty_data <- data %>%
  # Select columns for total poverty and poverty by age/sex
  select(YEAR, ZCTA, 
         `POVERTY_Estimate Total Income in the past 12 months below poverty level`, 
         `POVERTY_Estimate Total`,
         # Male age groups
         `POVERTY_Estimate Total Income in the past 12 months below poverty level Male Under 5 years`,
         `POVERTY_Estimate Total Income in the past 12 months below poverty level Male 5 years`,
         `POVERTY_Estimate Total Income in the past 12 months below poverty level Male 6 to 11 years`,
         `POVERTY_Estimate Total Income in the past 12 months below poverty level Male 12 to 14 years`,
         `POVERTY_Estimate Total Income in the past 12 months below poverty level Male 15 years`,
         `POVERTY_Estimate Total Income in the past 12 months below poverty level Male 16 and 17 years`,
         # Female age groups
         `POVERTY_Estimate Total Income in the past 12 months below poverty level Female Under 5 years`,
         `POVERTY_Estimate Total Income in the past 12 months below poverty level Female 5 years`,
         `POVERTY_Estimate Total Income in the past 12 months below poverty level Female 6 to 11 years`,
         `POVERTY_Estimate Total Income in the past 12 months below poverty level Female 12 to 14 years`,
         `POVERTY_Estimate Total Income in the past 12 months below poverty level Female 15 years`,
         `POVERTY_Estimate Total Income in the past 12 months below poverty level Female 16 and 17 years`) %>%
  
  # Create the child poverty count by summing relevant age groups
  mutate(Child_Poverty_Count = rowSums(select(., 
                                              `POVERTY_Estimate Total Income in the past 12 months below poverty level Male Under 5 years`,
                                              `POVERTY_Estimate Total Income in the past 12 months below poverty level Male 5 years`,
                                              `POVERTY_Estimate Total Income in the past 12 months below poverty level Male 6 to 11 years`,
                                              `POVERTY_Estimate Total Income in the past 12 months below poverty level Male 12 to 14 years`,
                                              `POVERTY_Estimate Total Income in the past 12 months below poverty level Male 15 years`,
                                              `POVERTY_Estimate Total Income in the past 12 months below poverty level Male 16 and 17 years`,
                                              `POVERTY_Estimate Total Income in the past 12 months below poverty level Female Under 5 years`,
                                              `POVERTY_Estimate Total Income in the past 12 months below poverty level Female 5 years`,
                                              `POVERTY_Estimate Total Income in the past 12 months below poverty level Female 6 to 11 years`,
                                              `POVERTY_Estimate Total Income in the past 12 months below poverty level Female 12 to 14 years`,
                                              `POVERTY_Estimate Total Income in the past 12 months below poverty level Female 15 years`,
                                              `POVERTY_Estimate Total Income in the past 12 months below poverty level Female 16 and 17 years`), 
                                       na.rm = TRUE)) %>%
  
  # Rename the total poverty columns
  rename(Poverty_Count = `POVERTY_Estimate Total Income in the past 12 months below poverty level`,
         Poverty_Population = `POVERTY_Estimate Total`) %>%
  
  # Create poverty rates for total and child poverty
  mutate(Poverty_Rate = Poverty_Count / Poverty_Population,
         Child_Poverty_Rate = Child_Poverty_Count / age_data$Age_0_17) %>%
  
  # Select only the desired columns for the final output
  select(YEAR, ZCTA, Poverty_Count, Poverty_Population, Poverty_Rate, Child_Poverty_Rate)

# 8. Education ---------------------------------------------------------
education_data <- data %>%
  mutate(
    # No High School (less than 9th grade + 9th to 12th grade, no diploma)
    No_HS = rowSums(select(., 
                           starts_with("EDUCATION_Estimate Total Male") & ends_with("Less than 9th grade"), 
                           starts_with("EDUCATION_Estimate Total Male") & ends_with("9th to 12th grade, no diploma"),
                           starts_with("EDUCATION_Estimate Total Female") & ends_with("Less than 9th grade"),
                           starts_with("EDUCATION_Estimate Total Female") & ends_with("9th to 12th grade, no diploma")), na.rm = TRUE),
    
    # High School Graduate (High school graduate, GED, or alternative)
    HS_Graduate = rowSums(select(., 
                                 starts_with("EDUCATION_Estimate Total Male") & ends_with("High school graduate, GED, or alternative"),
                                 starts_with("EDUCATION_Estimate Total Female") & ends_with("High school graduate, GED, or alternative")), na.rm = TRUE),
    
    # Some College (Some college, no degree + Associate's degree)
    Some_College = rowSums(select(., 
                                  starts_with("EDUCATION_Estimate Total Male") & ends_with("Some college, no degree"),
                                  starts_with("EDUCATION_Estimate Total Male") & ends_with("Associate's degree"),
                                  starts_with("EDUCATION_Estimate Total Female") & ends_with("Some college, no degree"),
                                  starts_with("EDUCATION_Estimate Total Female") & ends_with("Associate's degree")), na.rm = TRUE),
    
    # College Graduate (Bachelor's degree + Graduate or professional degree)
    College_Graduate = rowSums(select(., 
                                      starts_with("EDUCATION_Estimate Total Male") & ends_with("Bachelor's degree"),
                                      starts_with("EDUCATION_Estimate Total Male") & ends_with("Graduate or professional degree"),
                                      starts_with("EDUCATION_Estimate Total Female") & ends_with("Bachelor's degree"),
                                      starts_with("EDUCATION_Estimate Total Female") & ends_with("Graduate or professional degree")), na.rm = TRUE),
    
    # Use the total population from the education estimate
    Education_Population = `EDUCATION_Estimate Total`
  ) %>%
  
  # Select the relevant columns for output
  select(YEAR, ZCTA, No_HS, HS_Graduate, Some_College, College_Graduate, Education_Population)

# 9. Marital Status --------------------------------------------------------
marital_status_data <- data %>%
  mutate(
    # Never Married
    Never_Married = rowSums(select(., 
                                   matches("^MARITAL_STATUS_Estimate Total Male Never married$"),
                                   matches("^MARITAL_STATUS_Estimate Total Female Never married$")), na.rm = TRUE),
    
    # Married, Spouse Present
    Married_Spouse_Present = rowSums(select(., 
                                            matches("^MARITAL_STATUS_Estimate Total Male Now married Married, spouse present$"),
                                            matches("^MARITAL_STATUS_Estimate Total Female Now married Married, spouse present$")), na.rm = TRUE),
    
    # Married, Spouse Absent
    Married_Spouse_Absent = rowSums(select(., 
                                           matches("^MARITAL_STATUS_Estimate Total Male Now married Married, spouse absent$"),
                                           matches("^MARITAL_STATUS_Estimate Total Female Now married Married, spouse absent$")), na.rm = TRUE),
    
    # Widowed
    Widowed = rowSums(select(., 
                             matches("^MARITAL_STATUS_Estimate Total Male Widowed$"),
                             matches("^MARITAL_STATUS_Estimate Total Female Widowed$")), na.rm = TRUE),
    
    # Divorced
    Divorced = rowSums(select(., 
                              matches("^MARITAL_STATUS_Estimate Total Male Divorced$"),
                              matches("^MARITAL_STATUS_Estimate Total Female Divorced$")), na.rm = TRUE),
    
    # Use the total population from the marital status estimate
    Marital_Status_Population = `MARITAL_STATUS_Estimate Total Male` + `MARITAL_STATUS_Estimate Total Female`
  ) %>%
  
  # Select the relevant columns for output
  select(YEAR, ZCTA, Never_Married, Married_Spouse_Present, Married_Spouse_Absent, Widowed, Divorced, Marital_Status_Population)

# A. Combine Data ------------------------------------------------

# Combine all dataframes into one without duplicating YEAR and ZCTA
final_combined_data <- population_data %>%
  left_join(employment_industry_data, by = c("YEAR", "ZCTA")) %>%
  left_join(poverty_data, by = c("YEAR", "ZCTA")) %>%
  left_join(median_income_data, by = c("YEAR", "ZCTA")) %>%
  left_join(race_ethnicity_data, by = c("YEAR", "ZCTA")) %>%
  left_join(sex_data, by = c("YEAR", "ZCTA")) %>%
  left_join(age_data, by = c("YEAR", "ZCTA")) %>%
  left_join(education_data, by = c("YEAR", "ZCTA")) %>%
  left_join(marital_status_data, by = c("YEAR", "ZCTA"))

# 9. Variable Rates --------------------------------------------------------

# Ensure that the columns used for calculations are numeric
final_combined_data <- final_combined_data %>%
  mutate(
    Poverty_Rate = as.numeric(Poverty_Rate),
    Child_Poverty_Rate = as.numeric(Child_Poverty_Rate),
    Poverty_Population = as.numeric(Poverty_Population),
    Median_Income = as.numeric(Median_Income),
    Total_Population = as.numeric(Total_Population),
    Hispanic = as.numeric(Hispanic),
    White_non_Hispanic = as.numeric(White_non_Hispanic),
    Black_non_Hispanic = as.numeric(Black_non_Hispanic),
    Asian_non_Hispanic = as.numeric(Asian_non_Hispanic),
    Other_non_Hispanic = as.numeric(Other_non_Hispanic),
    No_HS = as.numeric(No_HS),
    HS_Graduate = as.numeric(HS_Graduate),
    Some_College = as.numeric(Some_College),
    College_Graduate = as.numeric(College_Graduate),
    Male = as.numeric(Male),
    Female = as.numeric(Female),
    Age_0_4 = as.numeric(Age_0_4),
    Age_5_9 = as.numeric(Age_5_9),
    Age_10_17 = as.numeric(Age_10_17),
    Age_18_24 = as.numeric(Age_18_24),
    Age_25_34 = as.numeric(Age_25_34),
    Age_35_44 = as.numeric(Age_35_44),
    Age_45_54 = as.numeric(Age_45_54),
    Age_55_64 = as.numeric(Age_55_64),
    Age_65_Plus = as.numeric(Age_65_Plus),
    Age_0_17 = as.numeric(Age_0_17),
    
    # Marital Status Variables
    Never_Married = as.numeric(Never_Married),
    Married_Spouse_Present = as.numeric(Married_Spouse_Present),
    Married_Spouse_Absent = as.numeric(Married_Spouse_Absent),
    Widowed = as.numeric(Widowed),
    Divorced = as.numeric(Divorced),
    Marital_Status_Population = as.numeric(Marital_Status_Population),
    
    # Employment Status Variables
    employment_industry_data <- employment_industry_data %>%
      mutate(
        `EMPLOYMENT_Estimate EMPLOYMENT STATUS Population 16 years and over` = as.numeric(`EMPLOYMENT_Estimate EMPLOYMENT STATUS Population 16 years and over`),
        `EMPLOYMENT_Estimate EMPLOYMENT STATUS In labor force Civilian labor force Employed` = as.numeric(`EMPLOYMENT_Estimate EMPLOYMENT STATUS In labor force Civilian labor force Employed`),
        `EMPLOYMENT_Estimate EMPLOYMENT STATUS In labor force Civilian labor force Unemployed` = as.numeric(`EMPLOYMENT_Estimate EMPLOYMENT STATUS In labor force Civilian labor force Unemployed`),
        `EMPLOYMENT_Estimate EMPLOYMENT STATUS In labor force Armed Forces` = as.numeric(`EMPLOYMENT_Estimate EMPLOYMENT STATUS In labor force Armed Forces`),
        `EMPLOYMENT_Estimate EMPLOYMENT STATUS Not in labor force` = as.numeric(`EMPLOYMENT_Estimate EMPLOYMENT STATUS Not in labor force`),
        `EMPLOYMENT_Estimate EMPLOYMENT STATUS In labor force` = as.numeric(`EMPLOYMENT_Estimate EMPLOYMENT STATUS In labor force`)  # Include the correct new variable name
      ),
    
    # Industry Variables
    `EMPLOYMENT_Estimate INDUSTRY Civilian employed population 16 years and over` = as.numeric(`EMPLOYMENT_Estimate INDUSTRY Civilian employed population 16 years and over`),
    `EMPLOYMENT_Estimate INDUSTRY Agriculture, forestry, fishing and hunting, and mining` = as.numeric(`EMPLOYMENT_Estimate INDUSTRY Agriculture, forestry, fishing and hunting, and mining`),
    `EMPLOYMENT_Estimate INDUSTRY Construction` = as.numeric(`EMPLOYMENT_Estimate INDUSTRY Construction`),
    `EMPLOYMENT_Estimate INDUSTRY Manufacturing` = as.numeric(`EMPLOYMENT_Estimate INDUSTRY Manufacturing`),
    `EMPLOYMENT_Estimate INDUSTRY Wholesale trade` = as.numeric(`EMPLOYMENT_Estimate INDUSTRY Wholesale trade`),
    `EMPLOYMENT_Estimate INDUSTRY Retail trade` = as.numeric(`EMPLOYMENT_Estimate INDUSTRY Retail trade`),
    `EMPLOYMENT_Estimate INDUSTRY Transportation and warehousing, and utilities` = as.numeric(`EMPLOYMENT_Estimate INDUSTRY Transportation and warehousing, and utilities`),
    `EMPLOYMENT_Estimate INDUSTRY Information` = as.numeric(`EMPLOYMENT_Estimate INDUSTRY Information`),
    `EMPLOYMENT_Estimate INDUSTRY Finance and insurance, and real estate and rental and leasing` = as.numeric(`EMPLOYMENT_Estimate INDUSTRY Finance and insurance, and real estate and rental and leasing`),
    `EMPLOYMENT_Estimate INDUSTRY Professional, scientific, and management, and administrative and waste management services` = as.numeric(`EMPLOYMENT_Estimate INDUSTRY Professional, scientific, and management, and administrative and waste management services`),
    `EMPLOYMENT_Estimate INDUSTRY Educational services, and health care and social assistance` = as.numeric(`EMPLOYMENT_Estimate INDUSTRY Educational services, and health care and social assistance`),
    `EMPLOYMENT_Estimate INDUSTRY Arts, entertainment, and recreation, and accommodation and food services` = as.numeric(`EMPLOYMENT_Estimate INDUSTRY Arts, entertainment, and recreation, and accommodation and food services`),
    `EMPLOYMENT_Estimate INDUSTRY Other services, except public administration` = as.numeric(`EMPLOYMENT_Estimate INDUSTRY Other services, except public administration`),
    `EMPLOYMENT_Estimate INDUSTRY Public administration` = as.numeric(`EMPLOYMENT_Estimate INDUSTRY Public administration`)
  )

# Add checks to ensure that each category equals its total population
final_combined_data <- final_combined_data %>%
  mutate(
    # Sex Population Check
    Sex_Check = ifelse(Total_Population == Male + Female, TRUE, FALSE),
    
    # Race Population Check
    Race_Check = ifelse(Total_Population == Hispanic + White_non_Hispanic + Black_non_Hispanic + Asian_non_Hispanic + Other_non_Hispanic, TRUE, FALSE),
    
    # Age Population Check
    Age_Check = ifelse(Total_Population == Age_0_4 + Age_5_9 + Age_10_17 + Age_18_24 + Age_25_34 + Age_35_44 + Age_45_54 + Age_55_64 + Age_65_Plus, TRUE, FALSE),
    
    # Marital Status Population Check
    Marital_Status_Check = ifelse(Marital_Status_Population == Never_Married + Married_Spouse_Present + Married_Spouse_Absent + Widowed + Divorced, TRUE, FALSE),
    
    # Employment Population Check
    Employment_Status_Check = ifelse(`EMPLOYMENT_Estimate EMPLOYMENT STATUS Population 16 years and over` ==
                                       `EMPLOYMENT_Estimate EMPLOYMENT STATUS In labor force Civilian labor force Employed` + 
                                       `EMPLOYMENT_Estimate EMPLOYMENT STATUS In labor force Civilian labor force Unemployed` +
                                       `EMPLOYMENT_Estimate EMPLOYMENT STATUS In labor force Armed Forces` + 
                                       `EMPLOYMENT_Estimate EMPLOYMENT STATUS Not in labor force`, TRUE, FALSE),
    
    # Industry Population Check
    Industry_Check = ifelse(`EMPLOYMENT_Estimate INDUSTRY Civilian employed population 16 years and over` ==
                              `EMPLOYMENT_Estimate INDUSTRY Agriculture, forestry, fishing and hunting, and mining` + 
                              `EMPLOYMENT_Estimate INDUSTRY Construction` + 
                              `EMPLOYMENT_Estimate INDUSTRY Manufacturing` +
                              `EMPLOYMENT_Estimate INDUSTRY Wholesale trade` +
                              `EMPLOYMENT_Estimate INDUSTRY Retail trade` +
                              `EMPLOYMENT_Estimate INDUSTRY Transportation and warehousing, and utilities` +
                              `EMPLOYMENT_Estimate INDUSTRY Information` +
                              `EMPLOYMENT_Estimate INDUSTRY Finance and insurance, and real estate and rental and leasing` +
                              `EMPLOYMENT_Estimate INDUSTRY Professional, scientific, and management, and administrative and waste management services` +
                              `EMPLOYMENT_Estimate INDUSTRY Educational services, and health care and social assistance` +
                              `EMPLOYMENT_Estimate INDUSTRY Arts, entertainment, and recreation, and accommodation and food services` +
                              `EMPLOYMENT_Estimate INDUSTRY Other services, except public administration` +
                              `EMPLOYMENT_Estimate INDUSTRY Public administration`, TRUE, FALSE)
  )

# Calculate the number of incorrect rows per category
incorrect_rows_per_category <- final_combined_data %>%
  summarize(
    Sex_Incorrect = sum(Sex_Check == FALSE, na.rm = TRUE),
    Race_Incorrect = sum(Race_Check == FALSE, na.rm = TRUE),
    Age_Incorrect = sum(Age_Check == FALSE, na.rm = TRUE),
    Marital_Status_Incorrect = sum(Marital_Status_Check == FALSE, na.rm = TRUE),
    Employment_Status_Incorrect = sum(Employment_Status_Check == FALSE, na.rm = TRUE),
    Industry_Incorrect = sum(Industry_Check == FALSE, na.rm = TRUE)
  )

# Create rates (percentages) for EMPLOYMENT STATUS, INDUSTRY, RACE_ETHNICITY, SEX, AGE, EDUCATION, MARITAL STATUS
final_combined_data <- final_combined_data %>%
  mutate(
    # EMPLOYMENT STATUS Rates
    Employment_Rate = `EMPLOYMENT_Estimate EMPLOYMENT STATUS In labor force Civilian labor force Employed` / `EMPLOYMENT_Estimate EMPLOYMENT STATUS Population 16 years and over`,
    
    # Calculate Unemployment and Armed Forces rate using Labor Force
    Unemployment_Rate = `EMPLOYMENT_Estimate EMPLOYMENT STATUS In labor force Civilian labor force Unemployed` / `EMPLOYMENT_Estimate EMPLOYMENT STATUS In labor force`,
    Armed_Forces_Rate = `EMPLOYMENT_Estimate EMPLOYMENT STATUS In labor force Armed Forces` / `EMPLOYMENT_Estimate EMPLOYMENT STATUS In labor force`,
    
    # Not in Labor Force Rate remains the same
    Not_in_Labor_Force_Rate = `EMPLOYMENT_Estimate EMPLOYMENT STATUS Not in labor force` / `EMPLOYMENT_Estimate EMPLOYMENT STATUS Population 16 years and over`,

    
    # INDUSTRY Rates
    Agriculture_Rate = `EMPLOYMENT_Estimate INDUSTRY Agriculture, forestry, fishing and hunting, and mining` / `EMPLOYMENT_Estimate INDUSTRY Civilian employed population 16 years and over`,
    Construction_Rate = `EMPLOYMENT_Estimate INDUSTRY Construction` / `EMPLOYMENT_Estimate INDUSTRY Civilian employed population 16 years and over`,
    Manufacturing_Rate = `EMPLOYMENT_Estimate INDUSTRY Manufacturing` / `EMPLOYMENT_Estimate INDUSTRY Civilian employed population 16 years and over`,
    Wholesale_Trade_Rate = `EMPLOYMENT_Estimate INDUSTRY Wholesale trade` / `EMPLOYMENT_Estimate INDUSTRY Civilian employed population 16 years and over`,
    Retail_Trade_Rate = `EMPLOYMENT_Estimate INDUSTRY Retail trade` / `EMPLOYMENT_Estimate INDUSTRY Civilian employed population 16 years and over`,
    Transport_Utilities_Rate = `EMPLOYMENT_Estimate INDUSTRY Transportation and warehousing, and utilities` / `EMPLOYMENT_Estimate INDUSTRY Civilian employed population 16 years and over`,
    Information_Rate = `EMPLOYMENT_Estimate INDUSTRY Information` / `EMPLOYMENT_Estimate INDUSTRY Civilian employed population 16 years and over`,
    Finance_Rate = `EMPLOYMENT_Estimate INDUSTRY Finance and insurance, and real estate and rental and leasing` / `EMPLOYMENT_Estimate INDUSTRY Civilian employed population 16 years and over`,
    Prof_Scientific_Rate = `EMPLOYMENT_Estimate INDUSTRY Professional, scientific, and management, and administrative and waste management services` / `EMPLOYMENT_Estimate INDUSTRY Civilian employed population 16 years and over`,
    Education_Health_Rate = `EMPLOYMENT_Estimate INDUSTRY Educational services, and health care and social assistance` / `EMPLOYMENT_Estimate INDUSTRY Civilian employed population 16 years and over`,
    Arts_Entertainment_Rate = `EMPLOYMENT_Estimate INDUSTRY Arts, entertainment, and recreation, and accommodation and food services` / `EMPLOYMENT_Estimate INDUSTRY Civilian employed population 16 years and over`,
    Other_Services_Rate = `EMPLOYMENT_Estimate INDUSTRY Other services, except public administration` / `EMPLOYMENT_Estimate INDUSTRY Civilian employed population 16 years and over`,
    Public_Admin_Rate = `EMPLOYMENT_Estimate INDUSTRY Public administration` / `EMPLOYMENT_Estimate INDUSTRY Civilian employed population 16 years and over`,
    
    # RACE/ETHNICITY Rates
    Hispanic_Rate = Hispanic / Race_Population,
    White_non_Hispanic_Rate = White_non_Hispanic / Race_Population,
    Black_non_Hispanic_Rate = Black_non_Hispanic / Race_Population,
    Asian_non_Hispanic_Rate = Asian_non_Hispanic / Race_Population,
    Other_non_Hispanic_Rate = Other_non_Hispanic / Race_Population,
    
    # SEX Rates
    Male_Rate = Male / Sex_Population,
    Female_Rate = Female / Sex_Population,
    
    # AGE Rates
    Age_0_4_Rate = Age_0_4 / Age_Population,
    Age_5_9_Rate = Age_5_9 / Age_Population,
    Age_10_17_Rate = Age_10_17 / Age_Population,
    Age_18_24_Rate = Age_18_24 / Age_Population,
    Age_25_34_Rate = Age_25_34 / Age_Population,
    Age_35_44_Rate = Age_35_44 / Age_Population,
    Age_45_54_Rate = Age_45_54 / Age_Population,
    Age_55_64_Rate = Age_55_64 / Age_Population,
    Age_65_Plus_Rate = Age_65_Plus / Age_Population,
    
    # EDUCATION Rates
    No_HS_Rate = No_HS / Education_Population,
    HS_Graduate_Rate = HS_Graduate / Education_Population,
    Some_College_Rate = Some_College / Education_Population,
    College_Graduate_Rate = College_Graduate / Education_Population,
    
    # MARITAL STATUS Rates
    Never_Married_Rate = Never_Married / Marital_Status_Population,
    Married_Spouse_Present_Rate = Married_Spouse_Present / Marital_Status_Population,
    Married_Spouse_Absent_Rate = Married_Spouse_Absent / Marital_Status_Population,
    Widowed_Rate = Widowed / Marital_Status_Population,
    Divorced_Rate = Divorced / Marital_Status_Population
  )

# Weighted Averages ----------------------------------------
weighted_data_all <- final_combined_data %>%
  group_by(YEAR) %>%
  summarize(
    # Weighted averages for poverty-related variables
    Weighted_Poverty_Rate = weighted.mean(Poverty_Rate, w = Poverty_Population, na.rm = TRUE),
    Weighted_Child_Poverty_Rate = weighted.mean(Child_Poverty_Rate, w = Age_0_17, na.rm = TRUE),
    
    # Median Income weighted by total population
    Weighted_Median_Income = weighted.mean(Median_Income, w = Total_Population, na.rm = TRUE),
    
    # Race/Ethnicity weighted by Race_Population
    Weighted_Hispanic_Rate = weighted.mean(Hispanic_Rate, w = Race_Population, na.rm = TRUE),
    Weighted_White_non_Hispanic_Rate = weighted.mean(White_non_Hispanic_Rate, w = Race_Population, na.rm = TRUE),
    Weighted_Black_non_Hispanic_Rate = weighted.mean(Black_non_Hispanic_Rate, w = Race_Population, na.rm = TRUE),
    Weighted_Asian_non_Hispanic_Rate = weighted.mean(Asian_non_Hispanic_Rate, w = Race_Population, na.rm = TRUE),
    Weighted_Other_non_Hispanic_Rate = weighted.mean(Other_non_Hispanic_Rate, w = Race_Population, na.rm = TRUE),
    
    # Sex weighted by Sex_Population
    Weighted_Male_Rate = weighted.mean(Male_Rate, w = Sex_Population, na.rm = TRUE),
    Weighted_Female_Rate = weighted.mean(Female_Rate, w = Sex_Population, na.rm = TRUE),
    
    # Age weighted by Age_Population
    Weighted_Age_0_4_Rate = weighted.mean(Age_0_4_Rate, w = Age_Population, na.rm = TRUE),
    Weighted_Age_5_9_Rate = weighted.mean(Age_5_9_Rate, w = Age_Population, na.rm = TRUE),
    Weighted_Age_10_17_Rate = weighted.mean(Age_10_17_Rate, w = Age_Population, na.rm = TRUE),
    Weighted_Age_18_24_Rate = weighted.mean(Age_18_24_Rate, w = Age_Population, na.rm = TRUE),
    Weighted_Age_25_34_Rate = weighted.mean(Age_25_34_Rate, w = Age_Population, na.rm = TRUE),
    Weighted_Age_35_44_Rate = weighted.mean(Age_35_44_Rate, w = Age_Population, na.rm = TRUE),
    Weighted_Age_45_54_Rate = weighted.mean(Age_45_54_Rate, w = Age_Population, na.rm = TRUE),
    Weighted_Age_55_64_Rate = weighted.mean(Age_55_64_Rate, w = Age_Population, na.rm = TRUE),
    Weighted_Age_65_Plus_Rate = weighted.mean(Age_65_Plus_Rate, w = Age_Population, na.rm = TRUE),
    
    # Education weighted by Education_Population
    Weighted_No_HS_Rate = weighted.mean(No_HS_Rate, w = Education_Population, na.rm = TRUE),
    Weighted_HS_Graduate_Rate = weighted.mean(HS_Graduate_Rate, w = Education_Population, na.rm = TRUE),
    Weighted_Some_College_Rate = weighted.mean(Some_College_Rate, w = Education_Population, na.rm = TRUE),
    Weighted_College_Graduate_Rate = weighted.mean(College_Graduate_Rate, w = Education_Population, na.rm = TRUE),
    
    # Marital Status weighted by Marital_Status_Population
    Weighted_Never_Married_Rate = weighted.mean(Never_Married_Rate, w = Marital_Status_Population, na.rm = TRUE),
    Weighted_Married_Spouse_Present_Rate = weighted.mean(Married_Spouse_Present_Rate, w = Marital_Status_Population, na.rm = TRUE),
    Weighted_Married_Spouse_Absent_Rate = weighted.mean(Married_Spouse_Absent_Rate, w = Marital_Status_Population, na.rm = TRUE),
    Weighted_Widowed_Rate = weighted.mean(Widowed_Rate, w = Marital_Status_Population, na.rm = TRUE),
    Weighted_Divorced_Rate = weighted.mean(Divorced_Rate, w = Marital_Status_Population, na.rm = TRUE),
    
    # Employment Status weighted by Employment Population
    Weighted_Employment_Rate = weighted.mean(Employment_Rate, w = `EMPLOYMENT_Estimate EMPLOYMENT STATUS Population 16 years and over`, na.rm = TRUE),
    Weighted_Unemployment_Rate = weighted.mean(Unemployment_Rate, w = `EMPLOYMENT_Estimate EMPLOYMENT STATUS In labor force`, na.rm = TRUE),
    Weighted_Armed_Forces_Rate = weighted.mean(Armed_Forces_Rate, w = `EMPLOYMENT_Estimate EMPLOYMENT STATUS In labor force`, na.rm = TRUE),
    Weighted_Not_in_Labor_Force_Rate = weighted.mean(Not_in_Labor_Force_Rate, w = `EMPLOYMENT_Estimate EMPLOYMENT STATUS Population 16 years and over`, na.rm = TRUE),
    
    # Industry weighted by Industry Population
    Weighted_Agriculture_Rate = weighted.mean(Agriculture_Rate, w = `EMPLOYMENT_Estimate INDUSTRY Civilian employed population 16 years and over`, na.rm = TRUE),
    Weighted_Construction_Rate = weighted.mean(Construction_Rate, w = `EMPLOYMENT_Estimate INDUSTRY Civilian employed population 16 years and over`, na.rm = TRUE),
    Weighted_Manufacturing_Rate = weighted.mean(Manufacturing_Rate, w = `EMPLOYMENT_Estimate INDUSTRY Civilian employed population 16 years and over`, na.rm = TRUE),
    Weighted_Wholesale_Trade_Rate = weighted.mean(Wholesale_Trade_Rate, w = `EMPLOYMENT_Estimate INDUSTRY Civilian employed population 16 years and over`, na.rm = TRUE),
    Weighted_Retail_Trade_Rate = weighted.mean(Retail_Trade_Rate, w = `EMPLOYMENT_Estimate INDUSTRY Civilian employed population 16 years and over`, na.rm = TRUE),
    Weighted_Transport_Utilities_Rate = weighted.mean(Transport_Utilities_Rate, w = `EMPLOYMENT_Estimate INDUSTRY Civilian employed population 16 years and over`, na.rm = TRUE),
    Weighted_Information_Rate = weighted.mean(Information_Rate, w = `EMPLOYMENT_Estimate INDUSTRY Civilian employed population 16 years and over`, na.rm = TRUE),
    Weighted_Finance_Rate = weighted.mean(Finance_Rate, w = `EMPLOYMENT_Estimate INDUSTRY Civilian employed population 16 years and over`, na.rm = TRUE),
    Weighted_Prof_Scientific_Rate = weighted.mean(Prof_Scientific_Rate, w = `EMPLOYMENT_Estimate INDUSTRY Civilian employed population 16 years and over`, na.rm = TRUE),
    Weighted_Education_Health_Rate = weighted.mean(Education_Health_Rate, w = `EMPLOYMENT_Estimate INDUSTRY Civilian employed population 16 years and over`, na.rm = TRUE),
    Weighted_Arts_Entertainment_Rate = weighted.mean(Arts_Entertainment_Rate, w = `EMPLOYMENT_Estimate INDUSTRY Civilian employed population 16 years and over`, na.rm = TRUE),
    Weighted_Other_Services_Rate = weighted.mean(Other_Services_Rate, w = `EMPLOYMENT_Estimate INDUSTRY Civilian employed population 16 years and over`, na.rm = TRUE),
    Weighted_Public_Admin_Rate = weighted.mean(Public_Admin_Rate, w = `EMPLOYMENT_Estimate INDUSTRY Civilian employed population 16 years and over`, na.rm = TRUE),
    
    .groups = "drop"
  )

# Merge weighted averages with the final combined dataset
final_combined_data <- final_combined_data %>%
  left_join(weighted_data_all, by = c("YEAR"))

# Save the combined dataframe as a CSV
write.csv(final_combined_data, "Data Outputs/VDSS_ZCTA_ACS_Characteristics/VDSS_ZCTA_ACS_Characteristics_Cleaned.csv", row.names = FALSE)

# Save the combined dataframe as an Excel file
write_xlsx(final_combined_data, "Data Outputs/VDSS_ZCTA_ACS_Characteristics/VDSS_ZCTA_ACS_Characteristics_Cleaned.xlsx")