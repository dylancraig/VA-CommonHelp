# VA-CommonHelp

This project processes and analyzes Virginia **ZIP code** and **ZCTA** data to study VDSS service access, treatment status, and quality metrics.  
Processing is done in **R** (cleaning → merge → analysis → visualization).

---

## 📂 Project Structure
- `Raw Data/` – Input datasets (VDSS offices, USPS, IRS, ACS, HRSA ZCTA, TIGRIS shapefiles, etc.)
- `Scripts/` – R scripts for geocoding, distances, classifications, analysis, and maps
- `Data Outputs/` – Cleaned datasets, merged panels, final outputs
- `Plots/` – Generated visualizations (maps, heat maps, histograms, bar charts)
- `Write-Ups/` – Reports and supporting documentation

---

## ⚙️ Workflow Overview
1. **Geocoding**: Match ZIP codes to latitude/longitude using VDSS and USPS data.  
2. **ZIP changes**: Track changes in ZIP codes over time.  
3. **Distance calculations**: Compute haversine, driving, and transit distances (ZIP and ZCTA).  
4. **Analysis**: Produce summary statistics, classifications (border, residential, type), and treatment maps.  
5. **Final dataset**: Merge all processed datasets into a single output file.  

---

## 🚀 Running the Project
1. Open `VA_CommonHelp.Rproj` in RStudio (sets working directory automatically).  
2. Run scripts in `Scripts/` as needed, or in order for the full pipeline.  
3. Outputs are saved to `Data Outputs/`; figures are saved to `Plots/`.  

> ⚠️ Some scripts use the **Google Maps API** (`gmapsdistance` package).  
> A valid **Google API key** must be set in your environment before running those scripts.  

---

## 🧾 Key Scripts
- `VDSS_Zip_GGMAP_Geocoding_All.R` / `VDSS_Zip_GGMAP_Geocoding_2012.R` → Geocode ZIPs  
- `VDSS_Zip_Code_Changes_By_Year.R` → Track ZIP changes  
- `VDSS_Distances_All.R` / `VDSS_Distances_2012.R` → ZIP distances  
- `VDSS_Distances_All_ZCTA.R` / `VDSS_Distances_2012_ZCTA.R` → ZCTA distances  
- `VDSS_Distances_Analysis_All.R` → Distance summary stats & plots  
- `VDSS_Zip_Bad_ZipCounty_Rate_Heat_Map.R` → Heat map of error rates  
- `VDSS_Zip_Treat_Map.R` → Treatment maps  
- `VDSS_Zip_Type.R` → ZIP type classifications (PO Box, Unique, Standard)  
- `VDSS_Zip_Residential_Status.R` → Residential vs. non-residential ZIPs  
- `VDSS_Zip_Border_Status.R` / `VDSS_Zip_FIPS_Border_Status_Final_Designation.R` → Border status  
- `VDSS_Zip_ZCTA_Crosswalk_HRSA.R` → ZIP ↔ ZCTA crosswalk  
- `VDSS_ZCTA_ACS_Characteristics.R` → ACS ZCTA characteristics  
- `VDSS_Zip_Border_Status_Type_Bad_Zipcounty_Rate.R` → Summary stats and t-tests  
- `VDSS_Final_Data.R` → Final merged dataset

---

## 📦 Required R Packages
`readxl`  
`dplyr`  
`writexl`  
`tidyverse`  
`pbapply`  
`tidyr`  
`openxlsx`  
`purrr`  
`tigris`  
`sf`  
`geosphere`  
`gmapsdistance`  
`readr`  
`stringr`  
`progress`  
`future.apply`  
`progressr`  
`ggplot2`  
`officer`  
`lubridate`  
`ggmap`  
`haven`  

---

## ✅ Quick Check
After running:
- Geocoded and distance files should exist in `Data Outputs/`  
- Maps and plots should exist in `Plots/`  
- **Final dataset** should exist at `Data Outputs/Final_Data.xlsx`  
