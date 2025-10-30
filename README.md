# Advanced Programming in R  
### University of Augsburg — Centre for Climate Resilience  

This repository contains all teaching materials, R scripts, and example applications used in the **Advanced Programming in R** course taught by **Dr. César Iván Álvarez Mendoza** at the **University of Augsburg**.  

The course introduces students to advanced techniques in R for data analysis, visualization, spatial data processing, and interactive application development using *Shiny* and *Leaflet*.

---

## 🎯 Learning Objectives

By the end of this course, students will be able to:

- Apply R programming for scientific data analysis and visualization.  
- Import and process spatial and environmental data (shapefiles, rasters, Excel, CSV).  
- Build and customize static and interactive charts using **ggplot2** and **plotly**.  
- Create and deploy **interactive web maps** using **Leaflet** and **Shiny**.  
- Integrate open data sources (e.g., DWD, EEA, ERA5-Land) into R workflows.  
- Export and share analytical outputs for research and publications.

---

## 📂 Repository Structure

| File | Description |
|------|--------------|
| `01_LOADING_SHAPEFILE.R` | Example on how to load and visualize shapefiles in R. |
| `02_METEOROLOGICAL_CHARTS.R` | Scripts for visualizing meteorological datasets. |
| `03_AIRQUALITYANALYSIS_FILTERS_GRAPHS_MAPS.R` | Analysis of air quality indicators (PM2.5) with filtering, summarization, and mapping. |
| `03_CREATIONSINGLE_APP.R` | Template for creating a single R Shiny application. |
| `05_R_CODE_MAP_WMS.R` | Example of integrating WMS (Web Map Services) in R maps. |
| `GERMANY_TEMPERATURE_APP.R` | Interactive dashboard visualizing temperature data from DWD. |
| `GermanyInteractiveMap.R` | Leaflet-based dynamic map for displaying geographic data. |
| `UPLOAD_ANAPPTO_SHINYWEB.R` | Script to upload and deploy R Shiny apps to **ShinyApps.io**. |
| `Advanced Programming with R.pdf` | Course presentation and reference material. |

---

## 🌍 Data Sources

The datasets used in these examples are obtained from:
- **European Environment Agency (EEA)**: [https://www.eea.europa.eu](https://www.eea.europa.eu)  
- **German Weather Service (DWD)**: [https://opendata.dwd.de](https://opendata.dwd.de)  
- **Copernicus / ECMWF ERA5-Land**  
- **OpenStreetMap** (for basemaps in Leaflet)

---

## 🧑‍🏫 Instructor

**Dr. César Iván Álvarez Mendoza**  
Wissenschaftlicher Mitarbeiter | Centre for Climate Resilience  
University of Augsburg, Germany  
📧 cesar.alvarez@uni-a.de  
🔗 [Google Scholar](https://scholar.google.es/citations?user=50ILKdkAAAAJ&hl=es)  
🔗 [GitHub](https://github.com/osoivan)

---

## ⚙️ How to Use the Code

1. Clone or download this repository.  
2. Open the R scripts in **RStudio**.  
3. Run each script section by section to understand the workflow.  
4. Modify file paths (`C:/data/...`) to match your own directory structure.  
5. Install required packages when prompted (e.g., `ggplot2`, `leaflet`, `readxl`, `dplyr`, `shiny`).  

---

## 🚀 Deployment to ShinyApps.io

If you want to share your R Shiny applications online, follow these steps:

1. Install and load the **rsconnect** package:
   ```r
   install.packages("rsconnect")
   library(rsconnect)
