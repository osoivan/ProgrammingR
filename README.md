<div align="center">

# Programming in R

![R](https://img.shields.io/badge/R-276DC3?style=for-the-badge&logo=r&logoColor=white)
![RStudio](https://img.shields.io/badge/RStudio-75AADB?style=for-the-badge&logo=rstudio&logoColor=white)
![Shiny](https://img.shields.io/badge/Shiny-1E90FF?style=for-the-badge&logo=rstudio&logoColor=white)
![Leaflet](https://img.shields.io/badge/Leaflet-199900?style=for-the-badge&logo=leaflet&logoColor=white)
![ggplot2](https://img.shields.io/badge/ggplot2-EA4AAA?style=for-the-badge)
![sf](https://img.shields.io/badge/sf-2E8B57?style=for-the-badge)
![terra](https://img.shields.io/badge/terra-8B4513?style=for-the-badge)
![Spatial%20Analysis](https://img.shields.io/badge/Spatial%20Analysis-FF8C00?style=for-the-badge)
![GIS](https://img.shields.io/badge/GIS-228B22?style=for-the-badge)
![Remote%20Sensing](https://img.shields.io/badge/Remote%20Sensing-6A5ACD?style=for-the-badge)

</div>

---

# Programming in R

## About this repository / Acerca de este repositorio

### English

Hello, here we share our **Programming in R** module, developed for students, researchers, and professionals who want to learn R from basic exercises to more advanced applications in environmental data analysis, spatial data processing, statistics, visualization, and Shiny web applications.

### Español

Hola, aquí compartimos nuestro módulo de **Programación en R**, desarrollado para estudiantes, investigadores y profesionales que desean aprender R desde ejercicios básicos hasta aplicaciones más avanzadas en análisis de datos ambientales, procesamiento espacial, estadística, visualización y aplicaciones web con Shiny.

🌐 LinkedIn:  
https://www.linkedin.com/in/cesar-ivan-alvarez-0847253a/

🎓 Google Scholar:  
https://scholar.google.com/citations?user=50ILKdkAAAAJ&hl=es

---

## Repository description

This repository contains practical scripts and teaching materials for learning and applying **R programming** in environmental sciences, GIS, data analysis, spatial modelling, and interactive dashboard development.

The material follows a step-by-step structure, starting with basic spatial data loading and progressing toward meteorological charts, air quality analysis, trend modelling, health data analysis, interpolation, Leaflet maps, and Shiny applications.

---

## Repository structure

| File / Folder | Description |
|---|---|
| `data/` | Supporting datasets for the exercises |
| `EcuadorInamhi/` | Full Shiny application example |
| `07_MODULARCODE_IDW/code/` | Modular R code for IDW interpolation |
| `01_LOADING_SHAPEFILE.R` | Load, clean, and visualize shapefiles |
| `02_METEOROLOGICAL_CHARTS.R` | Create meteorological charts and time-series plots |
| `03_AIRQUALITYANALYSIS_FILTERS_GRAPHS_MAP.R` | Air quality analysis with filters, graphs, and maps |
| `04_TRENDMODELLING.R` | Trend modelling using regression and time-series workflows |
| `05_HEALTHDATA_ANALYSIS.R` | Environmental and health data analysis |
| `06_R_CODE_MAP_WMS.R` | Load and visualize WMS layers in R |
| `08_HYPOTHESISTEST_DYNAMICGRAPHS.R` | Hypothesis testing with dynamic graphs |
| `09_CREATIONSINGLESHINYAPP.R` | Create a basic Shiny app |
| `10_UPLOAD_ANAPPTOSHINYWEB.R` | Deploy a Shiny app online |
| `11_RMARKDOWN_INTERPOLATIONGERMANY.Rmd` | R Markdown example for spatial interpolation in Germany |
| `GERMANY_TEMPERATURE_APP.R` | Interactive temperature converter app |
| `GermanyInteractiveMap.R` | Interactive Germany map using Leaflet |
| `Advanced Programming with R.pdf` | Supporting teaching material |

---

## Main topics covered

- Introduction to R programming
- Data loading and cleaning
- Spatial data processing
- Shapefile visualization
- Meteorological data analysis
- Air quality data analysis
- Health and environmental data integration
- Trend modelling
- Hypothesis testing
- IDW interpolation
- WMS layers in R
- Leaflet interactive maps
- Shiny web applications
- Deployment to shinyapps.io
- R Markdown reports

---

## Requirements

To use this repository, you need:

- R
- RStudio or Visual Studio Code
- Internet connection for some examples
- Basic knowledge of data analysis is useful but not mandatory

Recommended R packages:

```r
install.packages(c(
  "sf",
  "terra",
  "raster",
  "sp",
  "leaflet",
  "shiny",
  "ggplot2",
  "dplyr",
  "tidyr",
  "readr",
  "lubridate",
  "plotly",
  "DT",
  "rsconnect",
  "rmarkdown"
))
```

---

## Getting started

Clone the repository:

```bash
git clone https://github.com/osoivan/ProgrammingR.git
```

Open the project folder in RStudio or Visual Studio Code.

Start with:

```text
01_LOADING_SHAPEFILE.R
```

Then continue progressively with the next scripts.

---

## Suggested learning path

```text
Basic R and spatial data loading
→ Data visualization
→ Environmental data analysis
→ Spatial analysis
→ Statistical modelling
→ Interactive maps
→ Shiny applications
→ Online deployment
```

---

## Example Shiny app structure

A basic Shiny application usually follows this structure:

```r
library(shiny)

ui <- fluidPage(
  titlePanel("My Shiny App")
)

server <- function(input, output) {
  # Server logic
}

shinyApp(ui = ui, server = server)
```

---

## Deploying Shiny apps

To deploy an app to shinyapps.io:

```r
install.packages("rsconnect")
library(rsconnect)

rsconnect::setAccountInfo(
  name = "YOUR_USERNAME",
  token = "YOUR_TOKEN",
  secret = "YOUR_SECRET"
)

rsconnect::deployApp("path/to/your/app")
```

---

## Important note

Some scripts may require more processing time depending on:

- Dataset size
- Number of records
- Spatial resolution
- Computer memory
- Internet connection
- Complexity of the analysis

For large datasets, it is recommended to test the workflow first using a smaller subset.

---

## Applications

This repository can be useful for:

- R programming courses
- GIS and spatial data science teaching
- Environmental data analysis
- Air quality studies
- Climate and meteorological analysis
- Health-environment studies
- Interactive mapping
- Shiny dashboard development
- Reproducible research workflows

---

## Author

**Dr. César Iván Alvarez Mendoza**  
Geospatial Data Scientist | Remote Sensing Researcher  
University of Augsburg, Germany  

📧 Email:  
cesar.alvarez@uni-a.de  

🌐 LinkedIn:  
https://www.linkedin.com/in/cesar-ivan-alvarez-0847253a/

🎓 Google Scholar:  
https://scholar.google.com/citations?user=50ILKdkAAAAJ&hl=es

💻 GitHub:  
https://github.com/osoivan

---

## Citation

If you use this repository for teaching, research, or applied work, please cite it as:

```text
Alvarez Mendoza, C. I. (2026). Programming in R: Practical scripts for environmental data analysis, GIS, statistics, visualization, and Shiny applications. GitHub repository: https://github.com/osoivan/ProgrammingR
```

---

## License

This repository is shared for academic, teaching, and research purposes. Please acknowledge the author when using or adapting the material.

---

⭐ If you find this repository useful, consider giving it a star.
