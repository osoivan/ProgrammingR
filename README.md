
<h1 align="center">📘 Programming in R – Teaching Repository</h1>

<p align="center">
  <img src="https://img.shields.io/badge/R%20version-%3E%3D4.0-blue?style=flat-square">
  <img src="https://img.shields.io/badge/Learning%20Level-Beginner%20to%20Intermediate-green?style=flat-square">
  <img src="https://img.shields.io/badge/Shiny-Interactive%20Apps-orange?style=flat-square">
  <img src="https://img.shields.io/badge/Status-Active-brightgreen?style=flat-square">
</p>

<p align="center">  
  A curated collection of scripts and Shiny apps for teaching R programming, GIS, spatial data analysis, and interactive dashboards.  
  Designed with a <strong>step-by-step, classroom-oriented approach</strong>.  
</p>

---

# 📁 Repository Overview

This repository includes:

- **R scripts** for loading, processing, visualizing, and modelling environmental and spatial data.
- **Complete Shiny apps** ready for deployment.
- **Datasets** for hands-on learning and assignments.
- **Modular code examples** for teaching good programming practices.

---

# 🗂 Folder Structure

```
ProgrammingR/
│
├── data/                        # Supporting datasets
├── EcuadorInamhi/               # Full Shiny application (deployable)
├── 07_MODULARCODE_IDW/code/     # Modular IDW interpolation examples
│
├── *.R                          # Individual teaching scripts
└── README.md                    # Documentation
```

---

# 📄 Description of Key R Scripts

### **📌 01_LOADING_SHAPEFILE.R**
Learn how to load, clean, and visualize shapefiles using `sf`.

### **📌 02_METEOROLOGICAL_CHARTS.R**
Generate time series of climate variables (temperature, rainfall).

### **📌 03_AIRQUALITYANALYSIS_FILTERS_GRAPHS_M...R**
Air quality data cleaning, filtering, exploration, and graphics.

### **📌 04_TRENDMODELLING.R**
Detect trends using regression, LOESS, and time-series workflows.

### **📌 05_HEALTHDATA_ANALYSIS.R**
Combine environmental and health data for applied analysis.

### **📌 06_R_CODE_MAP_WMS.R**
Load WMS layers into R using `leaflet`.

### **📌 08_HYPOTHESESTEST_DYNAMICGRAPHS.R**
Interactive hypothesis testing with dynamic charts.

### **📌 09_CREATIONINSINGLESHINYAPP.R**
A minimal Shiny app created step-by-step.

### **📌 10_UPLOAD_ANAPPTO SHINYWEB.R**
How to deploy Shiny apps online via shinyapps.io.

### **📌 GERMANY_TEMPERATURE_APP.R**
Interactive temperature converter app.

### **📌 GermanyInteractiveMap.R**
Interactive Germany map built with Leaflet.

---

# 🌟 How to Build a Shiny App (Visual Guide)

### 🧱 **Structure of every Shiny app**
```
library(shiny)

ui <- fluidPage()       # What user sees
server <- function(){}  # What the app does

shinyApp(ui, server)    # Run the app
```

### 🔄 How UI and Server communicate
```
User Input → (input$...) → Server Logic → Output → UI Display
```

### ⭐ Example: Temperature Converter
```r
library(shiny)

ui <- fluidPage(
  titlePanel("Temperature Converter"),
  selectInput("unit", "Convert:",
              c("Celsius to Kelvin", "Kelvin to Celsius")),
  numericInput("value", "Temperature:", 0),
  textOutput("result")
)

server <- function(input, output) {
  output$result <- renderText({
    if (input$unit == "Celsius to Kelvin") {
      paste(input$value, "°C =", input$value + 273.15, "K")
    } else {
      paste(input$value, "K =", input$value - 273.15, "°C")
    }
  })
}

shinyApp(ui, server)
```

---

# 🌐 Deploying a Shiny App Online (shinyapps.io)

### ✔️ Requirements
1. Create an account at https://www.shinyapps.io/  
2. Get your `token` and `secret` from **Account → Tokens**  
3. Your app folder must look like this:

```
MyApp/
 ├── app.R
 ├── data.csv
 ├── logo.png
 └── stations.geojson
```

### 🚀 Deployment Script
```r
# install.packages("rsconnect")
library(rsconnect)

rsconnect::setAccountInfo(
  name   = "YOUR_USERNAME",
  token  = "YOUR_TOKEN",
  secret = "YOUR_SECRET"
)

rsconnect::deployApp("C:/Path/To/MyApp")
rsconnect::showLogs(appName = "MyApp", streaming = TRUE)
```

---

# 🎓 Teaching Goals

This repository supports courses and workshops on:

- Introduction to R  
- Environmental data analysis  
- Spatial data handling  
- Interactive visualization  
- Building and deploying Shiny apps  

---

# 🧑‍🏫 About the Author

**César Iván Alvarez**  
University of Augsburg  
📧 cesar.alvarez@uni-a.de  
🌐 GitHub: https://github.com/osoivan  

---

<p align="center">
  ⭐ If you find this repository useful, consider giving it a star!  
</p>
