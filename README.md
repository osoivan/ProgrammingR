# Programming in R – Teaching Repository

This repository contains R scripts, datasets, and Shiny applications used for teaching **Programming in R**, **GIS**, **Environmental Data Analysis**, and **Cartography**.  
The materials are designed in a **step-by-step, beginner-friendly style** to help students learn R from basic scripting to interactive web applications.

---

# 📁 Repository Structure

Below is an overview of the main folders and R scripts currently included.

## Folders

### **/data/**
Contains supporting datasets (CSV, spatial files, etc.) used in examples.

### **/EcuadorInamhi/**
Complete Shiny application for visualizing meteorological data in Ecuador.  
This folder is **ready for deployment** on shinyapps.io and includes:

- `app.R`  
- data files  
- supporting resources  

### **/07_MODULARCODE_IDW/code/**
Examples of modular programming in R, including IDW interpolation routines.

---

# 📄 R Script Descriptions

### **01_LOADING_SHAPEFILE.R**
Introduction to reading and visualizing shapefiles using the `sf` package.

### **02_METEOROLOGICAL_CHARTS.R**
Creates basic meteorological charts (temperature, precipitation time series).

### **03_AIRQUALITYANALYSIS_FILTERS_GRAPHS_M...R**
Air quality analysis including filtering, visualization, and exploratory statistics.

### **04_TRENDMODELLING.R**
Trend modeling with regression and smoothing for environmental datasets.

### **05_HEALTHDATA_ANALYSIS.R**
Combining environmental and health data for descriptive analytics.

### **06_R_CODE_MAP_WMS.R**
Loads and visualizes WMS map layers inside R using `leaflet`.

### **08_HYPOTHESESTEST_DYNAMICGRAPHS.R**
Dynamic graphs for hypothesis testing and statistical exploration.

### **09_CREATIONINSINGLESHINYAPP.R**
Teaches the full structure of a simple Shiny app in a single file.

### **10_UPLOAD_ANAPPTO SHINYWEB.R**
Step-by-step tutorial on publishing a Shiny app to shinyapps.io.

### **GERMANY_TEMPERATURE_APP.R**
Shiny app example demonstrating temperature conversion and visualization.

### **GermanyInteractiveMap.R**
Interactive map of Germany using `leaflet`.

---

# 🌟 How to Create a Shiny App (Beginner Explanation)

Every Shiny app has **three essential parts**:

1. Load the Shiny package  
2. Define the UI (what the user sees)  
3. Define the server (what the app does)  
4. Run the app  

### Minimal template

```r
library(shiny)

ui <- fluidPage(
  "Hello world!"
)

server <- function(input, output) {}

shinyApp(ui, server)
```

---

# 🌡 Example Shiny App: Temperature Converter

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

# 📂 Folder Structure for Shiny Apps

To deploy a Shiny app online, your folder **must** look like this:

```
MyApp/
 ├── app.R
 ├── data.csv
 ├── stations.geojson
 └── logo.png
```

⚠️ Important:

- All files used by the app must be **inside this folder**.  
- Do **not** use absolute paths like `C:/Users/.../data.csv`.  
- Only use relative paths: `"data.csv"`, `"stations.geojson"`.

---

# 🌐 How to Publish a Shiny App to shinyapps.io

### Step 1 — Create an account  
Go to: https://www.shinyapps.io/  
Then navigate to **Account → Tokens → Show Token**  
Copy:

- your **username**  
- your **token**  
- your **secret**  

### Step 2 — Deployment Code

```r
# Install (only once)
# install.packages("rsconnect")

library(rsconnect)

# Connect R to your shinyapps.io account
rsconnect::setAccountInfo(
  name   = "YOUR_USERNAME",
  token  = "YOUR_TOKEN",
  secret = "YOUR_SECRET"
)

# Deploy the folder containing app.R
rsconnect::deployApp("C:/Path/To/MyApp")

# Check logs (useful for debugging)
rsconnect::showLogs(appName = "MyApp", streaming = TRUE)
```

---

# 🎓 Teaching Purpose

This repository supports courses and workshops on:

- Introduction to R  
- Environmental data analysis  
- Spatial data handling  
- Interactive visualization  
- Building and deploying Shiny apps  

---

# 📬 Contact

**César Iván Alvarez**  
University of Augsburg  
Email: cesar.alvarez@uni-a.de  
GitHub: https://github.com/osoivan  

If you use or adapt these materials, citations or acknowledgments are appreciated.  
Contributions and pull requests are welcome.
