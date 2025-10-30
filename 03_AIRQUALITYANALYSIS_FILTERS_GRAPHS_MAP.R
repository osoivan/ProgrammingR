# ==============================================================
# 🧭 Objective:
# The objective of this code is to compare the air quality between
# European countries using public data (PM2.5 levels). PM2.5 is a key
# air pollutant related to respiratory and cardiovascular diseases.
# ==============================================================

# --------------------------------------------------------------
# 1️⃣ Load packages
# --------------------------------------------------------------

# We can comment these lines after the first installation
# install.packages("readxl")
# install.packages("dplyr")
# install.packages("ggplot2")
# install.packages("leaflet")

library(readxl)
library(dplyr)
library(ggplot2)
library(leaflet)

# --------------------------------------------------------------
# 2️⃣ Load dataset
# --------------------------------------------------------------

# Read Excel file (choose sheet number if multiple sheets exist)
table <- read_excel("C:/data/Europe (2023).xlsx", sheet = 2)

# Source: European Environment Agency (EEA)
# https://www.eea.europa.eu/en/analysis/publications/air-quality-status-report-2025

# Check the column names available in the file
colnames(table)

# --------------------------------------------------------------
# 3️⃣ Filter and analyze basic statistics
# --------------------------------------------------------------

# EU annual limit for PM2.5 = 25 µg/m³
# WHO guideline for PM2.5 = 5 µg/m³

# Filter stations exceeding the EU limit
EUannual <- table[table$`Air Pollution Level` > 25, ]

# Count how many stations per country exceed the EU limit
counts <- table(EUannual$Country1)

# Create a pie chart of stations exceeding the EU limit
pie(
  counts,
  main = "Stations with PM2.5 Above EU Annual Limit",
  col = rainbow(length(counts)),
  labels = paste(names(counts), "(", counts, ")", sep = "")
)

# Example: Filter stations in Germany above WHO limit
germany <- table[table$`Air Pollution Level` > 5 & table$Country1 == "Germany", ]
nrow(germany) / nrow(table)  # proportion of high-level stations in Germany

# --------------------------------------------------------------
# 4️⃣ Add classification column for Air Quality
# --------------------------------------------------------------

# Classify each station by its PM2.5 level
table$Air_Status <- ifelse(
  table$`Air Pollution Level` >= 25, "Above EU limits",
  ifelse(
    table$`Air Pollution Level` >= 5 & table$`Air Pollution Level` < 25,
    "Above WHO limits",
    "Air Quality Acceptable"
  )
)

# --------------------------------------------------------------
# 5️⃣ Summary visualization: Air Quality status (pie chart)
# --------------------------------------------------------------

# Count number of stations in each air quality category
status_counts <- as.data.frame(table(table$Air_Status))
colnames(status_counts) <- c("Status", "Count")

# Compute percentages for the pie chart
status_counts$Percent <- round(100 * status_counts$Count / sum(status_counts$Count), 1)

# Create pie chart to visualize air quality status
ggplot(status_counts, aes(x = "", y = Count, fill = Status)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  geom_text(
    aes(label = paste0(Percent, "%")),
    position = position_stack(vjust = 0.5),
    color = "white", size = 5, fontface = "bold"
  ) +
  scale_fill_manual(values = c(
    "Above EU limits" = "red",
    "Above WHO limits" = "orange",
    "Air Quality Acceptable" = "forestgreen"
  )) +
  labs(
    title = "Air Quality Status Among European Stations",
    fill = "Air Quality Category"
  ) +
  theme_void(base_size = 14)

# --------------------------------------------------------------
# 6️⃣ Country-level summary: mean and max PM2.5
# --------------------------------------------------------------

data <- table %>%
  group_by(Country1) %>%
  summarise(
    mean_pm25 = mean(`Air Pollution Level`, na.rm = TRUE),
    max_pm25 = max(`Air Pollution Level`, na.rm = TRUE),
    count = n()
  )

# --------------------------------------------------------------
# 7️⃣ Visualization: Mean PM2.5 by Country
# --------------------------------------------------------------

# Order countries by mean PM2.5 (descending)
data <- data[order(data$mean_pm25, decreasing = TRUE), ]
data$Country1 <- factor(data$Country1, levels = data$Country1)

# Create bar plot to compare average PM2.5 among countries
ggplot(data, aes(x = Country1, y = mean_pm25, fill = mean_pm25)) +
  geom_bar(stat = "identity") +
  geom_hline(yintercept = 25, color = "red", linetype = "dashed", size = 1) +
  geom_hline(yintercept = 5, color = "orange", linetype = "dashed", size = 1) +
  labs(
    title = "Average PM2.5 Concentration by Country (2023)",
    x = "Country",
    y = "Mean PM2.5 (µg/m³)"
  ) +
  scale_fill_gradient(low = "skyblue", high = "darkred") +
  theme_minimal(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "none"
  )

# --------------------------------------------------------------
# 8️⃣ Interactive Map: Air Quality by Station (Leaflet)
# --------------------------------------------------------------

# Define color palette for Air_Status
pal <- leaflet::colorFactor(
  palette = c(
    "Above EU limits" = "red",
    "Above WHO limits" = "orange",
    "Air Quality Acceptable" = "green"
  ),
  domain = table$Air_Status
)

# Create interactive map
leaflet(table) %>%
  addTiles() %>%  # Base map (OpenStreetMap)
  addCircleMarkers(
    lng = ~Longitude,
    lat = ~Latitude,
    color = ~pal(Air_Status),
    radius = 3,
    stroke = TRUE,
    weight = 1,
    opacity = 1,
    fillOpacity = 0.8,
    popup = ~paste0(
      "<b>", Country1, "</b><br>",
      "PM2.5 Level: ", round(`Air Pollution Level`, 2), " µg/m³<br>",
      "Status: <b>", Air_Status, "</b>"
    )
  ) %>%
  addLegend(
    "bottomright",
    pal = pal,
    values = ~Air_Status,
    title = "Air Quality Status",
    opacity = 1
  )
