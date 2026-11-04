
# ============================================================
# PRACTICE: CLIMATE DATA ANALYSIS USING R
# Germany - Temperature and Precipitation (1950-2025)
#
# Source: World Bank Climate Change Knowledge Portal
# Dataset: ERA5 - Historical Climate Data
# https://climateknowledgeportal.worldbank.org/download-data
# ============================================================


# ============================================================
# 1. LOAD LIBRARY
# ============================================================

# Install the library if necessary
# install.packages("readxl")

library(readxl)                      # Read Excel files


# ============================================================
# 2. READ THE EXCEL FILE
# ============================================================

# Location of the Excel file
file_path <- "C:/data/data.xlsx"

# Read the Excel sheet
excel <- read_excel(file_path, sheet = "CCKP Data")

# Explore the original data
head(excel)                          # First six rows
names(excel)                         # Column names
dim(excel)                           # Number of rows and columns


# ============================================================
# 3. PREPARE THE CLIMATE DATA
# ============================================================

# The Excel file contains:
# Column 1: Country code
# Column 2: Country name
# Row 1: Temperature observations
# Row 2: Precipitation observations
# Columns 3 onward: Annual climate values

# Extract the years from the column names
years <- names(excel)[3:ncol(excel)]

# Create a new dataframe with three columns
data <- data.frame(
  Year = as.numeric(substr(years, 1, 4)),
  Temperature = as.numeric(unlist(excel[1, 3:ncol(excel)])),
  Precipitation = as.numeric(unlist(excel[2, 3:ncol(excel)]))
)

# Sort observations by year
data <- data[order(data$Year), ]

# Explore the new dataframe
head(data)                          # First six observations
summary(data)                       # Summary statistics
View(data)                          # Open the table


# ============================================================
# 4. EXPORT THE DATA TO CSV
# ============================================================

# Save CSV in the same folder as the Excel file
output_file <- file.path(
  dirname(file_path),
  "climate_data_germany.csv"
)

write.csv(data, output_file, row.names = FALSE)


# ============================================================
# 5. DESCRIPTIVE STATISTICS
# ============================================================

# Temperature statistics
mean(data$Temperature, na.rm = TRUE)  # Mean temperature
min(data$Temperature, na.rm = TRUE)   # Minimum temperature
max(data$Temperature, na.rm = TRUE)   # Maximum temperature
sd(data$Temperature, na.rm = TRUE)    # Standard deviation

# Precipitation statistics
mean(data$Precipitation, na.rm = TRUE) # Mean precipitation
min(data$Precipitation, na.rm = TRUE)  # Minimum precipitation
max(data$Precipitation, na.rm = TRUE)  # Maximum precipitation
sd(data$Precipitation, na.rm = TRUE)   # Standard deviation


# ============================================================
# 6. LINE PLOT - ANNUAL TEMPERATURE
# ============================================================

plot(data$Year, data$Temperature,
     type = "l",                    # l = lines
     col = "red",                   # Line colour
     lwd = 2,                       # Line width
     main = "Annual Temperature - Germany",
     xlab = "Year",
     ylab = "Temperature (°C)")


# ============================================================
# 7. HISTOGRAM - TEMPERATURE DISTRIBUTION
# ============================================================

hist(data$Temperature,
     col = "lightblue",             # Bar colour
     border = "black",              # Border colour
     breaks = 10,                   # Approximate number of bins
     main = "Temperature Distribution",
     xlab = "Temperature (°C)",
     ylab = "Frequency")


# ============================================================
# 8. BOXPLOT - TEMPERATURE VARIABILITY
# ============================================================

boxplot(data$Temperature,
        col = "lightgreen",         # Box colour
        main = "Temperature Variability",
        ylab = "Temperature (°C)")


# ============================================================
# 9. SCATTER PLOT - TEMPERATURE VS PRECIPITATION
# ============================================================

plot(data$Temperature, data$Precipitation,
     type = "p",                    # p = points
     pch = 16,                      # Solid circles
     col = "blue",                  # Point colour
     main = "Temperature vs Precipitation",
     xlab = "Temperature (°C)",
     ylab = "Precipitation (mm)")


# ============================================================
# 10. LINE PLOT - ANNUAL PRECIPITATION
# ============================================================


# 1. Plot annual precipitation
plot(data$Year, data$Precipitation,
     type = "l",                    # Line graph
     col = "blue",
     lwd = 2,
     ylim = c(0, 1100),             # Vertical axis limits
     main = "Annual Climate Data",
     xlab = "Year",
     ylab = "Precipitation (mm)")

# 2. Add another line to the same graph
lines(data$Year, data$Temperature,
      col = "red",                  # Second line in red
      lwd = 2)

# 3. Add a legend
legend("topright",
       legend = c("Precipitation", "Temperature"),
       col = c("blue", "red"),
       lty = 1,
       lwd = 2)


# ============================================================
# 11. BAR PLOT - ANNUAL PRECIPITATION
# ============================================================

barplot(data$Precipitation,
        names.arg = data$Year,      # Year labels
        col = "skyblue",            # Bar colour
        border = NA,                # No bar borders
        main = "Annual Precipitation",
        xlab = "Year",
        ylab = "Precipitation (mm)",
        las = 2,                    # Rotate labels
        cex.names = 0.5)            # Smaller text


# ============================================================
# 12. CORRELATION
# ============================================================

# Calculate correlation between temperature and precipitation
cor(data$Temperature,
    data$Precipitation,
    use = "complete.obs")


# ============================================================
# 13. SIMPLE LINEAR REGRESSION
# ============================================================

# Model: Temperature = b0 + b1 * Year
model <- lm(Temperature ~ Year, data = data)

# Display model results
summary(model)

# Plot original temperature observations
plot(data$Year, data$Temperature,
     type = "p",                    # Scatter plot
     pch = 16,                      # Solid circles
     col = "blue",
     main = "Linear Temperature Trend",
     xlab = "Year",
     ylab = "Temperature (°C)")

# Add regression line
abline(model, col = "red", lwd = 2)


# ============================================================
# 14. PREDICT TEMPERATURE FOR FUTURE YEARS
# ============================================================

# Create dataframe with future years
future <- data.frame(
  Year = c(2030, 2040, 2050)
)

# Predict temperature using the linear model
future$Temperature <- predict(model, newdata = future)

# Display predictions
print(future)

# Export predictions to CSV
write.csv(
  future,
  file.path(dirname(file_path), "temperature_predictions.csv"),
  row.names = FALSE
)


# ============================================================
# 15. OPTIONAL - EXPORT THE REGRESSION GRAPH
# ============================================================

# Save the regression plot as a PNG image
png(file.path(dirname(file_path), "temperature_trend.png"),
    width = 1000, height = 600)

plot(data$Year, data$Temperature,
     pch = 16,
     col = "blue",
     main = "Linear Temperature Trend",
     xlab = "Year",
     ylab = "Temperature (°C)")

abline(model, col = "red", lwd = 2)

dev.off()                         # Close and save the image
