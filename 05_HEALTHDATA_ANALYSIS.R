# ============================================================
# PRACTICE: RESPIRATORY MORTALITY IN GERMANY (1998-2024)
# Data: German Federal Health Reporting (GBE), ICD-10 J00-J99
# https://www.gbe-bund.de/gbe/?p_uid=gast&p_aid=36040623&p_sprache=E
# Map: BKG NUTS5000_N1 (German federal states)
# https://gdz.bkg.bund.de/index.php/default/open-data/nuts-gebiete-1-5-000-000-stand-31-12-nuts5000-31-12.html
# ============================================================

# 1. SET YOUR WORKING FOLDER AND LOAD LIBRARIES
setwd("C:/data")                          # Folder containing the files
# install.packages(c("writexl", "ggplot2", "sf"))  # Run once if needed
library(writexl)                          # Export Excel files
library(ggplot2)                          # Create graphs
library(sf)                               # Read shapefiles

# 2. READ THE ORIGINAL CSV
# The downloaded file has extra metadata and footnotes.
raw <- readLines("Indikator.csv", encoding = "UTF-8", warn = FALSE)
rows <- raw[grepl('^(" *[0-9]{4}";|;")', raw)] # Keep data rows only

df <- read.csv2(text = paste(rows, collapse = "\n"),
                header = FALSE, colClasses = "character")
df <- df[, 1:5]                       # Keep the five useful columns
names(df) <- c("Year", "Region", "Deaths", "Rate_total", "Rate_age_std")
head(df)                              # Explore the imported table

# 3. CLEAN THE TABLE
df$Year <- as.integer(trimws(df$Year)) # Convert the year to a number
for (i in 2:nrow(df)) {              # Fill empty years from the row above
  if (is.na(df$Year[i])) df$Year[i] <- df$Year[i - 1]
}
df$Region <- trimws(df$Region)       # Remove extra spaces
# Repair names if umlauts were damaged in the source text
df$Region[grepl("^Baden-W", df$Region)] <- "Baden-Württemberg"
df$Region[grepl("^Th", df$Region)] <- "Thüringen"

df$Deaths <- as.integer(gsub(".", "", df$Deaths, fixed = TRUE))
df$Rate_total <- as.numeric(gsub(",", ".", df$Rate_total, fixed = TRUE))
df$Rate_age_std <- as.numeric(gsub(",", ".", df$Rate_age_std, fixed = TRUE))

# 4. KEEP ONLY THE 16 FEDERAL STATES
df_states <- df[df$Region != "Deutschland", ] # Remove Germany total
df_states <- df_states[!grepl("Bundesgebiet|^Neue L", df_states$Region), ]
df_states <- df_states[order(df_states$Year, df_states$Region), ]
row.names(df_states) <- NULL
head(df_states)                      # Inspect the cleaned table
nrow(df_states)                      # Expected: 432 state-year observations

# IF / ELSE: check if we have all 16 German federal states
n_states <- length(unique(df_states$Region))  # Count different states

if (n_states == 16) {               # Condition: is the number equal to 16?
  print("Data ready: 16 federal states found.")  # Runs if TRUE
} else {
  print(paste("Check the data: only", n_states, "states found.")) # If FALSE
}

# 5. CALCULATE SIMPLE SUMMARIES
# The yearly mean is an unweighted average of the 16 states.
by_year <- aggregate(Rate_age_std ~ Year, data = df_states, FUN = mean)
names(by_year)[2] <- "Mean_age_std"

by_state <- aggregate(Rate_age_std ~ Region, data = df_states, FUN = mean)
names(by_state)[2] <- "Mean_age_std"
by_state <- by_state[order(-by_state$Mean_age_std), ] # Highest first
print(by_year)
print(by_state)

# Compare the two rates (association, not causation)
cor(df_states$Rate_total, df_states$Rate_age_std, use = "complete.obs")

# 6. BASIC GRAPHS
# A. Mean age-standardized rate across states over time
plot(by_year$Year, by_year$Mean_age_std,
     type = "l", col = "blue", lwd = 2,
     main = "Mean State Mortality Rate (1998-2024)",
     xlab = "Year", ylab = "Age-standardized deaths per 100,000")

# B. Boxplot: differences between states each year
ggplot(df_states, aes(x = factor(Year), y = Rate_age_std)) +
  geom_boxplot(fill = "lightblue") +
  theme_minimal() +
  labs(title = "Mortality Rates Across States", x = "Year",
       y = "Age-standardized deaths per 100,000") +
  theme(axis.text.x = element_text(angle = 90, size = 7))

# C. Heatmap: rates by state and year
ggplot(df_states, aes(x = Year, y = Region, fill = Rate_age_std)) +
  geom_tile() +
  scale_fill_gradient(low = "lightyellow", high = "red") +
  theme_minimal() +
  labs(title = "Respiratory Mortality by State and Year",
       fill = "Deaths per 100,000")

# D. Bar chart: ranking in the most recent year
latest_year <- max(df_states$Year)
latest <- df_states[df_states$Year == latest_year, ]
latest <- latest[order(latest$Rate_age_std), ]
barplot(latest$Rate_age_std, names.arg = latest$Region,
        horiz = TRUE, las = 1, cex.names = 0.65,
        col = "steelblue", main = paste("Mortality Rate -", latest_year),
        xlab = "Age-standardized deaths per 100,000")

# 7. SIMPLE LINEAR REGRESSION
model <- lm(Mean_age_std ~ Year, data = by_year)
summary(model)                       # View the annual trend and R-squared
plot(by_year$Year, by_year$Mean_age_std,
     pch = 16, col = "blue",
     main = "Linear Trend of Mean State Mortality",
     xlab = "Year", ylab = "Age-standardized deaths per 100,000")
abline(model, col = "red", lwd = 2) # Add the regression line

# 8. CREATE THE MAP (NUTS 1 = FEDERAL STATES)
# Extract the shapefile ZIP and put the .shp/.shx/.dbf/.prj files together.
shp <- st_read("NUTS250_N1.shp")
names(shp)                           # Check that NUTS_NAME exists
st_crs(shp)                          # Display the coordinate system

# Attach mortality rates by matching the state names.
shp$NUTS_NAME <- trimws(shp$NUTS_NAME)
shp$Rate_age_std <- latest$Rate_age_std[match(shp$NUTS_NAME, latest$Region)]
print(shp$NUTS_NAME[is.na(shp$Rate_age_std)]) # Should be character(0)

ggplot(shp) +
  geom_sf(aes(fill = Rate_age_std), color = "white") +
  scale_fill_gradient(low = "lightyellow", high = "darkred", na.value = "grey80") +
  theme_minimal() +
  labs(title = paste("Respiratory Mortality -", latest_year),
       fill = "Deaths per 100,000")

# 9. OPTIONAL: MAP OF CHANGE OVER THE LAST 10 YEARS
# Compare the same age-standardized rate in 2014 and 2024.
old <- df_states[df_states$Year == latest_year - 10, ]
shp$Rate_old <- old$Rate_age_std[match(shp$NUTS_NAME, old$Region)]
shp$Change_10yr <- 100 * (shp$Rate_age_std - shp$Rate_old) / shp$Rate_old

ggplot(shp) +
  geom_sf(aes(fill = Change_10yr), color = "white") +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0) +
  theme_minimal() +
  labs(title = paste("Change from", latest_year - 10, "to", latest_year),
       fill = "Change (%)")

# 10. EXPORT TABLES
write_xlsx(df_states, "mortality_clean.xlsx")
write.csv(df_states, "mortality_clean.csv", row.names = FALSE)
write.csv(by_year, "summary_by_year.csv", row.names = FALSE)
write.csv(by_state, "summary_by_state.csv", row.names = FALSE)
