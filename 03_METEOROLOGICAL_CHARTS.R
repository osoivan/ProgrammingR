
# =====================================================
# EXERCISE: HOURLY AND DAILY AIR TEMPERATURE
# Data source: German Weather Service (DWD)
#
# Download data from:
# https://opendata.dwd.de/climate_environment/CDC/observations_germany/climate/hourly/air_temperature/recent/
#
# Download a ZIP file and extract it manually.
# The file "produkt_tu_stunde_..." contains hourly
# air temperature (TT_TU) and relative humidity (RF_TU).
# =====================================================


# 1. LOAD LIBRARIES

# Install ggplot2 if necessary:
# install.packages("ggplot2")

library(ggplot2)


# 2. READ THE TEMPERATURE DATA

# Folder containing the extracted DWD files
folder <- "C:/GERMANYINTERACTIVEMAP/stundenwerte_TU_00078_akt"

# Name of the temperature file
filename <- "produkt_tu_stunde_20250426_20261027_00078.txt"

# Read the file
df <- read.csv(file.path(folder, filename), sep = ";")

# Display the first observations
head(df)

# Display variable names
names(df)

# Summary statistics
summary(df)


# 3. PREPARE THE DATA

# Convert measurement date to date-time format
# DWD dates follow YYYYMMDDHH (UTC)
df$DateTime <- as.POSIXct(
  as.character(df$MESS_DATUM),
  format = "%Y%m%d%H",
  tz = "UTC"
)

# Create a temperature variable
df$Temperature <- df$TT_TU

# Remove missing and invalid measurements
df <- df[!is.na(df$DateTime), ]
df <- df[!is.na(df$Temperature), ]
df <- df[df$Temperature != -999, ]

# Display the cleaned data
head(df)


# 4. EXPLORE HOURLY TEMPERATURE

# Calculate basic statistics
mean(df$Temperature)
min(df$Temperature)
max(df$Temperature)
sd(df$Temperature)

# Plot hourly air temperature
ggplot(df, aes(x = DateTime, y = Temperature)) +
  geom_line(color = "red") +
  labs(
    title = "Hourly Air Temperature",
    x = "Date (UTC)",
    y = "Temperature (°C)"
  ) +
  theme_minimal()


# 5. CALCULATE DAILY MEAN TEMPERATURE

# Extract the date (without the hour)
df$Date <- as.Date(df$DateTime)

# Calculate mean temperature for each day
daily_df <- aggregate(
  Temperature ~ Date,
  data = df,
  FUN = mean
)

# Rename the temperature column
names(daily_df)[2] <- "MeanTemp"

# Display daily results
head(daily_df)


# 6. PLOT DAILY MEAN TEMPERATURE

ggplot(daily_df, aes(x = Date, y = MeanTemp)) +
  geom_line(color = "blue") +
  labs(
    title = "Daily Mean Air Temperature",
    x = "Date (UTC)",
    y = "Mean Temperature (°C)"
  ) +
  theme_minimal()


# 7. SAVE THE RESULTS

# Save hourly temperature
write.csv(
  df,
  file.path(folder, "hourly_temperature.csv"),
  row.names = FALSE
)

# Save daily mean temperature
write.csv(
  daily_df,
  file.path(folder, "daily_temperature.csv"),
  row.names = FALSE
)


# =====================================================
# OPTIONAL: ANOTHER WAY TO CALCULATE DAILY MEANS
# =====================================================

# The same calculation can be performed using dplyr.
# The operator %>% passes the result of one operation
# to the next operation.
#
# First install and load dplyr:
# install.packages("dplyr")
# library(dplyr)
#
# daily_df <- df %>%
#   group_by(Date) %>%
#   summarise(MeanTemp = mean(Temperature))
#
# Both methods produce the same daily mean temperatures.
