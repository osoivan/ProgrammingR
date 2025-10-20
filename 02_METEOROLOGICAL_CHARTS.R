# ==========================================================
# CLEAN DWD TEMPERATURE DATA AND IMPROVE PLOT
# ==========================================================

# 1) Install & load packages
packages <- c("dplyr", "ggplot2", "readr", "lubridate")
new_pkgs <- packages[!(packages %in% installed.packages()[, "Package"])]
if(length(new_pkgs)) install.packages(new_pkgs)
lapply(packages, library, character.only = TRUE)

# ----------------------------------------------------------
# 2) Read directly from ZIP
# Source: https://opendata.dwd.de/climate_environment/CDC/observations_germany/climate/hourly/air_temperature/recent/
zip_path <- "C:/GERMANYINTERACTIVEMAP/stundenwerte_TU_00078_akt.zip"  # or full path if in another folder
zip_list <- unzip(zip_path, list = TRUE)
data_file <- zip_list$Name[grepl("produkt_tu_stunde_.*\\.txt$", zip_list$Name)]

df <- read_csv2(unz(zip_path, data_file), locale = locale(decimal_mark = ","))

# ----------------------------------------------------------
# 3) Clean data
df <- df %>%
  mutate(
    Datetime = ymd_h(MESS_DATUM),
    Temperature_C = as.numeric(TT_TU)
  ) %>%
  # Remove invalid or missing values (-999 or NA)
  filter(!is.na(Temperature_C), Temperature_C > -100) %>%
  arrange(Datetime)

cat("✅ Clean records:", nrow(df), "\n")

# ----------------------------------------------------------
# 4) Plot hourly temperature (clean)
ggplot(df, aes(x = Datetime, y = Temperature_C)) +
  geom_line(color = "firebrick", linewidth = 0.5) +
  theme_minimal(base_size = 14) +
  labs(
    title = "Hourly Air Temperature (°C)",
    subtitle = paste("Cleaned DWD data from", basename(zip_path)),
    x = "Date and Time",
    y = "Temperature (°C)"
  )

# ----------------------------------------------------------
# 5) Compute daily means
daily_df <- df %>%
  group_by(Date = as.Date(Datetime)) %>%
  summarise(DailyMeanTemp = mean(Temperature_C, na.rm = TRUE))

# ----------------------------------------------------------
# 6) Plot daily mean temperature (smoothed)
ggplot(daily_df, aes(x = Date, y = DailyMeanTemp)) +
  geom_line(color = "steelblue", linewidth = 0.8) +
  geom_smooth(span = 0.2, color = "darkred", se = FALSE) +
  theme_minimal(base_size = 14) +
  labs(
    title = "Daily Mean Air Temperature (°C)",
    subtitle = "Computed from hourly data",
    x = "Date",
    y = "Mean Temperature (°C)"
  )

# ----------------------------------------------------------
# 7) Save cleaned data
write_csv(df, "temperature_hourly_clean.csv")
write_csv(daily_df, "temperature_daily_mean.csv")
cat("💾 Saved cleaned files: 'temperature_hourly_clean.csv' and 'temperature_daily_mean.csv'\n")
