# ==========================================================
# READ DWD HOURLY TEMPERATURE + METADATA (FIXED VERSION)
# ==========================================================

# 1) Install & load libraries
packages <- c("dplyr", "ggplot2", "readr", "lubridate", "stringr")
new_pkgs <- packages[!(packages %in% installed.packages()[,"Package"])]
if(length(new_pkgs)) install.packages(new_pkgs)
lapply(packages, library, character.only = TRUE)

# ----------------------------------------------------------
# 2) Path to your DWD ZIP file
zip_path <- "C:/GERMANYINTERACTIVEMAP/stundenwerte_TU_00078_akt.zip"  # or full path if in another folder

# ----------------------------------------------------------
# 3) Find files inside the ZIP
zip_list <- unzip(zip_path, list = TRUE)
data_file <- zip_list$Name[grepl("produkt_tu_stunde_.*\\.txt$", zip_list$Name)]
meta_file <- zip_list$Name[grepl("Metadaten_Geraete_Lufttemperatur_.*\\.txt$", zip_list$Name)]

cat("✅ Data file:", data_file, "\n")
cat("✅ Metadata file:", meta_file, "\n")

# ----------------------------------------------------------
# 4) Read metadata table correctly
meta_raw <- readLines(unz(zip_path, meta_file), encoding = "latin1")

# Find the line where the actual table starts
start_line <- grep("^Stations_ID", meta_raw)
if(length(start_line) == 0) start_line <- grep("^Stationsname", meta_raw)
meta_df <- read_delim(
  unz(zip_path, meta_file),
  delim = ";",
  skip = start_line - 1,
  locale = locale(decimal_mark = ".", encoding = "latin1"),
  show_col_types = FALSE
)

# Extract station info
station_name <- meta_df$Stationsname[1]
longitude <- meta_df$`Geo. Laenge [Grad]`[1]
latitude <- meta_df$`Geo. Breite [Grad]`[1]

cat("📍 Station:", station_name, "\n")
cat("   Longitude:", longitude, "\n")
cat("   Latitude:", latitude, "\n")

# ----------------------------------------------------------
# 5) Read and clean hourly temperature data
df <- read_csv2(unz(zip_path, data_file), locale = locale(decimal_mark = ",")) %>%
  mutate(
    Datetime = ymd_h(MESS_DATUM),
    Temperature_C = as.numeric(TT_TU)
  ) %>%
  filter(!is.na(Temperature_C), Temperature_C > -100) %>%
  arrange(Datetime)

cat("✅ Hourly records:", nrow(df), "\n")

# ----------------------------------------------------------
# 6) Plot hourly temperature
title_hourly <- paste0(
  "Hourly Air Temperature (°C) - ", station_name,
  "\nLat: ", latitude, " | Lon: ", longitude
)

ggplot(df, aes(x = Datetime, y = Temperature_C)) +
  geom_line(color = "firebrick", linewidth = 0.5) +
  theme_minimal(base_size = 14) +
  labs(
    title = title_hourly,
    subtitle = basename(zip_path),
    x = "Date & Time",
    y = "Temperature (°C)"
  )

# ----------------------------------------------------------
# 7) Compute and plot daily means
daily_df <- df %>%
  group_by(Date = as.Date(Datetime)) %>%
  summarise(DailyMeanTemp = mean(Temperature_C, na.rm = TRUE))

title_daily <- paste0(
  "Daily Mean Temperature (°C) - ", station_name,
  "\nLat: ", latitude, " | Lon: ", longitude
)

ggplot(daily_df, aes(x = Date, y = DailyMeanTemp)) +
  geom_line(color = "steelblue", linewidth = 0.8) +
  geom_smooth(span = 0.2, color = "darkred", se = FALSE) +
  theme_minimal(base_size = 14) +
  labs(
    title = title_daily,
    subtitle = "Computed from hourly DWD data",
    x = "Date",
    y = "Mean Temperature (°C)"
  )

# ----------------------------------------------------------
# 8) Save outputs
write_csv(df, paste0("hourly_temperature_", station_name, ".csv"))
write_csv(daily_df, paste0("daily_temperature_", station_name, ".csv"))
cat("💾 Saved cleaned CSVs for station:", station_name, "\n")
