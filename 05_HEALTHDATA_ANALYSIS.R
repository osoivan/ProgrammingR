# ============================================================
# 0️⃣ Install & Load Libraries (auto-check)
# ============================================================
# This loop checks whether each package is installed.
# If not, R installs it. Then the library() command loads it.
packages <- c("dplyr", "tidyr", "ggplot2", "stringr", "readxl", 
              "sf", "viridis", "corrplot", "GGally", "gganimate")

for (p in packages) {
  if (!require(p, character.only = TRUE)) {
    install.packages(p)
    library(p, character.only = TRUE)
  }
}

setwd("C:/data")  # set working directory where your files are stored
# DATA FROM
# https://www.gbe-bund.de/gbe/?p_uid=gast&p_aid=36040623&p_sprache=E
# https://gdz.bkg.bund.de/index.php/default/digitale-geodaten/verwaltungsgebiete/nuts-gebiete-1-5-000-000-stand-31-12-nuts5000-31-12.html

# ============================================================
# 1️⃣ Load Dataset
# ============================================================
# The Excel file contains respiratory mortality statistics in Germany.
# We skip the first 5 rows because they contain metadata.
file_path <- "Indikator.xlsx"
df_raw <- read_excel(file_path)

# ============================================================
# 2️⃣ Rename Columns
# ============================================================
# We rename columns to simpler names for easier coding.
names(df_raw) <- c("Year", "Region", "Deaths", "Rate_total", "Rate_age_std")

# ============================================================
# 3️⃣ Clean Data
# ============================================================
# - Remove empty rows
# - Convert columns to numeric
# - Trim region names
# - Remove aggregated rows (Old East/West Germany)
df <- df_raw %>%
  filter(!is.na(Year)) %>%
  mutate(
    Region = str_trim(Region),
    Deaths = as.numeric(Deaths),
    Rate_total = as.numeric(Rate_total),
    Rate_age_std = as.numeric(Rate_age_std),
    Year = as.integer(Year)
  )

df_states <- df %>% filter(
  !Region %in% c("Früheres Bundesgebiet und Berlin-Ost",
                 "Neue Länder ohne Berlin-Ost")
)

# ============================================================
# 4️⃣ Summary Statistics by Year
# ============================================================
# This summarizes the national trend:
# - Mean mortality rate
# - Median
# - Min and max values per year
summary_stats <- df_states %>%
  group_by(Year) %>%
  summarise(
    mean_rate = mean(Rate_total, na.rm = TRUE),
    median_rate = median(Rate_total, na.rm = TRUE),
    min_rate = min(Rate_total, na.rm = TRUE),
    max_rate = max(Rate_total, na.rm = TRUE)
  )

print(summary_stats)

# ============================================================
# 5️⃣ Extended State-Level Statistics
# ============================================================
# This compares states in terms of:
# - Mean mortality rate
# - Standard deviation (variability)
# - Coefficient of variation (%)
# - Long-term trend (% change across all years)
# - Mean age-standardized rate (better comparison)
state_summary <- df_states %>%
  group_by(Region) %>%
  summarise(
    mean_rate = mean(Rate_total, na.rm = TRUE),
    sd_rate = sd(Rate_total, na.rm = TRUE),
    cv_rate = sd_rate / mean_rate * 100,
    trend_rate_pct = (last(Rate_total, order_by = Year) -
                        first(Rate_total, order_by = Year)) /
      first(Rate_total, order_by = Year) * 100,
    mean_age_std = mean(Rate_age_std, na.rm = TRUE)
  ) %>%
  arrange(desc(mean_rate))

print(state_summary)

# Correlation: does age-standardized mortality explain total mortality?
cor_value <- cor(df_states$Rate_total, df_states$Rate_age_std, use = "complete.obs")
print(paste("Correlation:", round(cor_value, 3)))

# ============================================================
# 6️⃣ Additional Analytical Methods
# ============================================================

# ⭐ Year-over-year change (how mortality changes each year)
df_change <- df_states %>%
  arrange(Region, Year) %>%
  group_by(Region) %>%
  mutate(
    change_rate = Rate_total - lag(Rate_total),
    pct_change = (Rate_total - lag(Rate_total)) / lag(Rate_total) * 100
  )

# ⭐ Find best and worst performing states per year
top_bottom <- df_states %>%
  group_by(Year) %>%
  summarise(
    top_state = Region[which.max(Rate_total)],
    top_value = max(Rate_total),
    bottom_state = Region[which.min(Rate_total)],
    bottom_value = min(Rate_total)
  )

print(top_bottom)

# ⭐ Linear regression model of national trend
# Useful to determine if mortality is increasing or decreasing over time.
nat_model <- lm(Rate_total ~ Year, data = df_states)
summary(nat_model)

# ⭐ Correlation matrix (multivariate analysis)
num_data <- df_states %>% select(Deaths, Rate_total, Rate_age_std)
corr_matrix <- cor(num_data, use = "complete.obs")
corrplot(corr_matrix, method = "color", addCoef.col = "black")

# ============================================================
# 7️⃣ Graphs & Visualizations
# ============================================================

## 📈 Plot 1 — Trends by State
# Shows mortality changes over time for each state.
ggplot(df_states, aes(Year, Rate_total, color = Region)) +
  geom_line(size = 1) + geom_point(size = 2) +
  theme_bw() + labs(title = "Mortality Rate by State")

## 🔥 Plot 2 — Heatmap of mortality
# Shows intensity of mortality by region and year.
ggplot(df_states, aes(Year, Region, fill = Rate_total)) +
  geom_tile() +
  scale_fill_gradient(low="white", high="red") +
  theme_bw() + labs(title = "Heatmap of Mortality Rates")

## 🏆 Plot 3 — Ranking barplot
# Identifies states with highest/lowest mortality each year.
ggplot(df_states, aes(reorder(Region, Rate_total), Rate_total)) +
  geom_col(fill="steelblue") + coord_flip() +
  facet_wrap(~Year) + theme_bw() +
  labs(title = "State Rankings by Mortality")

## 📦 Plot 4 — Boxplot by year
# Shows distribution and variation of mortality between states.
ggplot(df_states, aes(factor(Year), Rate_total)) +
  geom_boxplot(fill="skyblue") +
  theme_bw() + labs(title = "Annual Mortality Distribution")

## 🗺️ Plot 5 — Faceted trends
# One trend graph per state (small multiples).
ggplot(df_states, aes(Year, Rate_total)) +
  geom_line(color="darkred") +
  facet_wrap(~Region, scales="free_y") +
  theme_bw() + labs(title="Trend by State")

## 🔗 Plot 6 — Scatter + regression
# Shows relationship between total and age-standardized mortality.
ggplot(df_states, aes(Rate_age_std, Rate_total, color=Region)) +
  geom_point(size=2) +
  geom_smooth(method="lm", color="black") +
  theme_bw() +
  labs(title="Relationship Between Total vs Age-Standardized Rate")

## 🔍 Plot 7 — Pair Plot
# Multivariate exploration of all numeric variables.
GGally::ggpairs(num_data)

## 📊 Plot 8 — National Mean ± SD
# Shows uncertainty around national mortality trend.
df_year_stats <- df_states %>%
  group_by(Year) %>%
  summarise(
    mean_rate = mean(Rate_total),
    sd_rate = sd(Rate_total)
  )

ggplot(df_year_stats, aes(Year, mean_rate)) +
  geom_line(size=1.2, color="blue") +
  geom_ribbon(aes(ymin=mean_rate-sd_rate, ymax=mean_rate+sd_rate),
              fill="lightblue", alpha=.3) +
  theme_bw() + labs(title="Mean Mortality ± SD")

## ↕️ Plot 9 — Diverging Barplot
# Shows which states are above or below national average.
avg_rate <- mean(df_states$Rate_total)
df_div <- df_states %>%
  filter(Year == max(Year)) %>%
  mutate(diff = Rate_total - avg_rate,
         dir = ifelse(diff > 0, "Above","Below"))

ggplot(df_div, aes(reorder(Region, diff), diff, fill=dir)) +
  geom_col() + coord_flip() +
  scale_fill_manual(values=c("Above"="red","Below"="blue")) +
  theme_bw() + labs(title = "Deviation from National Average")

# ============================================================
# 8️⃣ Spatial Mapping (Shapefile NUTS5000_N1)
# ============================================================

# Load Germany NUTS-level regions
# The CRS is not EPSG:4326 so we convert it for mapping.
shp <- st_read("NUTS5000_N1.shp")
shp <- st_transform(shp, 4326)

# Select latest year with complete data
latest_year <- max(df_states$Year) - 1
df_latest <- df_states %>% filter(Year == latest_year)

# Join shapefile with mortality data using NUTS_NAME
map_latest <- shp %>%
  left_join(df_latest, by = c("NUTS_NAME" = "Region"))

## 🗺️ Map 1 — Basic Choropleth
# Visualizes mortality levels geographically across Germany.
ggplot(map_latest) +
  geom_sf(aes(fill = Rate_total)) +
  scale_fill_viridis(option = "magma", direction = -1) +
  theme_minimal() +
  labs(title = paste("Mortality Rate —", latest_year))

## 🗺️ Map 2 — Percent Change (10-year)
# Shows where mortality improved or worsened in 10 years.
df_change_map <- df_states %>%
  group_by(Region) %>%
  summarise(pct10 = (last(Rate_total) - first(Rate_total)) / first(Rate_total) * 100)

map_change <- shp %>%
  left_join(df_change_map, by = c("NUTS_NAME"="Region"))

ggplot(map_change) +
  geom_sf(aes(fill = pct10)) +
  scale_fill_gradient2(low="blue", mid="white", high="red", midpoint=0) +
  theme_minimal() +
  labs(title = "10-Year Percent Change in Mortality Rate")

# ============================================================
# 9️⃣ Export summary tables
# ============================================================
write.csv(summary_stats, "summary_by_year.csv", row.names = FALSE)
write.csv(state_summary, "summary_by_state.csv", row.names = FALSE)
write.csv(top_bottom, "top_bottom.csv", row.names = FALSE)

