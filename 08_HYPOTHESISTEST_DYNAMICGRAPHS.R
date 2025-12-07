# ============================================================
#   AIR POLLUTION ANALYSIS IN EUROPE (2023)
#   Full clean script with formal analytical explanations
# ============================================================

# ---------------------------------------
# 1. Load libraries
# ---------------------------------------
library(tidyverse)   # Data manipulation + ggplot2 plotting
library(readxl)      # Read Excel files
library(rstatix)     # Easy statistical tests
library(plotly)      # Interactive charts

# ---------------------------------------
# 2. Set working directory
# ---------------------------------------
setwd("C:/data")   # Change to your path

# ---------------------------------------
# 3. Load Excel (Sheet 2) and rename columns
# Data available on https://www.eea.europa.eu/en/analysis/publications/air-quality-status-report-2025/particulate-matter-pm2.5
# ---------------------------------------
df <- read_excel("PM25_2023.xlsx", sheet = 2)

df <- df %>% 
  rename(
    AirPollution = `Air Pollution Level`,
    Country = Country1
  )

cat("=== First rows of the dataset ===\n")
print(head(df))


# ============================================================
# 4. DESCRIPTIVE STATISTICS
# ============================================================

cat("\n=== BASIC DESCRIPTIVE STATISTICS ===\n")
print(summary(df$AirPollution))

descriptive_stats <- df %>%
  summarise(
    Mean = mean(AirPollution, na.rm = TRUE),
    Median = median(AirPollution, na.rm = TRUE),
    SD = sd(AirPollution, na.rm = TRUE),
    Min = min(AirPollution, na.rm = TRUE),
    Max = max(AirPollution, na.rm = TRUE)
  )

cat("\n=== Detailed Descriptive Statistics ===\n")
print(descriptive_stats)


# ============================================================
# 5. VISUALIZATIONS (Histogram + Boxplot)
# ============================================================

# ------------------------------------------------------------
# Histogram explanation:
# A histogram displays the distribution of PM2.5 concentrations.
# Each bar represents how many monitoring stations fall within
# a specific range of PM2.5 values.
#
# It reveals the overall shape (e.g., symmetric or skewed),
# concentration of lower or higher values, and presence of extreme
# pollution levels (outliers).
# ------------------------------------------------------------

ggplot(df, aes(AirPollution)) +
  geom_histogram(bins = 20, fill = "steelblue", color = "white") +
  theme_minimal() +
  labs(
    title = "PM2.5 - Europe 2023",
    x = "Air Pollution (µg/m³)",
    y = "Frequency"
  )


# ------------------------------------------------------------
# Boxplot explanation:
# A boxplot summarizes the PM2.5 distribution using the median,
# interquartile range (IQR), and whiskers representing the spread
# of non-outlier values. Points outside the whiskers represent
# potential pollution outliers.
#
# This plot helps identify asymmetry, variability, and unusually
# high PM2.5 values that may be caused by local hotspots.
# ------------------------------------------------------------

ggplot(df, aes(x = "", y = AirPollution)) +
  geom_boxplot(fill = "orange") +
  theme_minimal() +
  labs(
    title = "Boxplot of Air Pollution Levels",
    y = "Air Pollution (µg/m³)",
    x = ""
  )


# ============================================================
# 6. MEAN PM2.5 BY COUNTRY
# ============================================================

cat("\n=== Mean Air Pollution by Country (PM2.5) ===\n")

country_means <- df %>%
  group_by(Country) %>%
  summarise(
    Mean_Air = mean(AirPollution, na.rm = TRUE),
    N = n()
  )

print(country_means)


# ============================================================
# 7. HYPOTHESIS TESTING (ONE-SIDED: Is mean > EU/WHO limit?)
# ============================================================

EU_limit <- 25
WHO_limit <- 5

# ------------------------------------------------------------
# Hypothesis testing explanation:
#
# Purpose:
# Assess whether a country's mean PM2.5 concentration exceeds
# regulatory thresholds (WHO = 5 µg/m³, EU = 25 µg/m³).
#
# We apply a ONE-SIDED one-sample t-test:
#
#   H0 (Null Hypothesis):     mean_PM2.5 ≤ limit
#   H1 (Alternative):          mean_PM2.5 > limit
#
# A t-test evaluates whether the difference between the sample
# mean and the regulatory limit is greater than one would expect
# by random sampling variability.
#
# Test statistic:
#       t = (mean - limit) / (sd / sqrt(n))
#
# p-value interpretation:
# - p < 0.05: evidence that mean PM2.5 is above the limit.
# - p ≥ 0.05: no statistical evidence that the limit is exceeded.
#
# Requirement of at least two observations:
# The t-test requires an estimate of standard deviation. With only
# one measurement (n = 1), variance cannot be computed; therefore
# the test cannot be performed.
# ------------------------------------------------------------

cat("\n==================================================\n")
cat("ONE-SIDED HYPOTHESIS TESTING (Mean > EU or WHO limit)\n")
cat("==================================================\n")

results <- df %>%
  group_by(Country) %>%
  summarise(
    Mean_Air = mean(AirPollution, na.rm = TRUE),
    N = n(),
    
    # ---- EU one-sided test ----
    EU_t = ifelse(N >= 2,
                  t.test(AirPollution, mu = EU_limit, alternative = "greater")$statistic,
                  NA),
    EU_p = ifelse(N >= 2,
                  t.test(AirPollution, mu = EU_limit, alternative = "greater")$p.value,
                  NA),
    EU_Decision = case_when(
      N < 2 ~ "Not enough data (≥ 2 values required)",
      EU_p < 0.05 ~ "Mean is significantly ABOVE EU limit",
      TRUE ~ "Mean is NOT above EU limit"
    ),
    
    # ---- WHO one-sided test ----
    WHO_t = ifelse(N >= 2,
                   t.test(AirPollution, mu = WHO_limit, alternative = "greater")$statistic,
                   NA),
    WHO_p = ifelse(N >= 2,
                   t.test(AirPollution, mu = WHO_limit, alternative = "greater")$p.value,
                   NA),
    WHO_Decision = case_when(
      N < 2 ~ "Not enough data (≥ 2 values required)",
      WHO_p < 0.05 ~ "Mean is significantly ABOVE WHO limit",
      TRUE ~ "Mean is NOT above WHO limit"
    )
  )

cat("\n=== One-Sided Hypothesis Test Results (Mean > Limit) ===\n")
print(results)



# ============================================================
# 8. PIE CHART WITH PERCENTAGES – WHO Limit Compliance
# ============================================================

# Reclassify into two categories for pie chart
pie_data <- results %>%
  mutate(
    WHO_Category = case_when(
      WHO_Decision == "Mean is significantly ABOVE WHO limit" ~ "Above WHO limit",
      TRUE ~ "Not above WHO limit"
    )
  ) %>%
  count(WHO_Category)

pie_data <- pie_data %>%
  mutate(Percentage = round(n / sum(n) * 100, 1),
         Label = paste0(WHO_Category, "\n", Percentage, "%"))

print(pie_data)

ggplot(pie_data, aes(x = "", y = n, fill = WHO_Category)) +
  geom_col(width = 1, color = "white") +
  coord_polar(theta = "y") +
  geom_text(aes(label = Label),
            position = position_stack(vjust = 0.5),
            size = 5,
            color = "white",
            fontface = "bold") +
  labs(
    title = "Countries Significantly Above the WHO PM2.5 Limit (5 µg/m³)",
    fill = "Category"
  ) +
  theme_void() +
  theme(
    plot.title = element_text(size = 15, face = "bold", hjust = 0.5)
  )



# ============================================================
# 9. STATIC BOXPLOT PER COUNTRY (Ordered high → low PM2.5)
# ============================================================

df <- df %>%
  group_by(Country) %>%
  mutate(MedianCountry = median(AirPollution, na.rm = TRUE)) %>%
  ungroup() %>%
  arrange(desc(MedianCountry))

df$Country <- factor(df$Country, levels = unique(df$Country))

ggplot(df, aes(x = AirPollution, y = Country)) +
  geom_boxplot(fill = "skyblue", alpha = 0.6, color = "black") +
  geom_vline(xintercept = WHO_limit, color = "red", linetype = "dashed", size = 1.2) +
  geom_vline(xintercept = EU_limit, color = "red", size = 1.2) +
  labs(
    title = "PM2.5 Levels by Country (Ordered High → Low)",
    x = "Air Pollution (µg/m³)",
    y = ""
  ) +
  theme_minimal(base_size = 12)



# ============================================================
# 10. INTERACTIVE BOXPLOT (PLOTLY)
# ============================================================

country_stats <- df %>%
  group_by(Country) %>%
  summarise(
    Mean   = mean(AirPollution, na.rm = TRUE),
    Median = median(AirPollution, na.rm = TRUE),
    Min    = min(AirPollution, na.rm = TRUE),
    Max    = max(AirPollution, na.rm = TRUE)
  )

df_plot <- df %>% inner_join(country_stats, by = "Country")
df_plot$Country <- factor(df_plot$Country, levels = country_stats$Country)

p <- plot_ly(
  data = df_plot,
  x = ~AirPollution,
  y = ~Country,
  type = "box",
  orientation = "h",
  boxpoints = "outliers",
  customdata = ~cbind(Mean, Median, Min, Max),
  hovertemplate = paste(
    "<b>%{y}</b><br>",
    "Mean: %{customdata[0]:.2f} µg/m³<br>",
    "Median: %{customdata[1]:.2f} µg/m³<br>",
    "Min: %{customdata[2]:.2f} µg/m³<br>",
    "Max: %{customdata[3]:.2f} µg/m³<br>",
    "<extra></extra>"
  )
)

p <- p %>% layout(
  title = "Interactive PM2.5 Boxplots by Country",
  xaxis = list(title = "PM2.5 [µg/m³]"),
  yaxis = list(title = ""),
  shapes = list(
    list(type = "line",
         x0 = WHO_limit, x1 = WHO_limit,
         y0 = 0, y1 = 1, xref = "x", yref = "paper",
         line = list(color = "red", dash = "dash", width = 2)),
    list(type = "line",
         x0 = EU_limit, x1 = EU_limit,
         y0 = 0, y1 = 1, xref = "x", yref = "paper",
         line = list(color = "red", width = 2))
  )
)

p   # Display interactive graphic
