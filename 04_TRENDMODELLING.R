# ============================================================
# 1️⃣ Load required libraries
# ============================================================
# These packages cover data import, transformation, plotting, and forecasting
install.packages(c("readxl","tidyr","dplyr","tidyverse","forecast","prophet"))
library(readxl)
library(tidyr)
library(dplyr)
library(tidyverse)
library(forecast)
library(prophet)

# ============================================================
# 2️⃣ Read Excel sheets (update path)
# ============================================================
file_path <- "C:/data/data.xlsx"   # <-- change this
# Sheets: "pr" (precipitation), "tas" (temperature) 
# Data available on https://climateknowledgeportal.worldbank.org/download-data
pr  <- read_excel(file_path, sheet = "pr")
tas <- read_excel(file_path, sheet = "tas")

# ============================================================
# 3️⃣ Convert both sheets to long format and merge
# ============================================================
pr_long <- pr %>%
  pivot_longer(cols = -c(code, name),
               names_to = "Year",
               values_to = "Precipitation") %>%
  mutate(Year = as.numeric(substr(Year,1,4)))

tas_long <- tas %>%
  pivot_longer(cols = -c(code, name),
               names_to = "Year",
               values_to = "Temperature") %>%
  mutate(Year = as.numeric(substr(Year,1,4)))

data <- left_join(pr_long, tas_long, by = c("code","name","Year"))
head(data)

# ============================================================
# 4️⃣ Choose variable for modelling
# ============================================================
# You can switch between "Temperature" or "Precipitation"
var <- "Temperature"
df <- data %>%
  select(Year, all_of(var)) %>%
  rename(Value = all_of(var))

# ============================================================
# 5️⃣ Linear Regression Model
# ============================================================
model_lin <- lm(Value ~ Year, data = df)
summary(model_lin)

# Prediction years
target_years <- data.frame(Year = c(2027,2030,2035,2040,2045,2050,2075,2100))
target_years$Linear <- predict(model_lin, newdata = target_years)

# Plot
ggplot(df,aes(Year,Value)) +
  geom_point(color="blue") +
  geom_smooth(method="lm",se=TRUE,color="red") +
  geom_point(data=target_years,aes(Year,Linear),color="darkgreen",size=3) +
  labs(title=paste("Linear Trend of",var,"(Ecuador, 1950–2023)"),
       subtitle="Predictions for 2030–2100", y=var, x="Year") +
  theme_minimal()

# ============================================================
# 6️⃣ ARIMA Model
# ============================================================
ts_data <- ts(df$Value, start=min(df$Year), frequency=1)
fit_arima <- auto.arima(ts_data)
summary(fit_arima)

# Forecast to 2100
forecast_arima <- forecast(fit_arima, h=2100-max(df$Year))
autoplot(forecast_arima) +
  labs(title=paste("ARIMA Forecast for",var),
       x="Year",y=var)+theme_minimal()

# Extract forecasted values for target years
years_ahead <- target_years$Year - max(df$Year)
target_years$ARIMA <- forecast_arima$mean[years_ahead]

# ============================================================
# 7️⃣ Prophet Model
# ============================================================
df_prophet <- data.frame(ds=as.Date(paste0(df$Year,"-01-01")), y=df$Value)
m <- prophet(df_prophet)
future <- make_future_dataframe(m, periods=2100-2023, freq="year")
forecast_prophet <- predict(m,future)

# Plot Prophet results
plot(m,forecast_prophet)
prophet_plot_components(m,forecast_prophet)

# Extract Prophet predictions for selected years
prophet_selected <- forecast_prophet %>%
  mutate(Year = as.numeric(format(ds,"%Y"))) %>%
  filter(Year %in% target_years$Year) %>%
  select(Year, yhat)

# Merge Prophet predictions
target_years <- left_join(target_years, prophet_selected, by="Year")
colnames(target_years)[ncol(target_years)] <- "Prophet"

# ============================================================
# 8️⃣ Combine all predictions in one summary dataframe
# ============================================================
summary_results <- target_years %>%
  select(Year, Linear, ARIMA, Prophet)

View(summary_results)

# ============================================================
# 9️⃣ Interpretation and validation metrics
# ============================================================
# 1. Linear model:
#      - Slope (β1): yearly rate of change (e.g., °C/year)
#      - R²: proportion of variance explained
#      - p-value: trend significance
#
# 2. ARIMA model:
#      - AIC (Akaike Information Criterion): model fit (lower = better)
#      - RMSE (Root Mean Square Error): average error
#      - Residuals should resemble white noise
#
# 3. Prophet model:
#      - Flexible nonlinear trend with uncertainty intervals
#      - Metrics: MAPE, RMSE (via performance_metrics if desired)
#      - Components: trend(t) + seasonality(t) + error(t)
#
# Together, these three approaches allow comparison between
# deterministic (linear), stochastic (ARIMA), and nonlinear (Prophet)
# projections for climate variables.

