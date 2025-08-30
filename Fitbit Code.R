library(tidyverse)
library(corrplot)
library(BlandAltmanLeh)
library(forecast)
library(tseries)
library(ggplot2)
library(dplyr)
library(readr)
df <- read_csv("/Users/qixinzhao/Desktop/Fitbit_Cleaned.csv")
df <- df %>%
  rename(
    Steps = `Steps(Fitbit)`,
    SleepDuration = `Sleep duration (hours)`,
    SleepQuality = `Quality of Sleep Score (1-5)`,
    MoodScore = `Mood Score (1-5)`,
    Temp = `Average Temperature (℃)`,
    Weather = `Weather (Sunny/Cloudy/Rainy)`,
    ManualSteps = `Manual Estimation of Steps`
  )
# Check Missing Data
missing_counts <- colSums(is.na(df))
print("Missing values per column:")
print(missing_counts)
# multiple linear regression analysis (MLRA)
model <- lm(Steps ~ SleepDuration + MoodScore + Temp, data = df)
summary(model)
# Bland-Altman analysis (Fitbit vs manual estimation)
bland.altman.plot(df$ManualSteps, df$Steps, main = "Bland-Altman Plot: Manual vs Fitbit Steps")
bland.altman.plot(df$ManualSteps, df$Steps, 
                  main = "Bland-Altman Plot: Manual vs Fitbit Steps",
                  xlab = "Fitbit Steps", 
                  ylab = "Difference (Manual - Fitbit)")
# One-way ANOVA (weather effect on steps)
df$Weather <- as.factor(df$Weather)
aov_result <- aov(Steps ~ Weather, data = df)
summary(aov_result)
# Correlation analysis and visualization matrices
cor_data <- df %>%
  select(Steps, SleepDuration, MoodScore, Temp)
cor_matrix <- cor(cor_data, use = "complete.obs")
corrplot(cor_matrix, method = "circle", addCoef.col = "black", tl.col = "black")
# Check if the model residuals are normally distributed
shapiro.test
# Ensure Date column is in date format and sorted
df <- df %>%
  mutate(Date = as.Date(Date, format = "%Y-%m-%d")) %>%
  arrange(Date)
# Define a function to analyze time series
analyze_ts <- function(ts_data, var_name, ylab_text) {
  cat("--------------------------------------------------")
  cat(paste("Analyzing variable:", var_name, "\n"))
  # Step 1: ADF test for stationarity
  cat("Performing ADF Test for stationarity...\n")
  print(adf.test(ts_data))
  # Step 2: Fit ARIMA model
  cat("Fitting ARIMA model...\n")
  model <- auto.arima(ts_data)
  print(summary(model))
  # Step 3: Forecast next 7 days
  cat("Forecasting next 7 days...\n")
  # Step 4: Plot forecast
  forecast_result <- forecast(model, h = 7)
  print(forecast_result)
  plot(forecast_result,
       ma4in = paste("7-Day Forecast of", var_name),
       xlab = "Time", ylab = ylab_text)
  # Step 5: Residual check
  cat("Checking residuals...\n")
  checkresiduals(model)
  return(model)
}
# Create time series objects
sleep_ts <- ts(df$SleepDuration, frequency = 7)
steps_ts <- ts(df$Steps, frequency = 7)
mood_ts  <- ts(df$MoodScore, frequency = 7)
# Run analysis for each variable
arima_sleep <- analyze_ts(sleep_ts, "Sleep Duration", "Hours")
arima_steps <- analyze_ts(steps_ts, "Steps", "Steps Count")
arima_mood  <- analyze_ts(mood_ts, "Mood Score", "Score (1-5)")
# Shapiro-Wilk normality test for residuals
shapiro.test(residuals(arima_sleep))
shapiro.test(residuals(arima_steps))
shapiro.test(residuals(arima_mood))

