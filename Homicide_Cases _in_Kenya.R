# Load necessary packages
library(forecast)
library(ggplot2)

# Create a data frame with the new homicide data
homicide_data <- data.frame(
  Year = 2002:2022,
  Homicide_Number = c(1354,1234,1395,1260,1286,1281,1413, 2218,2239,2641,
                      2761,2878, 2649, 2648, 2751, 2774, 2856,2082, 2786,
                      3281, 3056)
)

# Convert data frame to time series
homicide_ts <- ts(homicide_data$Homicide_Number,
                  start = 2002, frequency = 1)

# Visualize the time series data
autoplot(homicide_ts,
         main = "Homicide Numbers Over Past 20 Years.",
         color = 'blue', size = 1) +
  xlab("Year") +
  ylab("Homicide Number")


# Fit an ARIMA model to the time series data
arima_model <- auto.arima(homicide_ts, seasonal = TRUE)

# Generate forecast for the next 5 years
forecast_values <- forecast(arima_model, h = 5)

# Print the ARIMA model summary
print(summary(arima_model))

# Plot line graph of predicted values with trend
plot(forecast_values, main = "Predicted Homicide Numbers with Trend")

# Print the forecasted values
print(forecast_values)

# Plot the forecasted values
autoplot(forecast_values,
         main = "Forecasted Homicide Numbers") +
  xlab("Year") +
  ylab("Homicide Number") +
  autolayer(homicide_ts, series = "Observed",
            color = "blue",size = 1) +
  autolayer(forecast_values$mean,
            series = "Forecast", color = "red") +
  autolayer(forecast_values$lower,
            series = "95% Confidence Interval",
            color = "green",size = 1) +
  autolayer(forecast_values$upper,
            series = NULL, color = "purple",size = 1) +
  guides(color = guide_legend(title = "Legend")) +
  theme(legend.position = "bottom")
