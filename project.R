# step 1
library(ggplot2)
library(readr)

data <- read_csv("gold_price_data.csv")

head(data)
summary(data)

data$Date <- as.Date(data$Date, format="%d/%m/%Y")

str(data$Date)

ggplot(data, aes(x=Date, y=Value)) +
  geom_line(color="blue") +
  labs(title="Gold Price Time Series",
       x="Date", y="Gold Price") +
  theme_minimal()

# step 2
summary(data$Value)

library(moments)
variance <- var(data$Value)
skewness <- skewness(data$Value)
kurtosis <- kurtosis(data$Value)

cat("Variance:", variance, "\n")
cat("Skewness:", skewness, "\n")
cat("Kurtosis:", kurtosis, "\n")

ggplot(data, aes(x=Value)) +
  geom_histogram(binwidth=50, fill="blue", color="black", alpha=0.7) +
  labs(title="Histogram of Gold Prices", x="Gold Price", y="Frequency") +
  theme_minimal()

#step 3
library(forecast)
acf(data$Value, main="Autocorrelation Function of Gold Prices")

gold_ts <- ts(data$Value, start=c(1970, 1), frequency=12)

decomposed_add <- decompose(gold_ts, type="additive")
decomposed_mult <- decompose(gold_ts, type="multiplicative")

plot(decomposed_add)

plot(decomposed_mult)

# step 4
acf(data$Value, main="Autocorrelation Function of Gold Prices")

pacf(data$Value, main="Partial Autocorrelation Function of Gold Prices")

#step 5

gold_ts <- ts(data$Value, start=c(1970, 1), frequency=12)

train_length <- round(0.8 * length(gold_ts))  # 0.8 training data
train <- window(gold_ts, end=c(1970 + (train_length-1)/12))
test <- window(gold_ts, start=c(1970 + train_length/12))

naive_model <- naive(train, h=length(test))
accuracy(naive_model, test)
plot(naive_model, main="Naive Forecast")

rw_drift_model <- rwf(train, drift=TRUE, h=length(test))
accuracy(rw_drift_model, test)
plot(rw_drift_model, main="Random Walk with Drift Forecast")

ets_model <- ets(train)
ets_forecast <- forecast(ets_model, h=length(test))
accuracy(ets_forecast, test)
plot(ets_forecast, main="Exponential Smoothing Forecast")

arima_model <- auto.arima(train)
arima_forecast <- forecast(arima_model, h=length(test))
accuracy(arima_forecast, test)
plot(arima_forecast, main="ARIMA Forecast")

#step 6
# Train ARIMA and ETS models
arima_model <- auto.arima(train)
ets_model <- ets(train)

# Generate forecasts
arima_forecast <- forecast(arima_model, h=length(test))
ets_forecast <- forecast(ets_model, h=length(test))

# Combine forecasts with weights (adjust w1 and w2 for optimal performance)
w1 <- 0.7  # Weight for ARIMA
w2 <- 0.3  # Weight for ETS
custom_forecast <- w1 * arima_forecast$mean + w2 * ets_forecast$mean

# Evaluate Custom Forecast
custom_rmse <- sqrt(mean((test - custom_forecast)^2))
cat("Custom Forecast RMSE:", custom_rmse, "\n")

# Plot Custom Forecast
plot(test, col="black", lwd=2, main="Custom Forecast vs Actual")
lines(custom_forecast, col="blue", lwd=2, lty=2)
legend("topright", legend=c("Actual", "Custom Forecast"), col=c("black", "blue"), lty=1:2, lwd=2)

# step 7
# Define short-term and long-term horizons
short_term <- 1:6  # First 6 months of the test set
long_term <- 7:length(test)  # Remaining test set

# Calculate RMSE for each model on short-term and long-term horizons
calculate_rmse <- function(actual, forecast) {
  sqrt(mean((actual - forecast)^2))
}

# Naive model
short_naive_rmse <- calculate_rmse(test[short_term], naive_model$mean[short_term])
long_naive_rmse <- calculate_rmse(test[long_term], naive_model$mean[long_term])

# Random Walk with Drift
short_rw_rmse <- calculate_rmse(test[short_term], rw_drift_model$mean[short_term])
long_rw_rmse <- calculate_rmse(test[long_term], rw_drift_model$mean[long_term])

# ETS model
short_ets_rmse <- calculate_rmse(test[short_term], ets_forecast$mean[short_term])
long_ets_rmse <- calculate_rmse(test[long_term], ets_forecast$mean[long_term])

# ARIMA model
short_arima_rmse <- calculate_rmse(test[short_term], arima_forecast$mean[short_term])
long_arima_rmse <- calculate_rmse(test[long_term], arima_forecast$mean[long_term])

# Custom model
short_custom_rmse <- calculate_rmse(test[short_term], custom_forecast[short_term])
long_custom_rmse <- calculate_rmse(test[long_term], custom_forecast[long_term])

# Print results
cat("Short-term RMSE:\n")
cat("Naive:", short_naive_rmse, "\nRandom Walk with Drift:", short_rw_rmse,
    "\nETS:", short_ets_rmse, "\nARIMA:", short_arima_rmse, "\nCustom:", short_custom_rmse, "\n")

cat("\nLong-term RMSE:\n")
cat("Naive:", long_naive_rmse, "\nRandom Walk with Drift:", long_rw_rmse,
    "\nETS:", long_ets_rmse, "\nARIMA:", long_arima_rmse, "\nCustom:", long_custom_rmse, "\n")

# Define horizons to evaluate (1 to length of test set)
horizons <- seq(1, length(test), by=3)  # Evaluate RMSE at intervals of 3 months

# Function to compute RMSE for a given horizon
rmse_by_horizon <- function(actual, forecast, horizons) {
  sapply(horizons, function(h) {
    sqrt(mean((actual[1:h] - forecast[1:h])^2))
  })
}

# Calculate RMSE for each model at different horizons
rmse_naive <- rmse_by_horizon(test, naive_model$mean, horizons)
rmse_rw <- rmse_by_horizon(test, rw_drift_model$mean, horizons)
rmse_ets <- rmse_by_horizon(test, ets_forecast$mean, horizons)
rmse_arima <- rmse_by_horizon(test, arima_forecast$mean, horizons)
rmse_custom <- rmse_by_horizon(test, custom_forecast, horizons)

# Combine RMSE results into a data frame for plotting
rmse_df <- data.frame(
  Horizon = horizons,
  Naive = rmse_naive,
  RandomWalk = rmse_rw,
  ETS = rmse_ets,
  ARIMA = rmse_arima,
  Custom = rmse_custom
)

# Plot RMSE vs. Horizon
library(ggplot2)
rmse_df_long <- tidyr::pivot_longer(rmse_df, -Horizon, names_to = "Model", values_to = "RMSE")

ggplot(rmse_df_long, aes(x=Horizon, y=RMSE, color=Model)) +
  geom_line(linewidth=1) +
  labs(title="RMSE vs. Forecast Horizon", x="Horizon (Months)", y="RMSE") +
  theme_minimal()
