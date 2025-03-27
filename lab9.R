library(tidyverse)
filename <- "sample9.csv"
data <- read.csv(filename)

my_subset <- data[7001:7500, ,drop=FALSE]
time_series <- ts(my_subset$V1)

plot(time_series, main = "Time Series of my Dataset", ylab = "Values", xlab = "Time")


mean_ts <- mean(time_series, na.rm = TRUE)
print(paste("Mean of the time series:", mean_ts))

acf(time_series, main = "Autocorrelation Function of the Time Series")

spectrum(time_series, main = "Periodogram of the Time Series - Log Scale")
spectrum(time_series, main = "Periodogram of the Time Series - Linear Scale", log = "no")

spectrum(time_series, spans = c(5, 5), main = "Smoothed Spectral Density Estimate")