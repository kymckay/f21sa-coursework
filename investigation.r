# Wind data is stored as CSV, x column contains wind speeds
wind_data = read.csv("wind.csv")[["x"]]

# Histogram to summarise data
hist(wind_data)

# Summary statistics
cat("Wind Speed Summary Statistics\n")
cat("Mean:", mean(wind_data), "mph\n")
cat("Standard deviation:", sd(wind_data), "mph\n")
cat("Median:", median(wind_data), "mph\n")
cat("Quantiles:", quantile(wind_data), "(all mph)\n")