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

# MLE

# Consider likelihood function
# L_sigma = prod(x_i / sigma^2 * exp( -x_i^2 / (2 * sigma^2) ))
#         = 1 / sigma^2n * prod(x_i * exp( -x_i^2 / (2 * sigma^2) ))

# Log liklihood function (turns product into sum of logs)
# l_sigma = log(1 / sigma^2n) + sum( log(x_i) - x_i / (2 * sigma^2) )
#         = -2n * log(sigma) + sum(log(x_i)) - 1 / (2 * sigma^2) * sum(x_i^2)

# Score function U(sigma) is just first derivative
# U_sigma = dl_dsigma = -2n/sigma + 1/sigma^3 * sum(x_i^2)

# For MLE need to find where derivative is 0

# -2n/sigma_hat + 1/sigma_hat^3 * sum(x_i^2) = 0

# Multiply by sigma_hat^3

# -2n * sigma_hat^2 + sum(x_i^2) = 0

# sigma_hat = sqrt(sum(x_i^2)/2n)

sigma_hat = sqrt(sum(wind_data ^ 2) / (2 * length(wind_data)))
cat("MLE:", sigma_hat, "mph\n")