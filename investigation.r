# Wind data is stored as CSV, x column contains wind speeds
wind_data = read.csv("wind.csv")[["x"]]

# Histogram to summarise data
pdf("DailyMean.pdf", width=6, height=4)
par(mar=c(4,4,1,1)+0.1)
hist(wind_data, main=NULL, xlab="Daily average wind speed (mph)")
suppress <- dev.off()

# Summary statistics
cat("\nWind Speed Summary Statistics\n")
cat("Mean:", mean(wind_data), "mph\n")
cat("Standard deviation:", sd(wind_data), "mph\n")
cat("Median:", median(wind_data), "mph\n")
cat("Quantiles:", quantile(wind_data), "(all mph)\n\n")

# MLE
# Derivated in report
sigma_hat = sqrt(sum(wind_data ^ 2) / (2 * length(wind_data)))
cat("MLE:", sigma_hat, "mph\n")

ese = sqrt(sigma_hat^2 / (4 * length(wind_data)))
cat("ese:", ese, "mph\n")

# From NCST Table 5
z025 = 1.96

# 95% confidence interval for sigma_hat
I095 = c(sigma_hat - z025 * ese, sigma_hat + z025 * ese)
cat("Confidence Interval: [", I095[1], ",", I095[2], "] mph\n\n")

# Define functions used for parts 5, 6, 7
source("simulate.r")
source("probability.r")

# This ensures random simulations are consistent between runs
set.seed(21)

sample_size = 1000 # Given by problem
Y_prime = simulate(sigma_hat, sample_size)

pdf("PredictionMean.pdf", width=6, height=4)
par(mar=c(4,4,1,1)+0.1)
hist(Y_prime, main=NULL,
xlab="Predicted mean wind speed in next 1000 days (mph)")
suppress <- dev.off()

cat("\nPredicted Mean Summary Statistics\n")
cat("Mean:", mean(Y_prime), "mph\n")
cat("Standard deviation:", sd(Y_prime), "mph\n")
cat("Variance:", var(Y_prime), "mph\n")
cat("Median:", median(Y_prime), "mph\n")
cat("Quantiles:", quantile(Y_prime), "(all mph)\n\n")

cat("For MLE\n")
prob = prob_S_from_means(sd(wind_data), sample_size, Y_prime)

# Recalculate with lower estimate
cat("For lower estimate\n")
Y_prime = simulate(I095[1], sample_size)
prob = prob_S_from_means(sd(wind_data), sample_size, Y_prime)

# Recalculate with lower estimate
cat("For upper estimate\n")
Y_prime = simulate(I095[2], sample_size)
prob = prob_S_from_means(sd(wind_data), sample_size, Y_prime)
